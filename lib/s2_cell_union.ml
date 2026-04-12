open Core

(* Internally we store boxed Int64.t array since S2_cell_id.t = int64# cannot live
   in regular arrays or refs. Conversions happen at boundaries. *)
type t = { cell_ids : Int64.t array }

let sexp_of_t t =
  Sexp.List
    (Array.to_list
       (Array.map t.cell_ids ~f:(fun id ->
          Sexp.Atom (S2_cell_id.to_token (S2_cell_id.of_int64 id)))))
;;

(* {1 Internal helpers} *)

let[@inline] to_cid (id : Int64.t) : S2_cell_id.t = S2_cell_id.of_int64 id
let[@inline] of_cid (id : S2_cell_id.t) : Int64.t = S2_cell_id.id id

(* Unsigned comparison for boxed Int64.t. S2CellIds are compared as uint64 in C++. *)
let[@inline] unsigned_compare (a : Int64.t) (b : Int64.t) : int =
  Int64.compare (Int64.bit_xor a Int64.min_value) (Int64.bit_xor b Int64.min_value)
;;

let[@inline] unsigned_lt a b = unsigned_compare a b < 0
let[@inline] unsigned_le a b = unsigned_compare a b <= 0

(* Returns true if a lies entirely before b on the Hilbert curve. *)
let entirely_precedes a b =
  unsigned_lt
    (of_cid (S2_cell_id.range_max (to_cid a)))
    (of_cid (S2_cell_id.range_min (to_cid b)))
;;

(* Binary search for the first cell whose range might overlap target. *)
let lower_bound ids ~lo ~hi (target : Int64.t) =
  let rec loop lo hi =
    if lo >= hi
    then lo
    else (
      let mid = lo + ((hi - lo) / 2) in
      if entirely_precedes ids.(mid) target then loop (mid + 1) hi else loop lo mid)
  in
  loop lo hi
;;

(* Returns true if the given four distinct cells have a common parent. *)
let are_siblings a b c d =
  if not (Int64.equal (Int64.bit_xor (Int64.bit_xor a b) c) d)
  then false
  else (
    let d_lsb = Int64.bit_and d (Int64.neg d) in
    let mask_bits = Int64.shift_left d_lsb 1 in
    let mask = Int64.bit_not (Int64.( + ) mask_bits (Int64.shift_left mask_bits 1)) in
    let id_masked = Int64.bit_and d mask in
    Int64.equal (Int64.bit_and a mask) id_masked
    && Int64.equal (Int64.bit_and b mask) id_masked
    && Int64.equal (Int64.bit_and c mask) id_masked
    && not (S2_cell_id.is_face (to_cid d)))
;;

(* {1 Constructors} *)

let empty () = { cell_ids = [||] }

(* True when the current top of [out] already contains [id], so [id] can be
   dropped entirely. *)
let[@inline] is_contained_or_absorbed out out_len id =
  out_len > 0 && S2_cell_id.contains (to_cid out.(out_len - 1)) (to_cid id)
;;

(* True when the top three cells of [out] plus [id] form a complete set of
   four siblings that should collapse into their common parent. *)
let[@inline] are_merged_siblings out out_len id =
  out_len >= 3 && are_siblings out.(out_len - 3) out.(out_len - 2) out.(out_len - 1) id
;;

let normalize_ids ids =
  Array.sort ids ~compare:unsigned_compare;
  let n = Array.length ids in
  if n = 0
  then [||]
  else (
    let out = Array.create ~len:n 0L in
    let out_len = ref 0 in
    let push id =
      out.(!out_len) <- id;
      incr out_len
    in
    let pop () = decr out_len in
    let top () = out.(!out_len - 1) in
    for idx = 0 to n - 1 do
      let id = ref ids.(idx) in
      if not (is_contained_or_absorbed out !out_len !id)
      then (
        while !out_len > 0 && S2_cell_id.contains (to_cid !id) (to_cid (top ())) do
          pop ()
        done;
        while are_merged_siblings out !out_len !id do
          id := of_cid (S2_cell_id.parent_exn (to_cid !id));
          pop ();
          pop ();
          pop ()
        done;
        push !id)
    done;
    Array.sub out ~pos:0 ~len:!out_len)
;;

let create ids =
  let ids = Array.copy ids in
  { cell_ids = normalize_ids ids }
;;

let from_verbatim ids = { cell_ids = Array.copy ids }

let from_begin_end begin_id end_id =
  let result = Queue.create () in
  let id = ref (of_cid (S2_cell_id.maximum_tile begin_id end_id)) in
  while not (Int64.equal !id (of_cid end_id)) do
    Queue.enqueue result !id;
    id := of_cid (S2_cell_id.maximum_tile (S2_cell_id.next (to_cid !id)) end_id)
  done;
  { cell_ids = Queue.to_array result }
;;

let from_min_max min_id max_id = from_begin_end min_id (S2_cell_id.next max_id)

let whole_sphere () =
  { cell_ids =
      [| of_cid (S2_cell_id.from_face_exn 0)
       ; of_cid (S2_cell_id.from_face_exn 1)
       ; of_cid (S2_cell_id.from_face_exn 2)
       ; of_cid (S2_cell_id.from_face_exn 3)
       ; of_cid (S2_cell_id.from_face_exn 4)
       ; of_cid (S2_cell_id.from_face_exn 5)
      |]
  }
;;

(* {1 Accessors} *)

let cell_ids_raw t = Array.copy t.cell_ids
let num_cells t = Array.length t.cell_ids
let is_empty t = Array.is_empty t.cell_ids
let cell_id t i = to_cid t.cell_ids.(i)

(* {1 Validation} *)

(* Shared driver for [is_valid] and [is_normalized]: every cell must be a
   valid S2CellId, and [pair_ok ids i] is run for every adjacent pair
   [(ids.(i-1), ids.(i))] so each caller can layer its own ordering /
   sibling constraints on top. *)
let validate_cells t ~pair_ok =
  let ids = t.cell_ids in
  let n = Array.length ids in
  if n > 0 && not (S2_cell_id.is_valid (to_cid ids.(0)))
  then false
  else (
    let ok = ref true in
    let i = ref 1 in
    while !i < n && !ok do
      if not (S2_cell_id.is_valid (to_cid ids.(!i)))
      then ok := false
      else if not (pair_ok ids !i)
      then ok := false;
      incr i
    done;
    !ok)
;;

let is_valid t =
  validate_cells t ~pair_ok:(fun ids i ->
    let prev_max = of_cid (S2_cell_id.range_max (to_cid ids.(i - 1))) in
    let curr_min = of_cid (S2_cell_id.range_min (to_cid ids.(i))) in
    unsigned_lt prev_max curr_min)
;;

let is_normalized t =
  validate_cells t ~pair_ok:(fun ids i ->
    let prev_max = of_cid (S2_cell_id.range_max (to_cid ids.(i - 1))) in
    let curr_min = of_cid (S2_cell_id.range_min (to_cid ids.(i))) in
    Int64.( < ) prev_max curr_min
    && not (i >= 3 && are_siblings ids.(i - 3) ids.(i - 2) ids.(i - 1) ids.(i)))
;;

(* {1 Normalize / Denormalize} *)

let normalize t = { cell_ids = normalize_ids (Array.copy t.cell_ids) }

let denormalize t ~min_level ~level_mod =
  let result = Queue.create () in
  Array.iter t.cell_ids ~f:(fun raw_id ->
    let id = to_cid raw_id in
    let level = S2_cell_id.level id in
    let new_level = ref (Int.max level min_level) in
    if level_mod > 1
    then (
      new_level
      := !new_level + ((S2_cell_id.max_level - (!new_level - min_level)) % level_mod);
      new_level := Int.min S2_cell_id.max_level !new_level);
    if !new_level = level
    then Queue.enqueue result raw_id
    else (
      let end_id = of_cid (S2_cell_id.child_end_at_level id !new_level) in
      let ci = ref (of_cid (S2_cell_id.child_begin_at_level id !new_level)) in
      while not (Int64.equal !ci end_id) do
        Queue.enqueue result !ci;
        ci := of_cid (S2_cell_id.next (to_cid !ci))
      done));
  Queue.to_array result
;;

(* {1 Containment and Intersection} *)

let contains_cell_id t id =
  let ids = t.cell_ids in
  let n = Array.length ids in
  let raw = of_cid id in
  let i = lower_bound ids ~lo:0 ~hi:n raw in
  i < n && S2_cell_id.contains (to_cid ids.(i)) id
;;

let intersects_cell_id t id =
  let ids = t.cell_ids in
  let n = Array.length ids in
  let raw = of_cid id in
  let i = lower_bound ids ~lo:0 ~hi:n raw in
  i < n && S2_cell_id.intersects (to_cid ids.(i)) id
;;

let contains_union t other =
  if is_empty other
  then true
  else if is_empty t
  then false
  else (
    let ids = t.cell_ids in
    let n = Array.length ids in
    let ok = ref true in
    let i = ref 0 in
    let j = ref 0 in
    let other_n = Array.length other.cell_ids in
    while !j < other_n && !ok do
      let y_id = other.cell_ids.(!j) in
      if entirely_precedes ids.(!i) y_id
      then (
        i := lower_bound ids ~lo:(!i + 1) ~hi:n y_id;
        if !i >= n
        then ok := false
        else if not (S2_cell_id.contains (to_cid ids.(!i)) (to_cid y_id))
        then ok := false
        else incr j)
      else if not (S2_cell_id.contains (to_cid ids.(!i)) (to_cid y_id))
      then ok := false
      else incr j
    done;
    !ok)
;;

let intersects_union t other =
  let ids = t.cell_ids in
  let n = Array.length ids in
  let other_ids = other.cell_ids in
  let other_n = Array.length other_ids in
  let found = ref false in
  let i = ref 0 in
  let j = ref 0 in
  while !i < n && !j < other_n && not !found do
    if entirely_precedes ids.(!i) other_ids.(!j)
    then i := lower_bound ids ~lo:(!i + 1) ~hi:n other_ids.(!j)
    else if entirely_precedes other_ids.(!j) ids.(!i)
    then j := lower_bound other_ids ~lo:(!j + 1) ~hi:other_n ids.(!i)
    else found := true
  done;
  !found
;;

let contains_cell t cell = contains_cell_id t (S2_cell.id cell)
let intersects_cell t cell = intersects_cell_id t (S2_cell.id cell)
let contains_point t p = contains_cell_id t (S2_cell_id.from_point p)

(* {1 Set Operations} *)

let union t other =
  let combined = Array.append t.cell_ids other.cell_ids in
  { cell_ids = normalize_ids combined }
;;

let intersection_with_cell_id t id =
  if contains_cell_id t id
  then { cell_ids = [| of_cid id |] }
  else (
    let ids = t.cell_ids in
    let n = Array.length ids in
    let result = Queue.create () in
    let id_max = of_cid (S2_cell_id.range_max id) in
    let i = ref (lower_bound ids ~lo:0 ~hi:n (of_cid (S2_cell_id.range_min id))) in
    while !i < n && unsigned_le ids.(!i) id_max do
      Queue.enqueue result ids.(!i);
      incr i
    done;
    { cell_ids = Queue.to_array result })
;;

let intersection t other =
  let ids = t.cell_ids in
  let n = Array.length ids in
  let other_ids = other.cell_ids in
  let other_n = Array.length other_ids in
  let result = Queue.create () in
  let i = ref 0 in
  let j = ref 0 in
  while !i < n && !j < other_n do
    let imin = of_cid (S2_cell_id.range_min (to_cid ids.(!i))) in
    let jmin = of_cid (S2_cell_id.range_min (to_cid other_ids.(!j))) in
    if unsigned_compare imin jmin > 0
    then
      if unsigned_le ids.(!i) (of_cid (S2_cell_id.range_max (to_cid other_ids.(!j))))
      then (
        Queue.enqueue result ids.(!i);
        incr i)
      else j := lower_bound other_ids ~lo:(!j + 1) ~hi:other_n ids.(!i)
    else if unsigned_compare jmin imin > 0
    then
      if unsigned_le other_ids.(!j) (of_cid (S2_cell_id.range_max (to_cid ids.(!i))))
      then (
        Queue.enqueue result other_ids.(!j);
        incr j)
      else i := lower_bound ids ~lo:(!i + 1) ~hi:n other_ids.(!j)
    else if unsigned_lt ids.(!i) other_ids.(!j)
    then (
      Queue.enqueue result ids.(!i);
      incr i)
    else (
      Queue.enqueue result other_ids.(!j);
      incr j)
  done;
  let out = Queue.to_array result in
  { cell_ids = normalize_ids out }
;;

let rec difference_internal raw_id y result =
  let id = to_cid raw_id in
  if not (intersects_cell_id y id)
  then Queue.enqueue result raw_id
  else if not (contains_cell_id y id)
  then
    for pos = 0 to 3 do
      difference_internal (of_cid (S2_cell_id.child_exn id pos)) y result
    done
;;

let difference t other =
  let result = Queue.create () in
  Array.iter t.cell_ids ~f:(fun raw_id -> difference_internal raw_id other result);
  { cell_ids = Queue.to_array result }
;;

(* {1 Measures} *)

let leaf_cells_covered t =
  let num_leaves = ref 0L in
  Array.iter t.cell_ids ~f:(fun raw_id ->
    let id = to_cid raw_id in
    let inverted_level = S2_cell_id.max_level - S2_cell_id.level id in
    num_leaves := Int64.( + ) !num_leaves (Int64.shift_left 1L (inverted_level lsl 1)));
  !num_leaves
;;

let average_based_area t =
  let open Float_u.O in
  S2_cell.average_area S2_cell_id.max_level
  * Float_u.of_float (Int64.to_float (leaf_cells_covered t))
;;

let approx_area t =
  let n = Array.length t.cell_ids in
  let mutable acc = #0.0 in
  for i = 0 to n - 1 do
    acc
    <- Float_u.O.(acc + S2_cell.approx_area (S2_cell.of_cell_id (to_cid t.cell_ids.(i))))
  done;
  acc
;;

let exact_area t =
  let n = Array.length t.cell_ids in
  let mutable acc = #0.0 in
  for i = 0 to n - 1 do
    acc
    <- Float_u.O.(acc + S2_cell.exact_area (S2_cell.of_cell_id (to_cid t.cell_ids.(i))))
  done;
  acc
;;

(* {1 Bounding} *)

(* C++ S2CellUnion::GetCapBound. Computes the area-weighted centroid as the cap
   axis, then expands by adding the bounding cap of each cell. *)
let cap_bound t =
  let n = Array.length t.cell_ids in
  if n = 0
  then S2_cap.empty
  else (
    let mutable cx = #0.0 in
    let mutable cy = #0.0 in
    let mutable cz = #0.0 in
    for i = 0 to n - 1 do
      let id = to_cid t.cell_ids.(i) in
      let area = S2_cell.average_area (S2_cell_id.level id) in
      let p = S2_cell_id.to_point id in
      cx <- Float_u.O.(cx + (R3_vector.x p * area));
      cy <- Float_u.O.(cy + (R3_vector.y p * area));
      cz <- Float_u.O.(cz + (R3_vector.z p * area))
    done;
    let centroid = R3_vector.create ~x:cx ~y:cy ~z:cz in
    let centroid =
      if Float_u.O.(R3_vector.norm2 centroid = #0.0)
      then S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0
      else R3_vector.normalize centroid
    in
    let mutable cap = S2_cap.of_point centroid in
    for i = 0 to n - 1 do
      let id = to_cid t.cell_ids.(i) in
      cap <- S2_cap.add_cap cap (S2_cell.cap_bound (S2_cell.of_cell_id id))
    done;
    cap)
;;

let rect_bound t =
  let n = Array.length t.cell_ids in
  let mutable bound = S2_latlng_rect.empty in
  for i = 0 to n - 1 do
    let id = to_cid t.cell_ids.(i) in
    bound <- S2_latlng_rect.union bound (S2_latlng_rect.from_cell (S2_cell.of_cell_id id))
  done;
  bound
;;

let cell_union_bound t = S2_cap.cell_union_bound (cap_bound t)

(* {1 Comparison} *)

let equal t other =
  let n = Array.length t.cell_ids in
  let m = Array.length other.cell_ids in
  if n <> m
  then false
  else (
    let ok = ref true in
    let i = ref 0 in
    while !i < n && !ok do
      if not (Int64.equal t.cell_ids.(!i) other.cell_ids.(!i)) then ok := false;
      incr i
    done;
    !ok)
;;
