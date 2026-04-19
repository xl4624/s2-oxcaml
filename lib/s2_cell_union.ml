open Core

(* Internally we store S2_cell_id.t array (unboxed int64# array). *)
type t = { cell_ids : S2_cell_id.t array }

let sexp_of_t t =
  let n = Array.length t.cell_ids in
  let mutable i = 0 in
  let acc = ref [] in
  while i < n do
    acc := Sexp.Atom (S2_cell_id.to_token t.cell_ids.(i)) :: !acc;
    i <- i + 1
  done;
  Sexp.List (List.rev !acc)
;;

(* {1 Internal helpers} *)

(* Unsigned comparison for S2_cell_id.t. Cell ids use the full 64-bit range and must be
   compared as unsigned values for correct ordering on the Hilbert curve. *)
let[@inline] unsigned_compare (a : S2_cell_id.t) (b : S2_cell_id.t) : int =
  Stdlib_upstream_compatible.Int64_u.unsigned_compare (S2_cell_id.id a) (S2_cell_id.id b)
;;

let[@inline] unsigned_lt a b = unsigned_compare a b < 0
let[@inline] unsigned_le a b = unsigned_compare a b <= 0

(* Returns true if a lies entirely before b on the Hilbert curve. *)
let entirely_precedes a b = unsigned_lt (S2_cell_id.range_max a) (S2_cell_id.range_min b)

(* Binary search for the first cell whose range might overlap target. *)
let lower_bound (ids : S2_cell_id.t array) ~lo ~hi (target : S2_cell_id.t) =
  let mutable lo = lo in
  let mutable hi = hi in
  while lo < hi do
    let mid = lo + ((hi - lo) / 2) in
    if entirely_precedes ids.(mid) target then lo <- mid + 1 else hi <- mid
  done;
  lo
;;

(* Returns true if the given four distinct cells have a common parent. *)
let are_siblings a b c d =
  let open Int64_u.O in
  let a = S2_cell_id.id a in
  let b = S2_cell_id.id b in
  let c = S2_cell_id.id c in
  let d = S2_cell_id.id d in
  if not (Int64_u.equal (a lxor b lxor c) d)
  then false
  else (
    let d_lsb = d land Int64_u.neg d in
    let mask_bits = d_lsb lsl 1 in
    let mask = Int64_u.lnot (mask_bits + (mask_bits lsl 1)) in
    let id_masked = d land mask in
    Int64_u.equal (a land mask) id_masked
    && Int64_u.equal (b land mask) id_masked
    && Int64_u.equal (c land mask) id_masked
    && not (S2_cell_id.is_face (S2_cell_id.of_int64 d)))
;;

(* Array.sort is not templated for bits64, so we need our own. *)
let sort_unsigned (arr : S2_cell_id.t array) =
  let rec qsort lo hi =
    if lo >= hi
    then ()
    else (
      let pivot = arr.(lo + ((hi - lo) / 2)) in
      let mutable i = lo in
      let mutable j = hi in
      while i <= j do
        while unsigned_compare arr.(i) pivot < 0 do
          i <- i + 1
        done;
        while unsigned_compare arr.(j) pivot > 0 do
          j <- j - 1
        done;
        if i <= j
        then (
          (Array.swap [@kind bits64]) arr i j;
          i <- i + 1;
          j <- j - 1)
      done;
      qsort lo j;
      qsort i hi)
  in
  let n = Array.length arr in
  if n > 1 then qsort 0 (n - 1)
;;

(* {1 Growable buffer for S2_cell_id.t} *)

type cid_buf =
  { mutable data : S2_cell_id.t array
  ; mutable len : int
  }

let cid_buf_create () = { data = Array.create ~len:8 S2_cell_id.none; len = 0 }

let cid_buf_push buf id =
  let cap = Array.length buf.data in
  if buf.len >= cap
  then (
    let new_cap = cap * 2 in
    let new_data = Array.create ~len:new_cap S2_cell_id.none in
    (Array.unsafe_blit [@kind bits64])
      ~src:buf.data
      ~dst:new_data
      ~src_pos:0
      ~dst_pos:0
      ~len:cap;
    buf.data <- new_data);
  buf.data.(buf.len) <- id;
  buf.len <- buf.len + 1
;;

let cid_buf_to_array buf = (Array.sub [@kind bits64]) buf.data ~pos:0 ~len:buf.len

(* {1 Constructors} *)

let empty () = { cell_ids = [||] }

(* True when the current top of [out] already contains [id], so [id] can be
   dropped entirely. *)
let[@inline] is_contained_or_absorbed (out : S2_cell_id.t array) out_len id =
  out_len > 0 && S2_cell_id.contains out.(out_len - 1) id
;;

(* True when the top three cells of [out] plus [id] form a complete set of
   four siblings that should collapse into their common parent. *)
let[@inline] are_merged_siblings (out : S2_cell_id.t array) out_len id =
  out_len >= 3 && are_siblings out.(out_len - 3) out.(out_len - 2) out.(out_len - 1) id
;;

(* Append [id] to the [(out, out_len)] accumulator, applying the
   ancestor-absorbs-descendant and four-siblings-collapse rules. Assumes ids
   are pushed in sorted order; returns the new length. *)
let[@inline] push_normalized (out : S2_cell_id.t array) out_len id =
  if is_contained_or_absorbed out out_len id
  then out_len
  else (
    let mutable out_len = out_len in
    let mutable id = id in
    while out_len > 0 && S2_cell_id.contains id out.(out_len - 1) do
      out_len <- out_len - 1
    done;
    while are_merged_siblings out out_len id do
      id <- S2_cell_id.parent_exn id;
      out_len <- out_len - 3
    done;
    out.(out_len) <- id;
    out_len + 1)
;;

let normalize_ids (ids : S2_cell_id.t array) =
  sort_unsigned ids;
  let n = Array.length ids in
  if n = 0
  then [||]
  else (
    let out = Array.create ~len:n S2_cell_id.none in
    let mutable out_len = 0 in
    for idx = 0 to n - 1 do
      out_len <- push_normalized out out_len ids.(idx)
    done;
    (Array.sub [@kind bits64]) out ~pos:0 ~len:out_len)
;;

let create ids =
  let ids = (Array.copy [@kind bits64]) ids in
  { cell_ids = normalize_ids ids }
;;

let of_raw_owned ids = { cell_ids = normalize_ids ids }
let from_verbatim ids = { cell_ids = (Array.copy [@kind bits64]) ids }
let of_verbatim_owned ids = { cell_ids = ids }

let from_begin_end begin_id end_id =
  let result = cid_buf_create () in
  let mutable id = S2_cell_id.maximum_tile begin_id end_id in
  while not (S2_cell_id.equal id end_id) do
    cid_buf_push result id;
    id <- S2_cell_id.maximum_tile (S2_cell_id.next id) end_id
  done;
  { cell_ids = cid_buf_to_array result }
;;

let from_min_max min_id max_id = from_begin_end min_id (S2_cell_id.next max_id)

let whole_sphere () =
  { cell_ids =
      [| S2_cell_id.from_face_exn 0
       ; S2_cell_id.from_face_exn 1
       ; S2_cell_id.from_face_exn 2
       ; S2_cell_id.from_face_exn 3
       ; S2_cell_id.from_face_exn 4
       ; S2_cell_id.from_face_exn 5
      |]
  }
;;

(* {1 Accessors} *)

let cell_ids_raw t = (Array.copy [@kind bits64]) t.cell_ids
let num_cells t = Array.length t.cell_ids
let is_empty t = Array.length t.cell_ids = 0
let cell_id t i = t.cell_ids.(i)

(* {1 Validation} *)

let is_valid t =
  let ids = t.cell_ids in
  let n = Array.length ids in
  if n > 0 && not (S2_cell_id.is_valid ids.(0))
  then false
  else (
    let mutable ok = true in
    let mutable i = 1 in
    while i < n && ok do
      if not (S2_cell_id.is_valid ids.(i))
      then ok <- false
      else if not
                (unsigned_lt
                   (S2_cell_id.range_max ids.(i - 1))
                   (S2_cell_id.range_min ids.(i)))
      then ok <- false;
      i <- i + 1
    done;
    ok)
;;

let is_normalized t =
  let ids = t.cell_ids in
  let n = Array.length ids in
  if n > 0 && not (S2_cell_id.is_valid ids.(0))
  then false
  else (
    let mutable ok = true in
    let mutable i = 1 in
    while i < n && ok do
      if not (S2_cell_id.is_valid ids.(i))
      then ok <- false
      else (
        let prev_max = S2_cell_id.range_max ids.(i - 1) in
        let curr_min = S2_cell_id.range_min ids.(i) in
        if not
             (Int64_u.compare (S2_cell_id.id prev_max) (S2_cell_id.id curr_min) < 0
              && not (i >= 3 && are_siblings ids.(i - 3) ids.(i - 2) ids.(i - 1) ids.(i))
             )
        then ok <- false);
      i <- i + 1
    done;
    ok)
;;

(* {1 Normalize / Denormalize} *)

let normalize t = { cell_ids = normalize_ids ((Array.copy [@kind bits64]) t.cell_ids) }

let denormalize t ~min_level ~level_mod =
  let result = cid_buf_create () in
  let n = Array.length t.cell_ids in
  for idx = 0 to n - 1 do
    let id = t.cell_ids.(idx) in
    let level = S2_cell_id.level id in
    let mutable new_level = Int.max level min_level in
    if level_mod > 1
    then (
      new_level
      <- new_level + ((S2_cell_id.max_level - (new_level - min_level)) % level_mod);
      new_level <- Int.min S2_cell_id.max_level new_level);
    if new_level = level
    then cid_buf_push result id
    else (
      let end_id = S2_cell_id.child_end_at_level id new_level in
      let mutable ci = S2_cell_id.child_begin_at_level id new_level in
      while not (S2_cell_id.equal ci end_id) do
        cid_buf_push result ci;
        ci <- S2_cell_id.next ci
      done)
  done;
  cid_buf_to_array result
;;

(* {1 Containment and Intersection} *)

let contains_cell_id t id =
  let ids = t.cell_ids in
  let n = Array.length ids in
  let i = lower_bound ids ~lo:0 ~hi:n id in
  i < n && S2_cell_id.contains ids.(i) id
;;

let intersects_cell_id t id =
  let ids = t.cell_ids in
  let n = Array.length ids in
  let i = lower_bound ids ~lo:0 ~hi:n id in
  i < n && S2_cell_id.intersects ids.(i) id
;;

let contains_union t other =
  if is_empty other
  then true
  else if is_empty t
  then false
  else (
    let ids = t.cell_ids in
    let n = Array.length ids in
    let mutable ok = true in
    let mutable i = 0 in
    let mutable j = 0 in
    let other_n = Array.length other.cell_ids in
    while j < other_n && ok do
      let y_id = other.cell_ids.(j) in
      if entirely_precedes ids.(i) y_id
      then (
        i <- lower_bound ids ~lo:(i + 1) ~hi:n y_id;
        if i >= n
        then ok <- false
        else if not (S2_cell_id.contains ids.(i) y_id)
        then ok <- false
        else j <- j + 1)
      else if not (S2_cell_id.contains ids.(i) y_id)
      then ok <- false
      else j <- j + 1
    done;
    ok)
;;

let intersects_union t other =
  let ids = t.cell_ids in
  let n = Array.length ids in
  let other_ids = other.cell_ids in
  let other_n = Array.length other_ids in
  let mutable found = false in
  let mutable i = 0 in
  let mutable j = 0 in
  while i < n && j < other_n && not found do
    if entirely_precedes ids.(i) other_ids.(j)
    then i <- lower_bound ids ~lo:(i + 1) ~hi:n other_ids.(j)
    else if entirely_precedes other_ids.(j) ids.(i)
    then j <- lower_bound other_ids ~lo:(j + 1) ~hi:other_n ids.(i)
    else found <- true
  done;
  found
;;

let contains_cell t cell = contains_cell_id t (S2_cell.id cell)
let intersects_cell t cell = intersects_cell_id t (S2_cell.id cell)
let contains_point t p = contains_cell_id t (S2_cell_id.from_point p)

(* {1 Set Operations} *)

(* Both inputs are already sorted and individually normalized. Walk the two
   runs in lockstep, pushing the smaller head through [push_normalized] to
   handle the rare ancestor-absorbs-descendant and four-siblings-collapse
   cases that can arise at the boundary between the two inputs. *)
let union t other =
  let a = t.cell_ids in
  let b = other.cell_ids in
  let na = Array.length a in
  let nb = Array.length b in
  if na = 0
  then { cell_ids = (Array.copy [@kind bits64]) b }
  else if nb = 0
  then { cell_ids = (Array.copy [@kind bits64]) a }
  else (
    let out = Array.create ~len:(na + nb) S2_cell_id.none in
    let mutable out_len = 0 in
    let mutable i = 0 in
    let mutable j = 0 in
    while i < na && j < nb do
      if unsigned_le a.(i) b.(j)
      then (
        out_len <- push_normalized out out_len a.(i);
        i <- i + 1)
      else (
        out_len <- push_normalized out out_len b.(j);
        j <- j + 1)
    done;
    while i < na do
      out_len <- push_normalized out out_len a.(i);
      i <- i + 1
    done;
    while j < nb do
      out_len <- push_normalized out out_len b.(j);
      j <- j + 1
    done;
    { cell_ids = (Array.sub [@kind bits64]) out ~pos:0 ~len:out_len })
;;

let intersection_with_cell_id t id =
  if contains_cell_id t id
  then { cell_ids = [| id |] }
  else (
    let ids = t.cell_ids in
    let n = Array.length ids in
    let result = cid_buf_create () in
    let id_max = S2_cell_id.range_max id in
    let mutable i = lower_bound ids ~lo:0 ~hi:n (S2_cell_id.range_min id) in
    while i < n && unsigned_le ids.(i) id_max do
      cid_buf_push result ids.(i);
      i <- i + 1
    done;
    { cell_ids = cid_buf_to_array result })
;;

let intersection t other =
  let ids = t.cell_ids in
  let n = Array.length ids in
  let other_ids = other.cell_ids in
  let other_n = Array.length other_ids in
  let result = cid_buf_create () in
  let mutable i = 0 in
  let mutable j = 0 in
  while i < n && j < other_n do
    let imin = S2_cell_id.range_min ids.(i) in
    let jmin = S2_cell_id.range_min other_ids.(j) in
    if unsigned_compare imin jmin > 0
    then
      if unsigned_le ids.(i) (S2_cell_id.range_max other_ids.(j))
      then (
        cid_buf_push result ids.(i);
        i <- i + 1)
      else j <- lower_bound other_ids ~lo:(j + 1) ~hi:other_n ids.(i)
    else if unsigned_compare jmin imin > 0
    then
      if unsigned_le other_ids.(j) (S2_cell_id.range_max ids.(i))
      then (
        cid_buf_push result other_ids.(j);
        j <- j + 1)
      else i <- lower_bound ids ~lo:(i + 1) ~hi:n other_ids.(j)
    else if unsigned_lt ids.(i) other_ids.(j)
    then (
      cid_buf_push result ids.(i);
      i <- i + 1)
    else (
      cid_buf_push result other_ids.(j);
      j <- j + 1)
  done;
  let out = cid_buf_to_array result in
  { cell_ids = normalize_ids out }
;;

let rec difference_internal id y result =
  if not (intersects_cell_id y id)
  then cid_buf_push result id
  else if not (contains_cell_id y id)
  then
    for pos = 0 to 3 do
      difference_internal (S2_cell_id.child_exn id pos) y result
    done
;;

let difference t other =
  let result = cid_buf_create () in
  let n = Array.length t.cell_ids in
  for i = 0 to n - 1 do
    difference_internal t.cell_ids.(i) other result
  done;
  { cell_ids = cid_buf_to_array result }
;;

(* {1 Measures} *)

let[@inline] leaf_cells_covered_u t =
  let mutable num_leaves = #0L in
  let n = Array.length t.cell_ids in
  for i = 0 to n - 1 do
    let id = t.cell_ids.(i) in
    let inverted_level = S2_cell_id.max_level - S2_cell_id.level id in
    let shift = inverted_level lsl 1 in
    num_leaves <- Int64_u.O.(num_leaves + Int64_u.shift_left #1L shift)
  done;
  num_leaves
;;

let leaf_cells_covered t = Int64_u.to_int64 (leaf_cells_covered_u t)

let average_based_area t =
  let open Float_u.O in
  S2_cell.average_area S2_cell_id.max_level * Int64_u.to_float (leaf_cells_covered_u t)
;;

let approx_area t =
  let n = Array.length t.cell_ids in
  let mutable acc = #0.0 in
  for i = 0 to n - 1 do
    acc <- Float_u.O.(acc + S2_cell.approx_area (S2_cell.of_cell_id t.cell_ids.(i)))
  done;
  acc
;;

let exact_area t =
  let n = Array.length t.cell_ids in
  let mutable acc = #0.0 in
  for i = 0 to n - 1 do
    acc <- Float_u.O.(acc + S2_cell.exact_area (S2_cell.of_cell_id t.cell_ids.(i)))
  done;
  acc
;;

(* {1 Bounding} *)

let cap_bound t =
  let n = Array.length t.cell_ids in
  if n = 0
  then S2_cap.empty
  else (
    let mutable cx = #0.0 in
    let mutable cy = #0.0 in
    let mutable cz = #0.0 in
    for i = 0 to n - 1 do
      let id = t.cell_ids.(i) in
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
      let id = t.cell_ids.(i) in
      cap <- S2_cap.add_cap cap (S2_cell.cap_bound (S2_cell.of_cell_id id))
    done;
    cap)
;;

let rect_bound t =
  let n = Array.length t.cell_ids in
  let mutable bound = S2_latlng_rect.empty in
  for i = 0 to n - 1 do
    let id = t.cell_ids.(i) in
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
    let mutable ok = true in
    let mutable i = 0 in
    while i < n && ok do
      if not (S2_cell_id.equal t.cell_ids.(i) other.cell_ids.(i)) then ok <- false;
      i <- i + 1
    done;
    ok)
;;
