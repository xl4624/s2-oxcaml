open Core

(* Tables and lookup arrays used to convert between [(i, j)] cell coordinates and Hilbert
   curve positions in chunks of [lookup_bits] bits at a time. The tables encode the four
   Hilbert-curve orientations: [swap_mask] swaps the [i]/[j] axes when set, [invert_mask]
   reverses the traversal direction.

   [pos_to_ij.(orient).(k)] gives the [(i, j)] offset of the [k]-th cell in Hilbert order
   under orientation [orient] (packed as [2*i+j]). [pos_to_orientation.(k)] is the
   orientation tweak applied when descending into the [k]-th child.

   The two lookup arrays let us convert [lookup_bits]-bit chunks of [(i, j)] to a Hilbert
   offset (and vice versa) in a single table lookup rather than recursing through four
   levels bit-by-bit. *)
module Hilbert = struct
  let swap_mask = 0x01
  let invert_mask = 0x02

  let pos_to_ij =
    [| [| 0; 1; 3; 2 |]; [| 0; 2; 3; 1 |]; [| 3; 2; 0; 1 |]; [| 3; 1; 0; 2 |] |]
  ;;

  let pos_to_orientation = [| swap_mask; 0; 0; invert_mask + swap_mask |]
  let lookup_bits = 4

  type lookup_tables =
    { pos : int array
    ; ij : int array
    }

  let[@zero_alloc ignore] make_lookup_tables () =
    let lookup =
      { pos = Array.create ~len:(1 lsl ((2 * lookup_bits) + 2)) 0
      ; ij = Array.create ~len:(1 lsl ((2 * lookup_bits) + 2)) 0
      }
    in
    let rec init_lookup_cell level i j orig_orientation pos orientation =
      if level = lookup_bits
      then (
        let ij_val = (i lsl lookup_bits) + j in
        lookup.pos.((ij_val lsl 2) + orig_orientation) <- (pos lsl 2) + orientation;
        lookup.ij.((pos lsl 2) + orig_orientation) <- (ij_val lsl 2) + orientation)
      else (
        let level = level + 1 in
        let i = i lsl 1 in
        let j = j lsl 1 in
        let pos = pos lsl 2 in
        let r = pos_to_ij.(orientation) in
        for k = 0 to 3 do
          init_lookup_cell
            level
            (i + (r.(k) lsr 1))
            (j + (r.(k) land 1))
            orig_orientation
            (pos + k)
            (orientation lxor pos_to_orientation.(k))
        done)
    in
    init_lookup_cell 0 0 0 0 0 0;
    init_lookup_cell 0 0 0 swap_mask 0 swap_mask;
    init_lookup_cell 0 0 0 invert_mask 0 invert_mask;
    init_lookup_cell 0 0 0 (swap_mask lor invert_mask) 0 (swap_mask lor invert_mask);
    lookup
  ;;

  let lookup = make_lookup_tables ()
end

type t = int64# [@@deriving unboxed_option { sentinel = true }]

let%template[@alloc a = (heap, stack)] [@inline] [@zero_alloc ignore] sexp_of_t t : Sexp.t
  =
  Sexp.Atom (Int64_u.to_string t) [@exclave_if_stack a]
;;

(* The [@alloc] template above generates a [sexp_of_t__stack] companion that the .mli does
   not re-export; silence the unused-value warning. *)
let _ = sexp_of_t__stack
let[@inline] [@zero_alloc ignore] hash t = Int64.hash (Int64_u.to_int64 t)

let[@inline] [@zero_alloc ignore] hash_fold_t state t =
  Int64.hash_fold_t state (Int64_u.to_int64 t)
;;

let face_bits = 3
let num_faces = 6
let max_level = 30
let pos_bits = (2 * max_level) + 1
let max_size = 1 lsl max_level
let[@inline] [@zero_alloc] of_int64 (x : int64#) : t = x
let[@inline] [@zero_alloc] id (t : t) : int64# = t
let[@inline] [@zero_alloc] to_int64 t = id t
let none : t = #0L
let sentinel : t = -#1L
let[@inline] [@zero_alloc] lsb t = Int64_u.O.(t land Int64_u.neg t)

(* Valid iff the face is in range and the low-bit tag indicates a level between 0 and
   [max_level]. *)
let[@inline] [@zero_alloc] is_valid t =
  let face = Int64_u.to_int_trunc Int64_u.O.(t lsr pos_bits) in
  face < num_faces
  && face >= 0
  && not (Int64_u.equal Int64_u.O.(lsb t land #0x1555555555555555L) #0L)
;;

let[@inline] [@zero_alloc] face t = Int64_u.to_int_trunc Int64_u.O.(t lsr pos_bits)

(* [lsb_for_level(0) - 1]: mask covering all position bits below the face bits. *)
let pos_mask =
  let n = pos_bits - 1 in
  Int64_u.O.((#1L lsl n) - #1L)
;;

(* Mask for the low [pos_bits] bits (all bits below the face field). *)
let pos_id_mask = Int64_u.O.((#1L lsl pos_bits) - #1L)
let[@inline] [@zero_alloc] pos t : int64# = Int64_u.O.(t land pos_id_mask)

(* O(1) trailing-zero count. Uses [Int64_u.ctz] which is unspecified for 0. *)
let[@inline] [@zero_alloc] count_trailing_zeros t =
  if Int64_u.equal t #0L then 64 else Int64_u.to_int_trunc (Int64_u.ctz t)
;;

let[@inline] [@zero_alloc] level t =
  (* level = max_level - (trailing_zeros(t) / 2) *)
  let ctz = count_trailing_zeros t in
  max_level - (ctz lsr 1)
;;

let[@inline] [@zero_alloc] is_leaf t = not (Int64_u.equal Int64_u.O.(t land #1L) #0L)
let[@inline] [@zero_alloc] is_face t = Int64_u.equal Int64_u.O.(t land pos_mask) #0L

let[@inline] [@zero_alloc] from_face_exn face =
  if face < 0 || face >= num_faces
  then (
    match raise_s [%message "S2CellId.from_face_exn: face out of range" (face : int)] with
    | (_ : Nothing.t) -> .)
  else (
    let n = pos_bits - 1 in
    Int64_u.O.((Int64_u.of_int face lsl pos_bits) lor (#1L lsl n)))
;;

let[@inline] [@zero_alloc] size_ij level = 1 lsl (max_level - level)
let[@inline] [@zero_alloc] size_st level = S2_coords.ij_to_st_min (size_ij level)

let[@inline] [@zero_alloc] lsb_for_level level =
  let n = 2 * (max_level - level) in
  Int64_u.O.(#1L lsl n)
;;

(* Walk the [(i, j)] coordinates from most-significant nibble to least-significant in
   4-bit chunks, using [Hilbert.lookup.pos] to convert each chunk plus the running
   orientation bits into the corresponding Hilbert-position nibble. The final id has the
   face in the top bits and [pos | 1] in the low bits, where the trailing "1" marks the
   leaf level. *)
let[@inline] [@zero_alloc] from_face_ij face i j =
  let hi_shift = pos_bits - 1 in
  let mutable n = Int64_u.O.(Int64_u.of_int face lsl hi_shift) in
  let mutable bits = face land Hilbert.swap_mask in
  let lookup = Hilbert.lookup in
  let mask = (1 lsl Hilbert.lookup_bits) - 1 in
  for k = 7 downto 0 do
    bits
    <- bits + (((i lsr (k * Hilbert.lookup_bits)) land mask) lsl (Hilbert.lookup_bits + 2));
    bits <- bits + (((j lsr (k * Hilbert.lookup_bits)) land mask) lsl 2);
    bits <- lookup.pos.(bits);
    let shifted_bits = Stdlib.( lsr ) bits 2 in
    let shift_amount = k * 2 * Hilbert.lookup_bits in
    n <- Int64_u.O.(n lor (Int64_u.of_int shifted_bits lsl shift_amount));
    bits <- bits land (Hilbert.swap_mask lor Hilbert.invert_mask)
  done;
  Int64_u.O.((n lsl 1) lor #1L)
;;

(* Clamps (i, j) to one cell beyond the face boundary, then reprojects via XYZ to find the
   adjacent face and corresponding (i', j'). *)
let[@zero_alloc] from_face_ij_wrap face i j =
  let limit_ij = max_size in
  let i = Int.max (-1) (Int.min limit_ij i) in
  let j = Int.max (-1) (Int.min limit_ij j) in
  let int_i = (2 * (i - (limit_ij / 2))) + 1 in
  let int_j = (2 * (j - (limit_ij / 2))) + 1 in
  let open Float_u.O in
  let scale = #1.0 / Float_u.of_int limit_ij in
  let lim = #1.0 + Float_u.epsilon_float in
  let u =
    Float_util.max_u
      (Float_u.neg lim)
      (Float_util.min_u lim (scale * Float_u.of_int int_i))
  in
  let v =
    Float_util.max_u
      (Float_u.neg lim)
      (Float_util.min_u lim (scale * Float_u.of_int int_j))
  in
  let p = S2_coords.face_uv_to_xyz face u v in
  let new_face = S2_coords.get_face p in
  let new_uv = S2_coords.valid_face_xyz_to_uv new_face p in
  let new_i = S2_coords.st_to_ij (#0.5 * (R2_point.x new_uv + #1.0)) in
  let new_j = S2_coords.st_to_ij (#0.5 * (R2_point.y new_uv + #1.0)) in
  from_face_ij new_face new_i new_j
;;

let[@inline] [@zero_alloc] from_face_ij_same face i j ~same_face =
  if same_face then from_face_ij face i j else from_face_ij_wrap face i j
;;

let[@inline] [@zero_alloc] from_point (p : R3_vector.t) =
  let face = S2_coords.get_face p in
  let uv = S2_coords.valid_face_xyz_to_uv face p in
  let i = S2_coords.st_to_ij (S2_coords.uv_to_st (R2_point.x uv)) in
  let j = S2_coords.st_to_ij (S2_coords.uv_to_st (R2_point.y uv)) in
  from_face_ij face i j
;;

let[@inline] [@zero_alloc] from_latlng ll = from_point (S2_latlng.to_point ll)

let[@inline] [@zero_alloc] parent_exn t =
  if is_face t
  then (
    match
      raise_s
        [%message "S2CellId.parent_exn: already a face cell" ~id:(Int64_u.to_string t)]
    with
    | (_ : Nothing.t) -> .)
  else (
    let lsb_val = lsb t in
    let new_lsb = Int64_u.O.(lsb_val lsl 2) in
    Int64_u.O.(t land Int64_u.neg new_lsb lor new_lsb))
;;

let[@inline] [@zero_alloc] parent_level t level =
  let lsb_val = lsb_for_level level in
  Int64_u.O.(t land Int64_u.neg lsb_val lor lsb_val)
;;

(* Construct from face, Hilbert position, and level: place [pos | 1] at the face position,
   then snap to [level] by calling [parent_level]. *)
let[@inline] [@zero_alloc] from_face_pos_level face (pos : int64#) level =
  let open Int64_u.O in
  let cell = (Int64_u.of_int face lsl pos_bits) + (pos lor #1L) in
  parent_level cell level
;;

(** Hilbert subtree range for [t] at a target [level]: first leaf-descendant boundary. *)
let[@inline] [@zero_alloc] child_begin_at_level t level =
  Int64_u.O.(t - lsb t + lsb_for_level level)
;;

(** Upper bound of Hilbert subtree at a target [level] (exclusive). *)
let[@inline] [@zero_alloc] child_end_at_level t level =
  Int64_u.O.(t + lsb t + lsb_for_level level)
;;

let[@inline] [@zero_alloc] child_begin t =
  let old_lsb = lsb t in
  Int64_u.O.(t - old_lsb + (old_lsb lsr 2))
;;

let[@inline] [@zero_alloc] child_end t =
  let old_lsb = lsb t in
  Int64_u.O.(t + old_lsb + (old_lsb lsr 2))
;;

(** First cell at [level] on the global Hilbert curve (starts at face 0). *)
let[@inline] [@zero_alloc] hilbert_begin level =
  child_begin_at_level (from_face_exn 0) level
;;

(** One past the last cell at [level] on the global Hilbert curve (past face 5). *)
let[@inline] [@zero_alloc] hilbert_end level = child_end_at_level (from_face_exn 5) level

(* [S2CellId::child]: [t + (2*pos - 3) * (lsb t >> 2)]. *)
let[@inline] [@zero_alloc] child_exn t pos =
  if is_leaf t || pos < 0 || pos > 3
  then (
    match
      raise_s
        [%message
          "S2CellId.child_exn: invalid leaf cell or child index"
            ~id:(Int64_u.to_string t)
            (pos : int)]
    with
    | (_ : Nothing.t) -> .)
  else (
    let new_lsb = Int64_u.O.(lsb t lsr 2) in
    let factor = Int64_u.of_int ((2 * pos) - 3) in
    Int64_u.O.(t + (factor * new_lsb)))
;;

let[@inline] [@zero_alloc] child_position t =
  let lsb_val = lsb t in
  let shift = count_trailing_zeros lsb_val + 1 in
  Int64_u.to_int_trunc Int64_u.O.(t lsr shift) land 3
;;

let[@inline] [@zero_alloc] child_position_level t level =
  let shift = (2 * (max_level - level)) + 1 in
  Int64_u.to_int_trunc Int64_u.O.(t lsr shift) land 3
;;

let[@inline] [@zero_alloc] range_min t = Int64_u.O.(t - (lsb t - #1L))
let[@inline] [@zero_alloc] range_max t = Int64_u.O.(t + (lsb t - #1L))

let[@inline] [@zero_alloc] contains t other =
  let lo = range_min t in
  let hi = range_max t in
  Stdlib_upstream_compatible.Int64_u.unsigned_compare other lo >= 0
  && Stdlib_upstream_compatible.Int64_u.unsigned_compare other hi <= 0
;;

let[@inline] [@zero_alloc] intersects t other =
  let lo_t = range_min t in
  let hi_t = range_max t in
  let lo_o = range_min other in
  let hi_o = range_max other in
  Stdlib_upstream_compatible.Int64_u.unsigned_compare lo_o hi_t <= 0
  && Stdlib_upstream_compatible.Int64_u.unsigned_compare hi_o lo_t >= 0
;;

let[@inline] [@zero_alloc] next t = Int64_u.O.(t + (lsb t lsl 1))
let[@inline] [@zero_alloc] prev t = Int64_u.O.(t - (lsb t lsl 1))
let wrap_offset = Int64_u.O.(Int64_u.of_int num_faces lsl pos_bits)

let[@inline] [@zero_alloc] next_wrap t =
  let n = next t in
  if face n < num_faces && face n >= 0 then n else Int64_u.O.(n - wrap_offset)
;;

let[@inline] [@zero_alloc] prev_wrap t =
  let p = prev t in
  if face p < num_faces && face p >= 0 then p else Int64_u.O.(p + wrap_offset)
;;

(* [S2CellId::advance]: clamp then [id + (uint64_t)steps << shift]. *)
let[@inline] [@zero_alloc] advance t (steps : int64#) =
  if Int64_u.equal steps #0L
  then t
  else (
    let shift = (2 * (max_level - level t)) + 1 in
    let steps =
      if Int64_u.O.(steps < #0L)
      then (
        let min_steps = Int64_u.neg Int64_u.O.(t lsr shift) in
        if Int64_u.compare steps min_steps < 0 then min_steps else steps)
      else (
        let max_steps = Int64_u.O.((wrap_offset + lsb t - t) lsr shift) in
        if Stdlib_upstream_compatible.Int64_u.unsigned_compare steps max_steps > 0
        then max_steps
        else steps)
    in
    Int64_u.O.(t + (steps lsl shift)))
;;

(* [S2CellId::advance_wrap]: reduce [steps] mod wrap only when past hemisphere bound.
   Unlike [advance], the positive branch uses [wrap - id] without [+ lsb] so that we never
   land exactly on End(level). *)
let[@inline] [@zero_alloc] advance_wrap t (steps : int64#) =
  if Int64_u.equal steps #0L
  then t
  else (
    let shift = (2 * (max_level - level t)) + 1 in
    let steps =
      if Int64_u.compare steps #0L < 0
      then (
        let min_steps = Int64_u.neg Int64_u.O.(t lsr shift) in
        if Int64_u.compare steps min_steps < 0
        then (
          let step_wrap = Int64_u.O.(wrap_offset lsr shift) in
          let steps = Int64_u.rem steps step_wrap in
          if Int64_u.compare steps min_steps < 0
          then Int64_u.O.(steps + step_wrap)
          else steps)
        else steps)
      else (
        let max_steps = Int64_u.O.((wrap_offset - t) lsr shift) in
        if Stdlib_upstream_compatible.Int64_u.unsigned_compare steps max_steps > 0
        then (
          let step_wrap = Int64_u.O.(wrap_offset lsr shift) in
          let steps = Int64_u.rem steps step_wrap in
          if Stdlib_upstream_compatible.Int64_u.unsigned_compare steps max_steps > 0
          then Int64_u.O.(steps - step_wrap)
          else steps)
        else steps)
    in
    Int64_u.O.(t + (steps lsl shift)))
;;

let[@inline] [@zero_alloc] distance_from_begin t : int64# =
  let shift = (2 * (max_level - level t)) + 1 in
  Int64_u.O.(t lsr shift)
;;

let[@inline] [@zero_alloc] max_u x y =
  if Stdlib_upstream_compatible.Int64_u.unsigned_compare x y >= 0 then x else y
;;

let[@inline] [@zero_alloc] get_common_ancestor_level t other =
  let xor_bits = Int64_u.O.(t lxor other) in
  let bits = max_u xor_bits (max_u (lsb t) (lsb other)) in
  let bw = 64 - Int64_u.to_int_trunc (Int64_u.clz bits) in
  let x = Stdlib.max (61 - bw) (-1) in
  if x < 0 then -1 else x / 2
;;

let[@inline] [@zero_alloc] rec shrink_maximum_tile id limit =
  if Stdlib_upstream_compatible.Int64_u.unsigned_compare (range_max id) limit < 0
  then id
  else shrink_maximum_tile (child_exn id 0) limit
;;

let[@inline] [@zero_alloc] rec grow_maximum_tile id ~start ~limit =
  if is_face id
  then id
  else (
    let p = parent_exn id in
    if Stdlib_upstream_compatible.Int64_u.unsigned_compare (range_min p) start <> 0
       || Stdlib_upstream_compatible.Int64_u.unsigned_compare (range_max p) limit >= 0
    then id
    else grow_maximum_tile p ~start ~limit)
;;

let[@inline] [@zero_alloc] maximum_tile t limit =
  let start = range_min t in
  if Stdlib_upstream_compatible.Int64_u.unsigned_compare start (range_min limit) >= 0
  then limit
  else if Stdlib_upstream_compatible.Int64_u.unsigned_compare (range_max t) limit >= 0
  then shrink_maximum_tile t limit
  else grow_maximum_tile t ~start ~limit
;;

let[@inline] [@zero_alloc] to_face_ij_orientation t =
  let mutable i = 0 in
  let mutable j = 0 in
  let face_val = face t in
  let mutable bits = face_val land Hilbert.swap_mask in
  let lookup = Hilbert.lookup in
  for k = 7 downto 0 do
    let nbits =
      if k = 7 then max_level - (7 * Hilbert.lookup_bits) else Hilbert.lookup_bits
    in
    let shift_amount = (k * 2 * Hilbert.lookup_bits) + 1 in
    let shifted = Int64_u.O.(t lsr shift_amount) in
    let nb2 = 2 * nbits in
    let mask = Int64_u.O.((#1L lsl nb2) - #1L) in
    let hilbert_pos = Int64_u.to_int_trunc Int64_u.O.(shifted land mask) in
    bits <- bits + (hilbert_pos lsl 2);
    bits <- lookup.ij.(bits);
    i <- i + ((bits lsr (Hilbert.lookup_bits + 2)) lsl (k * Hilbert.lookup_bits));
    j
    <- j
       + (((bits lsr 2) land ((1 lsl Hilbert.lookup_bits) - 1))
          lsl (k * Hilbert.lookup_bits));
    bits <- bits land (Hilbert.swap_mask lor Hilbert.invert_mask)
  done;
  (* For non-leaf cells the Hilbert position suffix "10" causes one extra swap. Detect
     this by testing whether any of the bits in 0x1111111111111110 are set in the least
     significant bit of the cell id, then flip swap_mask accordingly. *)
  let orientation =
    if not (Int64_u.equal Int64_u.O.(lsb t land #0x1111111111111110L) #0L)
    then bits lxor Hilbert.swap_mask
    else bits
  in
  #(face_val, i, j, orientation)
;;

(* Returns the cells at [level] that share the closest cell vertex to [t]. Result has 3 or
   4 entries. *)
let[@zero_alloc ignore] vertex_neighbors t level =
  let #(face, i, j, _) = to_face_ij_orientation t in
  let halfsize = size_ij (level + 1) in
  let size = halfsize lsl 1 in
  let ioffset, isame =
    if i land halfsize <> 0 then size, i + size < max_size else -size, i - size >= 0
  in
  let joffset, jsame =
    if j land halfsize <> 0 then size, j + size < max_size else -size, j - size >= 0
  in
  let n0 = parent_level t level in
  let n1 = parent_level (from_face_ij_same face (i + ioffset) j ~same_face:isame) level in
  let n2 = parent_level (from_face_ij_same face i (j + joffset) ~same_face:jsame) level in
  if isame || jsame
  then (
    let n3 =
      parent_level
        (from_face_ij_same face (i + ioffset) (j + joffset) ~same_face:(isame && jsame))
        level
    in
    [| n0; n1; n2; n3 |])
  else [| n0; n1; n2 |]
;;

let[@inline] [@zero_alloc] get_center_si_ti t =
  let #(face, i, j, _) = to_face_ij_orientation t in
  let delta =
    if is_leaf t
    then 1
    else (
      let i_shifted = Int64_u.to_int_trunc Int64_u.O.(t lsr 2) in
      let xor_val = i lxor i_shifted in
      if xor_val land 1 <> 0 then 2 else 0)
  in
  #(face, (i lsl 1) + delta, (j lsl 1) + delta)
;;

let[@inline] [@zero_alloc] to_center_uv t =
  let #(_face, si, ti) = get_center_si_ti t in
  let s = S2_coords.si_ti_to_st si in
  let t_val = S2_coords.si_ti_to_st ti in
  R2_point.create ~x:(S2_coords.st_to_uv s) ~y:(S2_coords.st_to_uv t_val)
;;

let[@inline] [@zero_alloc] to_point_raw t : R3_vector.t =
  let #(face, si, ti) = get_center_si_ti t in
  S2_coords.face_si_ti_to_xyz face si ti
;;

let[@inline] [@zero_alloc] to_point t : R3_vector.t =
  R3_vector.normalize (to_point_raw t) [@nontail]
;;

(* Token: hex representation without low trailing zero nibbles; id 0 encodes as ["X"]. *)
let[@zero_alloc ignore] to_token t =
  if Int64_u.equal t #0L
  then "X"
  else (
    let num_zero_digits = count_trailing_zeros t / 4 in
    let zsh = 4 * num_zero_digits in
    let shifted = Int64_u.O.(t lsr zsh) in
    let num_digits = 16 - num_zero_digits in
    let buf = Bytes.create num_digits in
    let hex = "0123456789abcdef" in
    let mutable v = shifted in
    for d = num_digits - 1 downto 0 do
      let nib = Int64_u.to_int_trunc Int64_u.O.(v land #0xfL) in
      Bytes.set buf d (Stdlib.String.get hex nib);
      v <- Int64_u.O.(v lsr 4)
    done;
    Bytes.to_string buf)
;;

let[@zero_alloc ignore] from_token s =
  if String.equal s "X" || String.is_empty s
  then none
  else if String.length s > 16
  then none
  else (
    let len = String.length s in
    let rec loop i shift acc =
      if i >= len
      then acc
      else (
        let c = s.[i] in
        match
          if Char.(c >= '0' && c <= '9')
          then Some (Char.to_int c - Char.to_int '0')
          else if Char.(c >= 'a' && c <= 'f')
          then Some (10 + Char.to_int c - Char.to_int 'a')
          else if Char.(c >= 'A' && c <= 'F')
          then Some (10 + Char.to_int c - Char.to_int 'A')
          else None
        with
        | None -> none
        | Some d ->
          loop (i + 1) (shift - 4) Int64_u.O.(acc lor (Int64_u.of_int d lsl shift)))
    in
    loop 0 60 #0L)
;;

let[@zero_alloc ignore] to_string t =
  if not (is_valid t)
  then sprintf "Invalid: %016Lx" (Int64_u.to_int64 t)
  else (
    let out = Buffer.create 34 in
    Buffer.add_string out (sprintf "%d/" (face t));
    for current_level = 1 to level t do
      let child_pos = child_position_level t current_level in
      Buffer.add_char out "0123".[child_pos]
    done;
    Buffer.contents out)
;;

let[@inline] [@zero_alloc] compare a b =
  Stdlib_upstream_compatible.Int64_u.unsigned_compare a b
;;

let[@inline] [@zero_alloc] equal a b = Int64_u.equal a b
