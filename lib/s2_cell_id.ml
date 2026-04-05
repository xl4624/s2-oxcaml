open Core

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

  let make_lookup_tables () =
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

  let lookup = lazy (make_lookup_tables ())
end

type t = Int64.t [@@deriving compare, equal, sexp_of, hash]

let face_bits = 3
let num_faces = 6
let max_level = 30
let pos_bits = (2 * max_level) + 1
let max_size = 1 lsl max_level
let of_int64 (x : Int64.t) : t = x
let zero = 0L
let one = 1L
let none : t = zero
let sentinel : t = -1L (* 0xFFFFFFFFFFFFFFFF *)
let lsb t = Int64.bit_and t (Int64.neg t)

(* Matches [S2CellId::is_valid]: face in range and [(lsb t & mask) <> 0]. *)
let is_valid t =
  let face = Int64.to_int_trunc (Int64.shift_right_logical t pos_bits) in
  face < num_faces
  && face >= 0
  && not (Int64.equal (Int64.bit_and (lsb t) 0x1555555555555555L) zero)
;;

let face t = Int64.to_int_trunc (Int64.shift_right_logical t pos_bits)

(* [lsb_for_level(0) - 1] - used by [is_face] (C++ masks low non-face bits). *)
let pos_mask = Int64.pred (Int64.shift_left one (pos_bits - 1))

(* Low [pos_bits] bits: C++ [id & (~0 >> kFaceBits)]. *)
let pos_id_mask = Int64.pred (Int64.shift_left one pos_bits)
let pos t = Int64.bit_and t pos_id_mask

(* O(1) trailing-zero count via binary search. Equivalent to C++ [absl::countr_zero]. *)
let count_trailing_zeros t =
  if Int64.equal t zero
  then 64
  else (
    let mutable v = lsb t in
    let mutable n = 0 in
    if Int64.equal (Int64.bit_and v 0xFFFFFFFFL) zero
    then (
      v <- Int64.shift_right_logical v 32;
      n <- 32);
    if Int64.equal (Int64.bit_and v 0xFFFFL) zero
    then (
      v <- Int64.shift_right_logical v 16;
      n <- n + 16);
    if Int64.equal (Int64.bit_and v 0xFFL) zero
    then (
      v <- Int64.shift_right_logical v 8;
      n <- n + 8);
    if Int64.equal (Int64.bit_and v 0xFL) zero
    then (
      v <- Int64.shift_right_logical v 4;
      n <- n + 4);
    if Int64.equal (Int64.bit_and v 0x3L) zero
    then (
      v <- Int64.shift_right_logical v 2;
      n <- n + 2);
    if Int64.equal (Int64.bit_and v one) zero then n <- n + 1;
    n)
;;

let level t =
  (* level = max_level - (trailing_zeros(t) / 2) *)
  let ctz = count_trailing_zeros t in
  max_level - (ctz lsr 1)
;;

let is_leaf t = not (Int64.equal (Int64.bit_and t one) zero)
let is_face t = Int64.equal (Int64.bit_and t pos_mask) zero

let from_face face =
  if face < 0 || face >= num_faces
  then None
  else
    Some
      (Int64.bit_or
         (Int64.shift_left (Int64.of_int face) pos_bits)
         (Int64.shift_left one (pos_bits - 1)))
;;

let from_face_exn face =
  match from_face face with
  | Some t -> t
  | None -> raise_s [%message "S2CellId.from_face_exn: face out of range" (face : int)]
;;

let size_ij level = 1 lsl (max_level - level)
let size_st level = S2_coords.ij_to_st_min (size_ij level)
let lsb_for_level level = Int64.shift_left one (2 * (max_level - level))

let from_face_ij face i j =
  let mutable n = Int64.shift_left (Int64.of_int face) (pos_bits - 1) in
  let mutable bits = face land Hilbert.swap_mask in
  let lookup = Lazy.force Hilbert.lookup in
  let mask = (1 lsl Hilbert.lookup_bits) - 1 in
  for k = 7 downto 0 do
    bits
    <- bits + (((i lsr (k * Hilbert.lookup_bits)) land mask) lsl (Hilbert.lookup_bits + 2));
    bits <- bits + (((j lsr (k * Hilbert.lookup_bits)) land mask) lsl 2);
    bits <- lookup.pos.(bits);
    let shifted_bits = Stdlib.( lsr ) bits 2 in
    let shift_amount = k * 2 * Hilbert.lookup_bits in
    n <- Int64.bit_or n (Int64.shift_left (Int64.of_int shifted_bits) shift_amount);
    bits <- bits land (Hilbert.swap_mask lor Hilbert.invert_mask)
  done;
  Int64.bit_or (Int64.shift_left n 1) one
;;

let from_point (p : R3_vector.t) =
  let face, u, v = S2_coords.xyz_to_face_uv p in
  let i = S2_coords.st_to_ij (S2_coords.uv_to_st u) in
  let j = S2_coords.st_to_ij (S2_coords.uv_to_st v) in
  from_face_ij face i j
;;

let from_latlng ll = from_point (S2_latlng.to_point ll)
let id (t : t) = t
let to_int64 = id

let parent t =
  if is_face t
  then None
  else (
    let lsb_val = lsb t in
    let new_lsb = Int64.shift_left lsb_val 2 in
    Some (Int64.bit_or (Int64.bit_and t (Int64.neg new_lsb)) new_lsb))
;;

let parent_exn t =
  match parent t with
  | Some p -> p
  | None ->
    raise_s [%message "S2CellId.parent_exn: already a face cell" ~id:(id t : int64)]
;;

let parent_level t level =
  let lsb_val = lsb_for_level level in
  Int64.bit_or (Int64.bit_and t (Int64.neg lsb_val)) lsb_val
;;

(* C++ [(face << kPosBits) + (pos | 1)] then [parent(level)]. *)
let from_face_pos_level face pos level =
  let cell = Int64.(shift_left (of_int face) pos_bits + bit_or pos one) in
  parent_level cell level
;;

(** Hilbert subtree range for [t] at a target [level] (C++ [child_begin(level)]). *)
let child_begin_at_level t level = Int64.(t - lsb t + lsb_for_level level)

(** Upper bound of Hilbert subtree (C++ [child_end(level)]). *)
let child_end_at_level t level = Int64.(t + lsb t + lsb_for_level level)

let child_begin t =
  let old_lsb = lsb t in
  Int64.(t - old_lsb + shift_right_logical old_lsb 2)
;;

let child_end t =
  let old_lsb = lsb t in
  Int64.(t + old_lsb + shift_right_logical old_lsb 2)
;;

(** First cell at [level] on the global Hilbert curve ([S2CellId::Begin]). *)
let hilbert_begin level = child_begin_at_level (from_face_exn 0) level

(** One past the last cell at [level] on face 5 ([S2CellId::End]). *)
let hilbert_end level = child_end_at_level (from_face_exn 5) level

(* [S2CellId::child]: [t + (2*pos - 3) * (lsb t >> 2)]. *)
let child t pos =
  if is_leaf t || pos < 0 || pos > 3
  then None
  else (
    let new_lsb = Int64.shift_right_logical (lsb t) 2 in
    let factor = Int64.of_int ((2 * pos) - 3) in
    Some Int64.(t + (factor * new_lsb)))
;;

let child_exn t pos =
  match child t pos with
  | Some c -> c
  | None ->
    raise_s
      [%message
        "S2CellId.child_exn: invalid leaf cell or child index"
          ~id:(id t : int64)
          (pos : int)]
;;

let child_position t =
  let lsb_val = lsb t in
  Int64.to_int_trunc (Int64.shift_right_logical t (count_trailing_zeros lsb_val + 1))
  land 3
;;

let child_position_level t level =
  Int64.to_int_trunc (Int64.shift_right_logical t ((2 * (max_level - level)) + 1)) land 3
;;

let range_min t = Int64.(t - pred (lsb t))
let range_max t = Int64.(t + pred (lsb t))

let contains t other =
  let lo = range_min t in
  let hi = range_max t in
  Stdlib.Int64.unsigned_compare other lo >= 0
  && Stdlib.Int64.unsigned_compare other hi <= 0
;;

let intersects t other =
  let lo_t = range_min t in
  let hi_t = range_max t in
  let lo_o = range_min other in
  let hi_o = range_max other in
  Stdlib.Int64.unsigned_compare lo_o hi_t <= 0
  && Stdlib.Int64.unsigned_compare hi_o lo_t >= 0
;;

let next t = Int64.(t + shift_left (lsb t) 1)
let prev t = Int64.(t - shift_left (lsb t) 1)
let wrap_offset = Int64.shift_left (Int64.of_int num_faces) pos_bits

let next_wrap t =
  let n = next t in
  if face n < num_faces && face n >= 0 then n else Int64.(n - wrap_offset)
;;

let prev_wrap t =
  let p = prev t in
  if face p < num_faces && face p >= 0 then p else Int64.(p + wrap_offset)
;;

(* [S2CellId::advance]: clamp then [id + (uint64_t)steps << shift]. *)
let advance t steps =
  if Int64.(equal steps 0L)
  then t
  else (
    let shift = (2 * (max_level - level t)) + 1 in
    let steps =
      if Int64.(steps < 0L)
      then (
        let min_steps = Int64.neg (Int64.shift_right_logical t shift) in
        if Int64.(compare steps min_steps) < 0 then min_steps else steps)
      else (
        let max_steps = Int64.shift_right_logical Int64.(wrap_offset + lsb t - t) shift in
        if Stdlib.Int64.unsigned_compare steps max_steps > 0 then max_steps else steps)
    in
    Int64.(t + shift_left steps shift))
;;

(* [S2CellId::advance_wrap]: reduce [steps] mod wrap only when past hemisphere bound.
   Unlike [advance], the positive branch uses [wrap - id] without [+ lsb] so that
   we never land exactly on End(level). *)
let advance_wrap t steps =
  if Int64.(equal steps 0L)
  then t
  else (
    let shift = (2 * (max_level - level t)) + 1 in
    let steps =
      if Int64.(compare steps 0L) < 0
      then (
        let min_steps = Int64.neg (Int64.shift_right_logical t shift) in
        if Int64.(compare steps min_steps) < 0
        then (
          let step_wrap = Int64.shift_right_logical wrap_offset shift in
          let steps = Int64.rem steps step_wrap in
          if Int64.(compare steps min_steps) < 0 then Int64.(steps + step_wrap) else steps)
        else steps)
      else (
        let max_steps = Int64.shift_right_logical Int64.(wrap_offset - t) shift in
        if Stdlib.Int64.unsigned_compare steps max_steps > 0
        then (
          let step_wrap = Int64.shift_right_logical wrap_offset shift in
          let steps = Int64.rem steps step_wrap in
          if Stdlib.Int64.unsigned_compare steps max_steps > 0
          then Int64.(steps - step_wrap)
          else steps)
        else steps)
    in
    Int64.(t + shift_left steps shift))
;;

let distance_from_begin t =
  let shift = (2 * (max_level - level t)) + 1 in
  Int64.shift_right_logical t shift
;;

let[@inline] max_u x y = if Stdlib.Int64.unsigned_compare x y >= 0 then x else y

let get_common_ancestor_level t other =
  let xor_bits = Int64.bit_xor t other in
  let bits = max_u xor_bits (max_u (lsb t) (lsb other)) in
  let bw = 64 - Int64.to_int_trunc (Int64.clz bits) in
  let x = Stdlib.max (61 - bw) (-1) in
  if x < 0 then -1 else x / 2
;;

let rec shrink_maximum_tile id limit =
  if Stdlib.Int64.unsigned_compare (range_max id) limit < 0
  then id
  else shrink_maximum_tile (child_exn id 0) limit
;;

let rec grow_maximum_tile id ~start ~limit =
  if is_face id
  then id
  else (
    let p = parent_exn id in
    if Stdlib.Int64.unsigned_compare (range_min p) start <> 0
       || Stdlib.Int64.unsigned_compare (range_max p) limit >= 0
    then id
    else grow_maximum_tile p ~start ~limit)
;;

let maximum_tile t limit =
  let start = range_min t in
  if Stdlib.Int64.unsigned_compare start (range_min limit) >= 0
  then limit
  else if Stdlib.Int64.unsigned_compare (range_max t) limit >= 0
  then shrink_maximum_tile t limit
  else grow_maximum_tile t ~start ~limit
;;

let to_face_ij_orientation t =
  let mutable i = 0 in
  let mutable j = 0 in
  let face_val = face t in
  let mutable bits = face_val land Hilbert.swap_mask in
  let lookup = Lazy.force Hilbert.lookup in
  for k = 7 downto 0 do
    let nbits =
      if k = 7 then max_level - (7 * Hilbert.lookup_bits) else Hilbert.lookup_bits
    in
    let shift_amount = (k * 2 * Hilbert.lookup_bits) + 1 in
    let shifted = Int64.shift_right_logical t shift_amount in
    let mask = Int64.pred (Int64.shift_left one (2 * nbits)) in
    let hilbert_pos = Int64.to_int_trunc (Int64.bit_and shifted mask) in
    bits <- bits + (hilbert_pos lsl 2);
    bits <- lookup.ij.(bits);
    i <- i + ((bits lsr (Hilbert.lookup_bits + 2)) lsl (k * Hilbert.lookup_bits));
    j
    <- j
       + (((bits lsr 2) land ((1 lsl Hilbert.lookup_bits) - 1))
          lsl (k * Hilbert.lookup_bits));
    bits <- bits land (Hilbert.swap_mask lor Hilbert.invert_mask)
  done;
  (* For non-leaf cells the Hilbert position suffix "10" reverses kSwapMask.
     C++: [if (lsb() & 0x1111111111111110ULL) bits ^= kSwapMask]. *)
  let orientation =
    if not (Int64.equal (Int64.bit_and (lsb t) 0x1111111111111110L) Int64.zero)
    then bits lxor Hilbert.swap_mask
    else bits
  in
  face_val, i, j, orientation
;;

let get_center_si_ti t =
  let face, i, j, _ = to_face_ij_orientation t in
  let delta =
    if is_leaf t
    then 1
    else (
      let i_shifted = Int64.to_int_trunc (Int64.shift_right_logical t 2) in
      let xor_val = i lxor i_shifted in
      if xor_val land 1 <> 0 then 2 else 0)
  in
  face, (i lsl 1) + delta, (j lsl 1) + delta
;;

let to_center_uv t =
  let _face, si, ti = get_center_si_ti t in
  let s = S2_coords.si_ti_to_st si in
  let t_val = S2_coords.si_ti_to_st ti in
  R2_point.create
    ~x:(Float_u.of_float (S2_coords.st_to_uv s))
    ~y:(Float_u.of_float (S2_coords.st_to_uv t_val))
;;

let to_point_raw t : R3_vector.t =
  let face, si, ti = get_center_si_ti t in
  S2_coords.face_si_ti_to_xyz face si ti
;;

let to_point t : R3_vector.t = R3_vector.normalize (to_point_raw t) [@nontail]

(* C++ [S2CellId::ToToken] / [FromToken]: hex without low trailing zero nibbles; id 0 -> ["X"]. *)
let to_token t =
  if Int64.equal t 0L
  then "X"
  else (
    let num_zero_digits = count_trailing_zeros t / 4 in
    let shifted = Int64.shift_right_logical t (4 * num_zero_digits) in
    let num_digits = 16 - num_zero_digits in
    let buf = Bytes.create num_digits in
    let hex = "0123456789abcdef" in
    let mutable v = shifted in
    for d = num_digits - 1 downto 0 do
      let nib = Int64.to_int_trunc (Int64.bit_and v 0xfL) in
      Bytes.set buf d (Stdlib.String.get hex nib);
      v <- Int64.shift_right_logical v 4
    done;
    Bytes.to_string buf)
;;

let from_token s =
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
          loop
            (i + 1)
            (shift - 4)
            (Int64.bit_or acc (Int64.shift_left (Int64.of_int d) shift)))
    in
    loop 0 60 0L)
;;

let to_string t =
  if not (is_valid t)
  then sprintf "Invalid: %016Lx" (id t)
  else (
    let out = Buffer.create 34 in
    Buffer.add_string out (sprintf "%d/" (face t));
    for current_level = 1 to level t do
      let child_pos = child_position_level t current_level in
      Buffer.add_char out "0123".[child_pos]
    done;
    Buffer.contents out)
;;
