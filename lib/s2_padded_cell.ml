open Core

module Internal = struct
  (* kSwapMask and kInvertMask from s2coords_internal.h.  Mirrors S2_cell_id.Hilbert. *)
  let swap_mask = 0x01
  let invert_mask = 0x02

  (* kIJtoPos[orientation][2*i + j] -> pos *)
  let ij_to_pos =
    [| [| 0; 1; 3; 2 |]; [| 0; 3; 1; 2 |]; [| 2; 3; 1; 0 |]; [| 2; 1; 3; 0 |] |]
  ;;

  (* kPosToIJ[orientation][pos] -> 2*i + j *)
  let pos_to_ij =
    [| [| 0; 1; 3; 2 |]; [| 0; 2; 3; 1 |]; [| 3; 2; 0; 1 |]; [| 3; 1; 0; 2 |] |]
  ;;

  let pos_to_orientation = [| swap_mask; 0; 0; invert_mask + swap_mask |]
end

type t =
  #{ id : S2_cell_id.t
   ; padding : float#
   ; bound : R2_rect.t
   ; middle : R2_rect.t
   ; i_lo : int
   ; j_lo : int
   ; orientation : int
   ; level : int
   }

let[@zero_alloc ignore] sexp_of_t t : Sexp.t =
  Sexp.List
    [ Sexp.List [ Sexp.Atom "id"; S2_cell_id.sexp_of_t t.#id ]
    ; Sexp.List [ Sexp.Atom "padding"; sexp_of_float (Float_u.to_float t.#padding) ]
    ; Sexp.List [ Sexp.Atom "bound"; R2_rect.sexp_of_t t.#bound ]
    ; Sexp.List [ Sexp.Atom "middle"; R2_rect.sexp_of_t t.#middle ]
    ; Sexp.List [ Sexp.Atom "i_lo"; sexp_of_int t.#i_lo ]
    ; Sexp.List [ Sexp.Atom "j_lo"; sexp_of_int t.#j_lo ]
    ; Sexp.List [ Sexp.Atom "orientation"; sexp_of_int t.#orientation ]
    ; Sexp.List [ Sexp.Atom "level"; sexp_of_int t.#level ]
    ]
;;

let[@inline] id t = t.#id
let[@inline] padding t = t.#padding
let[@inline] level t = t.#level
let[@inline] orientation t = t.#orientation
let[@inline] bound t = t.#bound

(* Mirrors the C++ constructor [S2PaddedCell(S2CellId, double)].  For a top-level face we
   precompute both [bound] and [middle]; otherwise [middle] is populated lazily by
   [middle] the first time it is queried (the C++ code uses a mutable field). *)
let create id ~padding =
  if S2_cell_id.is_face id
  then (
    let limit = Float_u.O.(#1.0 + padding) in
    let neg_limit = Float_u.neg limit in
    let neg_padding = Float_u.neg padding in
    let bound =
      R2_rect.create_intervals_exn
        ~x:(R1_interval.create ~lo:neg_limit ~hi:limit)
        ~y:(R1_interval.create ~lo:neg_limit ~hi:limit)
    in
    let middle =
      R2_rect.create_intervals_exn
        ~x:(R1_interval.create ~lo:neg_padding ~hi:padding)
        ~y:(R1_interval.create ~lo:neg_padding ~hi:padding)
    in
    #{ id
     ; padding
     ; bound
     ; middle
     ; i_lo = 0
     ; j_lo = 0
     ; orientation = S2_cell_id.face id land Internal.swap_mask
     ; level = 0
     })
  else (
    let _face, i, j, orientation = S2_cell_id.to_face_ij_orientation id in
    let level = S2_cell_id.level id in
    let ij_size = S2_cell_id.size_ij level in
    (* Mirror [S2CellId::IJLevelToBoundUV].  Snapping [i] and [j] to the cell's lower
       corner gives the (u, v)-bound of the cell before padding. *)
    let extract_interval coord =
      let lo_ij = coord land -ij_size in
      let hi_ij = lo_ij + ij_size in
      let lo = S2_coords.st_to_uv (S2_coords.ij_to_st_min lo_ij) in
      let hi = S2_coords.st_to_uv (S2_coords.ij_to_st_min hi_ij) in
      R1_interval.create ~lo ~hi
    in
    let raw_bound =
      R2_rect.create_intervals_exn ~x:(extract_interval i) ~y:(extract_interval j)
    in
    let bound = R2_rect.expanded_scalar raw_bound padding in
    #{ id
     ; padding
     ; bound
     ; middle = R2_rect.empty
     ; i_lo = i land -ij_size
     ; j_lo = j land -ij_size
     ; orientation
     ; level
     })
;;

(* Lazy [middle] accessor.  Because [t] is an unboxed record we cannot mutate it in place
   like the C++ version does; instead we recompute on demand when the cached field is
   empty.  Callers that need repeated access can store the result locally. *)
let middle t =
  if R2_rect.is_empty t.#middle
  then (
    let ij_size = S2_cell_id.size_ij t.#level in
    let u = S2_coords.st_to_uv (S2_coords.si_ti_to_st ((2 * t.#i_lo) + ij_size)) in
    let v = S2_coords.st_to_uv (S2_coords.si_ti_to_st ((2 * t.#j_lo) + ij_size)) in
    let open Float_u.O in
    R2_rect.create_intervals_exn
      ~x:(R1_interval.create ~lo:(u - t.#padding) ~hi:(u + t.#padding))
      ~y:(R1_interval.create ~lo:(v - t.#padding) ~hi:(v + t.#padding)))
  else t.#middle
;;

(* Compute the (i, j) child indices given a Hilbert traversal position.  This mirrors the
   inline [GetChildIJ] in s2padded_cell.h. *)
let child_ij_of_pos t ~pos =
  let ij = Internal.pos_to_ij.(t.#orientation).(pos) in
  #(ij lsr 1, ij land 1)
;;

let child_ij parent ~i ~j =
  let pos = Internal.ij_to_pos.(parent.#orientation).((2 * i) + j) in
  let new_id = S2_cell_id.child_exn parent.#id pos in
  let new_level = parent.#level + 1 in
  let ij_size = S2_cell_id.size_ij new_level in
  let i_lo = parent.#i_lo + (i * ij_size) in
  let j_lo = parent.#j_lo + (j * ij_size) in
  let orientation = parent.#orientation lxor Internal.pos_to_orientation.(pos) in
  (* For each child, one corner of the bound is taken directly from the parent while the
     diagonally opposite corner is taken from [middle].  Read the parent middle once. *)
  let mid = middle parent in
  let parent_x = R2_rect.x parent.#bound in
  let parent_y = R2_rect.y parent.#bound in
  let mid_x = R2_rect.x mid in
  let mid_y = R2_rect.y mid in
  let x =
    if i = 1
    then R1_interval.create ~lo:(R1_interval.lo mid_x) ~hi:(R1_interval.hi parent_x)
    else R1_interval.create ~lo:(R1_interval.lo parent_x) ~hi:(R1_interval.hi mid_x)
  in
  let y =
    if j = 1
    then R1_interval.create ~lo:(R1_interval.lo mid_y) ~hi:(R1_interval.hi parent_y)
    else R1_interval.create ~lo:(R1_interval.lo parent_y) ~hi:(R1_interval.hi mid_y)
  in
  let bound = R2_rect.create_intervals_exn ~x ~y in
  #{ id = new_id
   ; padding = parent.#padding
   ; bound
   ; middle = R2_rect.empty
   ; i_lo
   ; j_lo
   ; orientation
   ; level = new_level
   }
;;

let center t =
  let ij_size = S2_cell_id.size_ij t.#level in
  let si = (2 * t.#i_lo) + ij_size in
  let ti = (2 * t.#j_lo) + ij_size in
  R3_vector.normalize (S2_coords.face_si_ti_to_xyz (S2_cell_id.face t.#id) si ti)
;;

let entry_vertex t =
  let mutable i = t.#i_lo in
  let mutable j = t.#j_lo in
  if t.#orientation land Internal.invert_mask <> 0
  then (
    let ij_size = S2_cell_id.size_ij t.#level in
    i <- i + ij_size;
    j <- j + ij_size);
  R3_vector.normalize
    (S2_coords.face_si_ti_to_xyz (S2_cell_id.face t.#id) (2 * i) (2 * j))
;;

let exit_vertex t =
  let mutable i = t.#i_lo in
  let mutable j = t.#j_lo in
  let ij_size = S2_cell_id.size_ij t.#level in
  if t.#orientation = 0 || t.#orientation = Internal.swap_mask + Internal.invert_mask
  then i <- i + ij_size
  else j <- j + ij_size;
  R3_vector.normalize
    (S2_coords.face_si_ti_to_xyz (S2_cell_id.face t.#id) (2 * i) (2 * j))
;;

(* Port of S2PaddedCell::ShrinkToFit.  The 1.5 * epsilon padding compensates for the
   roundoff error in [uv_to_st]. *)
let shrink_to_fit t rect =
  let ij_size = S2_cell_id.size_ij t.#level in
  let rect_x = R2_rect.x rect in
  let rect_y = R2_rect.y rect in
  let quick_reject =
    if t.#level = 0
    then R1_interval.contains rect_x #0.0 || R1_interval.contains rect_y #0.0
    else (
      let cu = S2_coords.st_to_uv (S2_coords.si_ti_to_st ((2 * t.#i_lo) + ij_size)) in
      let cv = S2_coords.st_to_uv (S2_coords.si_ti_to_st ((2 * t.#j_lo) + ij_size)) in
      R1_interval.contains rect_x cu || R1_interval.contains rect_y cv)
  in
  if quick_reject
  then t.#id
  else (
    let extra = Float_u.O.(t.#padding + (#1.5 * Float_u.epsilon_float ())) in
    let padded = R2_rect.expanded_scalar rect extra in
    let padded_x = R2_rect.x padded in
    let padded_y = R2_rect.y padded in
    let shrink lo padded_lo padded_hi =
      let ij_min = Int.max lo (S2_coords.st_to_ij (S2_coords.uv_to_st padded_lo)) in
      let ij_max =
        Int.min (lo + ij_size - 1) (S2_coords.st_to_ij (S2_coords.uv_to_st padded_hi))
      in
      ij_min, ij_min lxor ij_max
    in
    let i_min, i_xor =
      shrink t.#i_lo (R1_interval.lo padded_x) (R1_interval.hi padded_x)
    in
    let j_min, j_xor =
      shrink t.#j_lo (R1_interval.lo padded_y) (R1_interval.hi padded_y)
    in
    (* [level_msb] is positive so [Int.ceil_log2] matches [absl::bit_width].  We use the
       explicit bit count to avoid depending on builtins we may not have. *)
    let level_msb = ((i_xor lor j_xor) lsl 1) + 1 in
    let bit_width n =
      let rec loop n acc = if n = 0 then acc else loop (n lsr 1) (acc + 1) in
      loop n 0
    in
    let level = S2_cell_id.max_level - (bit_width level_msb - 1) in
    if level <= t.#level
    then t.#id
    else
      S2_cell_id.parent_level
        (S2_cell_id.from_face_ij (S2_cell_id.face t.#id) i_min j_min)
        level)
;;
