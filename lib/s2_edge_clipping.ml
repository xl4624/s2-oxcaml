open Core

(* Error bound constants for face clipping and rectangle intersection. *)
let face_clip_error_radians = Float_u.O.(#3.0 * Float_u.epsilon_float ())
let face_clip_error_uv_dist = Float_u.O.(#9.0 * Float_u.epsilon_float ())

let face_clip_error_uv_coord =
  Float_u.O.(#9.0 * (#1.0 / Float_u.sqrt #2.0) * Float_u.epsilon_float ())
;;

let intersects_rect_error_uv_dist =
  Float_u.O.(#3.0 * Float_u.sqrt #2.0 * Float_u.epsilon_float ())
;;

let edge_clip_error_uv_coord = Float_u.O.(#2.25 * Float_u.epsilon_float ())
let edge_clip_error_uv_dist = Float_u.O.(#2.25 * Float_u.epsilon_float ())

let shape_index_cell_padding =
  Float_u.O.(#2.0 * (face_clip_error_uv_coord + edge_clip_error_uv_coord))
;;

(* A face segment is an edge clipped to a single cube face. *)
type face_segment =
  { face : int
  ; a : R2_point.t
  ; b : R2_point.t
  }
[@@deriving sexp_of]

type clipped_uv =
  { a : R2_point.t
  ; b : R2_point.t
  }
[@@deriving sexp_of]

(* --- Exact sign tricks on u + v vs w --------------------------------- *)

(* [sum_equals u v w] iff [u + v = w] exactly. *)
let[@inline] sum_equals u v w =
  let open Float_u.O in
  u + v = w && u = w - v && v = w - u
;;

(* [intersects_face n] iff the directed line with normal [n] (expressed in
   (u,v,w) coordinates of some face) intersects the [-1,1]x[-1,1] square of
   that face.  This is true iff |n.x| + |n.y| >= |n.z|, evaluated exactly
   evaluated exactly using floating-point sign checks. *)
let[@inline] intersects_face (n : R3_vector.t) =
  let open Float_u.O in
  let u = Float_u.abs (R3_vector.x n) in
  let v = Float_u.abs (R3_vector.y n) in
  let w = Float_u.abs (R3_vector.z n) in
  v >= w - u && u >= w - v
;;

(* [intersects_opposite_edges n] iff the line L with normal [n] intersects
   two opposite edges of the face (as opposed to adjacent edges or a
   corner), evaluated exactly. *)
let[@inline] intersects_opposite_edges (n : R3_vector.t) =
  let open Float_u.O in
  let u = Float_u.abs (R3_vector.x n) in
  let v = Float_u.abs (R3_vector.y n) in
  let w = Float_u.abs (R3_vector.z n) in
  if Float_u.abs (u - v) <> w
  then Float_u.abs (u - v) >= w
  else if u >= v
  then u - w >= v
  else v - w >= u
;;

(* Axis identifiers for exit axis: 0 means the line exits through a
   u = +/-1 edge, 1 means it exits through a v = +/-1 edge. *)

(* [exit_axis n] returns 0 if the line with normal [n] exits the face
   through u = +/-1, and 1 if it exits through v = +/-1. *)
let[@inline] exit_axis (n : R3_vector.t) =
  if intersects_opposite_edges n
  then
    if Float_u.O.(Float_u.abs (R3_vector.x n) >= Float_u.abs (R3_vector.y n))
    then 1
    else 0
  else (
    (* Adjacent edges: parity of sign bits determines the exit axis.  Uses
       signbit to avoid underflow in a direct multiplication. *)
    let sx = if Float_u.O.(R3_vector.x n < #0.0) then 1 else 0 in
    let sy = if Float_u.O.(R3_vector.y n < #0.0) then 1 else 0 in
    let sz = if Float_u.O.(R3_vector.z n < #0.0) then 1 else 0 in
    if sx lxor sy lxor sz = 0 then 1 else 0)
;;

(* [exit_point n axis] returns the UV coordinates of the point where the
   line with normal [n] exits the face on the given axis. *)
let[@inline] exit_point (n : R3_vector.t) axis : R2_point.t =
  if Int.equal axis 0
  then
    let open Float_u.O in
    let nx = R3_vector.x n in
    let ny = R3_vector.y n in
    let nz = R3_vector.z n in
    let u = if ny > #0.0 then #1.0 else -#1.0 in
    R2_point.create ~x:u ~y:(((Float_u.neg u * nx) - nz) / ny)
  else
    let open Float_u.O in
    let nx = R3_vector.x n in
    let ny = R3_vector.y n in
    let nz = R3_vector.z n in
    let v = if nx < #0.0 then #1.0 else -#1.0 in
    R2_point.create ~x:(((Float_u.neg v * ny) - nz) / nx) ~y:v
;;

(* --- ClipToPaddedFace and helpers ------------------------------------ *)

(* Clamp a UV coordinate to the [-1, 1] cube face rectangle. *)
let[@inline] clamp_uv (p : R2_point.t) : R2_point.t =
  let open Float_u.O in
  let x = R2_point.x p in
  let y = R2_point.y p in
  let x' = Float_util.max_u (-#1.0) (Float_util.min_u #1.0 x) in
  let y' = Float_util.max_u (-#1.0) (Float_util.min_u #1.0 y) in
  R2_point.create ~x:x' ~y:y'
;;

(* [move_origin_to_valid_face face a ab a_uv] handles the case where the
   normal of AB, due to rounding, does not quite intersect the given face
   containing A.  Returns the possibly-updated face and the corresponding
   UV coordinates of A. *)
let move_origin_to_valid_face face (a : S2_point.t) (ab : R3_vector.t) (a_uv : R2_point.t)
  =
  let open Float_u.O in
  let max_safe_uv_coord = #1.0 - face_clip_error_uv_coord in
  let ax = Float_u.abs (R2_point.x a_uv) in
  let ay = Float_u.abs (R2_point.y a_uv) in
  if Float_util.max_u ax ay <= max_safe_uv_coord
  then #(face, a_uv)
  else (
    let n = S2_coords.face_xyz_to_uvw face ab in
    let use_original =
      if not (intersects_face n)
      then false
      else (
        let exit_uv = exit_point n (exit_axis n) in
        let exit =
          S2_coords.face_uv_to_xyz face (R2_point.x exit_uv) (R2_point.y exit_uv)
        in
        let ab_norm = R3_vector.normalize ab in
        let a_tangent = R3_vector.cross ab_norm (S2_point.to_r3 a) in
        let diff = R3_vector.sub exit (S2_point.to_r3 a) in
        R3_vector.dot diff a_tangent >= Float_u.neg face_clip_error_radians)
    in
    if use_original
    then #(face, a_uv)
    else (
      let new_face =
        if ax >= ay
        then S2_coords.get_uvw_face face 0 (if R2_point.x a_uv > #0.0 then 1 else 0)
        else S2_coords.get_uvw_face face 1 (if R2_point.y a_uv > #0.0 then 1 else 0)
      in
      let uv = S2_coords.valid_face_xyz_to_uv new_face (S2_point.to_r3 a) in
      #(new_face, clamp_uv uv)))
;;

(* [get_next_face face exit axis n target_face] returns the next face to
   visit along the line AB when traversing the cube faces from A toward B. *)
let[@inline] get_next_face face (exit : R2_point.t) axis (n : R3_vector.t) target_face =
  let other = if Int.equal axis 0 then R2_point.y exit else R2_point.x exit in
  let other_pos = Float_u.O.(other > #0.0) in
  let other_axis = Int.( - ) 1 axis in
  if Float_u.equal (Float_u.abs other) #1.0
     && Int.equal
          (S2_coords.get_uvw_face face other_axis (if other_pos then 1 else 0))
          target_face
     && sum_equals
          Float_u.O.(R2_point.x exit * R3_vector.x n)
          Float_u.O.(R2_point.y exit * R3_vector.y n)
          (Float_u.neg (R3_vector.z n))
  then target_face
  else (
    let along = if Int.equal axis 0 then R2_point.x exit else R2_point.y exit in
    S2_coords.get_uvw_face face axis (if Float_u.O.(along > #0.0) then 1 else 0))
;;

(* [clip_destination b a scaled_n b_tangent a_tangent scale_uv] returns the
   (u, v) coordinates of the clipped endpoint A' on the given face along
   with a score in 0..3 used to decide whether the segment intersects the
   face.  All arguments are expressed in the (u, v, w) coordinates of the
   target face. *)
let clip_destination
  (a : R3_vector.t)
  (b : R3_vector.t)
  (scaled_n : R3_vector.t)
  (a_tangent : R3_vector.t)
  (b_tangent : R3_vector.t)
  scale_uv
  =
  let open Float_u.O in
  let max_safe_uv_coord = #1.0 - face_clip_error_uv_coord in
  let bx = R3_vector.x b in
  let by = R3_vector.y b in
  let bz = R3_vector.z b in
  let fast_uv =
    if bz > #0.0
    then (
      let uv = R2_point.create ~x:(bx / bz) ~y:(by / bz) in
      if Float_util.max_u (Float_u.abs (R2_point.x uv)) (Float_u.abs (R2_point.y uv))
         <= max_safe_uv_coord
      then R2_point.Option.some uv
      else R2_point.Option.none)
    else R2_point.Option.none
  in
  match%optional_u.R2_point.Option fast_uv with
  | Some uv -> #(uv, 0)
  | None ->
    let exit = exit_point scaled_n (exit_axis scaled_n) in
    let uv =
      R2_point.create ~x:(scale_uv * R2_point.x exit) ~y:(scale_uv * R2_point.y exit)
    in
    let p = R3_vector.create ~x:(R2_point.x uv) ~y:(R2_point.y uv) ~z:#1.0 in
    let score =
      if R3_vector.dot (R3_vector.sub p a) a_tangent < #0.0
      then 2
      else if R3_vector.dot (R3_vector.sub p b) b_tangent < #0.0
      then 1
      else 0
    in
    if Int.( > ) score 0
    then
      if bz <= #0.0 then #(uv, 3) else #(R2_point.create ~x:(bx / bz) ~y:(by / bz), score)
    else #(uv, score)
;;

let clip_to_padded_face (a_xyz : S2_point.t) (b_xyz : S2_point.t) face ~(padding : float#)
  : clipped_uv option
  =
  (* Fast path: both endpoints are on the given face. *)
  if Int.equal (S2_coords.get_face (S2_point.to_r3 a_xyz)) face
     && Int.equal (S2_coords.get_face (S2_point.to_r3 b_xyz)) face
  then (
    let a_uv = S2_coords.valid_face_xyz_to_uv face (S2_point.to_r3 a_xyz) in
    let b_uv = S2_coords.valid_face_xyz_to_uv face (S2_point.to_r3 b_xyz) in
    Some { a = a_uv; b = b_uv })
  else
    let open Float_u.O in
    (* Cross product must be computed in original xyz space. *)
    let ab_xyz = S2_point.robust_cross_prod a_xyz b_xyz in
    let n = S2_coords.face_xyz_to_uvw face ab_xyz in
    let a = S2_coords.face_xyz_to_uvw face (S2_point.to_r3 a_xyz) in
    let b = S2_coords.face_xyz_to_uvw face (S2_point.to_r3 b_xyz) in
    let scale_uv = #1.0 + padding in
    let scaled_n =
      R3_vector.create
        ~x:(scale_uv * R3_vector.x n)
        ~y:(scale_uv * R3_vector.y n)
        ~z:(R3_vector.z n)
    in
    if not (intersects_face scaled_n)
    then None
    else (
      let n = R3_vector.normalize n in
      let a_tangent = R3_vector.cross n a in
      let b_tangent = R3_vector.cross b n in
      (* Negating scaled_n for the a endpoint. *)
      let neg_scaled_n =
        R3_vector.create
          ~x:(Float_u.neg (R3_vector.x scaled_n))
          ~y:(Float_u.neg (R3_vector.y scaled_n))
          ~z:(Float_u.neg (R3_vector.z scaled_n))
      in
      let #(a_uv, a_score) =
        clip_destination b a neg_scaled_n b_tangent a_tangent scale_uv
      in
      let #(b_uv, b_score) = clip_destination a b scaled_n a_tangent b_tangent scale_uv in
      if Int.( < ) (Int.( + ) a_score b_score) 3
      then Some { a = a_uv; b = b_uv }
      else None)
;;

let[@inline] clip_to_face a b face = clip_to_padded_face a b face ~padding:#0.0

(* --- GetFaceSegments -------------------------------------------------- *)

let get_face_segments (a : S2_point.t) (b : S2_point.t) : face_segment list =
  let a_r3 = S2_point.to_r3 a in
  let b_r3 = S2_point.to_r3 b in
  let a_face_init = S2_coords.get_face a_r3 in
  let b_face_init = S2_coords.get_face b_r3 in
  let seg_a_init = S2_coords.valid_face_xyz_to_uv a_face_init a_r3 in
  let seg_b_init = S2_coords.valid_face_xyz_to_uv b_face_init b_r3 in
  if a_face_init = b_face_init
  then [ { face = a_face_init; a = seg_a_init; b = seg_b_init } ]
  else (
    let ab = S2_point.robust_cross_prod a b in
    let #(a_face, seg_a) = move_origin_to_valid_face a_face_init a ab seg_a_init in
    let neg_ab = R3_vector.neg ab in
    let #(b_face, seg_b) = move_origin_to_valid_face b_face_init b neg_ab seg_b_init in
    let b_saved = seg_b in
    (* Walk the line AB face-by-face, accumulating segments. *)
    let segments = ref [] in
    let mutable face = a_face in
    let mutable cur_a = seg_a in
    while face <> b_face do
      let n = S2_coords.face_xyz_to_uvw face ab in
      let ea = exit_axis n in
      let exit_uv = exit_point n ea in
      segments := { face; a = cur_a; b = exit_uv } :: !segments;
      let exit_xyz =
        S2_coords.face_uv_to_xyz face (R2_point.x exit_uv) (R2_point.y exit_uv)
      in
      let next_face = get_next_face face exit_uv ea n b_face in
      let exit_uvw = S2_coords.face_xyz_to_uvw next_face exit_xyz in
      face <- next_face;
      cur_a <- R2_point.create ~x:(R3_vector.x exit_uvw) ~y:(R3_vector.y exit_uvw)
    done;
    let final = { face; a = cur_a; b = b_saved } in
    List.rev (final :: !segments))
;;

(* --- Rectangle clipping ---------------------------------------------- *)

let intersects_rect (a : R2_point.t) (b : R2_point.t) (rect : R2_rect.t) =
  let bound = R2_rect.from_point_pair a b in
  if not (R2_rect.intersects rect bound)
  then false
  else (
    (* All four vertices of [rect] must not lie on the same side of the
       extended line AB. *)
    let n = R2_point.ortho (R2_point.sub b a) in
    let i = if Float_u.O.(R2_point.x n >= #0.0) then 1 else 0 in
    let j = if Float_u.O.(R2_point.y n >= #0.0) then 1 else 0 in
    let v_max = R2_rect.get_vertex_ij rect i j in
    let v_min = R2_rect.get_vertex_ij rect (Int.( - ) 1 i) (Int.( - ) 1 j) in
    let max_val = R2_point.dot n (R2_point.sub v_max a) in
    let min_val = R2_point.dot n (R2_point.sub v_min a) in
    Float_u.O.(max_val >= #0.0 && min_val <= #0.0))
;;

let interpolate_double x a b a1 b1 =
  let open Float_u.O in
  if a = b
  then a1
  else if Float_u.abs (a - x) <= Float_u.abs (b - x)
  then a1 + ((b1 - a1) * ((x - a) / (b - a)))
  else b1 + ((a1 - b1) * ((x - b) / (a - b)))
;;

(* Clip one axis of the bounding rectangle to the corresponding axis of
   the clip interval.  The new bound intervals (bound0', bound1') are
   packed into an R2_rect.t with bound0' as the x interval and bound1' as
   the y interval; the R2_rect layout here is merely a convenient carrier
   for two unboxed R1 intervals.  Returns [R2_rect.Option.none] if the
   clipped bounds are empty.  [diag] is 0 if AB has positive slope and 1
   if it has negative slope. *)
let clip_bound_axis
  a0
  b0
  (bound0 : R1_interval.t)
  a1
  b1
  (bound1 : R1_interval.t)
  diag
  (clip0 : R1_interval.t)
  : R2_rect.Option.t
  =
  let open Float_u.O in
  let clip_lo = R1_interval.lo clip0 in
  let clip_hi = R1_interval.hi clip0 in
  let mutable cur0_lo = R1_interval.lo bound0 in
  let mutable cur0_hi = R1_interval.hi bound0 in
  let mutable cur1_lo = R1_interval.lo bound1 in
  let mutable cur1_hi = R1_interval.hi bound1 in
  let mutable alive = true in
  if cur0_lo < clip_lo
  then
    if cur0_hi < clip_lo
    then alive <- false
    else (
      cur0_lo <- clip_lo;
      let value = interpolate_double clip_lo a0 b0 a1 b1 in
      (* Update cur1 at endpoint [diag]. *)
      if Int.equal diag 0
      then
        if cur1_hi < value
        then alive <- false
        else if cur1_lo < value
        then cur1_lo <- value
        else ()
      else if cur1_lo > value
      then alive <- false
      else if cur1_hi > value
      then cur1_hi <- value);
  if alive && cur0_hi > clip_hi
  then
    if cur0_lo > clip_hi
    then alive <- false
    else (
      cur0_hi <- clip_hi;
      let value = interpolate_double clip_hi a0 b0 a1 b1 in
      (* Update cur1 at endpoint [1 - diag]. *)
      if Int.equal (Int.( - ) 1 diag) 0
      then
        if cur1_hi < value
        then alive <- false
        else if cur1_lo < value
        then cur1_lo <- value
        else ()
      else if cur1_lo > value
      then alive <- false
      else if cur1_hi > value
      then cur1_hi <- value);
  if alive
  then (
    let x = R1_interval.create ~lo:cur0_lo ~hi:cur0_hi in
    let y = R1_interval.create ~lo:cur1_lo ~hi:cur1_hi in
    R2_rect.Option.some (R2_rect.create_intervals_exn ~x ~y))
  else R2_rect.Option.none
;;

let clip_edge_bound
  (a : R2_point.t)
  (b : R2_point.t)
  (clip : R2_rect.t)
  (bound : R2_rect.t)
  : R2_rect.Option.t
  =
  let open Float_u.O in
  let ax = R2_point.x a in
  let ay = R2_point.y a in
  let bx = R2_point.x b in
  let by = R2_point.y b in
  let diag = if Bool.equal (ax > bx) (ay > by) then 0 else 1 in
  match%optional_u.R2_rect.Option
    clip_bound_axis ax bx (R2_rect.x bound) ay by (R2_rect.y bound) diag (R2_rect.x clip)
  with
  | None -> R2_rect.Option.none
  | Some step1 ->
    (match%optional_u.R2_rect.Option
       clip_bound_axis
         ay
         by
         (R2_rect.y step1)
         ax
         bx
         (R2_rect.x step1)
         diag
         (R2_rect.y clip)
     with
     | None -> R2_rect.Option.none
     | Some step2 ->
       (* step2 carries (clipped y, clipped x): swap back so result has x/y
          matching the original rectangle axes. *)
       R2_rect.Option.some
         (R2_rect.create_intervals_exn ~x:(R2_rect.y step2) ~y:(R2_rect.x step2)))
;;

let get_clipped_edge_bound (a : R2_point.t) (b : R2_point.t) (clip : R2_rect.t)
  : R2_rect.t
  =
  let bound = R2_rect.from_point_pair a b in
  match%optional_u.R2_rect.Option clip_edge_bound a b clip bound with
  | Some r -> r
  | None -> R2_rect.empty
;;

let clip_edge (a : R2_point.t) (b : R2_point.t) (clip : R2_rect.t) : clipped_uv option =
  let open Float_u.O in
  let bound = R2_rect.from_point_pair a b in
  match%optional_u.R2_rect.Option clip_edge_bound a b clip bound with
  | None -> None
  | Some bound ->
    let ax = R2_point.x a in
    let ay = R2_point.y a in
    let bx = R2_point.x b in
    let by = R2_point.y b in
    let ai = if ax > bx then 1 else 0 in
    let aj = if ay > by then 1 else 0 in
    let a_clip = R2_rect.get_vertex_ij bound ai aj in
    let b_clip = R2_rect.get_vertex_ij bound (Int.( - ) 1 ai) (Int.( - ) 1 aj) in
    Some { a = a_clip; b = b_clip }
;;
