open Core

module Internal = struct
  let pos_to_ij =
    [| [| 0; 1; 3; 2 |]; [| 0; 2; 3; 1 |]; [| 3; 2; 0; 1 |]; [| 3; 1; 0; 2 |] |]
  ;;

  let pos_to_orientation = [| 1; 0; 0; 3 |]
end

type t =
  #{ id : S2_cell_id.t
   ; face : int
   ; level : int
   ; orientation : int
   ; uv : R2_rect.t
   }
[@@deriving sexp_of]

let[@inline] [@zero_alloc] id t = t.#id
let[@inline] [@zero_alloc] face t = t.#face
let[@inline] [@zero_alloc] level t = t.#level
let[@inline] [@zero_alloc] orientation t = t.#orientation
let[@inline] [@zero_alloc] is_leaf t = t.#level = S2_cell_id.max_level
let[@inline] [@zero_alloc] bound_uv t = t.#uv
let[@inline] [@zero_alloc] size_ij t = S2_cell_id.size_ij t.#level
let[@inline] [@zero_alloc] size_st t = S2_cell_id.size_st t.#level

let[@inline] [@zero_alloc] ij_level_to_bound_uv ~i ~j level =
  let sz = 1 lsl (S2_cell_id.max_level - level) in
  let extract_interval coord =
    let lo_ij = coord land -sz in
    let hi_ij = lo_ij + sz in
    let lo = S2_coords.st_to_uv (S2_coords.ij_to_st_min lo_ij) in
    let hi = S2_coords.st_to_uv (S2_coords.ij_to_st_min hi_ij) in
    R1_interval.create ~lo ~hi
  in
  let x = extract_interval i in
  let y = extract_interval j in
  R2_rect.create_intervals_exn ~x ~y
;;

let of_cell_id id =
  let #(face, i, j, orientation) = S2_cell_id.to_face_ij_orientation id in
  let level = S2_cell_id.level id in
  let uv = ij_level_to_bound_uv ~i ~j level in
  #{ id; face; level; orientation; uv }
;;

let of_point p = of_cell_id (S2_cell_id.from_point p)
let of_latlng ll = of_cell_id (S2_cell_id.from_latlng ll)
let from_face face = of_cell_id (S2_cell_id.from_face_exn face)

let from_face_pos_level ~face ~pos ~level =
  of_cell_id (S2_cell_id.from_face_pos_level face pos level)
;;

let vertex_raw t k =
  let v = R2_rect.get_vertex t.#uv k in
  S2_coords.face_uv_to_xyz t.#face (R2_point.x v) (R2_point.y v)
;;

let vertex t k = R3_vector.normalize (vertex_raw t k)

let[@inline] [@zero_alloc] edge_raw t k =
  match k land 3 with
  | 0 -> S2_coords.get_v_norm t.#face (R1_interval.lo (R2_rect.y t.#uv))
  | 1 -> S2_coords.get_u_norm t.#face (R1_interval.hi (R2_rect.x t.#uv))
  | 2 -> R3_vector.neg (S2_coords.get_v_norm t.#face (R1_interval.hi (R2_rect.y t.#uv)))
  | 3 -> R3_vector.neg (S2_coords.get_u_norm t.#face (R1_interval.lo (R2_rect.x t.#uv)))
  | _ -> assert false
;;

let[@inline] [@zero_alloc] edge t k = R3_vector.normalize (edge_raw t k)
let[@inline] [@zero_alloc] center_raw t = S2_cell_id.to_point_raw t.#id
let[@inline] [@zero_alloc] center t = S2_cell_id.to_point t.#id

let[@inline] [@zero_alloc] uv_endpoint interval i =
  if i = 0 then R1_interval.lo interval else R1_interval.hi interval
;;

let[@inline] [@zero_alloc] average_area level =
  (* AvgArea = 4*pi / (6 * 4^level): divide the sphere (area 4*pi) evenly over
     the 6 * 4^level cells at the given level. *)
  let open Float_u.O in
  Float_u.pi () * #4.0 / #6.0 / (#4.0 ** of_int level)
;;

let[@inline] [@zero_alloc] spherical_area a b c =
  let open Float_u.O in
  let area =
    #2.0
    * Float_u.atan2
        (R3_vector.dot a (R3_vector.cross b c))
        (#1.0 + R3_vector.dot a b + R3_vector.dot a c + R3_vector.dot b c)
  in
  Float_u.abs area
;;

let exact_area t =
  let v0 = vertex t 0 in
  let v1 = vertex t 1 in
  let v2 = vertex t 2 in
  let v3 = vertex t 3 in
  Float_u.O.(spherical_area v0 v1 v2 + spherical_area v0 v2 v3)
;;

let approx_area t =
  if t.#level < 2
  then average_area t.#level
  else
    let open Float_u.O in
    let v0 = vertex t 0 in
    let v1 = vertex t 1 in
    let v2 = vertex t 2 in
    let v3 = vertex t 3 in
    let flat_area =
      #0.5 * R3_vector.norm (R3_vector.cross (R3_vector.sub v2 v0) (R3_vector.sub v3 v1))
    in
    flat_area
    * #2.0
    / (#1.0 + Float_u.sqrt (#1.0 - Float_util.min_u #1.0 (flat_area / Float_u.pi ())))
;;

let contains_point t p =
  let uv = S2_coords.face_xyz_to_uv t.#face p in
  match%optional_u.R2_point.Option uv with
  | None -> false
  | Some uv ->
    let margin = Float_u.O.(#5.0 / #3.0 * Float_u.epsilon_float ()) in
    R2_rect.contains_point (R2_rect.expanded_scalar t.#uv margin) uv
;;

let[@inline] [@zero_alloc] contains_cell t other = S2_cell_id.contains t.#id other.#id
let[@inline] [@zero_alloc] intersects_cell t other = S2_cell_id.intersects t.#id other.#id

let[@inline] [@zero_alloc] child t ~pos =
  let uv_mid = S2_cell_id.to_center_uv t.#id in
  let uv_mid_x = R2_point.x uv_mid in
  let uv_mid_y = R2_point.y uv_mid in
  let cid = S2_cell_id.child_exn t.#id pos in
  let orientation = t.#orientation lxor Internal.pos_to_orientation.(pos) in
  let ij = Internal.pos_to_ij.(t.#orientation).(pos) in
  let i = ij lsr 1 in
  let j = ij land 1 in
  let x_intv =
    if i = 1
    then R1_interval.create ~lo:uv_mid_x ~hi:(R1_interval.hi (R2_rect.x t.#uv))
    else R1_interval.create ~lo:(R1_interval.lo (R2_rect.x t.#uv)) ~hi:uv_mid_x
  in
  let y_intv =
    if j = 1
    then R1_interval.create ~lo:uv_mid_y ~hi:(R1_interval.hi (R2_rect.y t.#uv))
    else R1_interval.create ~lo:(R1_interval.lo (R2_rect.y t.#uv)) ~hi:uv_mid_y
  in
  let uv = R2_rect.create_intervals_exn ~x:x_intv ~y:y_intv in
  #{ id = cid; face = t.#face; level = t.#level + 1; orientation; uv }
;;

let cap_bound t =
  let center =
    R3_vector.normalize
      (S2_coords.face_uv_to_xyz
         t.#face
         (R2_point.x (R2_rect.center t.#uv))
         (R2_point.y (R2_rect.center t.#uv)))
  in
  let cap = S2_cap.of_point center in
  let cap = S2_cap.add_point cap (vertex t 0) in
  let cap = S2_cap.add_point cap (vertex t 1) in
  let cap = S2_cap.add_point cap (vertex t 2) in
  S2_cap.add_point cap (vertex t 3)
;;

let cell_union_bound t = [| t.#id |]

let[@inline] [@zero_alloc] vertex_chord_dist t p i j =
  (* Both callers pass UVW-space points, so the vertex must also be in UVW space. *)
  let u =
    if i = 0 then R1_interval.lo (R2_rect.x t.#uv) else R1_interval.hi (R2_rect.x t.#uv)
  in
  let v =
    if j = 0 then R1_interval.lo (R2_rect.y t.#uv) else R1_interval.hi (R2_rect.y t.#uv)
  in
  let vert = R3_vector.normalize (R3_vector.create ~x:u ~y:v ~z:#1.0) in
  S2_point.chord_angle_between p vert
;;

let[@inline] [@zero_alloc] u_edge_is_closest t p v_end =
  let u0 = R1_interval.lo (R2_rect.x t.#uv) in
  let u1 = R1_interval.hi (R2_rect.x t.#uv) in
  let v =
    if Int.(v_end = 0)
    then R1_interval.lo (R2_rect.y t.#uv)
    else R1_interval.hi (R2_rect.y t.#uv)
  in
  let open Float_u.O in
  let dir0 =
    R3_vector.create ~x:((v * v) + #1.0) ~y:(Float_u.neg u0 * v) ~z:(Float_u.neg u0)
  in
  let dir1 =
    R3_vector.create ~x:((v * v) + #1.0) ~y:(Float_u.neg u1 * v) ~z:(Float_u.neg u1)
  in
  R3_vector.dot p dir0 > #0.0 && R3_vector.dot p dir1 < #0.0
;;

let[@inline] [@zero_alloc] v_edge_is_closest t p u_end =
  let v0 = R1_interval.lo (R2_rect.y t.#uv) in
  let v1 = R1_interval.hi (R2_rect.y t.#uv) in
  let u =
    if Int.(u_end = 0)
    then R1_interval.lo (R2_rect.x t.#uv)
    else R1_interval.hi (R2_rect.x t.#uv)
  in
  let open Float_u.O in
  let dir0 =
    R3_vector.create ~x:(Float_u.neg u * v0) ~y:((u * u) + #1.0) ~z:(Float_u.neg v0)
  in
  let dir1 =
    R3_vector.create ~x:(Float_u.neg u * v1) ~y:((u * u) + #1.0) ~z:(Float_u.neg v1)
  in
  R3_vector.dot p dir0 > #0.0 && R3_vector.dot p dir1 < #0.0
;;

let[@inline] [@zero_alloc] edge_distance dir_ij uv =
  let open Float_u.O in
  let pq2 = dir_ij * dir_ij / (#1.0 + (uv * uv)) in
  let qr = #1.0 - Float_u.sqrt (#1.0 - pq2) in
  S1_chord_angle.of_length2 (pq2 + (qr * qr))
;;

let distance_to_point_internal t target_xyz to_interior =
  let target = S2_coords.face_xyz_to_uvw t.#face target_xyz in
  let u0 = R1_interval.lo (R2_rect.x t.#uv) in
  let u1 = R1_interval.hi (R2_rect.x t.#uv) in
  let v0 = R1_interval.lo (R2_rect.y t.#uv) in
  let v1 = R1_interval.hi (R2_rect.y t.#uv) in
  let tx = R3_vector.x target in
  let ty = R3_vector.y target in
  let tz = R3_vector.z target in
  let dir00 = Float_u.O.(tx - (tz * u0)) in
  let dir01 = Float_u.O.(tx - (tz * u1)) in
  let dir10 = Float_u.O.(ty - (tz * v0)) in
  let dir11 = Float_u.O.(ty - (tz * v1)) in
  let inside = ref true in
  let module Res = struct
    type t = { mutable r : S1_chord_angle.t }
  end
  in
  let res = { Res.r = S1_chord_angle.Option.none } in
  if Float_u.O.(dir00 < #0.0)
  then (
    inside := false;
    if v_edge_is_closest t target 0
    then res.r <- S1_chord_angle.Option.some (edge_distance (Float_u.neg dir00) u0));
  if S1_chord_angle.Option.is_none res.r && Float_u.O.(dir01 > #0.0)
  then (
    inside := false;
    if v_edge_is_closest t target 1
    then res.r <- S1_chord_angle.Option.some (edge_distance dir01 u1));
  if S1_chord_angle.Option.is_none res.r && Float_u.O.(dir10 < #0.0)
  then (
    inside := false;
    if u_edge_is_closest t target 0
    then res.r <- S1_chord_angle.Option.some (edge_distance (Float_u.neg dir10) v0));
  if S1_chord_angle.Option.is_none res.r && Float_u.O.(dir11 > #0.0)
  then (
    inside := false;
    if u_edge_is_closest t target 1
    then res.r <- S1_chord_angle.Option.some (edge_distance dir11 v1));
  let min_ca a b = if S1_chord_angle.compare a b <= 0 then a else b in
  match%optional_u.S1_chord_angle.Option res.r with
  | Some r -> r
  | None ->
    if !inside
    then
      if to_interior
      then S1_chord_angle.zero
      else (
        let d0 = edge_distance (Float_u.neg dir00) u0 in
        let d1 = edge_distance dir01 u1 in
        let d2 = edge_distance (Float_u.neg dir10) v0 in
        let d3 = edge_distance dir11 v1 in
        min_ca (min_ca d0 d1) (min_ca d2 d3))
    else (
      let d0 = vertex_chord_dist t target 0 0 in
      let d1 = vertex_chord_dist t target 1 0 in
      let d2 = vertex_chord_dist t target 0 1 in
      let d3 = vertex_chord_dist t target 1 1 in
      min_ca (min_ca d0 d1) (min_ca d2 d3))
;;

let distance_to_point t p = distance_to_point_internal t p true
let boundary_distance_to_point t p = distance_to_point_internal t p false

let max_distance_to_point t target =
  let target_uvw = S2_coords.face_xyz_to_uvw t.#face target in
  let max_ca a b = if S1_chord_angle.compare a b >= 0 then a else b in
  let d0 = vertex_chord_dist t target_uvw 0 0 in
  let d1 = vertex_chord_dist t target_uvw 1 0 in
  let d2 = vertex_chord_dist t target_uvw 0 1 in
  let d3 = vertex_chord_dist t target_uvw 1 1 in
  let max_dist = max_ca (max_ca d0 d1) (max_ca d2 d3) in
  if S1_chord_angle.compare max_dist S1_chord_angle.right <= 0
  then max_dist
  else
    S1_chord_angle.sub
      S1_chord_angle.straight
      (distance_to_point t (R3_vector.neg target))
;;

let[@inline] [@zero_alloc] uv_coord_of_edge t k =
  let k = k land 3 in
  if k % 2 = 0
  then R2_point.y (R2_rect.get_vertex t.#uv k)
  else R2_point.x (R2_rect.get_vertex t.#uv k)
;;

let[@inline] [@zero_alloc] ij_coord_of_edge t k =
  Float_util.iround_half_to_even_u
    Float_u.O.(
      Float_u.of_int S2_coords.limit_ij * S2_coords.uv_to_st (uv_coord_of_edge t k))
;;
