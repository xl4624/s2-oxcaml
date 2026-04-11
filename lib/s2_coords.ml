open Core

let max_xyz_to_uv_error = Float_u.of_float (0.5 *. Float.epsilon_float)
let max_cell_level = 30
let limit_ij = 1 lsl max_cell_level
let max_si_ti = 1 lsl (max_cell_level + 1)

let[@inline] st_to_uv s =
  let open Float_u.O in
  if s >= #0.5
  then #1.0 / #3.0 * ((#4.0 * s * s) - #1.0)
  else #1.0 / #3.0 * (#1.0 - (#4.0 * (#1.0 - s) * (#1.0 - s)))
;;

let[@inline] uv_to_st u =
  let open Float_u.O in
  if u >= #0.0
  then #0.5 * Float_u.sqrt (#1.0 + (#3.0 * u))
  else #1.0 - (#0.5 * Float_u.sqrt (#1.0 - (#3.0 * u)))
;;

let[@inline] ij_to_st_min i =
  Float_u.O.(#1.0 / Float_u.of_int limit_ij * Float_u.of_int i)
;;

let[@inline] st_to_ij s =
  if not Float_u.O.(s > #0.0)
  then 0
  else Int.min (Float_u.to_int Float_u.O.(Float_u.of_int limit_ij * s)) (limit_ij - 1)
;;

let[@inline] si_ti_to_st si =
  Float_u.O.(#1.0 / Float_u.of_int max_si_ti * Float_u.of_int si)
;;

let[@inline] st_to_si_ti s =
  Float.iround_nearest_exn (Float_u.to_float Float_u.O.(s * Float_u.of_int max_si_ti))
;;

let face_uv_to_xyz face u v : R3_vector.t =
  let one = #1.0 in
  let neg_one = -#1.0 in
  match face with
  | 0 -> R3_vector.create ~x:one ~y:u ~z:v
  | 1 -> R3_vector.create ~x:(Float_u.neg u) ~y:one ~z:v
  | 2 -> R3_vector.create ~x:(Float_u.neg u) ~y:(Float_u.neg v) ~z:one
  | 3 -> R3_vector.create ~x:neg_one ~y:(Float_u.neg v) ~z:(Float_u.neg u)
  | 4 -> R3_vector.create ~x:v ~y:neg_one ~z:(Float_u.neg u)
  | _ -> R3_vector.create ~x:v ~y:u ~z:neg_one
;;

let get_face (p : R3_vector.t) =
  let face = R3_vector.largest_abs_component p in
  let c =
    match face with
    | 0 -> R3_vector.x p
    | 1 -> R3_vector.y p
    | _ -> R3_vector.z p
  in
  if Float_u.O.(c < #0.0) then face + 3 else face
;;

let valid_face_xyz_to_uv face (p : R3_vector.t) =
  let open Float_u.O in
  let px = R3_vector.x p in
  let py = R3_vector.y p in
  let pz = R3_vector.z p in
  match face with
  | 0 -> R2_point.create ~x:(py / px) ~y:(pz / px)
  | 1 -> R2_point.create ~x:(Float_u.neg px / py) ~y:(pz / py)
  | 2 -> R2_point.create ~x:(Float_u.neg px / pz) ~y:(Float_u.neg py / pz)
  | 3 -> R2_point.create ~x:(pz / px) ~y:(py / px)
  | 4 -> R2_point.create ~x:(pz / py) ~y:(Float_u.neg px / py)
  | _ -> R2_point.create ~x:(Float_u.neg py / pz) ~y:(Float_u.neg px / pz)
;;

let face_xyz_to_uv face (p : R3_vector.t) : R2_point.Option.t =
  if face < 3
  then (
    let c =
      match face with
      | 0 -> R3_vector.x p
      | 1 -> R3_vector.y p
      | _ -> R3_vector.z p
    in
    if Float_u.O.(c <= #0.0)
    then R2_point.Option.none
    else (
      let uv = valid_face_xyz_to_uv face p in
      R2_point.Option.some uv))
  else (
    let c =
      match face with
      | 3 -> R3_vector.x p
      | 4 -> R3_vector.y p
      | _ -> R3_vector.z p
    in
    if Float_u.O.(c >= #0.0)
    then R2_point.Option.none
    else (
      let uv = valid_face_xyz_to_uv face p in
      R2_point.Option.some uv))
;;

let face_xyz_to_uv_exn face p : R2_point.t =
  match%optional_u.R2_point.Option face_xyz_to_uv face p with
  | Some result -> result
  | None ->
    (match
       raise_s
         [%message "S2Coords.face_xyz_to_uv_exn: invalid face for point" (face : int)]
     with
     | (_ : Nothing.t) -> .)
;;

let face_xyz_to_uvw face (p : R3_vector.t) : R3_vector.t =
  let px = R3_vector.x p in
  let py = R3_vector.y p in
  let pz = R3_vector.z p in
  match face with
  | 0 -> R3_vector.create ~x:py ~y:pz ~z:px
  | 1 -> R3_vector.create ~x:(Float_u.neg px) ~y:pz ~z:py
  | 2 -> R3_vector.create ~x:(Float_u.neg px) ~y:(Float_u.neg py) ~z:pz
  | 3 -> R3_vector.create ~x:(Float_u.neg pz) ~y:(Float_u.neg py) ~z:(Float_u.neg px)
  | 4 -> R3_vector.create ~x:(Float_u.neg pz) ~y:px ~z:(Float_u.neg py)
  | _ -> R3_vector.create ~x:py ~y:px ~z:(Float_u.neg pz)
;;

let face_si_ti_to_xyz face si ti : R3_vector.t =
  let u = st_to_uv (si_ti_to_st si) in
  let v = st_to_uv (si_ti_to_st ti) in
  face_uv_to_xyz face u v
;;

let xyz_to_face_si_ti (p : R3_vector.t) =
  let face = get_face p in
  let uv = valid_face_xyz_to_uv face p in
  let si = st_to_si_ti (uv_to_st (R2_point.x uv)) in
  let ti = st_to_si_ti (uv_to_st (R2_point.y uv)) in
  let level = max_cell_level - Int.ctz (si lor max_si_ti) in
  if level < 0 || level <> max_cell_level - Int.ctz (ti lor max_si_ti)
  then #(face, si, ti, -1)
  else (
    let center = R3_vector.normalize (face_si_ti_to_xyz face si ti) in
    if R3_vector.equal p center then #(face, si, ti, level) else #(face, si, ti, -1))
;;

let k_face_uvw_axes =
  [| [| [| #0.; #1.; #0. |]; [| #0.; #0.; #1. |]; [| #1.; #0.; #0. |] |]
   ; [| [| -#1.; #0.; #0. |]; [| #0.; #0.; #1. |]; [| #0.; #1.; #0. |] |]
   ; [| [| -#1.; #0.; #0. |]; [| #0.; -#1.; #0. |]; [| #0.; #0.; #1. |] |]
   ; [| [| #0.; #0.; -#1. |]; [| #0.; -#1.; #0. |]; [| -#1.; #0.; #0. |] |]
   ; [| [| #0.; #0.; -#1. |]; [| #1.; #0.; #0. |]; [| #0.; -#1.; #0. |] |]
   ; [| [| #0.; #1.; #0. |]; [| #1.; #0.; #0. |]; [| #0.; #0.; -#1. |] |]
  |]
;;

let[@inline] get_uvw_axis face axis : R3_vector.t =
  let a = k_face_uvw_axes.(face).(axis) in
  R3_vector.create ~x:a.(0) ~y:a.(1) ~z:a.(2)
;;

let[@inline] get_norm face : R3_vector.t = get_uvw_axis face 2
let[@inline] get_u_axis face : R3_vector.t = get_uvw_axis face 0
let[@inline] get_v_axis face : R3_vector.t = get_uvw_axis face 1

let[@inline] get_u_norm face u : R3_vector.t =
  match face with
  | 0 -> R3_vector.create ~x:u ~y:(-#1.0) ~z:#0.0
  | 1 -> R3_vector.create ~x:#1.0 ~y:u ~z:#0.0
  | 2 -> R3_vector.create ~x:#1.0 ~y:#0.0 ~z:u
  | 3 -> R3_vector.create ~x:(Float_u.neg u) ~y:#0.0 ~z:#1.0
  | 4 -> R3_vector.create ~x:#0.0 ~y:(Float_u.neg u) ~z:#1.0
  | _ -> R3_vector.create ~x:#0.0 ~y:(-#1.0) ~z:(Float_u.neg u)
;;

let[@inline] get_v_norm face v : R3_vector.t =
  match face with
  | 0 -> R3_vector.create ~x:(Float_u.neg v) ~y:#0.0 ~z:#1.0
  | 1 -> R3_vector.create ~x:#0.0 ~y:(Float_u.neg v) ~z:#1.0
  | 2 -> R3_vector.create ~x:#0.0 ~y:(-#1.0) ~z:(Float_u.neg v)
  | 3 -> R3_vector.create ~x:v ~y:(-#1.0) ~z:#0.0
  | 4 -> R3_vector.create ~x:#1.0 ~y:v ~z:#0.0
  | _ -> R3_vector.create ~x:#1.0 ~y:#0.0 ~z:v
;;

let k_face_uvw_faces =
  [| [| [| 4; 1 |]; [| 5; 2 |]; [| 3; 0 |] |]
   ; [| [| 0; 3 |]; [| 5; 2 |]; [| 4; 1 |] |]
   ; [| [| 0; 3 |]; [| 1; 4 |]; [| 5; 2 |] |]
   ; [| [| 2; 5 |]; [| 1; 4 |]; [| 0; 3 |] |]
   ; [| [| 2; 5 |]; [| 3; 0 |]; [| 1; 4 |] |]
   ; [| [| 4; 1 |]; [| 3; 0 |]; [| 2; 5 |] |]
  |]
;;

let[@inline] get_uvw_face face axis direction = k_face_uvw_faces.(face).(axis).(direction)
