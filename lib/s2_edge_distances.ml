open Core

[@@@zero_alloc all]

let dbl_epsilon = Float_u.epsilon_float ()
let sqrt3 = Float_u.sqrt #3.0

(* If the minimum distance from X to AB is attained at an interior point of AB
   (not an endpoint), and that distance is less than min_dist (or always_update
   is true), return Some(dist). Otherwise return None. *)
let[@zero_alloc ignore] update_min_interior_distance_impl
  ~always_update
  x
  a
  b
  xa2
  xb2
  min_dist
  =
  let open Float_u.O in
  let ab2 = R3_vector.norm2 (R3_vector.sub a b) in
  let max_error =
    (#4.75 * dbl_epsilon * (xa2 + xb2 + ab2)) + (#8.0 * dbl_epsilon * dbl_epsilon)
  in
  if Float_u.abs (xa2 - xb2) >= ab2 + max_error
  then S1_chord_angle.Option.none
  else (
    let c = S2_point.robust_cross_prod a b in
    let c2 = R3_vector.norm2 c in
    let x_dot_c = R3_vector.dot x c in
    let x_dot_c2 = x_dot_c * x_dot_c in
    if (not always_update) && x_dot_c2 > c2 * S1_chord_angle.length2 min_dist
    then S1_chord_angle.Option.none
    else (
      let cx = R3_vector.cross c x in
      if R3_vector.dot (R3_vector.sub a x) cx >= #0.0
         || R3_vector.dot (R3_vector.sub b x) cx <= #0.0
      then S1_chord_angle.Option.none
      else (
        let qr = #1.0 - Float_u.sqrt (R3_vector.norm2 cx / c2) in
        let dist2 = (x_dot_c2 / c2) + (qr * qr) in
        if (not always_update) && dist2 >= S1_chord_angle.length2 min_dist
        then S1_chord_angle.Option.none
        else S1_chord_angle.Option.some (S1_chord_angle.of_length2 dist2))))
;;

(* Compute the distance from X to edge AB. If the distance is less than
   min_dist (or always_update is true), return Some(dist). *)
let[@zero_alloc ignore] update_min_distance_impl ~always_update x a b min_dist =
  let open Float_u.O in
  let xa2 = R3_vector.norm2 (R3_vector.sub x a) in
  let xb2 = R3_vector.norm2 (R3_vector.sub x b) in
  match%optional_u.S1_chord_angle.Option
    update_min_interior_distance_impl ~always_update x a b xa2 xb2 min_dist
  with
  | Some dist -> S1_chord_angle.Option.some dist
  | None ->
    let dist2 = Float_u.min xa2 xb2 in
    if (not always_update) && dist2 >= S1_chord_angle.length2 min_dist
    then S1_chord_angle.Option.none
    else S1_chord_angle.Option.some (S1_chord_angle.of_length2 dist2)
;;

let[@zero_alloc ignore] get_distance x a b =
  let result = update_min_distance_impl ~always_update:true x a b S1_chord_angle.zero in
  match%optional_u.S1_chord_angle.Option result with
  | Some d -> S1_chord_angle.to_angle d
  | None -> S1_angle.zero
;;

let[@zero_alloc ignore] update_min_distance x a b min_dist =
  update_min_distance_impl ~always_update:false x a b min_dist
;;

let[@zero_alloc ignore] is_distance_less x a b limit =
  S1_chord_angle.Option.is_some (update_min_distance x a b limit)
;;

let[@zero_alloc ignore] update_max_distance x a b max_dist =
  let xa = S1_chord_angle.of_length2 (R3_vector.norm2 (R3_vector.sub x a)) in
  let xb = S1_chord_angle.of_length2 (R3_vector.norm2 (R3_vector.sub x b)) in
  let dist = if S1_chord_angle.compare xa xb >= 0 then xa else xb in
  if S1_chord_angle.compare dist S1_chord_angle.right > 0
  then (
    (* Use the antipodal point trick: distance to -x from ab, then
       straight - that distance gives the max distance. *)
    let neg_x = R3_vector.neg x in
    let interior_result = update_min_distance_impl ~always_update:true neg_x a b dist in
    let dist' =
      match%optional_u.S1_chord_angle.Option interior_result with
      | Some d -> d
      | None -> dist
    in
    let final_dist =
      S1_chord_angle.of_length2
        Float_u.O.(
          S1_chord_angle.length2 S1_chord_angle.straight - S1_chord_angle.length2 dist')
    in
    if S1_chord_angle.compare max_dist final_dist < 0
    then S1_chord_angle.Option.some final_dist
    else S1_chord_angle.Option.none)
  else if S1_chord_angle.compare max_dist dist < 0
  then S1_chord_angle.Option.some dist
  else S1_chord_angle.Option.none
;;

let[@zero_alloc ignore] update_min_interior_distance x a b min_dist =
  let open Float_u.O in
  let xa2 = R3_vector.norm2 (R3_vector.sub x a) in
  let xb2 = R3_vector.norm2 (R3_vector.sub x b) in
  update_min_interior_distance_impl ~always_update:false x a b xa2 xb2 min_dist
;;

let[@zero_alloc ignore] is_interior_distance_less x a b limit =
  S1_chord_angle.Option.is_some (update_min_interior_distance x a b limit)
;;

(* Maximum error in the result of update_min_interior_distance. *)
let[@zero_alloc ignore] get_update_min_interior_distance_max_error dist =
  if S1_chord_angle.compare dist S1_chord_angle.right >= 0
  then #0.0
  else
    let open Float_u.O in
    let b = Float_u.min #1.0 (#0.5 * S1_chord_angle.length2 dist) in
    let a = Float_u.sqrt (b * (#2.0 - b)) in
    (((#2.5 + (#2.0 * sqrt3) + (#8.5 * a)) * a)
     + ((#2.0 + (#2.0 * sqrt3 / #3.0) + (#6.5 * (#1.0 - b))) * b)
     + ((#23.0 + (#16.0 / sqrt3)) * dbl_epsilon))
    * dbl_epsilon
;;

let[@zero_alloc ignore] get_update_min_distance_max_error dist =
  Float_u.max
    (get_update_min_interior_distance_max_error dist)
    (S1_chord_angle.max_point_error dist)
;;

let[@zero_alloc ignore] project x a b =
  (* Short-circuit if x equals a or b. *)
  if S2_point.equal x a || S2_point.equal x b
  then x
  else (
    let a_cross_b = S2_point.robust_cross_prod a b in
    let n = R3_vector.normalize a_cross_b in
    (* Find the closest point to x along the great circle through ab.
       Use n (normalized) rather than a_cross_b to avoid underflow. *)
    let nx = S2_point.robust_cross_prod n x in
    let p = R3_vector.normalize (R3_vector.cross nx n) in
    (* Check if p is on the edge ab by testing orientation.
       s2pred::Sign(p, n, a, pn) = pn . a, and similarly for b. *)
    let pn = R3_vector.cross p n in
    let open Float_u.O in
    if R3_vector.dot pn a > #0.0 && R3_vector.dot pn b < #0.0
    then p
    else if R3_vector.norm2 (R3_vector.sub x a) <= R3_vector.norm2 (R3_vector.sub x b)
    then a
    else b)
;;

let[@zero_alloc ignore] get_distance_fraction x a b =
  let da = S1_angle.radians (R3_vector.angle x a) in
  let db = S1_angle.radians (R3_vector.angle x b) in
  Float_u.to_float Float_u.O.(da / (da + db))
;;

let[@zero_alloc ignore] get_point_on_ray origin dir r =
  let cos_r = Float_u.cos (S1_angle.radians r) in
  let sin_r = Float_u.sin (S1_angle.radians r) in
  R3_vector.normalize
    (R3_vector.add (R3_vector.mul origin cos_r) (R3_vector.mul dir sin_r))
;;

let[@zero_alloc ignore] get_point_on_line a b r =
  let rcp = S2_point.robust_cross_prod a b in
  let dir = R3_vector.normalize (R3_vector.cross rcp a) in
  get_point_on_ray a dir r
;;

let[@zero_alloc ignore] interpolate a b t =
  if Float.( = ) t 0.0
  then a
  else if Float.( = ) t 1.0
  then b
  else (
    let ab = R3_vector.angle a b in
    let r = S1_angle.of_radians Float_u.O.(Float_u.of_float t * S1_angle.radians ab) in
    get_point_on_line a b r)
;;

let[@zero_alloc ignore] get_point_to_left a b r =
  let dir = R3_vector.normalize (S2_point.robust_cross_prod a b) in
  get_point_on_ray a dir r
;;

let[@zero_alloc ignore] get_point_to_right a b r =
  let dir = R3_vector.normalize (S2_point.robust_cross_prod b a) in
  get_point_on_ray a dir r
;;
