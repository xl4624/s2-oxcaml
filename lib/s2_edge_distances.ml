open Core

[@@@zero_alloc all]

(* If the minimum distance from X to AB is attained at an interior point of AB
   (not an endpoint), and that distance is less than min_dist (or always_update
   is true), return Some(dist). Otherwise return None. *)
let[@inline] [@zero_alloc] update_min_interior_distance_impl
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
    (#4.75 * Float_u.epsilon_float () * (xa2 + xb2 + ab2))
    + (#8.0 * Float_u.epsilon_float () * Float_u.epsilon_float ())
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
let[@inline] [@zero_alloc] update_min_distance_impl ~always_update x a b min_dist =
  let open Float_u.O in
  let xa2 = R3_vector.norm2 (R3_vector.sub x a) in
  let xb2 = R3_vector.norm2 (R3_vector.sub x b) in
  match%optional_u.S1_chord_angle.Option
    update_min_interior_distance_impl ~always_update x a b xa2 xb2 min_dist
  with
  | Some dist -> S1_chord_angle.Option.some dist
  | None ->
    let dist2 = Float_util.min_u xa2 xb2 in
    if (not always_update) && dist2 >= S1_chord_angle.length2 min_dist
    then S1_chord_angle.Option.none
    else S1_chord_angle.Option.some (S1_chord_angle.of_length2 dist2)
;;

let[@inline] [@zero_alloc] get_distance x a b =
  let result = update_min_distance_impl ~always_update:true x a b S1_chord_angle.zero in
  match%optional_u.S1_chord_angle.Option result with
  | Some d -> S1_chord_angle.to_angle d
  | None -> S1_angle.zero
;;

let[@inline] [@zero_alloc] update_min_distance x a b min_dist =
  update_min_distance_impl ~always_update:false x a b min_dist
;;

let[@inline] [@zero_alloc] is_distance_less x a b limit =
  S1_chord_angle.Option.is_some (update_min_distance x a b limit)
;;

let[@inline] [@zero_alloc] update_max_distance x a b max_dist =
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

let[@inline] [@zero_alloc] update_min_interior_distance x a b min_dist =
  let open Float_u.O in
  let xa2 = R3_vector.norm2 (R3_vector.sub x a) in
  let xb2 = R3_vector.norm2 (R3_vector.sub x b) in
  update_min_interior_distance_impl ~always_update:false x a b xa2 xb2 min_dist
;;

let[@inline] [@zero_alloc] is_interior_distance_less x a b limit =
  S1_chord_angle.Option.is_some (update_min_interior_distance x a b limit)
;;

(* Maximum error in the result of update_min_interior_distance. *)
let[@inline] [@zero_alloc] get_update_min_interior_distance_max_error dist =
  if S1_chord_angle.compare dist S1_chord_angle.right >= 0
  then #0.0
  else
    let open Float_u.O in
    let b = Float_util.min_u #1.0 (#0.5 * S1_chord_angle.length2 dist) in
    let a = Float_u.sqrt (b * (#2.0 - b)) in
    (((#2.5 + (#2.0 * Float_u.sqrt #3.0) + (#8.5 * a)) * a)
     + ((#2.0 + (#2.0 * Float_u.sqrt #3.0 / #3.0) + (#6.5 * (#1.0 - b))) * b)
     + ((#23.0 + (#16.0 / Float_u.sqrt #3.0)) * Float_u.epsilon_float ()))
    * Float_u.epsilon_float ()
;;

let[@inline] [@zero_alloc] get_update_min_distance_max_error dist =
  Float_u.max
    (get_update_min_interior_distance_max_error dist)
    (S1_chord_angle.max_point_error dist)
;;

let[@inline] [@zero_alloc] project x a b =
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
    (* Check if p is on the edge ab by testing orientation via pn = p x n:
       p is between a and b iff pn . a > 0 and pn . b < 0. *)
    let pn = R3_vector.cross p n in
    let open Float_u.O in
    if R3_vector.dot pn a > #0.0 && R3_vector.dot pn b < #0.0
    then p
    else if R3_vector.norm2 (R3_vector.sub x a) <= R3_vector.norm2 (R3_vector.sub x b)
    then a
    else b)
;;

let[@inline] [@zero_alloc] get_distance_fraction x a b =
  let open Float_u.O in
  let da = S1_angle.radians (R3_vector.angle x a) in
  let db = S1_angle.radians (R3_vector.angle x b) in
  da / (da + db)
;;

(* TODO: port the S1ChordAngle-argument overloads of GetPointOnRay,
   GetPointOnLine, GetPointToLeft, and GetPointToRight from
   s2edge_distances.h:155-177,201-203.  They are faster because
   S1ChordAngle sin/cos are cheaper than S1Angle's, but lose accuracy
   near 180 degrees. *)
let[@inline] [@zero_alloc] get_point_on_ray origin dir r =
  let cos_r = Float_u.cos (S1_angle.radians r) in
  let sin_r = Float_u.sin (S1_angle.radians r) in
  R3_vector.normalize
    (R3_vector.add (R3_vector.mul origin cos_r) (R3_vector.mul dir sin_r))
;;

let[@inline] [@zero_alloc] get_point_on_line a b r =
  let rcp = S2_point.robust_cross_prod a b in
  let dir = R3_vector.normalize (R3_vector.cross rcp a) in
  get_point_on_ray a dir r
;;

let[@inline] [@zero_alloc] interpolate a b t =
  let open Float_u.O in
  if t = #0.0
  then a
  else if t = #1.0
  then b
  else (
    let ab = R3_vector.angle a b in
    let r = S1_angle.of_radians (t * S1_angle.radians ab) in
    get_point_on_line a b r)
;;

let[@inline] [@zero_alloc] get_point_to_left a b r =
  let dir = R3_vector.normalize (S2_point.robust_cross_prod a b) in
  get_point_on_ray a dir r
;;

let[@inline] [@zero_alloc] get_point_to_right a b r =
  let dir = R3_vector.normalize (S2_point.robust_cross_prod b a) in
  get_point_on_ray a dir r
;;

type closest_points =
  #{ a : S2_point.t
   ; b : S2_point.t
   }

let[@inline] [@zero_alloc] project_with_cross x a b a_cross_b =
  if S2_point.equal x a || S2_point.equal x b
  then x
  else (
    let n = R3_vector.normalize a_cross_b in
    let nx = S2_point.robust_cross_prod n x in
    let p = R3_vector.normalize (R3_vector.cross nx n) in
    let pn = R3_vector.cross p n in
    let open Float_u.O in
    if R3_vector.dot pn a > #0.0 && R3_vector.dot pn b < #0.0
    then p
    else if R3_vector.norm2 (R3_vector.sub x a) <= R3_vector.norm2 (R3_vector.sub x b)
    then a
    else b)
;;

(* Thread a running minimum chord distance through a single vertex->edge
   update.  Returns the smaller of [cur] and the minimum distance from [x]
   to edge [ab]. *)
let[@inline] [@zero_alloc] min_step cur x a b =
  match%optional_u.S1_chord_angle.Option update_min_distance x a b cur with
  | Some d -> d
  | None -> cur
;;

let[@inline] [@zero_alloc] max_step cur x a b =
  match%optional_u.S1_chord_angle.Option update_max_distance x a b cur with
  | Some d -> d
  | None -> cur
;;

(* Fold the four vertex-to-edge updates into a running minimum. *)
let[@inline] [@zero_alloc] update_edge_pair_min_distance a0 a1 b0 b1 min_dist =
  if S1_chord_angle.is_zero min_dist
  then S1_chord_angle.Option.none
  else if S2_edge_crossings.crossing_sign a0 a1 b0 b1 >= 0
  then S1_chord_angle.Option.some S1_chord_angle.zero
  else (
    let cur = min_dist in
    let cur = min_step cur a0 b0 b1 in
    let cur = min_step cur a1 b0 b1 in
    let cur = min_step cur b0 a0 a1 in
    let cur = min_step cur b1 a0 a1 in
    if S1_chord_angle.compare cur min_dist < 0
    then S1_chord_angle.Option.some cur
    else S1_chord_angle.Option.none)
;;

let[@inline] [@zero_alloc] update_edge_pair_max_distance a0 a1 b0 b1 max_dist =
  if S1_chord_angle.compare max_dist S1_chord_angle.straight >= 0
  then S1_chord_angle.Option.none
  else if S2_edge_crossings.crossing_sign a0 a1 (R3_vector.neg b0) (R3_vector.neg b1) >= 0
  then S1_chord_angle.Option.some S1_chord_angle.straight
  else (
    let cur = max_dist in
    let cur = max_step cur a0 b0 b1 in
    let cur = max_step cur a1 b0 b1 in
    let cur = max_step cur b0 a0 a1 in
    let cur = max_step cur b1 a0 a1 in
    if S1_chord_angle.compare cur max_dist > 0
    then S1_chord_angle.Option.some cur
    else S1_chord_angle.Option.none)
;;

let[@inline] [@zero_alloc] is_edge_pair_distance_less a0 a1 b0 b1 distance =
  if S2_edge_crossings.crossing_sign a0 a1 b0 b1 >= 0
  then not (S1_chord_angle.is_zero distance)
  else
    is_distance_less a0 b0 b1 distance
    || is_distance_less a1 b0 b1 distance
    || is_distance_less b0 a0 a1 distance
    || is_distance_less b1 a0 a1 distance
;;

(* Compute the length2 of the minimum chord distance from x to edge ab.
   Always updates, hence always returns a valid chord length. *)
let[@inline] [@zero_alloc] vertex_edge_length2 x a b =
  match%optional_u.S1_chord_angle.Option
    update_min_distance_impl ~always_update:true x a b S1_chord_angle.infinity
  with
  | Some d -> S1_chord_angle.length2 d
  | None -> S1_chord_angle.length2 S1_chord_angle.infinity
;;

let[@inline] [@zero_alloc] get_edge_pair_closest_points a0 a1 b0 b1 =
  if S2_edge_crossings.crossing_sign a0 a1 b0 b1 > 0
  then (
    let x = S2_edge_crossings.get_intersection a0 a1 b0 b1 in
    #{ a = x; b = x })
  else (
    let d0 = vertex_edge_length2 a0 b0 b1 in
    let d1 = vertex_edge_length2 a1 b0 b1 in
    let d2 = vertex_edge_length2 b0 a0 a1 in
    let d3 = vertex_edge_length2 b1 a0 a1 in
    let open Float_u.O in
    let closest =
      if d0 <= d1 && d0 <= d2 && d0 <= d3
      then 0
      else if d1 <= d2 && d1 <= d3
      then 1
      else if d2 <= d3
      then 2
      else 3
    in
    match closest with
    | 0 -> #{ a = a0; b = project a0 b0 b1 }
    | 1 -> #{ a = a1; b = project a1 b0 b1 }
    | 2 -> #{ a = project b0 a0 a1; b = b0 }
    | _ -> #{ a = project b1 a0 a1; b = b1 })
;;

let[@inline] [@zero_alloc] is_edge_b_near_edge_a a0 a1 b0 b1 tolerance =
  let open Float_u.O in
  (* Compute an orthogonal to the plane containing A.  We use the raw
     robust_cross_prod when projecting (the Project helper normalizes it
     internally). *)
  let a_ortho_raw = S2_point.robust_cross_prod a0 a1 in
  let a_ortho0 = R3_vector.normalize a_ortho_raw in
  let a_nearest_b0 = project_with_cross b0 a0 a1 a_ortho_raw in
  let a_nearest_b1 = project_with_cross b1 a0 a1 a_ortho_raw in
  (* If the two projections have opposite orientation relative to a_ortho,
     flip a_ortho so that it agrees with a_nearest_b0 x a_nearest_b1.
     Sign(a, b, c) = (c x a) . b, so this is equivalent to checking whether
     (a_nearest_b1 x a_ortho) . a_nearest_b0 < 0. *)
  let s = R3_vector.dot (R3_vector.cross a_nearest_b1 a_ortho0) a_nearest_b0 in
  let a_ortho = if s < #0.0 then R3_vector.neg a_ortho0 else a_ortho0 in
  (* Work entirely in chord-length-squared space to avoid atan2 calls.
     For unit vectors p, q with angle t between them,
     |p - q|^2 = 2 - 2*cos(t), which is monotonic in t on [0, pi].
     t = pi/2 corresponds to length2 = 2. *)
  let tol2 = S1_chord_angle.length2 (S1_chord_angle.of_angle tolerance) in
  let b0_l2 = R3_vector.norm2 (R3_vector.sub b0 a_nearest_b0) in
  let b1_l2 = R3_vector.norm2 (R3_vector.sub b1 a_nearest_b1) in
  if b0_l2 > tol2 || b1_l2 > tol2
  then false
  else (
    let b_ortho = R3_vector.normalize (S2_point.robust_cross_prod b0 b1) in
    let planar_l2 = R3_vector.norm2 (R3_vector.sub a_ortho b_ortho) in
    if planar_l2 <= tol2
    then true
    else if planar_l2 >= #2.0
    then (
      (* When the planes meet at >= 90 degrees, edge B is near A iff b0 and
         b1 are closest to the same endpoint of A.  Compare planar distances
         (norm2) rather than geodesic distances for speed. *)
      let b0_close_to_a0 =
        R3_vector.norm2 (R3_vector.sub b0 a0) < R3_vector.norm2 (R3_vector.sub b0 a1)
      in
      let b1_close_to_a0 =
        R3_vector.norm2 (R3_vector.sub b1 a0) < R3_vector.norm2 (R3_vector.sub b1 a1)
      in
      Bool.equal b0_close_to_a0 b1_close_to_a0)
    else (
      (* Find the two points on circ(B) furthest from circ(A), then check
         whether either lies on the edge b0b1. *)
      let furthest =
        R3_vector.normalize
          (R3_vector.cross b_ortho (S2_point.robust_cross_prod a_ortho b_ortho))
      in
      let furthest_inv = R3_vector.neg furthest in
      (* A point p lies on edge b0b1 if we can traverse b_ortho -> b0 -> p ->
         b1 -> b_ortho without turning right. *)
      let furthest_on_b =
        S2_predicates.sign b_ortho b0 furthest && S2_predicates.sign furthest b1 b_ortho
      in
      let furthest_inv_on_b =
        S2_predicates.sign b_ortho b0 furthest_inv
        && S2_predicates.sign furthest_inv b1 b_ortho
      in
      not (furthest_on_b || furthest_inv_on_b)))
;;
