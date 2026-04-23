open Core

let[@inline] [@zero_alloc] planar_centroid a b c =
  let sum = R3_vector.add (R3_vector.add a b) c in
  R3_vector.mul sum Float_u.O.(#1.0 / #3.0)
;;

let[@zero_alloc] true_centroid a b c =
  let open Float_u.O in
  (* Using the side angles (not side chord lengths) keeps the [angle / sin(angle)]
     correction well-behaved for tiny triangles where [sin(angle) ~= angle]. *)
  let angle_a = S1_angle.radians (S2_point.distance b c) in
  let angle_b = S1_angle.radians (S2_point.distance c a) in
  let angle_c = S1_angle.radians (S2_point.distance a b) in
  let ra = if angle_a = #0.0 then #1.0 else angle_a / Float_u.sin angle_a in
  let rb = if angle_b = #0.0 then #1.0 else angle_b / Float_u.sin angle_b in
  let rc = if angle_c = #0.0 then #1.0 else angle_c / Float_u.sin angle_c in
  (* Solve
       [Ax Ay Az] [Mx]                       [ra]
       [Bx By Bz] [My]  =  0.5 * det(A,B,C) * [rb]
       [Cx Cy Cz] [Mz]                       [rc]
     using Cramer's rule. Row 0 is subtracted from rows 1 and 2 first to reduce
     cancellation error when A, B, C are very close together. See
     s2centroids.cc:32-72 for the derivation. *)
  let ax = R3_vector.x a in
  let ay = R3_vector.y a in
  let az = R3_vector.z a in
  let x = R3_vector.create ~x:ax ~y:(R3_vector.x b - ax) ~z:(R3_vector.x c - ax) in
  let y = R3_vector.create ~x:ay ~y:(R3_vector.y b - ay) ~z:(R3_vector.y c - ay) in
  let z = R3_vector.create ~x:az ~y:(R3_vector.z b - az) ~z:(R3_vector.z c - az) in
  let r = R3_vector.create ~x:ra ~y:(rb - ra) ~z:(rc - ra) in
  let mx = R3_vector.dot (R3_vector.cross y z) r in
  let my = R3_vector.dot (R3_vector.cross z x) r in
  let mz = R3_vector.dot (R3_vector.cross x y) r in
  R3_vector.create ~x:(mx * #0.5) ~y:(my * #0.5) ~z:(mz * #0.5)
;;

let[@zero_alloc] edge_true_centroid a b =
  (* If theta is the angle between [a] and [b], then
       |a - b| = 2 * sin(theta)
       |a + b| = 2 * cos(theta)
     The centroid-times-length is (2 sin theta) * midpoint_direction, and
     [sqrt(sin2 / cos2) * (a + b)] expresses that without explicit trig. *)
  let open Float_u.O in
  let vdiff = R3_vector.sub a b in
  let vsum = R3_vector.add a b in
  let sin2 = R3_vector.norm2 vdiff in
  let cos2 = R3_vector.norm2 vsum in
  if cos2 = #0.0 then R3_vector.zero else R3_vector.mul vsum (Float_u.sqrt (sin2 / cos2))
;;
