open Core

let pt x y z = S2.S2_point.of_coords ~x ~y ~z

let%expect_test "get_distance_on_edge_and_endpoints" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  (* Midpoint of edge ab: distance should be zero. *)
  let m = pt #1.0 #1.0 #0.0 in
  (* A point at the north pole: distance pi/2 to the equator edge. *)
  let north = pt #0.0 #0.0 #1.0 in
  printf
    "mid=%.17g a_to_a=%.17g north=%.17g\n"
    (Float_u.to_float (S2.S1_angle.radians (S2.S2_edge_distances.get_distance m a b)))
    (Float_u.to_float (S2.S1_angle.radians (S2.S2_edge_distances.get_distance a a b)))
    (Float_u.to_float (S2.S1_angle.radians (S2.S2_edge_distances.get_distance north a b)));
  [%expect {| mid=1.1102230246251565e-16 a_to_a=0 north=1.5707963267948968 |}]
;;

let%expect_test "is_distance_less" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let x = pt #1.0 #0.0 #0.1 in
  let tight = S2.S1_chord_angle.of_radians #0.01 in
  let loose = S2.S1_chord_angle.of_radians #1.0 in
  printf
    "tight=%b loose=%b\n"
    (S2.S2_edge_distances.is_distance_less x a b tight)
    (S2.S2_edge_distances.is_distance_less x a b loose);
  [%expect {| tight=false loose=true |}]
;;

let%expect_test "project_point_onto_edge" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let x = pt #1.0 #1.0 #1.0 in
  let p = S2.S2_edge_distances.project x a b in
  printf
    "proj=(%.17g, %.17g, %.17g)\n"
    (Float_u.to_float (S2.R3_vector.x p))
    (Float_u.to_float (S2.R3_vector.y p))
    (Float_u.to_float (S2.R3_vector.z p));
  [%expect {| proj=(0.70710678118654757, 0.70710678118654757, 0) |}]
;;

let%expect_test "interpolate_along_edge" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let show t p =
    printf
      "t=%.2f -> (%.17g, %.17g, %.17g)\n"
      (Float_u.to_float t)
      (Float_u.to_float (S2.R3_vector.x p))
      (Float_u.to_float (S2.R3_vector.y p))
      (Float_u.to_float (S2.R3_vector.z p))
  in
  let go t = show t (S2.S2_edge_distances.interpolate a b t) in
  go #0.0;
  go #0.5;
  go #1.0;
  [%expect {|
    t=0.00 -> (1, 0, 0)
    t=0.50 -> (0.70710678118654757, 0.70710678118654746, 0)
    t=1.00 -> (0, 1, 0)
    |}]
;;

let%expect_test "get_distance_fraction" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let m = pt #1.0 #1.0 #0.0 in
  let f = S2.S2_edge_distances.get_distance_fraction m a b in
  printf "fraction=%.17g\n" (Float_u.to_float f);
  [%expect {| fraction=0.5 |}]
;;

let%expect_test "edge_pair_closest_points" =
  (* Two crossing edges: a0a1 along x->y, b0b1 along (0.5,0.5,-z)->(0.5,0.5,+z). *)
  let a0 = pt #1.0 #0.0 #0.0 in
  let a1 = pt #0.0 #1.0 #0.0 in
  let b0 = pt #0.5 #0.5 (-#1.0) in
  let b1 = pt #0.5 #0.5 #1.0 in
  let #{ S2.S2_edge_distances.a; b } =
    S2.S2_edge_distances.get_edge_pair_closest_points a0 a1 b0 b1
  in
  printf
    "a=(%.17g,%.17g,%.17g) b=(%.17g,%.17g,%.17g)\n"
    (Float_u.to_float (S2.R3_vector.x a))
    (Float_u.to_float (S2.R3_vector.y a))
    (Float_u.to_float (S2.R3_vector.z a))
    (Float_u.to_float (S2.R3_vector.x b))
    (Float_u.to_float (S2.R3_vector.y b))
    (Float_u.to_float (S2.R3_vector.z b));
  [%expect {| a=(0.70710678118654746,0.70710678118654746,-0) b=(0.70710678118654746,0.70710678118654746,-0) |}]
;;

let%expect_test "update_min_distance_option" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let x = pt #1.0 #0.0 #0.1 in
  let cur = S2.S1_chord_angle.of_radians #1.0 in
  let result = S2.S2_edge_distances.update_min_distance x a b cur in
  let msg =
    match%optional_u.S2.S1_chord_angle.Option result with
    | None -> "none"
    | Some d -> sprintf "some r=%.17g" (Float_u.to_float (S2.S1_chord_angle.radians d))
  in
  printf "%s\n" msg;
  [%expect {| some r=0.099668652491162038 |}]
;;
