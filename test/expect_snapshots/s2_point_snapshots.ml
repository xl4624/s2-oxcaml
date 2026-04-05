open Core

let%expect_test "origin_sexp" =
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_point.t] S2.S2_point.origin));
  [%expect {| ((x -0.00999946643502502)(y 0.0025924542609324121)(z 0.999946643502502)) |}]
;;

let%expect_test "origin_is_unit" =
  printf "%b\n" (S2.S2_point.is_unit_length S2.S2_point.origin);
  [%expect {| true |}]
;;

let%expect_test "of_coords_normalizes" =
  let p = S2.S2_point.of_coords ~x:#3.0 ~y:#4.0 ~z:#0.0 in
  printf
    "is_unit: %b norm=%.17g\n"
    (S2.S2_point.is_unit_length p)
    (Float_u.to_float (S2.R3_vector.norm p));
  [%expect {| is_unit: true norm=1 |}]
;;

let%expect_test "distance_same" =
  let p = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  printf "%.17g\n" (Float_u.to_float (S2.S1_angle.radians (S2.S2_point.distance p p)));
  [%expect {| 0 |}]
;;

let%expect_test "distance_antipodal" =
  let a = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let b = S2.S2_point.of_coords ~x:(-#1.0) ~y:#0.0 ~z:#0.0 in
  printf "%.17g\n" (Float_u.to_float (S2.S1_angle.degrees (S2.S2_point.distance a b)));
  [%expect {| 180 |}]
;;

let%expect_test "distance_orthogonal" =
  let a = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let b = S2.S2_point.of_coords ~x:#0.0 ~y:#1.0 ~z:#0.0 in
  printf "%.17g\n" (Float_u.to_float (S2.S1_angle.degrees (S2.S2_point.distance a b)));
  [%expect {| 90 |}]
;;

let%expect_test "ortho_perpendicular_and_unit" =
  let p = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let o = S2.S2_point.ortho p in
  printf
    "dot=%.17g is_unit=%b\n"
    (Float_u.to_float (S2.R3_vector.dot p o))
    (S2.S2_point.is_unit_length o);
  [%expect {| dot=0 is_unit=true |}]
;;

let%expect_test "chord_angle_between" =
  let a = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let b = S2.S2_point.of_coords ~x:#0.0 ~y:#1.0 ~z:#0.0 in
  let ca = S2.S2_point.chord_angle_between a b in
  printf
    "length2=%.17g degrees=%.17g\n"
    (Float_u.to_float (S2.S1_chord_angle.length2 ca))
    (S2.S1_chord_angle.degrees ca);
  [%expect {| length2=2 degrees=90.000000000000014 |}]
;;

let%expect_test "rotate_around_z_axis" =
  let p = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let axis = S2.S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0 in
  let angle = S2.S1_angle.of_degrees #90.0 in
  let r = S2.S2_point.rotate p ~axis ~angle in
  printf "is_unit: %b\n" (S2.S2_point.is_unit_length r);
  printf
    "x=%.17g y=%.17g z=%.17g\n"
    (Float_u.to_float (S2.R3_vector.x r))
    (Float_u.to_float (S2.R3_vector.y r))
    (Float_u.to_float (S2.R3_vector.z r));
  [%expect {|
    is_unit: true
    x=6.123233995736766e-17 y=1 z=0 |}]
;;

let%expect_test "frame_roundtrip" =
  let z = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let frame = S2.S2_point.get_frame z in
  let local = S2.S2_point.to_frame frame z in
  let back = S2.S2_point.from_frame frame local in
  printf "roundtrip_approx: %b\n" (S2.S2_point.approx_equal ~max_error:1e-15 z back);
  [%expect {| roundtrip_approx: true |}]
;;
