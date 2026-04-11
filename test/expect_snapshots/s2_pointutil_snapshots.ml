open Core

let%expect_test "origin_is_unit_length" =
  printf "%b\n" (S2.S2_pointutil.is_unit_length (S2.S2_pointutil.origin ()));
  [%expect {| true |}]
;;

let%expect_test "is_unit_length_unit_vector" =
  let p = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  printf "%b\n" (S2.S2_pointutil.is_unit_length p);
  [%expect {| true |}]
;;

let%expect_test "is_unit_length_non_unit_vector" =
  let p = S2.R3_vector.create ~x:#3.0 ~y:#4.0 ~z:#0.0 in
  printf "%b\n" (S2.S2_pointutil.is_unit_length p);
  [%expect {| false |}]
;;

let%expect_test "is_unit_length_zero_vector" =
  let p = S2.R3_vector.create ~x:#0.0 ~y:#0.0 ~z:#0.0 in
  printf "%b\n" (S2.S2_pointutil.is_unit_length p);
  [%expect {| false |}]
;;

let%expect_test "ortho_x_axis" =
  let p = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let o = S2.S2_pointutil.ortho p in
  printf
    "dot=%.17g is_unit=%b\nx=%.17g y=%.17g z=%.17g\n"
    (Float_u.to_float (S2.R3_vector.dot p o))
    (S2.S2_pointutil.is_unit_length o)
    (Float_u.to_float (S2.R3_vector.x o))
    (Float_u.to_float (S2.R3_vector.y o))
    (Float_u.to_float (S2.R3_vector.z o));
  [%expect
    {|
    dot=0 is_unit=true
    x=0 y=-0.99998595529588608 z=0.0052999255630681958
    |}]
;;

let%expect_test "ortho_y_axis" =
  let p = S2.S2_point.of_coords ~x:#0.0 ~y:#1.0 ~z:#0.0 in
  let o = S2.S2_pointutil.ortho p in
  printf
    "dot=%.17g is_unit=%b\nx=%.17g y=%.17g z=%.17g\n"
    (Float_u.to_float (S2.R3_vector.dot p o))
    (S2.S2_pointutil.is_unit_length o)
    (Float_u.to_float (S2.R3_vector.x o))
    (Float_u.to_float (S2.R3_vector.y o))
    (Float_u.to_float (S2.R3_vector.z o));
  [%expect
    {|
    dot=0 is_unit=true
    x=0.004569952278750988 y=0 z=-0.99998955771356413
    |}]
;;

let%expect_test "ortho_z_axis" =
  let p = S2.S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0 in
  let o = S2.S2_pointutil.ortho p in
  printf
    "dot=%.17g is_unit=%b\nx=%.17g y=%.17g z=%.17g\n"
    (Float_u.to_float (S2.R3_vector.dot p o))
    (S2.S2_pointutil.is_unit_length o)
    (Float_u.to_float (S2.R3_vector.x o))
    (Float_u.to_float (S2.R3_vector.y o))
    (Float_u.to_float (S2.R3_vector.z o));
  [%expect
    {|
    dot=0 is_unit=true
    x=-0.99992800777506696 y=0.011999136093300803 z=0
    |}]
;;

let%expect_test "ortho_negation_identity" =
  let p = S2.S2_point.of_coords ~x:#0.6 ~y:#0.8 ~z:#0.0 in
  let o = S2.S2_pointutil.ortho p in
  let o_neg = S2.S2_pointutil.ortho (S2.R3_vector.neg p) in
  let sum = S2.R3_vector.add o o_neg in
  printf
    "sum=(%.17g,%.17g,%.17g)\n"
    (Float_u.to_float (S2.R3_vector.x sum))
    (Float_u.to_float (S2.R3_vector.y sum))
    (Float_u.to_float (S2.R3_vector.z sum));
  [%expect {| sum=(0,0,0) |}]
;;

let%expect_test "approx_equals_self" =
  let p = S2.S2_point.of_coords ~x:#0.5 ~y:#0.5 ~z:#0.70710678 in
  printf "%b\n" (S2.S2_pointutil.approx_equals p p);
  [%expect {| true |}]
;;

let%expect_test "approx_equals_distinct" =
  let a = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let b = S2.S2_point.of_coords ~x:#0.0 ~y:#1.0 ~z:#0.0 in
  printf
    "default=%b loose=%b\n"
    (S2.S2_pointutil.approx_equals a b)
    (S2.S2_pointutil.approx_equals ~max_error_radians:2.0 a b);
  [%expect {| default=false loose=true |}]
;;

let%expect_test "rotate_preserves_unit" =
  let p = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let axis = S2.S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0 in
  let angle = S2.S1_angle.of_degrees #45.0 in
  let r = S2.S2_pointutil.rotate p ~axis ~angle in
  printf "is_unit=%b\n" (S2.S2_pointutil.is_unit_length r);
  [%expect {| is_unit=true |}]
;;

let%expect_test "get_frame_z_axis" =
  let z = S2.S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0 in
  let frame = S2.S2_pointutil.get_frame z in
  let #{ S2.S2_point.col0; col1; col2 } = frame in
  printf
    "col0_unit=%b col1_unit=%b col2_unit=%b\n"
    (S2.S2_pointutil.is_unit_length col0)
    (S2.S2_pointutil.is_unit_length col1)
    (S2.S2_pointutil.is_unit_length col2);
  printf
    "col0.col1=%.17g col1.col2=%.17g col0.col2=%.17g\n"
    (Float_u.to_float (S2.R3_vector.dot col0 col1))
    (Float_u.to_float (S2.R3_vector.dot col1 col2))
    (Float_u.to_float (S2.R3_vector.dot col0 col2));
  [%expect
    {|
    col0_unit=true col1_unit=true col2_unit=true
    col0.col1=0 col1.col2=0 col0.col2=0 |}]
;;

let%expect_test "frame_roundtrip_north_pole" =
  let z = S2.S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0 in
  let frame = S2.S2_pointutil.get_frame z in
  let q = S2.S2_point.of_coords ~x:#0.3 ~y:#0.4 ~z:#0.5 in
  let local = S2.S2_pointutil.to_frame frame q in
  let back = S2.S2_pointutil.from_frame frame local in
  printf
    "roundtrip_approx=%b\n"
    (S2.S2_pointutil.approx_equals ~max_error_radians:1e-14 q back);
  [%expect {| roundtrip_approx=true |}]
;;

let%expect_test "frame_near_pole" =
  (* Near the north pole - numerical-stability-sensitive input. *)
  let z = S2.S2_point.of_coords ~x:#1e-15 ~y:#1e-15 ~z:#1.0 in
  let frame = S2.S2_pointutil.get_frame z in
  let #{ S2.S2_point.col0; col1; col2 } = frame in
  printf
    "all_unit=%b\n"
    (S2.S2_pointutil.is_unit_length col0
     && S2.S2_pointutil.is_unit_length col1
     && S2.S2_pointutil.is_unit_length col2);
  printf
    "col2_equals_z=%b\n"
    (S2.S2_pointutil.approx_equals ~max_error_radians:1e-14 col2 z);
  [%expect {|
    all_unit=true
    col2_equals_z=true |}]
;;

let%expect_test "ref_dir_matches_ortho" =
  let p = S2.S2_point.of_coords ~x:#0.6 ~y:#0.8 ~z:#0.0 in
  let r = S2.S2_pointutil.ref_dir p in
  let o = S2.S2_pointutil.ortho p in
  printf
    "same=%b is_unit=%b\n"
    Float_u.O.(
      S2.R3_vector.x r = S2.R3_vector.x o
      && S2.R3_vector.y r = S2.R3_vector.y o
      && S2.R3_vector.z r = S2.R3_vector.z o)
    (S2.S2_pointutil.is_unit_length r);
  [%expect {| same=true is_unit=true |}]
;;
