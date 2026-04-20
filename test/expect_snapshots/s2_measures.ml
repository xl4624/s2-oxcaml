open Core

let pt x y z = S2.S2_point.of_coords ~x ~y ~z

let%expect_test "octant_triangle_angles" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let c = pt #0.0 #0.0 #1.0 in
  let ang = S2.S2_measures.angle a b c in
  let turn = S2.S2_measures.turn_angle a b c in
  printf
    "angle=%.17g turn=%.17g\n"
    (Float_u.to_float (S2.S1_angle.radians ang))
    (Float_u.to_float (S2.S1_angle.radians turn));
  [%expect {| angle=1.5707963267948966 turn=1.5707963267948966 |}]
;;

let%expect_test "angle_symmetry" =
  let a = pt #1.0 #0.1 #0.0 in
  let b = pt #0.2 #1.0 #0.1 in
  let c = pt #0.0 #0.3 #1.0 in
  let abc = S2.S2_measures.angle a b c in
  let cba = S2.S2_measures.angle c b a in
  printf
    "abc=%.17g cba=%.17g\n"
    (Float_u.to_float (S2.S1_angle.radians abc))
    (Float_u.to_float (S2.S1_angle.radians cba));
  [%expect {| abc=1.6614598889829331 cba=1.6614598889829331 |}]
;;

let%expect_test "turn_angle_antisymmetry" =
  let a = pt #1.0 #0.1 #0.0 in
  let b = pt #0.2 #1.0 #0.1 in
  let c = pt #0.0 #0.3 #1.0 in
  let abc = S2.S2_measures.turn_angle a b c in
  let cba = S2.S2_measures.turn_angle c b a in
  printf
    "abc=%.17g cba=%.17g\n"
    (Float_u.to_float (S2.S1_angle.radians abc))
    (Float_u.to_float (S2.S1_angle.radians cba));
  [%expect {| abc=1.4801327646068603 cba=-1.4801327646068603 |}]
;;

let%expect_test "octant_triangle_area" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let c = pt #0.0 #0.0 #1.0 in
  let area = S2.S2_measures.area a b c in
  let girard = S2.S2_measures.girard_area a b c in
  let signed_ccw = S2.S2_measures.signed_area a b c in
  let signed_cw = S2.S2_measures.signed_area a c b in
  printf
    "area=%.17g girard=%.17g signed_ccw=%.17g signed_cw=%.17g\n"
    (Float_u.to_float area)
    (Float_u.to_float girard)
    (Float_u.to_float signed_ccw)
    (Float_u.to_float signed_cw);
  [%expect
    {| area=1.5707963267948966 girard=1.5707963267948966 signed_ccw=1.5707963267948966 signed_cw=-1.5707963267948966 |}]
;;

let%expect_test "degenerate_triangle_area" =
  let a = pt #1.0 #0.0 #0.0 in
  let area = S2.S2_measures.area a a a in
  printf "area=%.17g\n" (Float_u.to_float area);
  [%expect {| area=0 |}]
;;
