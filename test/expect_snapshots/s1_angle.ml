open Core

let%expect_test "zero_sexp" =
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S1_angle.t] S2.S1_angle.zero));
  [%expect {| ((radians 0)) |}]
;;

let%expect_test "of_degrees_90" =
  let a = S2.S1_angle.of_degrees #90.0 in
  printf
    "radians=%.17g degrees=%.17g\n"
    (Float_u.to_float (S2.S1_angle.radians a))
    (Float_u.to_float (S2.S1_angle.degrees a));
  [%expect {| radians=1.5707963267948966 degrees=90 |}]
;;

let%expect_test "infinity" =
  printf "is_inf: %b\n" (S2.S1_angle.is_inf S2.S1_angle.infinity);
  [%expect {| is_inf: true |}]
;;

let%expect_test "normalized_positive" =
  let a = S2.S1_angle.of_degrees #540.0 in
  let n = S2.S1_angle.normalized a in
  printf "%.17g\n" (Float_u.to_float (S2.S1_angle.degrees n));
  [%expect {| 180 |}]
;;

let%expect_test "neg_involution" =
  let a = S2.S1_angle.of_degrees #45.0 in
  let nn = S2.S1_angle.neg (S2.S1_angle.neg a) in
  printf
    "equal: %b\n"
    (Float.( = )
       (Float_u.to_float (S2.S1_angle.radians a))
       (Float_u.to_float (S2.S1_angle.radians nn)));
  [%expect {| equal: true |}]
;;

let%expect_test "e5_e6_e7" =
  let a5 = S2.S1_angle.of_e5 4500000 in
  let a6 = S2.S1_angle.of_e6 45000000 in
  let a7 = S2.S1_angle.of_e7 450000000 in
  printf
    "e5=%.17g e6=%.17g e7=%.17g\n"
    (Float_u.to_float (S2.S1_angle.degrees a5))
    (Float_u.to_float (S2.S1_angle.degrees a6))
    (Float_u.to_float (S2.S1_angle.degrees a7));
  [%expect {| e5=45.000000000000007 e6=45 e7=45 |}]
;;

let%expect_test "exn_path" =
  (* The E5/E6/E7 encodings round degrees * 10^k to a signed 32-bit int, so an infinite
     angle overflows all three encodings and their [_exn] variants raise. *)
  let inf = S2.S1_angle.infinity in
  Expect_test_helpers_core.show_raise (fun () -> ignore (S2.S1_angle.e5_exn inf : int));
  [%expect {| (raised ("S1Angle.e5_exn: angle overflows int" (degrees INF))) |}];
  Expect_test_helpers_core.show_raise (fun () -> ignore (S2.S1_angle.e6_exn inf : int));
  [%expect {| (raised ("S1Angle.e6_exn: angle overflows int" (degrees INF))) |}];
  Expect_test_helpers_core.show_raise (fun () -> ignore (S2.S1_angle.e7_exn inf : int));
  [%expect {| (raised ("S1Angle.e7_exn: angle overflows int" (degrees INF))) |}]
;;

let%expect_test "add_sub" =
  let a = S2.S1_angle.of_degrees #30.0 in
  let b = S2.S1_angle.of_degrees #60.0 in
  printf
    "add=%.17g sub=%.17g\n"
    (Float_u.to_float (S2.S1_angle.degrees (S2.S1_angle.add a b)))
    (Float_u.to_float (S2.S1_angle.degrees (S2.S1_angle.sub a b)));
  [%expect {| add=90 sub=-29.999999999999996 |}]
;;
