open Core

let%expect_test "zero_sexp" =
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.R3_vector.t] S2.R3_vector.zero));
  [%expect {| ((x 0)(y 0)(z 0)) |}]
;;

let%expect_test "create_and_accessors" =
  let v = S2.R3_vector.create ~x:#1.0 ~y:#2.0 ~z:#3.0 in
  printf
    "x=%.17g y=%.17g z=%.17g\n"
    (Float_u.to_float (S2.R3_vector.x v))
    (Float_u.to_float (S2.R3_vector.y v))
    (Float_u.to_float (S2.R3_vector.z v));
  [%expect {| x=1 y=2 z=3 |}]
;;

let%expect_test "norm_norm2" =
  let v = S2.R3_vector.create ~x:#1.0 ~y:#2.0 ~z:#2.0 in
  printf
    "norm2=%.17g norm=%.17g\n"
    (Float_u.to_float (S2.R3_vector.norm2 v))
    (Float_u.to_float (S2.R3_vector.norm v));
  [%expect {| norm2=9 norm=3 |}]
;;

let%expect_test "normalize_unit" =
  let v = S2.R3_vector.create ~x:#0.0 ~y:#0.0 ~z:#5.0 in
  let n = S2.R3_vector.normalize v in
  printf
    "z=%.17g norm=%.17g\n"
    (Float_u.to_float (S2.R3_vector.z n))
    (Float_u.to_float (S2.R3_vector.norm n));
  [%expect {| z=1 norm=1 |}]
;;

let%expect_test "cross_product" =
  let x = S2.R3_vector.create ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let y = S2.R3_vector.create ~x:#0.0 ~y:#1.0 ~z:#0.0 in
  let z = S2.R3_vector.cross x y in
  Stdlib.print_endline (S2.R3_vector.to_string z);
  [%expect {| (0., 0., 1.) |}]
;;

let%expect_test "largest_abs_component" =
  let v = S2.R3_vector.create ~x:#1.0 ~y:(-#3.0) ~z:#2.0 in
  printf "%d\n" (S2.R3_vector.largest_abs_component v);
  [%expect {| 1 |}]
;;
