open Core

let%expect_test "zero_sexp" =
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.R2_point.t] S2.R2_point.zero));
  [%expect {| ((x 0)(y 0)) |}]
;;

let%expect_test "create_and_accessors" =
  let p = S2.R2_point.create ~x:#3.0 ~y:#4.0 in
  printf
    "x=%.17g y=%.17g\n"
    (Float_u.to_float (S2.R2_point.x p))
    (Float_u.to_float (S2.R2_point.y p));
  [%expect {| x=3 y=4 |}]
;;

let%expect_test "add_sub_neg" =
  let a = S2.R2_point.create ~x:#1.0 ~y:#2.0 in
  let b = S2.R2_point.create ~x:#3.0 ~y:#5.0 in
  let sum = S2.R2_point.add a b in
  let diff = S2.R2_point.sub a b in
  let neg = S2.R2_point.neg a in
  printf "sum: %s\n" (S2.R2_point.to_string sum);
  printf "diff: %s\n" (S2.R2_point.to_string diff);
  printf "neg: %s\n" (S2.R2_point.to_string neg);
  [%expect {|
    sum: (4., 7.)
    diff: (-2., -3.)
    neg: (-1., -2.)
    |}]
;;

let%expect_test "dot_cross_norm" =
  let a = S2.R2_point.create ~x:#3.0 ~y:#4.0 in
  let b = S2.R2_point.create ~x:#1.0 ~y:#0.0 in
  printf
    "dot=%.17g cross=%.17g norm2=%.17g norm=%.17g\n"
    (Float_u.to_float (S2.R2_point.dot a b))
    (Float_u.to_float (S2.R2_point.cross a b))
    (Float_u.to_float (S2.R2_point.norm2 a))
    (Float_u.to_float (S2.R2_point.norm a));
  [%expect {| dot=3 cross=-4 norm2=25 norm=5 |}]
;;

let%expect_test "ortho" =
  let p = S2.R2_point.create ~x:#3.0 ~y:#4.0 in
  let o = S2.R2_point.ortho p in
  printf
    "ortho: %s, dot=%.17g\n"
    (S2.R2_point.to_string o)
    (Float_u.to_float (S2.R2_point.dot p o));
  [%expect {| ortho: (-4., 3.), dot=0 |}]
;;

let%expect_test "normalize_unit_vector" =
  let p = S2.R2_point.create ~x:#3.0 ~y:#4.0 in
  let n = S2.R2_point.normalize p in
  printf "norm=%.17g\n" (Float_u.to_float (S2.R2_point.norm n));
  [%expect {| norm=1 |}]
;;

let%expect_test "option_none" =
  Stdlib.print_endline
    (Sexp.to_string ([%sexp_of: S2.R2_point.Option.t] S2.R2_point.Option.none));
  [%expect {| () |}]
;;

let%expect_test "option_some" =
  let p = S2.R2_point.create ~x:#3.0 ~y:#4.0 in
  let opt = S2.R2_point.Option.some p in
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.R2_point.Option.t] opt));
  [%expect {| (((x 3)(y 4))) |}]
;;

let%expect_test "option_value_exn_none" =
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (S2.R2_point.Option.value_exn S2.R2_point.Option.none : S2.R2_point.t));
  [%expect {| (raised (Failure "t.Option.value_exn: none")) |}]
;;

let%expect_test "option_value_default" =
  let default = S2.R2_point.create ~x:#7.0 ~y:#8.0 in
  let result = S2.R2_point.Option.value S2.R2_point.Option.none ~default in
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.R2_point.t] result));
  [%expect {| ((x 7)(y 8)) |}]
;;

let%expect_test "option_match_optional_u_some" =
  let p = S2.R2_point.create ~x:#1.0 ~y:#2.0 in
  let opt = S2.R2_point.Option.some p in
  let result =
    match%optional_u.S2.R2_point.Option opt with
    | None -> "none"
    | Some v -> S2.R2_point.to_string v
  in
  printf "%s\n" result;
  [%expect {| (1., 2.) |}]
;;

let%expect_test "option_match_optional_u_none" =
  let result =
    match%optional_u.S2.R2_point.Option S2.R2_point.Option.none with
    | None -> "none"
    | Some _ -> "some"
  in
  printf "%s\n" result;
  [%expect {| none |}]
;;
