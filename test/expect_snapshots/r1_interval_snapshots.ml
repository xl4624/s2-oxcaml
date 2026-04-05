open Core

let%expect_test "empty_sexp" =
  Stdlib.print_endline
    (Sexp.to_string ([%sexp_of: S2.R1_interval.t] S2.R1_interval.empty));
  [%expect {| ((lo 1)(hi 0)) |}]
;;

let%expect_test "empty_is_empty" =
  printf "%b\n" (S2.R1_interval.is_empty S2.R1_interval.empty);
  [%expect {| true |}]
;;

let%expect_test "empty_length_negative" =
  printf "%.17g\n" (Float_u.to_float (S2.R1_interval.length S2.R1_interval.empty));
  [%expect {| -1 |}]
;;

let%expect_test "create_and_accessors" =
  let i = S2.R1_interval.create ~lo:#1.5 ~hi:#3.5 in
  printf
    "lo=%.17g hi=%.17g len=%.17g mid=%.17g\n"
    (Float_u.to_float (S2.R1_interval.lo i))
    (Float_u.to_float (S2.R1_interval.hi i))
    (Float_u.to_float (S2.R1_interval.length i))
    (Float_u.to_float (S2.R1_interval.center i));
  [%expect {| lo=1.5 hi=3.5 len=2 mid=2.5 |}]
;;

let%expect_test "contains_point" =
  let i = S2.R1_interval.create ~lo:#1.0 ~hi:#5.0 in
  printf "contains 3.0: %b\n" (S2.R1_interval.contains i #3.0);
  printf "contains 0.5: %b\n" (S2.R1_interval.contains i #0.5);
  printf "contains 1.0: %b\n" (S2.R1_interval.contains i #1.0);
  printf "contains 5.0: %b\n" (S2.R1_interval.contains i #5.0);
  [%expect
    {|
    contains 3.0: true
    contains 0.5: false
    contains 1.0: true
    contains 5.0: true |}]
;;

let%expect_test "intersection_and_union" =
  let a = S2.R1_interval.create ~lo:#1.0 ~hi:#5.0 in
  let b = S2.R1_interval.create ~lo:#3.0 ~hi:#7.0 in
  let inter = S2.R1_interval.intersection a b in
  let union = S2.R1_interval.union a b in
  printf
    "inter: lo=%.17g hi=%.17g\n"
    (Float_u.to_float (S2.R1_interval.lo inter))
    (Float_u.to_float (S2.R1_interval.hi inter));
  printf
    "union: lo=%.17g hi=%.17g\n"
    (Float_u.to_float (S2.R1_interval.lo union))
    (Float_u.to_float (S2.R1_interval.hi union));
  [%expect {|
    inter: lo=3 hi=5
    union: lo=1 hi=7 |}]
;;
