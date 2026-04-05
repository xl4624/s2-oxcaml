open Core

let%expect_test "empty_sexp" =
  Stdlib.print_endline
    (Sexp.to_string ([%sexp_of: S2.S1_interval.t] S2.S1_interval.empty));
  [%expect {| ((lo 3.1415926535897931)(hi -3.1415926535897931)) |}]
;;

let%expect_test "full_sexp" =
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S1_interval.t] S2.S1_interval.full));
  [%expect {| ((lo -3.1415926535897931)(hi 3.1415926535897931)) |}]
;;

let%expect_test "empty_is_empty" =
  printf
    "is_empty: %b is_full: %b\n"
    (S2.S1_interval.is_empty S2.S1_interval.empty)
    (S2.S1_interval.is_full S2.S1_interval.empty);
  [%expect {| is_empty: true is_full: false |}]
;;

let%expect_test "full_is_full" =
  printf
    "is_empty: %b is_full: %b\n"
    (S2.S1_interval.is_empty S2.S1_interval.full)
    (S2.S1_interval.is_full S2.S1_interval.full);
  [%expect {| is_empty: false is_full: true |}]
;;

let%expect_test "complement" =
  let i = S2.S1_interval.from_point_pair #0.0 #1.0 in
  let c = S2.S1_interval.complement i in
  printf
    "orig: lo=%.17g hi=%.17g\n"
    (Float_u.to_float (S2.S1_interval.lo i))
    (Float_u.to_float (S2.S1_interval.hi i));
  printf
    "complement: lo=%.17g hi=%.17g\n"
    (Float_u.to_float (S2.S1_interval.lo c))
    (Float_u.to_float (S2.S1_interval.hi c));
  [%expect {|
    orig: lo=0 hi=1
    complement: lo=1 hi=0 |}]
;;

let%expect_test "length" =
  let i = S2.S1_interval.create ~lo:#0.0 ~hi:#1.0 in
  printf "length=%.17g\n" (Float_u.to_float (S2.S1_interval.length i));
  [%expect {| length=1 |}]
;;
