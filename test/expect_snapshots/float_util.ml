open Core

let%expect_test "ieee_remainder_differs_from_truncated_mod" =
  (* [ieee_remainder] rounds the quotient to nearest even (IEEE / C [remainder]);
     [mod_float] truncates toward zero. *)
  let a = 3.0 in
  let b = 2.0 in
  printf "ieee %.17g mod %.17g\n" (S2.Float_util.ieee_remainder a b) (Float.mod_float a b);
  [%expect {| ieee -1 mod 1 |}]
;;

let%expect_test "ieee_remainder_negative" =
  printf "%.17g\n" (S2.Float_util.ieee_remainder (-3.0) 2.0);
  [%expect {| 1 |}]
;;

let%expect_test "ieee_remainder_exact_half" =
  (* 0.5 / 1.0 = 0.5 exactly; IEEE rounds to even -> 0 *)
  printf "%.17g\n" (S2.Float_util.ieee_remainder 0.5 1.0);
  [%expect {| 0.5 |}]
;;

let%expect_test "ieee_remainder_at_2" =
  printf "%.17g\n" (S2.Float_util.ieee_remainder 2.0 2.0);
  [%expect {| 0 |}]
;;

let%expect_test "ieee_remainder_u_basic" =
  (* Unboxed variant: same semantics, float# in/out. *)
  let r = S2.Float_util.ieee_remainder_u #3.0 #2.0 in
  printf "%.17g\n" (Float_u.to_float r);
  [%expect {| -1 |}]
;;

let%expect_test "ieee_remainder_u_negative" =
  let r = S2.Float_util.ieee_remainder_u (-#3.0) #2.0 in
  printf "%.17g\n" (Float_u.to_float r);
  [%expect {| 1 |}]
;;
