open Core

let%expect_test "zero_sexp" =
  Stdlib.print_endline
    (Sexp.to_string ([%sexp_of: S2.S1_chord_angle.t] S2.S1_chord_angle.zero));
  [%expect {| ((length2 0)) |}]
;;

let%expect_test "constants" =
  printf
    "zero: %.17g\n"
    (Float_u.to_float (S2.S1_chord_angle.length2 S2.S1_chord_angle.zero));
  printf
    "right: %.17g\n"
    (Float_u.to_float (S2.S1_chord_angle.length2 S2.S1_chord_angle.right));
  printf
    "straight: %.17g\n"
    (Float_u.to_float (S2.S1_chord_angle.length2 S2.S1_chord_angle.straight));
  printf
    "negative: %.17g\n"
    (Float_u.to_float (S2.S1_chord_angle.length2 S2.S1_chord_angle.negative));
  [%expect {|
    zero: 0
    right: 2
    straight: 4
    negative: -1 |}]
;;

let%expect_test "predicates" =
  printf "zero is_zero: %b\n" (S2.S1_chord_angle.is_zero S2.S1_chord_angle.zero);
  printf
    "negative is_negative: %b\n"
    (S2.S1_chord_angle.is_negative S2.S1_chord_angle.negative);
  printf
    "infinity is_infinity: %b\n"
    (S2.S1_chord_angle.is_infinity S2.S1_chord_angle.infinity);
  printf
    "negative is_special: %b\n"
    (S2.S1_chord_angle.is_special S2.S1_chord_angle.negative);
  printf
    "infinity is_special: %b\n"
    (S2.S1_chord_angle.is_special S2.S1_chord_angle.infinity);
  printf "right is_special: %b\n" (S2.S1_chord_angle.is_special S2.S1_chord_angle.right);
  printf "right is_valid: %b\n" (S2.S1_chord_angle.is_valid S2.S1_chord_angle.right);
  [%expect
    {|
    zero is_zero: true
    negative is_negative: true
    infinity is_infinity: true
    negative is_special: true
    infinity is_special: true
    right is_special: false
    right is_valid: true |}]
;;

let%expect_test "of_angle_roundtrip" =
  let a = S2.S1_angle.of_degrees #60.0 in
  let ca = S2.S1_chord_angle.of_angle a in
  let back = S2.S1_chord_angle.to_angle ca in
  printf "degrees=%.17g\n" (Float_u.to_float (S2.S1_angle.degrees back));
  [%expect {| degrees=59.999999999999993 |}]
;;

let%expect_test "trig_right_angle" =
  let r = S2.S1_chord_angle.right in
  printf
    "sin=%.17g cos=%.17g tan=%.17g\n"
    (Float_u.to_float (S2.S1_chord_angle.sin r))
    (Float_u.to_float (S2.S1_chord_angle.cos r))
    (Float_u.to_float (S2.S1_chord_angle.tan r));
  [%expect {| sin=1 cos=0 tan=inf |}]
;;

let%expect_test "successor_predecessor" =
  let z = S2.S1_chord_angle.zero in
  let s = S2.S1_chord_angle.successor z in
  let ps = S2.S1_chord_angle.predecessor s in
  printf "successor(zero)=%.17g\n" (Float_u.to_float (S2.S1_chord_angle.length2 s));
  printf
    "predecessor(successor(zero))=%.17g\n"
    (Float_u.to_float (S2.S1_chord_angle.length2 ps));
  printf
    "predecessor(zero)=%.17g\n"
    (Float_u.to_float (S2.S1_chord_angle.length2 (S2.S1_chord_angle.predecessor z)));
  [%expect
    {|
    successor(zero)=4.9406564584124654e-324
    predecessor(successor(zero))=0
    predecessor(zero)=-1 |}]
;;

let%expect_test "add_sub" =
  let d30 = S2.S1_chord_angle.of_degrees #30.0 in
  let d60 = S2.S1_chord_angle.of_degrees #60.0 in
  let sum = S2.S1_chord_angle.add d30 d60 in
  let diff = S2.S1_chord_angle.sub d60 d30 in
  printf
    "sum_deg=%.17g diff_deg=%.17g\n"
    (Float_u.to_float (S2.S1_chord_angle.degrees sum))
    (Float_u.to_float (S2.S1_chord_angle.degrees diff));
  [%expect {| sum_deg=89.999999999999986 diff_deg=29.999999999999996 |}]
;;

let%expect_test "option_none_sexp" =
  Stdlib.print_endline
    (Sexp.to_string
       ([%sexp_of: S2.S1_chord_angle.Option.t] S2.S1_chord_angle.Option.none));
  [%expect {| () |}]
;;

let%expect_test "option_some_sexp" =
  let ca = S2.S1_chord_angle.of_degrees #45.0 in
  let opt = S2.S1_chord_angle.Option.some ca in
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S1_chord_angle.Option.t] opt));
  [%expect {| (0.585786437626905) |}]
;;

let%expect_test "option_value_exn_none" =
  Expect_test_helpers_core.show_raise (fun () ->
    ignore
      (S2.S1_chord_angle.Option.value_exn S2.S1_chord_angle.Option.none
       : S2.S1_chord_angle.t));
  [%expect {| (raised (Failure "t.Option.value_exn: none")) |}]
;;

let%expect_test "option_match_optional_u_some" =
  let ca = S2.S1_chord_angle.of_degrees #90.0 in
  let opt = S2.S1_chord_angle.Option.some ca in
  let result =
    match%optional_u.S2.S1_chord_angle.Option opt with
    | None -> "none"
    | Some v -> sprintf "%.17g" (Float_u.to_float (S2.S1_chord_angle.degrees v))
  in
  printf "%s\n" result;
  [%expect {| 89.999999999999986 |}]
;;

let%expect_test "option_match_optional_u_none" =
  let result =
    match%optional_u.S2.S1_chord_angle.Option S2.S1_chord_angle.Option.none with
    | None -> "none"
    | Some _ -> "some"
  in
  printf "%s\n" result;
  [%expect {| none |}]
;;
