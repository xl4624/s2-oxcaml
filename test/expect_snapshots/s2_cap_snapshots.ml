open Core

let%expect_test "empty_sexp" =
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_cap.t] S2.S2_cap.empty));
  [%expect {| ((center((x 1)(y 0)(z 0)))(radius((length2 -1)))) |}]
;;

let%expect_test "full_sexp" =
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_cap.t] S2.S2_cap.full));
  [%expect {| ((center((x 1)(y 0)(z 0)))(radius((length2 4)))) |}]
;;

let%expect_test "empty_predicates" =
  printf
    "empty: %b full: %b valid: %b\n"
    (S2.S2_cap.is_empty S2.S2_cap.empty)
    (S2.S2_cap.is_full S2.S2_cap.empty)
    (S2.S2_cap.is_valid S2.S2_cap.empty);
  [%expect {| empty: true full: false valid: true |}]
;;

let%expect_test "full_predicates" =
  printf
    "empty: %b full: %b valid: %b\n"
    (S2.S2_cap.is_empty S2.S2_cap.full)
    (S2.S2_cap.is_full S2.S2_cap.full)
    (S2.S2_cap.is_valid S2.S2_cap.full);
  [%expect {| empty: false full: true valid: true |}]
;;

let%expect_test "complement_empty_is_full" =
  printf "%b\n" (S2.S2_cap.is_full (S2.S2_cap.complement S2.S2_cap.empty));
  [%expect {| true |}]
;;

let%expect_test "complement_full_is_empty" =
  printf "%b\n" (S2.S2_cap.is_empty (S2.S2_cap.complement S2.S2_cap.full));
  [%expect {| true |}]
;;

let%expect_test "of_point_contains_center" =
  let p = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let cap = S2.S2_cap.of_point p in
  printf
    "contains: %b height: %s\n"
    (S2.S2_cap.contains_point cap p)
    (Float_u.to_string (S2.S2_cap.height cap));
  [%expect {| contains: true height: 0. |}]
;;

let%expect_test "hemisphere_height" =
  let cap =
    S2.S2_cap.of_center_angle
      (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0)
      (S2.S1_angle.of_degrees #90.0)
  in
  printf "height=%s\n" (Float_u.to_string (S2.S2_cap.height cap));
  [%expect {| height=0.99999999999999978 |}]
;;

let%expect_test "full_area" =
  printf "%.17g\n" (Float_u.to_float (S2.S2_cap.area S2.S2_cap.full));
  [%expect {| 12.566370614359172 |}]
;;

let%expect_test "empty_area" =
  printf "%.17g\n" (Float_u.to_float (S2.S2_cap.area S2.S2_cap.empty));
  [%expect {| 0 |}]
;;

let%expect_test "add_point_to_empty" =
  let p = S2.S2_point.of_coords ~x:#0.0 ~y:#1.0 ~z:#0.0 in
  let cap = S2.S2_cap.add_point S2.S2_cap.empty p in
  printf
    "empty: %b contains: %b\n"
    (S2.S2_cap.is_empty cap)
    (S2.S2_cap.contains_point cap p);
  [%expect {| empty: false contains: true |}]
;;

let%expect_test "union_with_empty" =
  let cap =
    S2.S2_cap.of_center_angle
      (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0)
      (S2.S1_angle.of_degrees #10.0)
  in
  let u = S2.S2_cap.union cap S2.S2_cap.empty in
  printf "area_eq: %b\n" (Float_u.equal (S2.S2_cap.area u) (S2.S2_cap.area cap));
  [%expect {| area_eq: true |}]
;;

let%expect_test "exn_path" =
  (* [decode_exn] requires a 32-byte payload; shorter strings must raise. *)
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (S2.S2_cap.decode_exn "not-enough-bytes" : S2.S2_cap.t));
  [%expect {| (raised (Failure "t.Option.value_exn: none")) |}];
  (* [expanded_exn] with a negative distance is invalid. *)
  Expect_test_helpers_core.show_raise (fun () ->
    ignore
      (S2.S2_cap.expanded_exn S2.S2_cap.full (S2.S1_angle.of_degrees (-#1.0))
       : S2.S2_cap.t));
  [%expect
    {|
    (raised (
      "S2_cap.expanded: distance must be non-negative"
      (distance ((radians -0.017453292519943295)))))
    |}]
;;

let%expect_test "encode_decode_roundtrip" =
  let cap =
    S2.S2_cap.of_center_angle
      (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0)
      (S2.S1_angle.of_degrees #45.0)
  in
  let encoded = S2.S2_cap.encode cap in
  let decoded = S2.S2_cap.Option.value_exn (S2.S2_cap.decode encoded) in
  printf "equal: %b\n" (S2.S2_cap.equal decoded cap);
  [%expect {| equal: true |}]
;;
