open Core

let%expect_test "zero_sexp" =
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_latlng.t] S2.S2_latlng.zero));
  [%expect {| ((lat 0)(lng 0)) |}]
;;

let%expect_test "invalid_sexp" =
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_latlng.t] S2.S2_latlng.invalid));
  [%expect {| ((lat 3.1415926535897931)(lng 6.2831853071795862)) |}]
;;

let%expect_test "of_degrees" =
  let ll = S2.S2_latlng.of_degrees ~lat:#45.0 ~lng:#90.0 in
  printf
    "lat=%s lng=%s valid=%b\n"
    (Float_u.to_string (S2.S1_angle.degrees (S2.S2_latlng.lat ll)))
    (Float_u.to_string (S2.S1_angle.degrees (S2.S2_latlng.lng ll)))
    (S2.S2_latlng.is_valid ll);
  [%expect {| lat=45. lng=90. valid=true |}]
;;

let%expect_test "invalid_is_not_valid" =
  printf "%b\n" (S2.S2_latlng.is_valid S2.S2_latlng.invalid);
  [%expect {| false |}]
;;

let%expect_test "to_point_and_back" =
  let ll = S2.S2_latlng.of_degrees ~lat:#45.0 ~lng:#90.0 in
  let p = S2.S2_latlng.to_point ll in
  let back = S2.S2_latlng.of_point p in
  printf
    "lat=%s lng=%s\n"
    (Float_u.to_string (S2.S1_angle.degrees (S2.S2_latlng.lat back)))
    (Float_u.to_string (S2.S1_angle.degrees (S2.S2_latlng.lng back)));
  [%expect {| lat=45. lng=90. |}]
;;

let%expect_test "normalized_clamps_lat" =
  let ll = S2.S2_latlng.of_degrees ~lat:#100.0 ~lng:#200.0 in
  let n = S2.S2_latlng.normalized ll in
  printf
    "lat=%s lng=%s valid=%b\n"
    (Float_u.to_string (S2.S1_angle.degrees (S2.S2_latlng.lat n)))
    (Float_u.to_string (S2.S1_angle.degrees (S2.S2_latlng.lng n)))
    (S2.S2_latlng.is_valid n);
  [%expect {| lat=90. lng=-160. valid=true |}]
;;

let%expect_test "distance_same_point" =
  let ll = S2.S2_latlng.of_degrees ~lat:#45.0 ~lng:#90.0 in
  printf "%s\n" (Float_u.to_string (S2.S1_angle.radians (S2.S2_latlng.distance ll ll)));
  [%expect {| 0. |}]
;;

let%expect_test "distance_antipodal" =
  let a = S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#0.0 in
  let b = S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#180.0 in
  printf "%s\n" (Float_u.to_string (S2.S1_angle.degrees (S2.S2_latlng.distance a b)));
  [%expect {| 180. |}]
;;

let%expect_test "approx_equal" =
  let a = S2.S2_latlng.of_degrees ~lat:#45.0 ~lng:#90.0 in
  let b = S2.S2_latlng.of_degrees ~lat:#45.0 ~lng:#90.0 in
  let c = S2.S2_latlng.of_degrees ~lat:#46.0 ~lng:#91.0 in
  printf
    "same: %b different: %b\n"
    (S2.S2_latlng.approx_equal a b)
    (S2.S2_latlng.approx_equal a c);
  [%expect {| same: true different: false |}]
;;

let%expect_test "add_sub" =
  let a = S2.S2_latlng.of_radians ~lat:#0.1 ~lng:#0.2 in
  let b = S2.S2_latlng.of_radians ~lat:#0.3 ~lng:#0.4 in
  let sum = S2.S2_latlng.add a b in
  let diff = S2.S2_latlng.sub a b in
  printf
    "sum: lat=%s lng=%s\n"
    (Float_u.to_string (S2.S1_angle.radians (S2.S2_latlng.lat sum)))
    (Float_u.to_string (S2.S1_angle.radians (S2.S2_latlng.lng sum)));
  printf
    "diff: lat=%s lng=%s\n"
    (Float_u.to_string (S2.S1_angle.radians (S2.S2_latlng.lat diff)))
    (Float_u.to_string (S2.S1_angle.radians (S2.S2_latlng.lng diff)));
  [%expect
    {|
    sum: lat=0.4 lng=0.60000000000000009
    diff: lat=-0.19999999999999998 lng=-0.2
    |}]
;;
