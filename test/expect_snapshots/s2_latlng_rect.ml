open Core

(* No error-path snapshot: [S2_latlng_rect] exposes no [_exn] functions in
   [lib/s2_latlng_rect.mli]. *)

let%expect_test "empty_sexp" =
  Stdlib.print_endline
    (Sexp.to_string ([%sexp_of: S2.S2_latlng_rect.t] S2.S2_latlng_rect.empty));
  [%expect
    {| ((lat((lo 1)(hi 0)))(lng((lo 3.1415926535897931)(hi -3.1415926535897931)))) |}]
;;

let%expect_test "full_sexp" =
  Stdlib.print_endline
    (Sexp.to_string ([%sexp_of: S2.S2_latlng_rect.t] S2.S2_latlng_rect.full));
  [%expect
    {| ((lat((lo -1.5707963267948966)(hi 1.5707963267948966)))(lng((lo -3.1415926535897931)(hi 3.1415926535897931)))) |}]
;;

let%expect_test "empty_predicates" =
  printf
    "valid: %b empty: %b full: %b point: %b\n"
    (S2.S2_latlng_rect.is_valid S2.S2_latlng_rect.empty)
    (S2.S2_latlng_rect.is_empty S2.S2_latlng_rect.empty)
    (S2.S2_latlng_rect.is_full S2.S2_latlng_rect.empty)
    (S2.S2_latlng_rect.is_point S2.S2_latlng_rect.empty);
  [%expect {| valid: true empty: true full: false point: false |}]
;;

let%expect_test "full_predicates" =
  printf
    "valid: %b empty: %b full: %b point: %b\n"
    (S2.S2_latlng_rect.is_valid S2.S2_latlng_rect.full)
    (S2.S2_latlng_rect.is_empty S2.S2_latlng_rect.full)
    (S2.S2_latlng_rect.is_full S2.S2_latlng_rect.full)
    (S2.S2_latlng_rect.is_point S2.S2_latlng_rect.full);
  [%expect {| valid: true empty: false full: true point: false |}]
;;

(* A hand-crafted rectangle over central Europe, roughly [40, 55] lat x [0, 20] lng
   (degrees). *)
let europe () =
  S2.S2_latlng_rect.of_lo_hi
    ~lo:(S2.S2_latlng.of_degrees ~lat:#40.0 ~lng:#0.0)
    ~hi:(S2.S2_latlng.of_degrees ~lat:#55.0 ~lng:#20.0)
;;

let print_rect_deg label r =
  let lo = S2.S2_latlng_rect.lo r in
  let hi = S2.S2_latlng_rect.hi r in
  printf
    "%s lo=(%.4f, %.4f) hi=(%.4f, %.4f)\n"
    label
    (Float_u.to_float (S2.S1_angle.degrees (S2.S2_latlng.lat lo)))
    (Float_u.to_float (S2.S1_angle.degrees (S2.S2_latlng.lng lo)))
    (Float_u.to_float (S2.S1_angle.degrees (S2.S2_latlng.lat hi)))
    (Float_u.to_float (S2.S1_angle.degrees (S2.S2_latlng.lng hi)))
;;

let%expect_test "europe_rect_corners" =
  print_rect_deg "europe" (europe ());
  [%expect {| europe lo=(40.0000, 0.0000) hi=(55.0000, 20.0000) |}]
;;

let%expect_test "union_disjoint" =
  (* Two disjoint mid-latitude rects: Europe and a small box far east. *)
  let east =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:#30.0 ~lng:#100.0)
      ~hi:(S2.S2_latlng.of_degrees ~lat:#35.0 ~lng:#110.0)
  in
  let u = S2.S2_latlng_rect.union (europe ()) east in
  print_rect_deg "union" u;
  printf
    "contains_europe: %b contains_east: %b\n"
    (S2.S2_latlng_rect.contains u (europe ()))
    (S2.S2_latlng_rect.contains u east);
  [%expect
    {|
    union lo=(30.0000, 0.0000) hi=(55.0000, 110.0000)
    contains_europe: true contains_east: true
    |}]
;;

let%expect_test "intersection_overlapping" =
  let a =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#0.0)
      ~hi:(S2.S2_latlng.of_degrees ~lat:#10.0 ~lng:#10.0)
  in
  let b =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:#5.0 ~lng:#5.0)
      ~hi:(S2.S2_latlng.of_degrees ~lat:#15.0 ~lng:#15.0)
  in
  let i = S2.S2_latlng_rect.intersection a b in
  print_rect_deg "intersection" i;
  printf "is_empty: %b\n" (S2.S2_latlng_rect.is_empty i);
  [%expect
    {|
    intersection lo=(5.0000, 5.0000) hi=(10.0000, 10.0000)
    is_empty: false
    |}]
;;

let%expect_test "cap_bound_mid_latitude" =
  let r = europe () in
  let cap = S2.S2_latlng_rect.cap_bound r in
  let center = S2.S2_cap.center cap in
  printf
    "center=(%.6f, %.6f, %.6f) height=%.6f\n"
    (Float_u.to_float (S2.R3_vector.x center))
    (Float_u.to_float (S2.R3_vector.y center))
    (Float_u.to_float (S2.R3_vector.z center))
    (Float_u.to_float (S2.S2_cap.height cap));
  [%expect {| center=(0.665326, 0.117315, 0.737277) height=0.016418 |}]
;;

let%expect_test "expanded_by_margin" =
  let r =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:#10.0 ~lng:#20.0)
      ~hi:(S2.S2_latlng.of_degrees ~lat:#20.0 ~lng:#30.0)
  in
  let margin = S2.S2_latlng.of_degrees ~lat:#1.0 ~lng:#2.0 in
  let e = S2.S2_latlng_rect.expanded r margin in
  print_rect_deg "expanded" e;
  printf "contains_original: %b\n" (S2.S2_latlng_rect.contains e r);
  [%expect
    {|
    expanded lo=(9.0000, 18.0000) hi=(21.0000, 32.0000)
    contains_original: true
    |}]
;;

let%expect_test "containment_nested" =
  let outer =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:(-#30.0) ~lng:(-#60.0))
      ~hi:(S2.S2_latlng.of_degrees ~lat:#30.0 ~lng:#60.0)
  in
  let inner =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:(-#10.0) ~lng:(-#20.0))
      ~hi:(S2.S2_latlng.of_degrees ~lat:#10.0 ~lng:#20.0)
  in
  printf
    "outer_contains_inner: %b inner_contains_outer: %b intersects: %b\n"
    (S2.S2_latlng_rect.contains outer inner)
    (S2.S2_latlng_rect.contains inner outer)
    (S2.S2_latlng_rect.intersects outer inner);
  [%expect {| outer_contains_inner: true inner_contains_outer: false intersects: true |}]
;;

let%expect_test "containment_disjoint" =
  let a =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#0.0)
      ~hi:(S2.S2_latlng.of_degrees ~lat:#10.0 ~lng:#10.0)
  in
  let b =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:#20.0 ~lng:#20.0)
      ~hi:(S2.S2_latlng.of_degrees ~lat:#30.0 ~lng:#30.0)
  in
  printf
    "a_contains_b: %b b_contains_a: %b intersects: %b\n"
    (S2.S2_latlng_rect.contains a b)
    (S2.S2_latlng_rect.contains b a)
    (S2.S2_latlng_rect.intersects a b);
  [%expect {| a_contains_b: false b_contains_a: false intersects: false |}]
;;

let%expect_test "longitude_wraparound" =
  (* Rectangle crossing the antimeridian: longitude from 170 to -170 degrees. *)
  let r =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#170.0)
      ~hi:(S2.S2_latlng.of_degrees ~lat:#10.0 ~lng:(-#170.0))
  in
  printf "is_inverted: %b\n" (S2.S2_latlng_rect.is_inverted r);
  printf
    "contains (5, 175): %b\n"
    (S2.S2_latlng_rect.contains_latlng r (S2.S2_latlng.of_degrees ~lat:#5.0 ~lng:#175.0));
  printf
    "contains (5, -175): %b\n"
    (S2.S2_latlng_rect.contains_latlng
       r
       (S2.S2_latlng.of_degrees ~lat:#5.0 ~lng:(-#175.0)));
  printf
    "contains (5, 0): %b\n"
    (S2.S2_latlng_rect.contains_latlng r (S2.S2_latlng.of_degrees ~lat:#5.0 ~lng:#0.0));
  [%expect
    {|
    is_inverted: true
    contains (5, 175): true
    contains (5, -175): true
    contains (5, 0): false
    |}]
;;

let%expect_test "full_area" =
  printf "%.10f\n" (Float_u.to_float (S2.S2_latlng_rect.area S2.S2_latlng_rect.full));
  [%expect {| 12.5663706144 |}]
;;

let%expect_test "empty_area" =
  printf "%.10f\n" (Float_u.to_float (S2.S2_latlng_rect.area S2.S2_latlng_rect.empty));
  [%expect {| 0.0000000000 |}]
;;
