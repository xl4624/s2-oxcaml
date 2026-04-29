(* C++ test parity: s2geometry/src/s2/s2cap_test.cc
   Golden data from test/gen/s2cap.cc.

   Covered:
   -  TEST(S2Cap, Basic)
   -  TEST(S2Cap, AddEmptyCapToNonEmptyCap) + AddNonEmptyCapToEmptyCap (via add_cap)
   -  TEST(S2Cap, GetRectBound)
   -  TEST(S2Cap, Expanded)
   -  TEST(S2Cap, GetCentroid)
   -  TEST(S2Cap, Union)
   -  TEST(S2Cap, EncodeDecode)

   Deliberately omitted:
   -  TEST(S2Cap, S2CellMethods) — requires S2_cell
   -  TEST(S2Cap, GetCellUnionBoundLevel1Radius) — requires S2_metrics / cell cover
   -  TEST(S2Cap, S2CoderWorks) — legacy coder not ported *)

open Core
open Test_helpers
open Alcotest

let latlng_point lat_deg lng_deg =
  S2.S2_latlng.to_point
    (S2.S2_latlng.of_degrees
       ~lat:(Float_u.of_float lat_deg)
       ~lng:(Float_u.of_float lng_deg))
;;

let cap_of_json j =
  let center = r3_vector_of_json (member "center" j) in
  S2.S2_cap.of_center_chord_angle
    center
    (S2.S1_chord_angle.of_length2 (float_u_of_json_exn (member "length2" j)))
;;

let check_lat_lng_rect msg (expected_r : Yojson.Safe.t) (actual : S2.S2_latlng_rect.t) =
  let lat_j = member "lat" expected_r in
  let lng_j = member "lng" expected_r in
  let exp_lat = r1_interval_of_json lat_j in
  let exp_lng = s1_interval_of_json lng_j in
  let actual_lat = S2.S2_latlng_rect.lat actual in
  let actual_lng = S2.S2_latlng_rect.lng actual in
  check_float_u
    (msg ^ " lat lo")
    ~expected:(S2.R1_interval.lo exp_lat)
    ~actual:(S2.R1_interval.lo actual_lat);
  check_float_u
    (msg ^ " lat hi")
    ~expected:(S2.R1_interval.hi exp_lat)
    ~actual:(S2.R1_interval.hi actual_lat);
  check_float_u
    (msg ^ " lng lo")
    ~expected:(S2.S1_interval.lo exp_lng)
    ~actual:(S2.S1_interval.lo actual_lng);
  check_float_u
    (msg ^ " lng hi")
    ~expected:(S2.S1_interval.hi exp_lng)
    ~actual:(S2.S1_interval.hi actual_lng);
  check_bool
    (msg ^ " lat_empty")
    ~expected:(bool_of_json_exn (member "lat_empty" expected_r))
    ~actual:(S2.R1_interval.is_empty actual_lat);
  check_bool
    (msg ^ " lng_full")
    ~expected:(bool_of_json_exn (member "lng_full" expected_r))
    ~actual:(S2.S1_interval.is_full actual_lng)
;;

let test_basic f () =
  let b = member "basic" f in
  (check bool)
    "empty_valid"
    (bool_of_json_exn (member "empty_valid" b))
    (S2.S2_cap.is_valid S2.S2_cap.empty);
  (check bool)
    "empty_is_empty"
    (bool_of_json_exn (member "empty_is_empty" b))
    (S2.S2_cap.is_empty S2.S2_cap.empty);
  (check bool)
    "empty_complement_full"
    (bool_of_json_exn (member "empty_complement_full" b))
    (S2.S2_cap.is_full (S2.S2_cap.complement S2.S2_cap.empty));
  (check bool)
    "full_valid"
    (bool_of_json_exn (member "full_valid" b))
    (S2.S2_cap.is_valid S2.S2_cap.full);
  (check bool)
    "full_is_full"
    (bool_of_json_exn (member "full_is_full" b))
    (S2.S2_cap.is_full S2.S2_cap.full);
  (check bool)
    "full_complement_empty"
    (bool_of_json_exn (member "full_complement_empty" b))
    (S2.S2_cap.is_empty (S2.S2_cap.complement S2.S2_cap.full));
  check_float_u
    "full_height"
    ~expected:(float_u_of_json_exn (member "full_height" b))
    ~actual:(S2.S2_cap.height S2.S2_cap.full);
  check_float_u
    "full_radius_deg"
    ~expected:(float_u_of_json_exn (member "full_radius_deg" b))
    ~actual:(S2.S1_angle.degrees (S2.S2_cap.radius_angle S2.S2_cap.full));
  (check bool)
    "neg_angle_empty"
    (bool_of_json_exn (member "neg_angle_empty" b))
    (S2.S2_cap.is_empty
       (S2.S2_cap.of_center_angle
          (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0)
          (S2.S1_angle.of_radians (-#20.0))));
  (check bool)
    "large_angle_full"
    (bool_of_json_exn (member "large_angle_full" b))
    (S2.S2_cap.is_full
       (S2.S2_cap.of_center_angle
          (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0)
          (S2.S1_angle.of_radians #5.0)));
  (check bool)
    "infinity_angle_full"
    (bool_of_json_exn (member "infinity_angle_full" b))
    (S2.S2_cap.is_full
       (S2.S2_cap.of_center_angle
          (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0)
          S2.S1_angle.infinity));
  let ce = S2.S2_cap.center S2.S2_cap.empty in
  let re = S2.S2_cap.radius_chord S2.S2_cap.empty in
  let default_empty = S2.S2_cap.of_center_chord_angle ce re in
  (* C++ default [S2Cap] matches [Empty()] center and height. *)
  (check bool)
    "default_equals_empty_center"
    (bool_of_json_exn (member "default_equals_empty_center" b))
    (S2.S2_point.equal
       (S2.S2_cap.center S2.S2_cap.empty)
       (S2.S2_cap.center default_empty));
  (check bool)
    "default_equals_empty_height"
    (bool_of_json_exn (member "default_equals_empty_height" b))
    Float_u.(S2.S2_cap.height S2.S2_cap.empty = S2.S2_cap.height default_empty);
  (check bool)
    "empty_contains_empty"
    (bool_of_json_exn (member "empty_contains_empty" b))
    (S2.S2_cap.contains_cap S2.S2_cap.empty S2.S2_cap.empty);
  (check bool)
    "full_contains_empty"
    (bool_of_json_exn (member "full_contains_empty" b))
    (S2.S2_cap.contains_cap S2.S2_cap.full S2.S2_cap.empty);
  (check bool)
    "full_contains_full"
    (bool_of_json_exn (member "full_contains_full" b))
    (S2.S2_cap.contains_cap S2.S2_cap.full S2.S2_cap.full);
  (check bool)
    "empty_interior_intersects_empty"
    (bool_of_json_exn (member "empty_interior_intersects_empty" b))
    (S2.S2_cap.interior_intersects S2.S2_cap.empty S2.S2_cap.empty);
  (check bool)
    "full_interior_intersects_full"
    (bool_of_json_exn (member "full_interior_intersects_full" b))
    (S2.S2_cap.interior_intersects S2.S2_cap.full S2.S2_cap.full);
  (check bool)
    "full_interior_intersects_empty"
    (bool_of_json_exn (member "full_interior_intersects_empty" b))
    (S2.S2_cap.interior_intersects S2.S2_cap.full S2.S2_cap.empty);
  let xaxis = S2.S2_cap.of_point (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0) in
  let yaxis = S2.S2_cap.of_point (S2.S2_point.of_coords ~x:#0.0 ~y:#1.0 ~z:#0.0) in
  (check bool)
    "xaxis_contains_axis"
    (bool_of_json_exn (member "xaxis_contains_axis" b))
    (S2.S2_cap.contains_point xaxis (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0));
  (* Golden value is C++ [Contains(normalized near point)] (false for exterior). *)
  let np = r3_vector_of_json (member "xaxis_near_point" b) in
  let near =
    S2.S2_point.of_coords
      ~x:(S2.R3_vector.x np)
      ~y:(S2.R3_vector.y np)
      ~z:(S2.R3_vector.z np)
  in
  (check bool)
    "xaxis_not_contains_near"
    (bool_of_json_exn (member "xaxis_not_contains_near" b))
    (S2.S2_cap.contains_point xaxis near);
  check_float_u
    "xaxis_radius_rad"
    ~expected:(float_u_of_json_exn (member "xaxis_radius_rad" b))
    ~actual:(S2.S1_angle.radians (S2.S2_cap.radius_angle xaxis));
  (* Golden = C++ [yaxis.Contains(xaxis.center())]. *)
  (check bool)
    "yaxis_not_contains_xcenter"
    (bool_of_json_exn (member "yaxis_not_contains_xcenter" b))
    (S2.S2_cap.contains_point yaxis (S2.S2_cap.center xaxis));
  check_float_u
    "xaxis_height"
    ~expected:(float_u_of_json_exn (member "xaxis_height" b))
    ~actual:(S2.S2_cap.height xaxis);
  let xcomp = S2.S2_cap.complement xaxis in
  (check bool)
    "xcomp_full"
    (bool_of_json_exn (member "xcomp_full" b))
    (S2.S2_cap.is_full xcomp);
  (check bool)
    "xcomp_contains_center"
    (bool_of_json_exn (member "xcomp_contains_center" b))
    (S2.S2_cap.contains_point xcomp (S2.S2_cap.center xaxis));
  let xcc = S2.S2_cap.complement xcomp in
  (check bool)
    "xcompcomp_empty"
    (bool_of_json_exn (member "xcompcomp_empty" b))
    (S2.S2_cap.is_empty xcc);
  (check bool)
    "xcompcomp_not_contains"
    (bool_of_json_exn (member "xcompcomp_not_contains" b))
    (not (S2.S2_cap.contains_point xcc (S2.S2_cap.center xaxis)));
  let k_tiny = #1e-10 in
  let tiny =
    S2.S2_cap.of_center_angle
      (S2.S2_point.of_coords ~x:#1.0 ~y:#2.0 ~z:#3.0)
      (S2.S1_angle.of_radians k_tiny)
  in
  let tc = S2.S2_cap.center tiny in
  let tangent =
    S2.R3_vector.normalize
      (S2.R3_vector.cross tc (S2.R3_vector.create ~x:#3.0 ~y:#2.0 ~z:#1.0))
  in
  let inner =
    S2.R3_vector.normalize
      (S2.R3_vector.add tc (S2.R3_vector.mul tangent Float_u.(#0.99 * k_tiny)))
  in
  let outer =
    S2.R3_vector.normalize
      (S2.R3_vector.add tc (S2.R3_vector.mul tangent Float_u.(#1.01 * k_tiny)))
  in
  (check bool)
    "tiny_contains_inner"
    (bool_of_json_exn (member "tiny_contains_inner" b))
    (S2.S2_cap.contains_point tiny inner);
  (check bool)
    "tiny_not_contains_outer"
    (bool_of_json_exn (member "tiny_not_contains_outer" b))
    (not (S2.S2_cap.contains_point tiny outer));
  let hemi =
    S2.S2_cap.of_center_height (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#1.0) #1.0
  in
  let hemi_c = S2.S2_cap.center hemi in
  (check bool)
    "hemi_complement_center_neg"
    (bool_of_json_exn (member "hemi_complement_center_neg" b))
    (S2.S2_point.equal
       (S2.S2_cap.center (S2.S2_cap.complement hemi))
       (S2.R3_vector.neg hemi_c));
  check_float_u
    "hemi_complement_height"
    ~expected:(float_u_of_json_exn (member "hemi_complement_height" b))
    ~actual:(S2.S2_cap.height (S2.S2_cap.complement hemi));
  (check bool)
    "hemi_contains_x"
    (bool_of_json_exn (member "hemi_contains_x" b))
    (S2.S2_cap.contains_point hemi (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0));
  (check bool)
    "hemi_comp_not_x"
    (bool_of_json_exn (member "hemi_comp_not_x" b))
    (not
       (S2.S2_cap.contains_point
          (S2.S2_cap.complement hemi)
          (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0)));
  let k_eps = #1e-15 in
  (check bool)
    "hemi_contains_near"
    (bool_of_json_exn (member "hemi_contains_near" b))
    (S2.S2_cap.contains_point
       hemi
       (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:(Float_u.neg Float_u.(#1.0 - k_eps))));
  (check bool)
    "hemi_not_interior"
    (bool_of_json_exn (member "hemi_not_interior" b))
    (not
       (S2.S2_cap.interior_contains_point
          hemi
          (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:(Float_u.neg Float_u.(#1.0 + k_eps)))));
  let concave_center = latlng_point 80.0 10.0 in
  let concave =
    S2.S2_cap.of_center_chord_angle
      concave_center
      (S2.S1_chord_angle.of_angle (S2.S1_angle.of_degrees #150.0))
  in
  let max_err =
    Float_u.(
      S2.S1_chord_angle.max_point_error (S2.S2_cap.radius_chord concave)
      + S2.S1_chord_angle.max_angle_error (S2.S2_cap.radius_chord concave)
      + (#3.0 * Float_u.epsilon_float))
  in
  let concave_min =
    S2.S2_cap.of_center_chord_angle
      concave_center
      (S2.S1_chord_angle.plus_error
         (S2.S2_cap.radius_chord concave)
         (Float_u.neg max_err))
  in
  let concave_max =
    S2.S2_cap.of_center_chord_angle
      concave_center
      (S2.S1_chord_angle.plus_error (S2.S2_cap.radius_chord concave) max_err)
  in
  (check bool)
    "concave_max_contains_a"
    (bool_of_json_exn (member "concave_max_contains_a" b))
    (S2.S2_cap.contains_point concave_max (latlng_point (-70.0) 10.0));
  (check bool)
    "concave_min_not_a"
    (bool_of_json_exn (member "concave_min_not_a" b))
    (not (S2.S2_cap.contains_point concave_min (latlng_point (-70.0) 10.0)));
  (check bool)
    "concave_max_contains_b"
    (bool_of_json_exn (member "concave_max_contains_b" b))
    (S2.S2_cap.contains_point concave_max (latlng_point (-50.0) (-170.0)));
  (check bool)
    "concave_min_not_b"
    (bool_of_json_exn (member "concave_min_not_b" b))
    (not (S2.S2_cap.contains_point concave_min (latlng_point (-50.0) (-170.0))));
  (check bool)
    "empty_not_contains_xaxis"
    (bool_of_json_exn (member "empty_not_contains_xaxis" b))
    (not (S2.S2_cap.contains_cap S2.S2_cap.empty xaxis));
  (check bool)
    "empty_not_interior_xaxis"
    (bool_of_json_exn (member "empty_not_interior_xaxis" b))
    (not (S2.S2_cap.interior_intersects S2.S2_cap.empty xaxis));
  (check bool)
    "full_contains_xaxis"
    (bool_of_json_exn (member "full_contains_xaxis" b))
    (S2.S2_cap.contains_cap S2.S2_cap.full xaxis);
  (check bool)
    "full_interior_xaxis"
    (bool_of_json_exn (member "full_interior_xaxis" b))
    (S2.S2_cap.interior_intersects S2.S2_cap.full xaxis);
  (check bool)
    "xaxis_not_contains_full"
    (bool_of_json_exn (member "xaxis_not_contains_full" b))
    (not (S2.S2_cap.contains_cap xaxis S2.S2_cap.full));
  (check bool)
    "xaxis_not_interior_full"
    (bool_of_json_exn (member "xaxis_not_interior_full" b))
    (not (S2.S2_cap.interior_intersects xaxis S2.S2_cap.full));
  (check bool)
    "xaxis_contains_self"
    (bool_of_json_exn (member "xaxis_contains_self" b))
    (S2.S2_cap.contains_cap xaxis xaxis);
  (check bool)
    "xaxis_not_interior_self"
    (bool_of_json_exn (member "xaxis_not_interior_self" b))
    (not (S2.S2_cap.interior_intersects xaxis xaxis));
  (check bool)
    "xaxis_contains_empty"
    (bool_of_json_exn (member "xaxis_contains_empty" b))
    (S2.S2_cap.contains_cap xaxis S2.S2_cap.empty);
  (check bool)
    "xaxis_not_interior_empty"
    (bool_of_json_exn (member "xaxis_not_interior_empty" b))
    (not (S2.S2_cap.interior_intersects xaxis S2.S2_cap.empty));
  (check bool)
    "hemi_contains_tiny"
    (bool_of_json_exn (member "hemi_contains_tiny" b))
    (S2.S2_cap.contains_cap hemi tiny);
  let pi4 = Float_u.(Float_u.pi / #4.0) in
  (check bool)
    "hemi_contains_small_cap"
    (bool_of_json_exn (member "hemi_contains_small_cap" b))
    (S2.S2_cap.contains_cap
       hemi
       (S2.S2_cap.of_center_angle
          (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0)
          (S2.S1_angle.of_radians Float_u.(pi4 - k_eps))));
  (check bool)
    "hemi_not_contains_larger"
    (bool_of_json_exn (member "hemi_not_contains_larger" b))
    (not
       (S2.S2_cap.contains_cap
          hemi
          (S2.S2_cap.of_center_angle
             (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0)
             (S2.S1_angle.of_radians Float_u.(pi4 + k_eps)))));
  (check bool)
    "concave_contains_hemi"
    (bool_of_json_exn (member "concave_contains_hemi" b))
    (S2.S2_cap.contains_cap concave hemi);
  (check bool)
    "concave_interior_intersects_hemi_comp"
    (bool_of_json_exn (member "concave_interior_intersects_hemi_comp" b))
    (S2.S2_cap.interior_intersects concave (S2.S2_cap.complement hemi));
  (check bool)
    "concave_not_contains_antipode_cap"
    (bool_of_json_exn (member "concave_not_contains_antipode_cap" b))
    (not
       (S2.S2_cap.contains_cap
          concave
          (S2.S2_cap.of_center_height (S2.R3_vector.neg (S2.S2_cap.center concave)) #0.1)))
;;

let test_add_cap f () =
  let a = member "add_cap" f in
  let non_empty =
    S2.S2_cap.of_center_angle
      (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0)
      (S2.S1_angle.of_degrees #10.0)
  in
  let t1 = S2.S2_cap.add_cap non_empty S2.S2_cap.empty in
  check_float_u
    "add_empty_area"
    ~expected:(float_u_of_json_exn (member "add_empty_area" a))
    ~actual:(S2.S2_cap.area t1);
  let t2 = S2.S2_cap.add_cap S2.S2_cap.empty non_empty in
  check_float_u
    "empty_after_add_area"
    ~expected:(float_u_of_json_exn (member "empty_after_add_area" a))
    ~actual:(S2.S2_cap.area t2)
;;

let test_rect_bound f () =
  let cases = to_list (member "rect_bound" f) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let rect_j = member "rect" c in
    let cap =
      match name with
      | "empty" -> S2.S2_cap.empty
      | "full" -> S2.S2_cap.full
      | "south_pole_cap" ->
        S2.S2_cap.of_center_angle
          (latlng_point (-45.0) 57.0)
          (S2.S1_angle.of_degrees #50.0)
      | "north_tangent" ->
        S2.S2_cap.of_center_angle
          (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#1.0)
          (S2.S1_angle.of_radians Float_u.((Float_u.pi / #4.0) + #1e-16))
      | "north_45_eps" ->
        S2.S2_cap.of_center_angle
          (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#1.0)
          (S2.S1_angle.of_degrees Float_u.(#45.0 + #5e-15))
      | "eastern_hemi" ->
        S2.S2_cap.of_center_angle
          (S2.S2_point.of_coords ~x:#0.0 ~y:#1.0 ~z:#0.0)
          (S2.S1_angle.of_radians Float_u.((Float_u.pi / #2.0) + #2e-16))
      | "equator_50" ->
        S2.S2_cap.of_center_angle (latlng_point 0.0 50.0) (S2.S1_angle.of_degrees #20.0)
      | "north_pole_cap" ->
        S2.S2_cap.of_center_angle (latlng_point 90.0 123.0) (S2.S1_angle.of_degrees #10.0)
      | _ ->
        (match Alcotest.fail ("unknown rect_bound case " ^ name) with
         | (_ : Nothing.t) -> .)
    in
    check_lat_lng_rect name rect_j (S2.S2_latlng_rect.from_cap cap))
;;

let test_expanded f () =
  let e = member "expanded" f in
  (check bool)
    "empty_stays_empty"
    (bool_of_json_exn (member "empty_stays_empty" e))
    (S2.S2_cap.is_empty
       (S2.S2_cap.Option.value_exn
          (S2.S2_cap.expanded S2.S2_cap.empty (S2.S1_angle.of_radians #2.0))));
  (check bool)
    "full_stays_full"
    (bool_of_json_exn (member "full_stays_full" e))
    (S2.S2_cap.is_full
       (S2.S2_cap.Option.value_exn
          (S2.S2_cap.expanded S2.S2_cap.full (S2.S1_angle.of_radians #2.0))));
  let cap50 =
    S2.S2_cap.of_center_angle
      (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0)
      (S2.S1_angle.of_degrees #50.0)
  in
  let cap51 =
    S2.S2_cap.of_center_angle
      (S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0)
      (S2.S1_angle.of_degrees #51.0)
  in
  let exp0 =
    S2.S2_cap.Option.value_exn (S2.S2_cap.expanded cap50 (S2.S1_angle.of_radians #0.0))
  in
  (check bool)
    "expanded0_approx_cap50"
    true
    (S2.S2_cap.approx_equal ~max_error:(Packed_float_option.Unboxed.none) exp0 cap50);
  check_float_u
    "cap50_exp0_len2"
    ~expected:(float_u_of_json_exn (member "cap50_exp0_len2" e))
    ~actual:(S2.S1_chord_angle.length2 (S2.S2_cap.radius_chord exp0));
  let exp1 =
    S2.S2_cap.Option.value_exn (S2.S2_cap.expanded cap50 (S2.S1_angle.of_degrees #1.0))
  in
  check_float_u
    "cap50_exp1_len2"
    ~expected:(float_u_of_json_exn (member "cap50_exp1_len2" e))
    ~actual:(S2.S1_chord_angle.length2 (S2.S2_cap.radius_chord exp1));
  check_float_u
    "cap51_len2"
    ~expected:(float_u_of_json_exn (member "cap51_len2" e))
    ~actual:(S2.S1_chord_angle.length2 (S2.S2_cap.radius_chord cap51));
  (check bool)
    "exp12999_not_full"
    (bool_of_json_exn (member "exp12999_not_full" e))
    (not
       (S2.S2_cap.is_full
          (S2.S2_cap.Option.value_exn
             (S2.S2_cap.expanded cap50 (S2.S1_angle.of_degrees #129.99)))));
  (check bool)
    "exp13001_full"
    (bool_of_json_exn (member "exp13001_full" e))
    (S2.S2_cap.is_full
       (S2.S2_cap.Option.value_exn
          (S2.S2_cap.expanded cap50 (S2.S1_angle.of_degrees #130.01))))
;;

let test_centroid f () =
  let c = member "centroid" f in
  check_r3_vector_exact
    "empty_centroid"
    ~expected:(r3_vector_of_json (member "empty_centroid" c))
    ~actual:(S2.S2_cap.centroid S2.S2_cap.empty);
  check_float_u
    "full_centroid_norm"
    ~expected:(float_u_of_json_exn (member "full_centroid_norm" c))
    ~actual:(S2.R3_vector.norm (S2.S2_cap.centroid S2.S2_cap.full));
  let samples = to_list (member "samples" c) in
  List.iteri samples ~f:(fun i s ->
    let center = r3_vector_of_json (member "center" s) in
    let height = float_u_of_json_exn (member "height" s) in
    let cap = S2.S2_cap.of_center_height center height in
    let centroid = S2.S2_cap.centroid cap in
    let expected = r3_vector_of_json (member "expected" s) in
    check_r3_vector (sprintf "centroid sample %d" i) ~expected ~actual:centroid)
;;

let test_union f () =
  let a0 =
    S2.S2_cap.of_center_angle (latlng_point 50.0 10.0) (S2.S1_angle.of_degrees #0.2)
  in
  let b0 =
    S2.S2_cap.of_center_angle (latlng_point 50.0 10.0) (S2.S1_angle.of_degrees #0.3)
  in
  let c0 =
    S2.S2_cap.of_center_angle (latlng_point 51.0 11.0) (S2.S1_angle.of_degrees #1.5)
  in
  let d0 =
    S2.S2_cap.of_center_angle (latlng_point 51.0 11.0) (S2.S1_angle.of_degrees #0.1)
  in
  let e0 =
    S2.S2_cap.of_center_angle (latlng_point 50.3 10.3) (S2.S1_angle.of_degrees #0.2)
  in
  let f0 =
    S2.S2_cap.of_center_angle
      (S2.S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0)
      (S2.S1_angle.of_degrees #150.0)
  in
  let g0 =
    S2.S2_cap.of_center_angle
      (S2.S2_point.of_coords ~x:#0.0 ~y:#1.0 ~z:#0.0)
      (S2.S1_angle.of_degrees #150.0)
  in
  let hemi =
    S2.S2_cap.of_center_height (S2.S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0) #1.0
  in
  let cases = to_list (member "union" f) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    match name with
    | "same_center_larger" ->
      let u = S2.S2_cap.union a0 b0 in
      check_bool
        "b_contains_a"
        ~expected:(bool_of_json_exn (member "b_contains_a" c))
        ~actual:(S2.S2_cap.contains_cap b0 a0);
      check_bool
        "eq"
        ~expected:(bool_of_json_exn (member "eq" c))
        ~actual:(S2.S2_cap.equal u b0)
    | "with_full" ->
      check_bool
        "is_full"
        ~expected:(bool_of_json_exn (member "is_full" c))
        ~actual:(S2.S2_cap.is_full (S2.S2_cap.union a0 S2.S2_cap.full))
    | "with_empty" ->
      check_bool
        "area_eq"
        ~expected:(bool_of_json_exn (member "area_eq" c))
        ~actual:
          (Float_u.equal
             (S2.S2_cap.area (S2.S2_cap.union a0 S2.S2_cap.empty))
             (S2.S2_cap.area a0))
    | "encompasses" ->
      let u = S2.S2_cap.union a0 c0 in
      check_bool
        "contains"
        ~expected:(bool_of_json_exn (member "contains" c))
        ~actual:(S2.S2_cap.contains_cap c0 a0);
      check_bool
        "center_eq"
        ~expected:(bool_of_json_exn (member "center_eq" c))
        ~actual:(S2.S2_point.equal (S2.S2_cap.center u) (S2.S2_cap.center c0));
      check_bool
        "radius_eq"
        ~expected:(bool_of_json_exn (member "radius_eq" c))
        ~actual:(S2.S1_angle.equal (S2.S2_cap.radius_angle u) (S2.S2_cap.radius_angle c0))
    | "disjoint" ->
      let u = S2.S2_cap.union a0 d0 in
      let ll = S2.S2_latlng.of_point (S2.S2_cap.center u) in
      check_bool
        "not_contains"
        ~expected:(bool_of_json_exn (member "not_contains" c))
        ~actual:(not (S2.S2_cap.contains_cap d0 a0));
      check_bool
        "not_intersects"
        ~expected:(bool_of_json_exn (member "not_intersects" c))
        ~actual:(not (S2.S2_cap.intersects d0 a0));
      check_float_u
        "lat_deg"
        ~expected:(float_u_of_json_exn (member "lat_deg" c))
        ~actual:(S2.S1_angle.degrees (S2.S2_latlng.lat ll));
      check_float_u
        "lng_deg"
        ~expected:(float_u_of_json_exn (member "lng_deg" c))
        ~actual:(S2.S1_angle.degrees (S2.S2_latlng.lng ll));
      check_float_u
        "radius_deg"
        ~expected:(float_u_of_json_exn (member "radius_deg" c))
        ~actual:(S2.S1_angle.degrees (S2.S2_cap.radius_angle u))
    | "overlap" ->
      let u = S2.S2_cap.union a0 e0 in
      let ll = S2.S2_latlng.of_point (S2.S2_cap.center u) in
      check_bool
        "intersects"
        ~expected:(bool_of_json_exn (member "intersects" c))
        ~actual:(S2.S2_cap.intersects e0 a0);
      check_float_u
        "lat_deg"
        ~expected:(float_u_of_json_exn (member "lat_deg" c))
        ~actual:(S2.S1_angle.degrees (S2.S2_latlng.lat ll));
      check_float_u
        "lng_deg"
        ~expected:(float_u_of_json_exn (member "lng_deg" c))
        ~actual:(S2.S1_angle.degrees (S2.S2_latlng.lng ll));
      check_float_u
        "radius_deg"
        ~expected:(float_u_of_json_exn (member "radius_deg" c))
        ~actual:(S2.S1_angle.degrees (S2.S2_cap.radius_angle u))
    | "two_large" ->
      check_bool
        "is_full"
        ~expected:(bool_of_json_exn (member "is_full" c))
        ~actual:(S2.S2_cap.is_full (S2.S2_cap.union f0 g0))
    | "hemi_union_complement" ->
      check_bool
        "is_full"
        ~expected:(bool_of_json_exn (member "is_full" c))
        ~actual:(S2.S2_cap.is_full (S2.S2_cap.union hemi (S2.S2_cap.complement hemi)))
    | _ -> Alcotest.fail ("unknown union case " ^ name))
;;

let test_encode_decode f () =
  let j = member "encode_decode" f in
  let cap = cap_of_json (member "roundtrip" j) in
  let encoded = S2.S2_cap.encode cap in
  let dec = S2.S2_cap.Option.value_exn (S2.S2_cap.decode encoded) in
  (check bool) "decode roundtrip" true (S2.S2_cap.equal dec cap);
  let floats = to_list (member "encoded" j) in
  List.iteri floats ~f:(fun i elt ->
    let expected = float_of_json_exn elt in
    let off = i * 8 in
    let actual = get_le_f64_from_string encoded off in
    check_float (sprintf "encode[%d]" i) ~expected ~actual)
;;

let () =
  let fixture = load_fixture "s2cap.json" in
  Alcotest.run
    "S2_cap"
    [ "basic", [ test_case "Basic" `Quick (test_basic fixture) ]
    ; "add_cap", [ test_case "AddCap" `Quick (test_add_cap fixture) ]
    ; "rect_bound", [ test_case "RectBound" `Quick (test_rect_bound fixture) ]
    ; "expanded", [ test_case "Expanded" `Quick (test_expanded fixture) ]
    ; "centroid", [ test_case "Centroid" `Quick (test_centroid fixture) ]
    ; "union", [ test_case "Union" `Quick (test_union fixture) ]
    ; "encode_decode", [ test_case "EncodeDecode" `Quick (test_encode_decode fixture) ]
    ]
;;
