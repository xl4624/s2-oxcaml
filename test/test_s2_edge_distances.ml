(* C++ test parity: s2geometry/src/s2/s2edge_distances_test.cc Golden data from
   test/gen/s2edge_distances.cc.

   Covered:
   - TEST(S2, GetUpdateMinDistanceMaxError)
   - TEST(S2, Distance)
   - TEST(S2, MaxDistance)
   - TEST(S2, Interpolate)
   - TEST(S2, InterpolateCanExtrapolate)
   - TEST(S2, UpdateMinInteriorDistanceLowerBoundOptimizationIsConservative)
   - TEST(S2, DistanceFraction) (via project fixture)
   - TEST(S2, GetPointToLeftS1Angle)
   - TEST(S2, GetPointToLeftS1ChordAngle)
   - TEST(S2, GetPointToRightS1Angle)
   - TEST(S2, GetPointToRightS1ChordAngle)
   - GetPointOnLine (S1Angle and S1ChordAngle overloads)

   Deliberately omitted:
   - TEST(S2, GetUpdateMinInteriorDistanceMaxError) - randomized stress test
   - TEST(S2, UpdateMinInteriorDistanceRejectionTestIsConservative) - specific regression
     cases (partially covered by interior_distance_conservative)
   - TEST(S2, ProjectError) - randomized stress test
   - TEST(S2, RepeatedInterpolation) - randomized stress test
   - TEST(S2, EdgePairMinDistance) - requires s2_edge_crossings
   - TEST(S2, EdgePairMaxDistance) - requires s2_edge_crossings
   - TEST(IsEdgePairDistanceLess, Coverage) - requires s2_edge_crossings

   Covered via fixture-only (no upstream TEST() mirror beyond EdgeBNearEdgeA):
   - TEST(S2, EdgeBNearEdgeA) *)

open Core
open Test_helpers

let fixture = lazy (load_fixture "s2edge_distances.json")

let point_of_json j =
  match to_list j with
  | [ x; y; z ] ->
    S2.R3_vector.create
      ~x:(float_u_of_json_exn x)
      ~y:(float_u_of_json_exn y)
      ~z:(float_u_of_json_exn z)
  | _ ->
    (match failwith "expected [x, y, z]" with
     | (_ : Nothing.t) -> .)
;;

(* ---------- GetUpdateMinDistanceMaxError ---------- *)

let test_update_min_distance_max_error () =
  let cases = to_list (member "update_min_distance_max_error" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    let input_radians = float_u_of_json_exn (member "input_radians" c) in
    let max_error = float_u_of_json_exn (member "max_error" c) in
    let bound_radians = float_u_of_json_exn (member "bound_radians" c) in
    let ca = S2.S1_chord_angle.of_radians input_radians in
    let err = S2.S2_edge_distances.get_update_min_distance_max_error ca in
    let bound = S2.S1_chord_angle.to_angle (S2.S1_chord_angle.plus_error ca err) in
    let actual_bound = S2.S1_angle.radians bound in
    check_float_u (sprintf "bound[%d]" i) ~expected:bound_radians ~actual:actual_bound;
    let diff = Float_u.O.(actual_bound - input_radians) in
    if Float_u.O.(diff > max_error)
    then
      Alcotest.fail
        (sprintf
           "case %d: bound-input=%s > max_error=%s"
           i
           (Float_u.to_string diff)
           (Float_u.to_string max_error)))
;;

(* ---------- Distance ---------- *)

let test_distance () =
  let cases = to_list (member "distance" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    let x = point_of_json (member "x" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let expected_dist = float_u_of_json_exn (member "distance_radians" c) in
    let actual_dist = S2.S1_angle.radians (S2.S2_edge_distances.get_distance x a b) in
    check_float_u (sprintf "dist[%d]" i) ~expected:expected_dist ~actual:actual_dist;
    (* Also test update_min_distance with zero: should not update *)
    let result_zero =
      S2.S2_edge_distances.update_min_distance x a b S2.S1_chord_angle.zero
    in
    check_bool
      (sprintf "not_updated_zero[%d]" i)
      ~expected:false
      ~actual:(S2.S1_chord_angle.Option.is_some result_zero);
    (* update_min_distance with infinity: should update *)
    let result_inf =
      S2.S2_edge_distances.update_min_distance x a b S2.S1_chord_angle.infinity
    in
    check_bool
      (sprintf "updated_inf[%d]" i)
      ~expected:true
      ~actual:(S2.S1_chord_angle.Option.is_some result_inf);
    match%optional_u.S2.S1_chord_angle.Option result_inf with
    | None -> Alcotest.fail (sprintf "dist_inf[%d]: expected Some" i)
    | Some d ->
      let radians = S2.S1_chord_angle.radians d in
      check_float_u (sprintf "dist_inf[%d]" i) ~expected:expected_dist ~actual:radians)
;;

(* ---------- Project ---------- *)

let test_project () =
  let cases = to_list (member "distance" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    let x = point_of_json (member "x" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let expected_closest = point_of_json (member "closest" c) in
    let is_endpoint = bool_of_json_exn (member "expected_closest_is_endpoint" c) in
    let closest = S2.S2_edge_distances.project x a b in
    let dist_radians = float_u_of_json_exn (member "distance_radians" c) in
    (* When distance is pi/2, the closest point is ambiguous on which side *)
    if not Float_u.O.(dist_radians = Float_u.pi / #2.0)
    then
      if is_endpoint
      then
        check_bool
          (sprintf "proj_endpoint[%d]" i)
          ~expected:true
          ~actual:(S2.S2_point.equal closest a || S2.S2_point.equal closest b)
      else
        check_bool
          (sprintf "proj_approx[%d]" i)
          ~expected:true
          ~actual:
            (S2.S2_point.approx_equal
               ~max_error:Packed_float_option.Unboxed.none
               closest
               expected_closest))
;;

(* ---------- MaxDistance ---------- *)

let test_max_distance () =
  let cases = to_list (member "max_distance" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    let x = point_of_json (member "x" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let expected_dist = float_u_of_json_exn (member "distance_radians" c) in
    (* UpdateMaxDistance with Straight should not update *)
    let result_straight =
      S2.S2_edge_distances.update_max_distance x a b S2.S1_chord_angle.straight
    in
    check_bool
      (sprintf "not_updated_straight[%d]" i)
      ~expected:false
      ~actual:(S2.S1_chord_angle.Option.is_some result_straight);
    (* UpdateMaxDistance with Negative should update *)
    let result_neg =
      S2.S2_edge_distances.update_max_distance x a b S2.S1_chord_angle.negative
    in
    check_bool
      (sprintf "updated_neg[%d]" i)
      ~expected:true
      ~actual:(S2.S1_chord_angle.Option.is_some result_neg);
    match%optional_u.S2.S1_chord_angle.Option result_neg with
    | None -> Alcotest.fail (sprintf "max_dist[%d]: expected Some" i)
    | Some d ->
      let radians = S2.S1_chord_angle.radians d in
      check_float_u (sprintf "max_dist[%d]" i) ~expected:expected_dist ~actual:radians)
;;

(* ---------- Interpolate ---------- *)

let test_interpolate () =
  let cases = to_list (member "interpolate" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let t = float_u_of_json_exn (member "t" c) in
    let expected = point_of_json (member "actual" c) in
    let actual = S2.S2_edge_distances.interpolate a b t in
    let angle = S2.S1_angle.radians (S2.S2_point.distance actual expected) in
    if Float_u.O.(angle > #3e-15)
    then
      Alcotest.fail
        (sprintf "interpolate[%d]: angle=%s > 3e-15" i (Float_u.to_string angle)))
;;

let test_interpolate_extrapolate () =
  let cases = to_list (member "interpolate_extrapolate" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let t = float_u_of_json_exn (member "t" c) in
    let expected = point_of_json (member "actual" c) in
    let actual = S2.S2_edge_distances.interpolate a b t in
    let angle = S2.S1_angle.radians (S2.S2_point.distance actual expected) in
    if Float_u.O.(angle > #3e-15)
    then
      Alcotest.fail
        (sprintf "extrapolate[%d]: angle=%s > 3e-15" i (Float_u.to_string angle)))
;;

(* ---------- InteriorDistance conservative ---------- *)

let test_interior_distance_conservative () =
  let c = member "interior_distance_conservative" (Lazy.force fixture) in
  let x = point_of_json (member "x" c) in
  let a = point_of_json (member "a" c) in
  let b = point_of_json (member "b" c) in
  let expected_length2 = float_u_of_json_exn (member "min_distance_length2" c) in
  let result =
    S2.S2_edge_distances.update_min_distance x a b S2.S1_chord_angle.infinity
  in
  check_bool "updated" ~expected:true ~actual:(S2.S1_chord_angle.Option.is_some result);
  (match%optional_u.S2.S1_chord_angle.Option result with
   | None -> Alcotest.fail "expected Some"
   | Some d ->
     check_float_u
       "length2"
       ~expected:expected_length2
       ~actual:(S2.S1_chord_angle.length2 d));
  (* Also test that successor still updates (conservative lower bound test) *)
  match%optional_u.S2.S1_chord_angle.Option result with
  | None -> ()
  | Some d ->
    let succ = S2.S1_chord_angle.successor d in
    let result2 = S2.S2_edge_distances.update_min_distance x a b succ in
    check_bool
      "successor_updates"
      ~expected:true
      ~actual:(S2.S1_chord_angle.Option.is_some result2)
;;

(* ---------- DistanceFraction ---------- *)

let test_distance_fraction () =
  let cases = to_list (member "distance_fraction" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let x = point_of_json (member "x" c) in
    let expected = float_u_of_json_exn (member "expected_fraction" c) in
    let actual = S2.S2_edge_distances.get_distance_fraction x a b in
    check_float_u ~eps:1e-14 (sprintf "frac[%d]" i) ~expected ~actual)
;;

(* ---------- GetPointOnLine ---------- *)

let test_get_point_on_line () =
  let cases = to_list (member "get_point_on_line" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let r = float_u_of_json_exn (member "distance_radians" c) in
    let expected = point_of_json (member "result" c) in
    let actual = S2.S2_edge_distances.get_point_on_line a b (S2.S1_angle.of_radians r) in
    let angle =
      Float_u.to_float (S2.S1_angle.radians (S2.S2_point.distance actual expected))
    in
    if Float.( > ) angle 3e-15
    then Alcotest.fail (sprintf "point_on_line[%d]: angle=%g > 3e-15" i angle);
    (* Chord-angle overload: the C++ test only checks distances < 0.99*pi because the
       chord representation collapses near 180 degrees. *)
    if Float_u.O.(r >= #0.0) && Float.( < ) (Float_u.to_float r) (0.99 *. Float.pi)
    then (
      let r_ca = S2.S1_chord_angle.of_angle (S2.S1_angle.of_radians r) in
      let actual_chord = S2.S2_edge_distances.get_point_on_line_chord a b r_ca in
      let angle_chord =
        Float_u.to_float
          (S2.S1_angle.radians (S2.S2_point.distance actual_chord expected))
      in
      if Float.( > ) angle_chord 3e-15
      then
        Alcotest.fail (sprintf "point_on_line_chord[%d]: angle=%g > 3e-15" i angle_chord)))
;;

(* ---------- GetPointToLeft / GetPointToRight ---------- *)

let test_point_to_left_right () =
  let c = member "point_to_left_right" (Lazy.force fixture) in
  let a = point_of_json (member "a" c) in
  let b = point_of_json (member "b" c) in
  let r = float_u_of_json_exn (member "distance_radians" c) in
  let expected_left = point_of_json (member "left" c) in
  let expected_right = point_of_json (member "right" c) in
  let expected_left_dist = float_u_of_json_exn (member "left_dist" c) in
  let expected_right_dist = float_u_of_json_exn (member "right_dist" c) in
  let angle = S2.S1_angle.of_radians r in
  let actual_left = S2.S2_edge_distances.get_point_to_left a b angle in
  let actual_right = S2.S2_edge_distances.get_point_to_right a b angle in
  let left_dist = S2.S1_angle.radians (S2.S2_point.distance a actual_left) in
  let right_dist = S2.S1_angle.radians (S2.S2_point.distance a actual_right) in
  check_float_u "left_dist" ~expected:expected_left_dist ~actual:left_dist;
  check_float_u "right_dist" ~expected:expected_right_dist ~actual:right_dist;
  let me = Packed_float_option.Unboxed.some #1e-14 in
  check_bool
    "left_approx"
    ~expected:true
    ~actual:(S2.S2_point.approx_equal ~max_error:me actual_left expected_left);
  check_bool
    "right_approx"
    ~expected:true
    ~actual:(S2.S2_point.approx_equal ~max_error:me actual_right expected_right);
  let r_ca = S2.S1_chord_angle.of_angle angle in
  let actual_left_chord = S2.S2_edge_distances.get_point_to_left_chord a b r_ca in
  let actual_right_chord = S2.S2_edge_distances.get_point_to_right_chord a b r_ca in
  check_bool
    "left_chord_approx"
    ~expected:true
    ~actual:(S2.S2_point.approx_equal ~max_error:me actual_left_chord expected_left);
  check_bool
    "right_chord_approx"
    ~expected:true
    ~actual:(S2.S2_point.approx_equal ~max_error:me actual_right_chord expected_right)
;;

(* ---------- IsEdgeBNearEdgeA ---------- *)

let test_is_edge_b_near_edge_a () =
  let cases = to_list (member "is_edge_b_near_edge_a" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    let a0 = point_of_json (member "a0" c) in
    let a1 = point_of_json (member "a1" c) in
    let b0 = point_of_json (member "b0" c) in
    let b1 = point_of_json (member "b1" c) in
    let tol_deg = float_u_of_json_exn (member "tolerance_degrees" c) in
    let expected = bool_of_json_exn (member "expected" c) in
    let actual =
      S2.S2_edge_distances.is_edge_b_near_edge_a
        a0
        a1
        b0
        b1
        (S2.S1_angle.of_degrees tol_deg)
    in
    check_bool (sprintf "edge_b_near[%d]" i) ~expected ~actual)
;;

(* ---------- Project fixture ---------- *)

let test_project_fixture () =
  let cases = to_list (member "project" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    let x = point_of_json (member "x" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let expected = point_of_json (member "projected" c) in
    let actual = S2.S2_edge_distances.project x a b in
    check_bool
      (sprintf "project[%d]" i)
      ~expected:true
      ~actual:
        (S2.S2_point.approx_equal
           ~max_error:Packed_float_option.Unboxed.none
           actual
           expected))
;;

let test_project_with_cross_fixture () =
  let cases = to_list (member "project_with_cross" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    let x = point_of_json (member "x" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let a_cross_b = point_of_json (member "a_cross_b" c) in
    let expected = point_of_json (member "projected" c) in
    let actual = S2.S2_edge_distances.project_with_cross x a b a_cross_b in
    check_bool
      (sprintf "project_with_cross[%d]" i)
      ~expected:true
      ~actual:
        (S2.S2_point.approx_equal
           ~max_error:Packed_float_option.Unboxed.none
           actual
           expected))
;;

(* ---------- Alcotest suite ---------- *)

let () =
  Alcotest.run
    "S2_edge_distances"
    [ ( "distance"
      , [ Alcotest.test_case "get_distance" `Quick test_distance
        ; Alcotest.test_case "project" `Quick test_project
        ; Alcotest.test_case "project_fixture" `Quick test_project_fixture
        ; Alcotest.test_case
            "project_with_cross_fixture"
            `Quick
            test_project_with_cross_fixture
        ] )
    ; ( "max_distance"
      , [ Alcotest.test_case "update_max_distance" `Quick test_max_distance ] )
    ; ( "interior_distance"
      , [ Alcotest.test_case "conservative" `Quick test_interior_distance_conservative ] )
    ; ( "interpolation"
      , [ Alcotest.test_case "interpolate" `Quick test_interpolate
        ; Alcotest.test_case "extrapolate" `Quick test_interpolate_extrapolate
        ] )
    ; "fraction", [ Alcotest.test_case "distance_fraction" `Quick test_distance_fraction ]
    ; ( "point_on_line"
      , [ Alcotest.test_case "get_point_on_line" `Quick test_get_point_on_line ] )
    ; ( "point_to_left_right"
      , [ Alcotest.test_case "point_to_left_right" `Quick test_point_to_left_right ] )
    ; ( "error_bounds"
      , [ Alcotest.test_case "max_error" `Quick test_update_min_distance_max_error ] )
    ; ( "edge_b_near_edge_a"
      , [ Alcotest.test_case "is_edge_b_near_edge_a" `Quick test_is_edge_b_near_edge_a ] )
    ]
;;
