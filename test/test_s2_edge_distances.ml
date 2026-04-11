(* C++ test parity: s2geometry/src/s2/s2edge_distances_test.cc
   Golden data from test/gen/s2edge_distances.cc.

   Covered:
   -  TEST(S2, GetUpdateMinDistanceMaxError)
   -  TEST(S2, Distance)
   -  TEST(S2, MaxDistance)
   -  TEST(S2, Interpolate)
   -  TEST(S2, InterpolateCanExtrapolate)
   -  TEST(S2, UpdateMinInteriorDistanceLowerBoundOptimizationIsConservative)
   -  TEST(S2, DistanceFraction) (via project fixture)
   -  TEST(S2, GetPointToLeftS1Angle)
   -  TEST(S2, GetPointToRightS1Angle)
   -  GetPointOnLine

   Deliberately omitted:
   -  TEST(S2, GetUpdateMinInteriorDistanceMaxError) - randomized stress test
   -  TEST(S2, UpdateMinInteriorDistanceRejectionTestIsConservative) - specific
      regression cases (partially covered by interior_distance_conservative)
   -  TEST(S2, ProjectError) - randomized stress test
   -  TEST(S2, RepeatedInterpolation) - randomized stress test
   -  TEST(S2, EdgePairMinDistance) - requires s2_edge_crossings
   -  TEST(S2, EdgePairMaxDistance) - requires s2_edge_crossings
   -  TEST(IsEdgePairDistanceLess, Coverage) - requires s2_edge_crossings
   -  TEST(S2, EdgeBNearEdgeA) - requires s2_edge_crossings *)

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
    let dist_radians = float_of_json_exn (member "distance_radians" c) in
    (* When distance is pi/2, the closest point is ambiguous on which side *)
    if not (Float.( = ) dist_radians (Float.pi /. 2.0))
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
               ~max_error:(Packed_float_option.Unboxed.none ())
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
  let expected_length2 = float_of_json_exn (member "min_distance_length2" c) in
  let result =
    S2.S2_edge_distances.update_min_distance x a b S2.S1_chord_angle.infinity
  in
  check_bool "updated" ~expected:true ~actual:(S2.S1_chord_angle.Option.is_some result);
  (match%optional_u.S2.S1_chord_angle.Option result with
   | None -> Alcotest.fail "expected Some"
   | Some d ->
     check_float
       "length2"
       ~expected:expected_length2
       ~actual:(Float_u.to_float (S2.S1_chord_angle.length2 d)));
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
    then Alcotest.fail (sprintf "point_on_line[%d]: angle=%g > 3e-15" i angle))
;;

(* ---------- GetPointToLeft / GetPointToRight ---------- *)

let test_point_to_left_right () =
  let c = member "point_to_left_right" (Lazy.force fixture) in
  let a = point_of_json (member "a" c) in
  let b = point_of_json (member "b" c) in
  let r = float_u_of_json_exn (member "distance_radians" c) in
  let expected_left = point_of_json (member "left" c) in
  let expected_right = point_of_json (member "right" c) in
  let expected_left_dist = float_of_json_exn (member "left_dist" c) in
  let expected_right_dist = float_of_json_exn (member "right_dist" c) in
  let angle = S2.S1_angle.of_radians r in
  let actual_left = S2.S2_edge_distances.get_point_to_left a b angle in
  let actual_right = S2.S2_edge_distances.get_point_to_right a b angle in
  let left_dist =
    Float_u.to_float (S2.S1_angle.radians (S2.S2_point.distance a actual_left))
  in
  let right_dist =
    Float_u.to_float (S2.S1_angle.radians (S2.S2_point.distance a actual_right))
  in
  check_float "left_dist" ~expected:expected_left_dist ~actual:left_dist;
  check_float "right_dist" ~expected:expected_right_dist ~actual:right_dist;
  let me = Packed_float_option.Unboxed.some #1e-14 in
  check_bool
    "left_approx"
    ~expected:true
    ~actual:(S2.S2_point.approx_equal ~max_error:me actual_left expected_left);
  check_bool
    "right_approx"
    ~expected:true
    ~actual:(S2.S2_point.approx_equal ~max_error:me actual_right expected_right)
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
           ~max_error:(Packed_float_option.Unboxed.none ())
           actual
           expected))
;;

(* ---------- Quickcheck: edge-distance invariants ---------- *)

module S2_point_triple = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    ; x : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let gen_unit_point rnd =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let rec loop () =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      if Float.((x *. x) +. (y *. y) +. (z *. z) < 1e-6)
      then loop ()
      else
        S2.S2_point.of_coords
          ~x:(Float_u.of_float x)
          ~y:(Float_u.of_float y)
          ~z:(Float_u.of_float z)
    in
    loop ()
  ;;

  let quickcheck_generator =
    Base_quickcheck.Generator.create (fun ~size:_ ~random:rnd ->
      let a = gen_unit_point rnd in
      let b = gen_unit_point rnd in
      let x = gen_unit_point rnd in
      { a; b; x })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

let quickcheck_distance_to_degenerate_edge () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; x; _ } ->
      (* Distance from [x] to the degenerate edge [aa] is just the [xa] angle. *)
      let d_edge =
        Float_u.to_float (S2.S1_angle.radians (S2.S2_edge_distances.get_distance x a a))
      in
      let d_point = Float_u.to_float (S2.S1_angle.radians (S2.S2_point.distance x a)) in
      let diff = Float.abs (d_edge -. d_point) in
      if Float.( > ) diff 1e-13
      then Alcotest.failf "get_distance x a a vs dist(x,a): %g vs %g" d_edge d_point)
;;

let quickcheck_interpolate_endpoints () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; _ } ->
      let at_a = S2.S2_edge_distances.interpolate a b #0.0 in
      let at_b = S2.S2_edge_distances.interpolate a b #1.0 in
      let d_a = Float_u.to_float (S2.S1_angle.radians (S2.S2_point.distance at_a a)) in
      let d_b = Float_u.to_float (S2.S1_angle.radians (S2.S2_point.distance at_b b)) in
      if Float.( > ) d_a 3e-15 then Alcotest.failf "interpolate a b 0 angle to a = %g" d_a;
      if Float.( > ) d_b 3e-15 then Alcotest.failf "interpolate a b 1 angle to b = %g" d_b)
;;

let quickcheck_project_is_on_edge () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; x } ->
      (* [a] and [b] must be distinct for a well-defined projection; they come
         from independent random draws so equality is vanishingly unlikely, but
         we skip the case defensively. *)
      if not (S2.S2_point.equal a b)
      then (
        let proj = S2.S2_edge_distances.project x a b in
        let d =
          Float_u.to_float
            (S2.S1_angle.radians (S2.S2_edge_distances.get_distance proj a b))
        in
        if Float.( > ) d 1e-13 then Alcotest.failf "project then get_distance = %g" d))
;;

(* ---------- Alcotest suite ---------- *)

let () =
  Alcotest.run
    "S2_edge_distances"
    [ ( "distance"
      , [ Alcotest.test_case "get_distance" `Quick test_distance
        ; Alcotest.test_case "project" `Quick test_project
        ; Alcotest.test_case "project_fixture" `Quick test_project_fixture
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
    ; ( "quickcheck"
      , [ Alcotest.test_case
            "distance_degenerate_edge"
            `Quick
            quickcheck_distance_to_degenerate_edge
        ; Alcotest.test_case
            "interpolate_endpoints"
            `Quick
            quickcheck_interpolate_endpoints
        ; Alcotest.test_case "project_is_on_edge" `Quick quickcheck_project_is_on_edge
        ] )
    ]
;;
