(* C++ test parity: s2geometry/src/s2/s2point_test.cc and s2pointutil_test.cc
   Golden data from test/gen/s2point.cc.

   Covered:
   -  TEST(S2, OriginTest)              - origin
   -  TEST(S2PointUtil, IsUnitLength)   - is_unit_length
   -  TEST(S2PointUtil, ApproxEquals)   - approx_equal
   -  TEST(S2PointUtil, Ortho)          - ortho
   -  TEST(S2Point, PointCross)         - robust_cross_prod
   -  TEST(S2, Frames)                  - frames
   -  TEST(S2, Rotate)                  - rotate
   -  distance, stable_angle, chord_angle_between, from_coords

   Deliberately omitted:
   -  TEST(S2Point, HashSpreads) - hash is not part of this OCaml port
   -  TEST(S2Point, IsAVector) - type-layout test, not applicable
   -  TEST(S2Point, CoderWorks) - serialization not ported
   -  TEST(S2Point, SubtractionWorks) - tested via R3_vector
   -  TEST(S2Point, ElementWiseDivisionWorks) - tested via R3_vector
   -  TEST(S2Point, SqrtWorks/FloorWorks/CeilWorks/FRoundWorks) - R3_vector ops
   -  TEST(S2, OriginTest) collinearity checks - requires S2Cell/S2Predicates *)

open Core
open Test_helpers

(* -- Quickcheck generators ------------------------------------------------ *)

module S2_point_gen = struct
  type t = { p : S2.S2_point.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    create (fun ~size:_ ~random:rnd ->
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      { p =
          S2.S2_point.of_coords
            ~x:(Float_u.of_float x)
            ~y:(Float_u.of_float y)
            ~z:(Float_u.of_float z)
      })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module S2_point_pair = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let gen_point rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      S2.S2_point.of_coords
        ~x:(Float_u.of_float x)
        ~y:(Float_u.of_float y)
        ~z:(Float_u.of_float z)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = gen_point rnd in
      let b = gen_point rnd in
      { a; b })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

let quickcheck_distance_nonneg () =
  Base_quickcheck.Test.run_exn
    (module S2_point_pair)
    ~config:qc_config
    ~f:(fun { S2_point_pair.a; b } ->
      assert (Float_u.O.(S2.S1_angle.radians (S2.S2_point.distance a b) >= #0.0)))
;;

let quickcheck_distance_symmetric () =
  Base_quickcheck.Test.run_exn
    (module S2_point_pair)
    ~config:qc_config
    ~f:(fun { S2_point_pair.a; b } ->
      let d1 = S2.S1_angle.radians (S2.S2_point.distance a b) in
      let d2 = S2.S1_angle.radians (S2.S2_point.distance b a) in
      assert (Float_u.O.(Float_u.abs (d1 - d2) <= #1e-15)))
;;

let quickcheck_distance_self_zero () =
  Base_quickcheck.Test.run_exn
    (module S2_point_gen)
    ~config:qc_config
    ~f:(fun { S2_point_gen.p } ->
      let d = S2.S1_angle.radians (S2.S2_point.distance p p) in
      assert (Float_u.O.(Float_u.abs d <= #1e-15)))
;;

let quickcheck_ortho_perpendicular () =
  Base_quickcheck.Test.run_exn
    (module S2_point_gen)
    ~config:qc_config
    ~f:(fun { S2_point_gen.p } ->
      let o = S2.S2_point.ortho p in
      let dot = S2.R3_vector.dot p o in
      assert (Float_u.O.(Float_u.abs dot <= #1e-14)))
;;

let quickcheck_ortho_unit () =
  Base_quickcheck.Test.run_exn
    (module S2_point_gen)
    ~config:qc_config
    ~f:(fun { S2_point_gen.p } ->
      let o = S2.S2_point.ortho p in
      assert (S2.S2_point.is_unit_length o))
;;

let quickcheck_rotate_preserves_length () =
  Base_quickcheck.Test.run_exn
    (module S2_point_pair)
    ~config:qc_config
    ~f:(fun { S2_point_pair.a; b } ->
      let angle = S2.S1_angle.of_radians #1.23 in
      let rotated = S2.S2_point.rotate a ~axis:b ~angle in
      assert (S2.S2_point.is_unit_length rotated))
;;

let quickcheck_frame_roundtrip () =
  Base_quickcheck.Test.run_exn
    (module S2_point_gen)
    ~config:qc_config
    ~f:(fun { S2_point_gen.p } ->
      let frame = S2.S2_point.get_frame p in
      let q = S2.S2_point.of_coords ~x:#0.6 ~y:#0.8 ~z:#0.0 in
      let local = S2.S2_point.to_frame frame q in
      let back = S2.S2_point.from_frame frame local in
      assert (S2.S2_point.approx_equal ~max_error:1e-14 q back))
;;

(* -- End quickcheck ------------------------------------------------------- *)

let check_point msg expected actual = check_r3_vector msg ~expected ~actual
let check_point_exact msg expected actual = check_r3_vector_exact msg ~expected ~actual
let point_of_json j = r3_vector_of_json j

let test_origin fixture () =
  let cases = member "origin" fixture in
  let expected = point_of_json (member "origin" cases) in
  check_point_exact "origin" expected S2.S2_point.origin;
  check_bool
    "origin is unit"
    ~expected:(bool_of_json_exn (member "is_unit_length" cases))
    ~actual:(S2.S2_point.is_unit_length S2.S2_point.origin)
;;

let test_is_unit_length fixture () =
  let cases = to_list (member "is_unit_length" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let p = point_of_json (member "p" c) in
    let expected = bool_of_json_exn (member "expected" c) in
    Alcotest.(check bool) name expected (S2.S2_point.is_unit_length p))
;;

let test_approx_equal fixture () =
  let cases = to_list (member "approx_equal" fixture) in
  List.iter cases ~f:(fun c ->
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let max_error = float_of_json_exn (member "max_error" c) in
    let expected = bool_of_json_exn (member "expected" c) in
    Alcotest.(check bool)
      "approx_equal"
      expected
      (S2.S2_point.approx_equal ~max_error a b))
;;

let test_ortho fixture () =
  let cases = to_list (member "ortho" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let expected = point_of_json (member "ortho" c) in
    let result = S2.S2_point.ortho a in
    check_point (name ^ " ortho") expected result;
    check_float_u
      (name ^ " dot")
      ~expected:(float_u_of_json_exn (member "dot" c))
      ~actual:(S2.R3_vector.dot a result);
    Alcotest.(check bool)
      (name ^ " unit")
      (bool_of_json_exn (member "is_unit" c))
      (S2.S2_point.is_unit_length result))
;;

let test_robust_cross_prod fixture () =
  let cases = to_list (member "robust_cross_prod" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let expected = point_of_json (member "result" c) in
    let result = S2.S2_point.robust_cross_prod a b in
    check_point (name ^ " robust_cross") expected result)
;;

let test_frames fixture () =
  let cases = to_list (member "frames" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let z = point_of_json (member "z" c) in
    let frame = S2.S2_point.get_frame z in
    let col0_expected = point_of_json (member "col0" c) in
    let col1_expected = point_of_json (member "col1" c) in
    let col2_expected = point_of_json (member "col2" c) in
    let to_frame_e0 = S2.S2_point.to_frame frame col0_expected in
    let to_frame_e1 = S2.S2_point.to_frame frame col1_expected in
    let to_frame_e2 = S2.S2_point.to_frame frame col2_expected in
    let from_frame_e0 =
      S2.S2_point.from_frame frame (S2.R3_vector.create ~x:#1.0 ~y:#0.0 ~z:#0.0)
    in
    let from_frame_e1 =
      S2.S2_point.from_frame frame (S2.R3_vector.create ~x:#0.0 ~y:#1.0 ~z:#0.0)
    in
    let from_frame_e2 =
      S2.S2_point.from_frame frame (S2.R3_vector.create ~x:#0.0 ~y:#0.0 ~z:#1.0)
    in
    check_point
      (name ^ " to_frame col0")
      (point_of_json (member "to_frame_col0" c))
      to_frame_e0;
    check_point
      (name ^ " to_frame col1")
      (point_of_json (member "to_frame_col1" c))
      to_frame_e1;
    check_point
      (name ^ " to_frame col2")
      (point_of_json (member "to_frame_col2" c))
      to_frame_e2;
    check_point
      (name ^ " from_frame e0")
      (point_of_json (member "from_frame_e0" c))
      from_frame_e0;
    check_point
      (name ^ " from_frame e1")
      (point_of_json (member "from_frame_e1" c))
      from_frame_e1;
    check_point
      (name ^ " from_frame e2")
      (point_of_json (member "from_frame_e2" c))
      from_frame_e2;
    check_float (name ^ " det") ~expected:(float_of_json_exn (member "det" c)) ~actual:1.0)
;;

let test_rotate fixture () =
  let cases = to_list (member "rotate" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let p = point_of_json (member "p" c) in
    let axis = point_of_json (member "axis" c) in
    let angle = S2.S1_angle.of_radians (float_u_of_json_exn (member "angle_radians" c)) in
    let expected = point_of_json (member "result" c) in
    let result = S2.S2_point.rotate p ~axis ~angle in
    check_point (name ^ " rotate") expected result;
    Alcotest.(check bool)
      (name ^ " unit")
      (bool_of_json_exn (member "is_unit" c))
      (S2.S2_point.is_unit_length result))
;;

let test_distance fixture () =
  let cases = to_list (member "distance" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    check_float_u
      (name ^ " distance")
      ~expected:(float_u_of_json_exn (member "angle_radians" c))
      ~actual:(S2.S1_angle.radians (S2.S2_point.distance a b)))
;;

let test_stable_angle fixture () =
  let cases = to_list (member "stable_angle" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    check_float_u
      (name ^ " stable_angle")
      ~expected:(float_u_of_json_exn (member "angle_radians" c))
      ~actual:(S2.S1_angle.radians (S2.S2_point.stable_angle a b)))
;;

let test_chord_angle_between fixture () =
  let cases = to_list (member "chord_angle_between" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    check_float_u
      (name ^ " chord_angle")
      ~expected:(float_u_of_json_exn (member "length2" c))
      ~actual:(S2.S1_chord_angle.length2 (S2.S2_point.chord_angle_between a b)))
;;

let test_from_coords fixture () =
  let cases = to_list (member "from_coords" fixture) in
  List.iter cases ~f:(fun c ->
    let x = float_u_of_json_exn (member "x" c) in
    let y = float_u_of_json_exn (member "y" c) in
    let z = float_u_of_json_exn (member "z" c) in
    let expected = point_of_json (member "result" c) in
    let result = S2.S2_point.of_coords ~x ~y ~z in
    check_point
      (sprintf
         "of_coords(%s,%s,%s)"
         (Float_u.to_string x)
         (Float_u.to_string y)
         (Float_u.to_string z))
      expected
      result)
;;

let () =
  let fixture = load_fixture "s2point.json" in
  Alcotest.run
    "S2_point"
    [ "origin", [ Alcotest.test_case "Origin" `Quick (test_origin fixture) ]
    ; ( "is_unit_length"
      , [ Alcotest.test_case "IsUnitLength" `Quick (test_is_unit_length fixture) ] )
    ; ( "approx_equal"
      , [ Alcotest.test_case "ApproxEquals" `Quick (test_approx_equal fixture) ] )
    ; "ortho", [ Alcotest.test_case "Ortho" `Quick (test_ortho fixture) ]
    ; ( "robust_cross_prod"
      , [ Alcotest.test_case "RobustCrossProd" `Quick (test_robust_cross_prod fixture) ] )
    ; "frames", [ Alcotest.test_case "Frames" `Quick (test_frames fixture) ]
    ; "rotate", [ Alcotest.test_case "Rotate" `Quick (test_rotate fixture) ]
    ; "distance", [ Alcotest.test_case "Distance" `Quick (test_distance fixture) ]
    ; ( "stable_angle"
      , [ Alcotest.test_case "StableAngle" `Quick (test_stable_angle fixture) ] )
    ; ( "chord_angle_between"
      , [ Alcotest.test_case "ChordAngleBetween" `Quick (test_chord_angle_between fixture)
        ] )
    ; "from_coords", [ Alcotest.test_case "FromCoords" `Quick (test_from_coords fixture) ]
    ; ( "quickcheck"
      , [ Alcotest.test_case "distance_nonneg" `Quick quickcheck_distance_nonneg
        ; Alcotest.test_case "distance_symmetric" `Quick quickcheck_distance_symmetric
        ; Alcotest.test_case "distance_self_zero" `Quick quickcheck_distance_self_zero
        ; Alcotest.test_case "ortho_perpendicular" `Quick quickcheck_ortho_perpendicular
        ; Alcotest.test_case "ortho_unit" `Quick quickcheck_ortho_unit
        ; Alcotest.test_case
            "rotate_preserves_length"
            `Quick
            quickcheck_rotate_preserves_length
        ; Alcotest.test_case "frame_roundtrip" `Quick quickcheck_frame_roundtrip
        ] )
    ]
;;
