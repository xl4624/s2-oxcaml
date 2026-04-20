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
   -  TEST(S2Point, SubtractionWorks)   - add/sub/mul/neg via arith
   -  TEST(S2Point, ElementWiseDivisionWorks) - mul_components / div_components
   -  TEST(S2Point, SqrtWorks)          - sqrt
   -  TEST(S2Point, FloorWorks)         - floor
   -  TEST(S2Point, CeilWorks)          - ceil
   -  TEST(S2Point, FRoundWorks)        - fround
   -  Max / Min (inherited from Vector3) - max / min
   -  S2Point::NaN                      - nan / is_nan
   -  distance, stable_angle, chord_angle_between, from_coords

   Deliberately omitted:
   -  TEST(S2Point, HashSpreads) - hash is not part of this OCaml port
   -  TEST(S2Point, IsAVector) - type-layout test, not applicable
   -  TEST(S2Point, CoderWorks) - serialization not ported
   -  TEST(S2, OriginTest) collinearity checks - requires S2Cell/S2Predicates *)

open Core
open Test_helpers

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
    let max_error = float_u_of_json_exn (member "max_error" c) in
    let expected = bool_of_json_exn (member "expected" c) in
    Alcotest.(check bool)
      "approx_equal"
      expected
      (S2.S2_point.approx_equal
         ~max_error:(Packed_float_option.Unboxed.some max_error)
         a
         b))
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
    (* Determinant of the 3x3 frame via the scalar triple product of its
       columns: det = col0 . (col1 x col2). An orthonormal right-handed frame
       satisfies det = 1. *)
    let #{ S2.S2_point.col0; col1; col2 } = frame in
    let det = Float_u.to_float (S2.R3_vector.dot col0 (S2.R3_vector.cross col1 col2)) in
    check_float (name ^ " det") ~expected:(float_of_json_exn (member "det" c)) ~actual:det)
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

let test_arith fixture () =
  let cases = to_list (member "arith" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let k = float_u_of_json_exn (member "k" c) in
    check_point_exact
      (name ^ " add")
      (point_of_json (member "add" c))
      (S2.S2_point.add a b);
    check_point_exact
      (name ^ " sub")
      (point_of_json (member "sub" c))
      (S2.S2_point.sub a b);
    check_point_exact
      (name ^ " mul")
      (point_of_json (member "mul" c))
      (S2.S2_point.mul a k);
    check_point_exact (name ^ " neg") (point_of_json (member "neg" c)) (S2.S2_point.neg a))
;;

let test_components fixture () =
  let cases = to_list (member "components" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    check_point_exact
      (name ^ " mul_components")
      (point_of_json (member "mul_components" c))
      (S2.S2_point.mul_components a b);
    check_point_exact
      (name ^ " div_components")
      (point_of_json (member "div_components" c))
      (S2.S2_point.div_components a b))
;;

let test_minmax fixture () =
  let cases = to_list (member "minmax" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    check_point_exact
      (name ^ " max")
      (point_of_json (member "max" c))
      (S2.S2_point.max a b);
    check_point_exact
      (name ^ " min")
      (point_of_json (member "min" c))
      (S2.S2_point.min a b))
;;

let test_sqrt fixture () =
  let cases = to_list (member "sqrt" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    check_point_exact
      (name ^ " sqrt")
      (point_of_json (member "sqrt" c))
      (S2.S2_point.sqrt a))
;;

let test_floor fixture () =
  let cases = to_list (member "floor" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    check_point_exact
      (name ^ " floor")
      (point_of_json (member "floor" c))
      (S2.S2_point.floor a))
;;

let test_ceil fixture () =
  let cases = to_list (member "ceil" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    check_point_exact
      (name ^ " ceil")
      (point_of_json (member "ceil" c))
      (S2.S2_point.ceil a))
;;

let test_fround fixture () =
  let cases = to_list (member "fround" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    check_point_exact
      (name ^ " fround")
      (point_of_json (member "fround" c))
      (S2.S2_point.fround a))
;;

let test_nan fixture () =
  let cases = member "nan" fixture in
  let nan_point = S2.S2_point.nan () in
  check_bool
    "nan () is nan"
    ~expected:(bool_of_json_exn (member "nan_is_nan" cases))
    ~actual:(S2.S2_point.is_nan nan_point);
  let finite = point_of_json (member "finite" cases) in
  check_bool
    "finite point is not nan"
    ~expected:(bool_of_json_exn (member "finite_is_nan" cases))
    ~actual:(S2.S2_point.is_nan finite);
  (* Verify each component of [nan ()] is NaN. *)
  Alcotest.(check bool) "x is nan" true (Float_u.is_nan (S2.S2_point.x nan_point));
  Alcotest.(check bool) "y is nan" true (Float_u.is_nan (S2.S2_point.y nan_point));
  Alcotest.(check bool) "z is nan" true (Float_u.is_nan (S2.S2_point.z nan_point))
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
    ; "arith", [ Alcotest.test_case "Arith" `Quick (test_arith fixture) ]
    ; "components", [ Alcotest.test_case "Components" `Quick (test_components fixture) ]
    ; "minmax", [ Alcotest.test_case "MinMax" `Quick (test_minmax fixture) ]
    ; "sqrt", [ Alcotest.test_case "Sqrt" `Quick (test_sqrt fixture) ]
    ; "floor", [ Alcotest.test_case "Floor" `Quick (test_floor fixture) ]
    ; "ceil", [ Alcotest.test_case "Ceil" `Quick (test_ceil fixture) ]
    ; "fround", [ Alcotest.test_case "FRound" `Quick (test_fround fixture) ]
    ; "nan", [ Alcotest.test_case "NaN" `Quick (test_nan fixture) ]
    ]
;;
