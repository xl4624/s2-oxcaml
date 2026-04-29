(* C++ test parity: s2geometry/src/s2/s1angle_test.cc
   -  TEST(S1Angle, DefaultConstructor)            - full parity (constructors)
   -  TEST(S1Angle, Infinity)                      - full parity (special_values)
   -  TEST(S1Angle, Zero)                          - full parity (special_values)
   -  TEST(S1Angle, PiRadiansExactly180Degrees)    - full parity (constructors)
   -  TEST(S1Angle, E5E6E7Representations)         - full parity (constructors + to_e5_e6_e7)
   -  TEST(S1Angle, E6E7RepresentationsUnsigned)   - full parity (constructors)
   -  TEST(S1Angle, NormalizeCorrectlyCanonicalizesAngles) - full parity (normalized)
   -  TEST(S1Angle, ArithmeticOperationsOnAngles)  - full parity (arithmetic)
   -  TEST(S1Angle, Trigonometry)                  - full parity (trigonometry + sin_cos)
   -  TEST(S1Angle, DegreesVsE6)                   - full parity (degrees_vs_e6)
   -  TEST(S1Angle, DegreesVsE7)                   - full parity (degrees_vs_e7)
   -  TEST(S1Angle, E6VsE7)                        - full parity (e6_vs_e7, PRNG seed 12345)
   -  TEST(S1Angle, DegreesVsRadians)              - full parity (degrees_vs_radians)

   Deliberately omitted (other modules / I/O / benchmarks):
   -  TEST(S1Angle, ConstructorsThatMeasureAngles) - [S2Point] / [S2LatLng]; use [R3_vector]
   -  TEST(S1Angle, TestFormatting), TEST(S1Angle, RoundtripEncodingSucceeds),
      TEST(S1Angle, DecodeFailsWithTruncatedBuffer)
   -  BM_E6ToRadians, BM_RadiansToE6 in [s1angle_test.cc] *)

open Core
open Test_helpers
open Alcotest

let test_constructors fixture () =
  let cases = to_list (member "constructors" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let expected_rad = float_u_of_json_exn (member "radians" c) in
    let expected_deg = float_u_of_json_exn (member "degrees" c) in
    let angle =
      match op with
      | "default" -> S2.S1_angle.zero
      | "radians" -> S2.S1_angle.of_radians (float_u_of_json_exn (member "input" c))
      | "degrees" -> S2.S1_angle.of_degrees (float_u_of_json_exn (member "input" c))
      | "e5" -> S2.S1_angle.of_e5 (int_of_json_exn (member "input" c))
      | "e6" -> S2.S1_angle.of_e6 (int_of_json_exn (member "input" c))
      | "e7" -> S2.S1_angle.of_e7 (int_of_json_exn (member "input" c))
      | "unsigned_e6" ->
        S2.S1_angle.of_unsigned_e6 (int_of_json_exn (member "unsigned" c))
      | "unsigned_e7" ->
        S2.S1_angle.of_unsigned_e7 (int_of_json_exn (member "unsigned" c))
      | _ ->
        (match failwith (sprintf "unknown op: %s" op) with
         | (_ : Nothing.t) -> .)
    in
    check_float_u_exact
      (op ^ " radians")
      ~expected:expected_rad
      ~actual:(S2.S1_angle.radians angle);
    check_float_u
      (op ^ " degrees")
      ~expected:expected_deg
      ~actual:(S2.S1_angle.degrees angle))
;;

let test_to_e5_e6_e7 fixture () =
  let cases = to_list (member "to_e5_e6_e7" fixture) in
  List.iter cases ~f:(fun c ->
    let degu = float_u_of_json_exn (member "degrees" c) in
    let angle = S2.S1_angle.of_degrees degu in
    let label = Float_u.to_string degu in
    (check int)
      (label ^ " e5")
      (int_of_json_exn (member "e5" c))
      (S2.S1_angle.e5_exn angle);
    (check int)
      (label ^ " e6")
      (int_of_json_exn (member "e6" c))
      (S2.S1_angle.e6_exn angle);
    (check int)
      (label ^ " e7")
      (int_of_json_exn (member "e7" c))
      (S2.S1_angle.e7_exn angle))
;;

let test_special_values fixture () =
  let sv = member "special_values" fixture in
  check_float_u_exact
    "zero"
    ~expected:(float_u_of_json_exn (member "zero_radians" sv))
    ~actual:(S2.S1_angle.radians S2.S1_angle.zero);
  (check bool) "infinity is inf" true (S2.S1_angle.is_inf S2.S1_angle.infinity)
;;

let test_arithmetic fixture () =
  let cases = to_list (member "arithmetic" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let expected = float_u_of_json_exn (member "result" c) in
    let result =
      match op with
      | "abs" ->
        S2.S1_angle.radians
          (S2.S1_angle.abs
             (S2.S1_angle.of_radians (float_u_of_json_exn (member "input" c))))
      | "negate" ->
        S2.S1_angle.radians
          (S2.S1_angle.neg
             (S2.S1_angle.of_radians (float_u_of_json_exn (member "input" c))))
      | "add" ->
        let a = S2.S1_angle.of_radians (float_u_of_json_exn (member "a" c)) in
        let b = S2.S1_angle.of_radians (float_u_of_json_exn (member "b" c)) in
        S2.S1_angle.radians (S2.S1_angle.add a b)
      | "sub" ->
        let a = S2.S1_angle.of_radians (float_u_of_json_exn (member "a" c)) in
        let b = S2.S1_angle.of_radians (float_u_of_json_exn (member "b" c)) in
        S2.S1_angle.radians (S2.S1_angle.sub a b)
      | "mul" ->
        let a = S2.S1_angle.of_radians (float_u_of_json_exn (member "a" c)) in
        let mu = float_u_of_json_exn (member "m" c) in
        S2.S1_angle.radians (S2.S1_angle.mul a mu)
      | "div" ->
        let a = S2.S1_angle.of_radians (float_u_of_json_exn (member "a" c)) in
        let mu = float_u_of_json_exn (member "m" c) in
        S2.S1_angle.radians (S2.S1_angle.div a mu)
      | "div_angle" ->
        let a = S2.S1_angle.of_radians (float_u_of_json_exn (member "a" c)) in
        let b = S2.S1_angle.of_radians (float_u_of_json_exn (member "b" c)) in
        S2.S1_angle.ratio a b
      | _ ->
        (match failwith (sprintf "unknown op: %s" op) with
         | (_ : Nothing.t) -> .)
    in
    check_float_u_exact op ~expected ~actual:result)
;;

let test_normalized fixture () =
  let cases = to_list (member "normalized" fixture) in
  List.iter cases ~f:(fun c ->
    let deg = float_u_of_json_exn (member "input_degrees" c) in
    let expected_deg = float_u_of_json_exn (member "result_degrees" c) in
    let expected_rad = float_u_of_json_exn (member "result_radians" c) in
    let result = S2.S1_angle.normalized (S2.S1_angle.of_degrees deg) in
    let label = sprintf "normalized(%s)" (Float_u.to_string deg) in
    check_float_u
      (label ^ " degrees")
      ~expected:expected_deg
      ~actual:(S2.S1_angle.degrees result);
    check_float_u
      (label ^ " radians")
      ~expected:expected_rad
      ~actual:(S2.S1_angle.radians result))
;;

let test_trigonometry fixture () =
  let cases = to_list (member "trigonometry" fixture) in
  List.iter cases ~f:(fun c ->
    let deg = float_u_of_json_exn (member "degrees" c) in
    let angle = S2.S1_angle.of_degrees deg in
    let label = sprintf "trig(%s)" (Float_u.to_string deg) in
    check_float_u_exact
      (label ^ " sin")
      ~expected:(float_u_of_json_exn (member "sin" c))
      ~actual:(S2.S1_angle.sin angle);
    check_float_u_exact
      (label ^ " cos")
      ~expected:(float_u_of_json_exn (member "cos" c))
      ~actual:(S2.S1_angle.cos angle);
    check_float_u_exact
      (label ^ " tan")
      ~expected:(float_u_of_json_exn (member "tan" c))
      ~actual:(S2.S1_angle.tan angle))
;;

let test_degrees_vs_e6 fixture () =
  let cases = to_list (member "degrees_vs_e6" fixture) in
  List.iter cases ~f:(fun c ->
    let n = int_of_json_exn (member "n" c) in
    let from_deg = S2.S1_angle.of_degrees (Float_u.of_int n) in
    let from_e6 = S2.S1_angle.of_e6 (1000000 * n) in
    (check bool)
      (sprintf "deg(%d) == e6(%d)" n (1000000 * n))
      (bool_of_json_exn (member "equal" c))
      (S2.S1_angle.equal from_deg from_e6))
;;

let test_degrees_vs_e7 fixture () =
  let cases = to_list (member "degrees_vs_e7" fixture) in
  List.iter cases ~f:(fun c ->
    let n = int_of_json_exn (member "n" c) in
    let from_deg = S2.S1_angle.of_degrees (Float_u.of_int n) in
    let from_e7 = S2.S1_angle.of_e7 (10000000 * n) in
    (check bool)
      (sprintf "deg(%d) == e7(%d)" n (10000000 * n))
      (bool_of_json_exn (member "equal" c))
      (S2.S1_angle.equal from_deg from_e7))
;;

let test_degrees_vs_radians fixture () =
  let open Float_u.O in
  let cases = to_list (member "degrees_vs_radians" fixture) in
  List.iter cases ~f:(fun c ->
    let k = int_of_json_exn (member "k" c) in
    let from_deg = S2.S1_angle.of_degrees (#45.0 * Float_u.of_int k) in
    let from_rad = S2.S1_angle.of_radians (Float_u.of_int k * Float_u.pi / #4.0) in
    (check bool)
      (sprintf "deg(45*%d) == rad(%d*pi/4)" k k)
      (bool_of_json_exn (member "equal" c))
      (S2.S1_angle.equal from_deg from_rad);
    check_float_u_exact
      (sprintf "deg(45*%d).degrees" k)
      ~expected:(float_u_of_json_exn (member "expected_degrees" c))
      ~actual:(S2.S1_angle.degrees from_deg))
;;

let test_sin_cos fixture () =
  let cases = to_list (member "sin_cos" fixture) in
  List.iter cases ~f:(fun c ->
    let deg = float_u_of_json_exn (member "degrees" c) in
    let angle = S2.S1_angle.of_degrees deg in
    let label = sprintf "sin_cos(%s)" (Float_u.to_string deg) in
    let #(s, co) = S2.S1_angle.sin_cos angle in
    check_float_u_exact
      (label ^ " sin")
      ~expected:(float_u_of_json_exn (member "sin" c))
      ~actual:s;
    check_float_u_exact
      (label ^ " cos")
      ~expected:(float_u_of_json_exn (member "cos" c))
      ~actual:co)
;;

let test_e6_vs_e7 fixture () =
  let cases = to_list (member "e6_vs_e7" fixture) in
  List.iter cases ~f:(fun c ->
    let i = int_of_json_exn (member "e6_input" c) in
    (check bool)
      (sprintf "E6(%d)==E7(10*%d)" i i)
      (bool_of_json_exn (member "equal" c))
      (S2.S1_angle.equal (S2.S1_angle.of_e6 i) (S2.S1_angle.of_e7 (10 * i))))
;;

let () =
  let fixture = load_fixture "s1angle.json" in
  Alcotest.run
    "S1_angle"
    [ ( "constructors"
      , [ test_case "DefaultConstructor" `Quick (test_constructors fixture) ] )
    ; ( "to_e5_e6_e7"
      , [ test_case "E5E6E7Representations" `Quick (test_to_e5_e6_e7 fixture) ] )
    ; "special_values", [ test_case "Infinity" `Quick (test_special_values fixture) ]
    ; ( "arithmetic"
      , [ test_case "ArithmeticOperationsOnAngles" `Quick (test_arithmetic fixture) ] )
    ; ( "normalized"
      , [ test_case
            "NormalizeCorrectlyCanonicalizesAngles"
            `Quick
            (test_normalized fixture)
        ] )
    ; "trigonometry", [ test_case "Trigonometry" `Quick (test_trigonometry fixture) ]
    ; "degrees_vs_e6", [ test_case "DegreesVsE6" `Quick (test_degrees_vs_e6 fixture) ]
    ; "degrees_vs_e7", [ test_case "DegreesVsE7" `Quick (test_degrees_vs_e7 fixture) ]
    ; ( "degrees_vs_radians"
      , [ test_case "DegreesVsRadians" `Quick (test_degrees_vs_radians fixture) ] )
    ; "sin_cos", [ test_case "Trigonometry" `Quick (test_sin_cos fixture) ]
    ; "e6_vs_e7", [ test_case "E6VsE7" `Quick (test_e6_vs_e7 fixture) ]
    ]
;;
