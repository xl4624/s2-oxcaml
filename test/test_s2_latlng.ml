(* C++ test parity: s2geometry/src/s2/s2latlng_test.cc
   Golden data from test/gen/s2latlng.cc.

   Covered:
   -  TEST(S2LatLng, TestBasic)       - constructors, is_valid, normalized, arithmetic
   -  TEST(S2LatLng, TestConversion)  - conversion (point round-trips, Latitude/Longitude)
   -  TEST(S2LatLng, NegativeZeros)   - negative_zeros
   -  TEST(S2LatLng, TestDistance)    - distance
   -  E5/E6/E7 constructors          - e_constructors
   -  ToPoint                        - to_point
   -  ApproxEquals                   - approx_equal
   -  TEST(S2LatLng, TestToString)    - to_string_in_degrees

   Deliberately omitted:
   -  TEST(S2LatLng, InfIsInvalid) / NanIsInvalid - covered by is_valid cases
   -  TEST(S2LatLng, TestHashCode)    - hash not ported
   -  TEST(S2LatLng, S2CoderWorks)    - serialization not ported
   -  TEST(S2LatLng, SupportsAbslHash) - hash not ported
   -  Benchmarks *)

open Core
open Test_helpers
open Alcotest

let test_constructors fixture () =
  let cases = to_list (member "constructors" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let expected_lat = float_u_of_json_exn (member "lat" c) in
    let expected_lng = float_u_of_json_exn (member "lng" c) in
    let expected_valid = bool_of_json_exn (member "is_valid" c) in
    let result =
      match op with
      | "from_radians" ->
        S2.S2_latlng.of_radians
          ~lat:(float_u_of_json_exn (member "lat_rad" c))
          ~lng:(float_u_of_json_exn (member "lng_rad" c))
      | "from_degrees" ->
        S2.S2_latlng.of_degrees
          ~lat:(float_u_of_json_exn (member "lat_deg" c))
          ~lng:(float_u_of_json_exn (member "lng_deg" c))
      | "default" -> S2.S2_latlng.zero
      | "invalid" -> S2.S2_latlng.invalid
      | _ ->
        (match failwith ("unknown op: " ^ op) with
         | (_ : Nothing.t) -> .)
    in
    check_float_u_exact
      (op ^ " lat")
      ~expected:expected_lat
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat result));
    check_float_u_exact
      (op ^ " lng")
      ~expected:expected_lng
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng result));
    check_bool
      (op ^ " is_valid")
      ~expected:expected_valid
      ~actual:(S2.S2_latlng.is_valid result))
;;

let test_is_valid fixture () =
  let cases = to_list (member "is_valid" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let ll =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "lat_deg" c))
        ~lng:(float_u_of_json_exn (member "lng_deg" c))
    in
    let expected = bool_of_json_exn (member "is_valid" c) in
    check_bool name ~expected ~actual:(S2.S2_latlng.is_valid ll))
;;

let test_normalized fixture () =
  let cases = to_list (member "normalized" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let ll =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "lat_deg" c))
        ~lng:(float_u_of_json_exn (member "lng_deg" c))
    in
    let n = S2.S2_latlng.normalized ll in
    let expected_lat = float_u_of_json_exn (member "result_lat" c) in
    let expected_lng = float_u_of_json_exn (member "result_lng" c) in
    let expected_valid = bool_of_json_exn (member "result_is_valid" c) in
    check_float_u
      (name ^ " lat")
      ~expected:expected_lat
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat n));
    check_float_u
      (name ^ " lng")
      ~expected:expected_lng
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng n));
    check_bool
      (name ^ " is_valid")
      ~expected:expected_valid
      ~actual:(S2.S2_latlng.is_valid n))
;;

let test_arithmetic fixture () =
  let cases = to_list (member "arithmetic" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let a = latlng_of_json_exn (member "a" c) in
    let expected = latlng_of_json_exn (member "result" c) in
    let result =
      match op with
      | "add" ->
        let b = latlng_of_json_exn (member "b" c) in
        S2.S2_latlng.add a b
      | "sub" ->
        let b = latlng_of_json_exn (member "b" c) in
        S2.S2_latlng.sub a b
      | "mul" ->
        let s = float_u_of_json_exn (member "scalar" c) in
        S2.S2_latlng.mul a s
      | _ ->
        (match failwith ("unknown op: " ^ op) with
         | (_ : Nothing.t) -> .)
    in
    check_float_u_exact
      (op ^ " lat")
      ~expected:(S2.S1_angle.radians (S2.S2_latlng.lat expected))
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat result));
    check_float_u_exact
      (op ^ " lng")
      ~expected:(S2.S1_angle.radians (S2.S2_latlng.lng expected))
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng result)))
;;

let test_conversion fixture () =
  let cases = to_list (member "conversion" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let ll =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "lat_deg" c))
        ~lng:(float_u_of_json_exn (member "lng_deg" c))
    in
    let p = S2.S2_latlng.to_point ll in
    let expected_point = r3_vector_of_json (member "point" c) in
    check_r3_vector (name ^ " to_point") ~expected:expected_point ~actual:p;
    let back = S2.S2_latlng.of_point p in
    check_float_u
      (name ^ " back lat")
      ~expected:(float_u_of_json_exn (member "back_lat" c))
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat back));
    check_float_u
      (name ^ " back lng")
      ~expected:(float_u_of_json_exn (member "back_lng" c))
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng back)))
;;

let test_lat_lng_from_point fixture () =
  let cases = to_list (member "lat_lng_from_point" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let p = r3_vector_of_json (member "point" c) in
    let expected_lat = float_u_of_json_exn (member "lat" c) in
    let expected_lng = float_u_of_json_exn (member "lng" c) in
    check_float_u
      (name ^ " lat")
      ~expected:expected_lat
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.latitude p));
    check_float_u
      (name ^ " lng")
      ~expected:expected_lng
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.longitude p)))
;;

let test_negative_zeros fixture () =
  let open Float_u.O in
  let cases = to_list (member "negative_zeros" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let p = r3_vector_of_json (member "point" c) in
    let which = string_of_json_exn (member "which" c) in
    let expected = float_u_of_json_exn (member "value" c) in
    let is_neg_zero = bool_of_json_exn (member "is_negative_zero" c) in
    let actual =
      S2.S1_angle.radians
        (match which with
         | "lat" -> S2.S2_latlng.latitude p
         | "lng" -> S2.S2_latlng.longitude p
         | _ ->
           (match failwith "bad which" with
            | (_ : Nothing.t) -> .))
    in
    check_float_u_exact (name ^ " value") ~expected ~actual;
    check_bool
      (name ^ " not neg zero")
      ~expected:is_neg_zero
      ~actual:(actual = #0.0 && Float_u.ieee_negative actual))
;;

let test_distance fixture () =
  let cases = to_list (member "distance" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "a_lat_deg" c))
        ~lng:(float_u_of_json_exn (member "a_lng_deg" c))
    in
    let b =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "b_lat_deg" c))
        ~lng:(float_u_of_json_exn (member "b_lng_deg" c))
    in
    let expected = float_u_of_json_exn (member "distance_radians" c) in
    check_float_u
      (name ^ " distance")
      ~expected
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.distance a b)))
;;

let test_approx_equal fixture () =
  let cases = to_list (member "approx_equal" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = latlng_of_json_exn (member "a" c) in
    let b = latlng_of_json_exn (member "b" c) in
    let expected = bool_of_json_exn (member "approx_equal" c) in
    check_bool
      name
      ~expected
      ~actual:
        (S2.S2_latlng.approx_equal ~max_error:(Packed_float_option.Unboxed.none ()) a b))
;;

let test_e_constructors fixture () =
  let cases = to_list (member "e_constructors" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let expected_lat = float_u_of_json_exn (member "lat" c) in
    let expected_lng = float_u_of_json_exn (member "lng" c) in
    let result =
      match op with
      | "from_e5" ->
        S2.S2_latlng.of_e5_exn
          ~lat:(int_of_json_exn (member "lat_e5" c))
          ~lng:(int_of_json_exn (member "lng_e5" c))
      | "from_e6" ->
        S2.S2_latlng.of_e6_exn
          ~lat:(int_of_json_exn (member "lat_e6" c))
          ~lng:(int_of_json_exn (member "lng_e6" c))
      | "from_e7" ->
        S2.S2_latlng.of_e7_exn
          ~lat:(int_of_json_exn (member "lat_e7" c))
          ~lng:(int_of_json_exn (member "lng_e7" c))
      | _ ->
        (match failwith ("unknown op: " ^ op) with
         | (_ : Nothing.t) -> .)
    in
    check_float_u_exact
      (op ^ " lat")
      ~expected:expected_lat
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat result));
    check_float_u_exact
      (op ^ " lng")
      ~expected:expected_lng
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng result)))
;;

let test_to_point fixture () =
  let cases = to_list (member "to_point" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let ll =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "lat_deg" c))
        ~lng:(float_u_of_json_exn (member "lng_deg" c))
    in
    let expected = r3_vector_of_json (member "point" c) in
    let result = S2.S2_latlng.to_point ll in
    check_r3_vector (name ^ " to_point") ~expected ~actual:result)
;;

let test_to_string_in_degrees fixture () =
  let cases = to_list (member "to_string_in_degrees" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let ll =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "lat_deg" c))
        ~lng:(float_u_of_json_exn (member "lng_deg" c))
    in
    let expected = string_of_json_exn (member "expected" c) in
    let actual = S2.S2_latlng.to_string_in_degrees ll in
    Alcotest.check Alcotest.string (name ^ " to_string_in_degrees") expected actual)
;;

let () =
  let fixture = load_fixture "s2latlng.json" in
  Alcotest.run
    "S2_latlng"
    [ "constructors", [ test_case "Constructors" `Quick (test_constructors fixture) ]
    ; "is_valid", [ test_case "IsValid" `Quick (test_is_valid fixture) ]
    ; "normalized", [ test_case "Normalized" `Quick (test_normalized fixture) ]
    ; "arithmetic", [ test_case "Arithmetic" `Quick (test_arithmetic fixture) ]
    ; "conversion", [ test_case "Conversion" `Quick (test_conversion fixture) ]
    ; ( "lat_lng_from_point"
      , [ test_case "LatLngFromPoint" `Quick (test_lat_lng_from_point fixture) ] )
    ; "negative_zeros", [ test_case "NegativeZeros" `Quick (test_negative_zeros fixture) ]
    ; "distance", [ test_case "Distance" `Quick (test_distance fixture) ]
    ; "approx_equal", [ test_case "ApproxEquals" `Quick (test_approx_equal fixture) ]
    ; ( "e_constructors"
      , [ test_case "E_Constructors" `Quick (test_e_constructors fixture) ] )
    ; "to_point", [ test_case "ToPoint" `Quick (test_to_point fixture) ]
    ; ( "to_string_in_degrees"
      , [ test_case "TestToString" `Quick (test_to_string_in_degrees fixture) ] )
    ]
;;
