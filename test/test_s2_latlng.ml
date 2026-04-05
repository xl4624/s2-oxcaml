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

   Deliberately omitted:
   -  TEST(S2LatLng, InfIsInvalid) / NanIsInvalid - covered by is_valid cases
   -  TEST(S2LatLng, TestToString)    - string formatting not ported
   -  TEST(S2LatLng, TestHashCode)    - hash not ported
   -  TEST(S2LatLng, S2CoderWorks)    - serialization not ported
   -  TEST(S2LatLng, SupportsAbslHash) - hash not ported
   -  Benchmarks *)

open Core
open Test_helpers

let of_degrees ~lat ~lng =
  S2.S2_latlng.of_degrees ~lat:(Float_u.of_float lat) ~lng:(Float_u.of_float lng)
;;

let of_radians ~lat ~lng =
  S2.S2_latlng.of_radians ~lat:(Float_u.of_float lat) ~lng:(Float_u.of_float lng)
;;

(* -- Quickcheck generators ------------------------------------------------ *)

module Latlng_gen = struct
  type t =
    { lat : float
    ; lng : float
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let lat_gen = float_inclusive (-90.0) 90.0 in
    let lng_gen = float_inclusive (-180.0) 180.0 in
    map (both lat_gen lng_gen) ~f:(fun (lat, lng) -> { lat; lng })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  let to_latlng t = of_degrees ~lat:t.lat ~lng:t.lng
end

module Latlng_pair = struct
  type t =
    { a_lat : float
    ; a_lng : float
    ; b_lat : float
    ; b_lng : float
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let lat_gen = float_inclusive (-90.0) 90.0 in
    let lng_gen = float_inclusive (-180.0) 180.0 in
    create (fun ~size:_ ~random:rnd ->
      { a_lat = generate lat_gen ~size:30 ~random:rnd
      ; a_lng = generate lng_gen ~size:30 ~random:rnd
      ; b_lat = generate lat_gen ~size:30 ~random:rnd
      ; b_lng = generate lng_gen ~size:30 ~random:rnd
      })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Latlng_any = struct
  type t =
    { lat : float
    ; lng : float
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let gen = float_inclusive (-1000.0) 1000.0 in
    map (both gen gen) ~f:(fun (lat, lng) -> { lat; lng })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

let quickcheck_add_commutative () =
  Base_quickcheck.Test.run_exn
    (module Latlng_pair)
    ~config:qc_config
    ~f:(fun { Latlng_pair.a_lat; a_lng; b_lat; b_lng } ->
      let a = of_degrees ~lat:a_lat ~lng:a_lng in
      let b = of_degrees ~lat:b_lat ~lng:b_lng in
      let ab = S2.S2_latlng.add a b in
      let ba = S2.S2_latlng.add b a in
      assert (S2.S2_latlng.equal ab ba))
;;

let quickcheck_point_roundtrip () =
  Base_quickcheck.Test.run_exn (module Latlng_gen) ~config:qc_config ~f:(fun t ->
    let ll = Latlng_gen.to_latlng t in
    let p = S2.S2_latlng.to_point ll in
    let back = S2.S2_latlng.of_point p in
    assert (S2.S2_latlng.approx_equal ~max_error:1e-14 ll back))
;;

let quickcheck_normalized_is_valid () =
  Base_quickcheck.Test.run_exn
    (module Latlng_any)
    ~config:qc_config
    ~f:(fun { Latlng_any.lat; lng } ->
      let ll = of_degrees ~lat ~lng in
      let n = S2.S2_latlng.normalized ll in
      assert (S2.S2_latlng.is_valid n))
;;

let quickcheck_normalized_idempotent () =
  Base_quickcheck.Test.run_exn
    (module Latlng_any)
    ~config:qc_config
    ~f:(fun { Latlng_any.lat; lng } ->
      let ll = of_degrees ~lat ~lng in
      let n = S2.S2_latlng.normalized ll in
      let nn = S2.S2_latlng.normalized n in
      assert (S2.S2_latlng.equal n nn))
;;

let quickcheck_distance_nonneg () =
  Base_quickcheck.Test.run_exn
    (module Latlng_pair)
    ~config:qc_config
    ~f:(fun { Latlng_pair.a_lat; a_lng; b_lat; b_lng } ->
      let a = of_degrees ~lat:a_lat ~lng:a_lng in
      let b = of_degrees ~lat:b_lat ~lng:b_lng in
      let d = Float_u.to_float (S2.S1_angle.radians (S2.S2_latlng.distance a b)) in
      assert (Float.( >= ) d 0.0))
;;

let quickcheck_distance_symmetric () =
  Base_quickcheck.Test.run_exn
    (module Latlng_pair)
    ~config:qc_config
    ~f:(fun { Latlng_pair.a_lat; a_lng; b_lat; b_lng } ->
      let a = of_degrees ~lat:a_lat ~lng:a_lng in
      let b = of_degrees ~lat:b_lat ~lng:b_lng in
      let d1 = Float_u.to_float (S2.S1_angle.radians (S2.S2_latlng.distance a b)) in
      let d2 = Float_u.to_float (S2.S1_angle.radians (S2.S2_latlng.distance b a)) in
      assert (Float.( <= ) (Float.abs (d1 -. d2)) 1e-15))
;;

(* -- End quickcheck ------------------------------------------------------- *)

let latlng_of_json j =
  match to_list j with
  | [ lat; lng ] -> of_radians ~lat:(float_of_json_exn lat) ~lng:(float_of_json_exn lng)
  | _ ->
    (match failwith "expected [lat, lng]" with
     | (_ : Nothing.t) -> .)
;;

let test_constructors fixture () =
  let cases = to_list (member "constructors" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let expected_lat = float_of_json_exn (member "lat" c) in
    let expected_lng = float_of_json_exn (member "lng" c) in
    let expected_valid = bool_of_json_exn (member "is_valid" c) in
    let result =
      match op with
      | "from_radians" ->
        of_radians
          ~lat:(float_of_json_exn (member "lat_rad" c))
          ~lng:(float_of_json_exn (member "lng_rad" c))
      | "from_degrees" ->
        of_degrees
          ~lat:(float_of_json_exn (member "lat_deg" c))
          ~lng:(float_of_json_exn (member "lng_deg" c))
      | "default" -> S2.S2_latlng.zero
      | "invalid" -> S2.S2_latlng.invalid
      | _ ->
        (match failwith ("unknown op: " ^ op) with
         | (_ : Nothing.t) -> .)
    in
    check_float_u_exact
      (op ^ " lat")
      ~expected:(Float_u.of_float expected_lat)
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat result));
    check_float_u_exact
      (op ^ " lng")
      ~expected:(Float_u.of_float expected_lng)
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
      of_degrees
        ~lat:(float_of_json_exn (member "lat_deg" c))
        ~lng:(float_of_json_exn (member "lng_deg" c))
    in
    let expected = bool_of_json_exn (member "is_valid" c) in
    check_bool name ~expected ~actual:(S2.S2_latlng.is_valid ll))
;;

let test_normalized fixture () =
  let cases = to_list (member "normalized" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let ll =
      of_degrees
        ~lat:(float_of_json_exn (member "lat_deg" c))
        ~lng:(float_of_json_exn (member "lng_deg" c))
    in
    let n = S2.S2_latlng.normalized ll in
    let expected_lat = float_of_json_exn (member "result_lat" c) in
    let expected_lng = float_of_json_exn (member "result_lng" c) in
    let expected_valid = bool_of_json_exn (member "result_is_valid" c) in
    check_float_u
      (name ^ " lat")
      ~expected:(Float_u.of_float expected_lat)
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat n));
    check_float_u
      (name ^ " lng")
      ~expected:(Float_u.of_float expected_lng)
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
    let a = latlng_of_json (member "a" c) in
    let expected = latlng_of_json (member "result" c) in
    let result =
      match op with
      | "add" ->
        let b = latlng_of_json (member "b" c) in
        S2.S2_latlng.add a b
      | "sub" ->
        let b = latlng_of_json (member "b" c) in
        S2.S2_latlng.sub a b
      | "mul" ->
        let s = float_of_json_exn (member "scalar" c) in
        S2.S2_latlng.mul a (Float_u.of_float s)
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
      of_degrees
        ~lat:(float_of_json_exn (member "lat_deg" c))
        ~lng:(float_of_json_exn (member "lng_deg" c))
    in
    let p = S2.S2_latlng.to_point ll in
    let expected_point = r3_vector_of_json (member "point" c) in
    check_r3_vector (name ^ " to_point") ~expected:expected_point ~actual:p;
    let back = S2.S2_latlng.of_point p in
    let expected_back_lat = float_of_json_exn (member "back_lat" c) in
    let expected_back_lng = float_of_json_exn (member "back_lng" c) in
    check_float_u
      (name ^ " back lat")
      ~expected:(Float_u.of_float expected_back_lat)
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat back));
    check_float_u
      (name ^ " back lng")
      ~expected:(Float_u.of_float expected_back_lng)
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng back)))
;;

let test_lat_lng_from_point fixture () =
  let cases = to_list (member "lat_lng_from_point" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let p = r3_vector_of_json (member "point" c) in
    let expected_lat = float_of_json_exn (member "lat" c) in
    let expected_lng = float_of_json_exn (member "lng" c) in
    check_float_u
      (name ^ " lat")
      ~expected:(Float_u.of_float expected_lat)
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.latitude p));
    check_float_u
      (name ^ " lng")
      ~expected:(Float_u.of_float expected_lng)
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.longitude p)))
;;

let test_negative_zeros fixture () =
  let cases = to_list (member "negative_zeros" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let p = r3_vector_of_json (member "point" c) in
    let which = string_of_json_exn (member "which" c) in
    let expected = float_of_json_exn (member "value" c) in
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
    check_float_u_exact (name ^ " value") ~expected:(Float_u.of_float expected) ~actual;
    check_bool
      (name ^ " not neg zero")
      ~expected:is_neg_zero
      ~actual:
        (let actual_boxed = Float_u.to_float actual in
         Float.( = ) actual_boxed 0.0 && Float.is_negative actual_boxed))
;;

let test_distance fixture () =
  let cases = to_list (member "distance" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a =
      of_degrees
        ~lat:(float_of_json_exn (member "a_lat_deg" c))
        ~lng:(float_of_json_exn (member "a_lng_deg" c))
    in
    let b =
      of_degrees
        ~lat:(float_of_json_exn (member "b_lat_deg" c))
        ~lng:(float_of_json_exn (member "b_lng_deg" c))
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
    let a = latlng_of_json (member "a" c) in
    let b = latlng_of_json (member "b" c) in
    let expected = bool_of_json_exn (member "approx_equal" c) in
    check_bool name ~expected ~actual:(S2.S2_latlng.approx_equal a b))
;;

let test_e_constructors fixture () =
  let cases = to_list (member "e_constructors" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let expected_lat = float_of_json_exn (member "lat" c) in
    let expected_lng = float_of_json_exn (member "lng" c) in
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
      ~expected:(Float_u.of_float expected_lat)
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat result));
    check_float_u_exact
      (op ^ " lng")
      ~expected:(Float_u.of_float expected_lng)
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng result)))
;;

let test_to_point fixture () =
  let cases = to_list (member "to_point" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let ll =
      of_degrees
        ~lat:(float_of_json_exn (member "lat_deg" c))
        ~lng:(float_of_json_exn (member "lng_deg" c))
    in
    let expected = r3_vector_of_json (member "point" c) in
    let result = S2.S2_latlng.to_point ll in
    check_r3_vector (name ^ " to_point") ~expected ~actual:result)
;;

let () =
  let fixture = load_fixture "s2latlng.json" in
  Alcotest.run
    "S2_latlng"
    [ ( "constructors"
      , [ Alcotest.test_case "Constructors" `Quick (test_constructors fixture) ] )
    ; "is_valid", [ Alcotest.test_case "IsValid" `Quick (test_is_valid fixture) ]
    ; "normalized", [ Alcotest.test_case "Normalized" `Quick (test_normalized fixture) ]
    ; "arithmetic", [ Alcotest.test_case "Arithmetic" `Quick (test_arithmetic fixture) ]
    ; "conversion", [ Alcotest.test_case "Conversion" `Quick (test_conversion fixture) ]
    ; ( "lat_lng_from_point"
      , [ Alcotest.test_case "LatLngFromPoint" `Quick (test_lat_lng_from_point fixture) ]
      )
    ; ( "negative_zeros"
      , [ Alcotest.test_case "NegativeZeros" `Quick (test_negative_zeros fixture) ] )
    ; "distance", [ Alcotest.test_case "Distance" `Quick (test_distance fixture) ]
    ; ( "approx_equal"
      , [ Alcotest.test_case "ApproxEquals" `Quick (test_approx_equal fixture) ] )
    ; ( "e_constructors"
      , [ Alcotest.test_case "E_Constructors" `Quick (test_e_constructors fixture) ] )
    ; "to_point", [ Alcotest.test_case "ToPoint" `Quick (test_to_point fixture) ]
    ; ( "quickcheck"
      , [ Alcotest.test_case "add_commutative" `Quick quickcheck_add_commutative
        ; Alcotest.test_case "point_roundtrip" `Quick quickcheck_point_roundtrip
        ; Alcotest.test_case "normalized_is_valid" `Quick quickcheck_normalized_is_valid
        ; Alcotest.test_case
            "normalized_idempotent"
            `Quick
            quickcheck_normalized_idempotent
        ; Alcotest.test_case "distance_nonneg" `Quick quickcheck_distance_nonneg
        ; Alcotest.test_case "distance_symmetric" `Quick quickcheck_distance_symmetric
        ] )
    ]
;;
