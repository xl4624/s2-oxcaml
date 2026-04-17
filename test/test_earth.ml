(* Go test parity: geo/earth/earth_test.go
   Golden data from test/gen/earth.cc (computed via C++ S2Earth, which uses the
   same 6371010 m mean radius as Go's earth.Radius).

   Covered:
   -  TestAngleFromLength                                    - angle_length_roundtrip
   -  TestLengthFromAngle                                    - angle_length_roundtrip
   -  TestLengthFromPoints                                   - length_from_points
   -  TestLengthFromLatLngs                                  - length_from_latlngs
   -  TestAreaFromSteradians                                 - steradian_area_roundtrip
   -  TestSteradiansFromArea                                 - steradian_area_roundtrip
   -  TestInitialBearingFromLatLngs                          - initial_bearing
   -  TestInitialBearingFromLatLngsUndefinedResultDoesNotCrash
                                                              - initial_bearing_degenerate
   -  Constants (Radius, LowestAltitude, HighestAltitude)    - constants *)

open Core
open Test_helpers
open Alcotest

let fixture = load_fixture "earth.json"

(* -- Constants ----------------------------------------------------------- *)

let test_constants () =
  let c = member "constants" fixture in
  check_float_u_exact
    "radius_meters"
    ~expected:(float_u_of_json_exn (member "radius_meters" c))
    ~actual:S2.Earth.radius_meters;
  check_float_u_exact
    "radius_km"
    ~expected:(float_u_of_json_exn (member "radius_km" c))
    ~actual:S2.Earth.radius_km;
  check_float_u_exact
    "lowest_altitude_meters"
    ~expected:(float_u_of_json_exn (member "lowest_altitude_meters" c))
    ~actual:S2.Earth.lowest_altitude_meters;
  check_float_u_exact
    "highest_altitude_meters"
    ~expected:(float_u_of_json_exn (member "highest_altitude_meters" c))
    ~actual:S2.Earth.highest_altitude_meters
;;

(* -- Angle/length round trip --------------------------------------------- *)

let test_angle_length_roundtrip () =
  let cases = to_list (member "angle_length_roundtrip" fixture) in
  List.iter cases ~f:(fun c ->
    let degrees = float_u_of_json_exn (member "degrees" c) in
    let meters = float_u_of_json_exn (member "meters" c) in
    let expected_radians = float_u_of_json_exn (member "angle_from_length_radians" c) in
    let expected_meters = float_u_of_json_exn (member "length_from_angle_meters" c) in
    let actual_angle = S2.Earth.angle_from_length meters in
    check_float_u
      (sprintf "angle_from_length(%g)" (Float_u.to_float meters))
      ~expected:expected_radians
      ~actual:(S2.S1_angle.radians actual_angle);
    let input_angle = S2.S1_angle.of_degrees degrees in
    let actual_meters = S2.Earth.length_from_angle input_angle in
    check_float_u
      (sprintf "length_from_angle(%g deg)" (Float_u.to_float degrees))
      ~expected:expected_meters
      ~actual:actual_meters)
;;

(* -- Distance from points / latlngs -------------------------------------- *)

let point_of_list j =
  match to_list j with
  | [ x; y; z ] ->
    S2.S2_point.of_coords
      ~x:(float_u_of_json_exn x)
      ~y:(float_u_of_json_exn y)
      ~z:(float_u_of_json_exn z)
  | _ ->
    (match failwith "expected [x, y, z]" with
     | (_ : Nothing.t) -> .)
;;

let test_length_from_points () =
  let cases = to_list (member "length_from_points" fixture) in
  List.iter cases ~f:(fun c ->
    let a = point_of_list (member "a" c) in
    let b = point_of_list (member "b" c) in
    let expected = float_u_of_json_exn (member "length_meters" c) in
    let actual = S2.Earth.length_from_points a b in
    (* C++ S2Point::Angle uses a numerically stable formula that may
       differ by a couple of ulps from a single atan2-based Go
       [s2.Point.Distance]. Allow a small relative tolerance in meters. *)
    check_float_u ~eps:1e-6 "length_from_points" ~expected ~actual)
;;

let test_length_from_latlngs () =
  let cases = to_list (member "length_from_latlngs" fixture) in
  List.iter cases ~f:(fun c ->
    let a =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "lat1_degrees" c))
        ~lng:(float_u_of_json_exn (member "lng1_degrees" c))
    in
    let b =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "lat2_degrees" c))
        ~lng:(float_u_of_json_exn (member "lng2_degrees" c))
    in
    let expected = float_u_of_json_exn (member "length_meters" c) in
    let actual = S2.Earth.length_from_latlngs a b in
    check_float_u ~eps:1e-6 "length_from_latlngs" ~expected ~actual)
;;

(* -- Steradians/area round trip ------------------------------------------ *)

let test_steradian_area_roundtrip () =
  let cases = to_list (member "steradian_area_roundtrip" fixture) in
  List.iter cases ~f:(fun c ->
    let steradians = float_u_of_json_exn (member "steradians" c) in
    let square_meters = float_u_of_json_exn (member "square_meters" c) in
    let actual_area = S2.Earth.area_from_steradians steradians in
    check_float_u
      ~eps:1e-6
      (sprintf "area_from_steradians(%g)" (Float_u.to_float steradians))
      ~expected:square_meters
      ~actual:actual_area;
    let actual_steradians = S2.Earth.steradians_from_area square_meters in
    check_float_u
      ~eps:1e-14
      (sprintf "steradians_from_area(%g)" (Float_u.to_float square_meters))
      ~expected:steradians
      ~actual:actual_steradians)
;;

(* -- Initial bearing ----------------------------------------------------- *)

let test_initial_bearing () =
  let cases = to_list (member "initial_bearing" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "a_lat_degrees" c))
        ~lng:(float_u_of_json_exn (member "a_lng_degrees" c))
    in
    let b =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "b_lat_degrees" c))
        ~lng:(float_u_of_json_exn (member "b_lng_degrees" c))
    in
    let expected_radians = float_u_of_json_exn (member "bearing_radians" c) in
    let bearing = S2.Earth.initial_bearing_from_latlngs a b in
    check_float_u
      ~eps:1e-12
      (sprintf "initial_bearing %s" name)
      ~expected:expected_radians
      ~actual:(S2.S1_angle.radians bearing))
;;

let test_initial_bearing_degenerate () =
  let cases = to_list (member "initial_bearing_degenerate" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "a_lat_degrees" c))
        ~lng:(float_u_of_json_exn (member "a_lng_degrees" c))
    in
    let b =
      S2.S2_latlng.of_degrees
        ~lat:(float_u_of_json_exn (member "b_lat_degrees" c))
        ~lng:(float_u_of_json_exn (member "b_lng_degrees" c))
    in
    let bearing = S2.Earth.initial_bearing_from_latlngs a b in
    let rad = Float_u.to_float (S2.S1_angle.radians bearing) in
    if Float.is_nan rad || Float.is_inf rad
    then Alcotest.fail (sprintf "initial_bearing %s produced non-finite value" name))
;;

let () =
  run
    "earth"
    [ "constants", [ test_case "radius_and_altitudes" `Quick test_constants ]
    ; ( "conversions"
      , [ test_case "angle_length_roundtrip" `Quick test_angle_length_roundtrip
        ; test_case "length_from_points" `Quick test_length_from_points
        ; test_case "length_from_latlngs" `Quick test_length_from_latlngs
        ; test_case "steradian_area_roundtrip" `Quick test_steradian_area_roundtrip
        ] )
    ; ( "bearing"
      , [ test_case "initial_bearing" `Quick test_initial_bearing
        ; test_case "initial_bearing_degenerate" `Quick test_initial_bearing_degenerate
        ] )
    ]
;;
