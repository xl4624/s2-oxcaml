(* C++ test parity: s2geometry/src/s2/s2projections_test.cc
   - TEST(PlateCarreeProjection, Interpolate)
   - TEST(PlateCarreeProjection, ProjectUnproject)
   - TEST(MercatorProjection, ProjectUnproject)
   - Additional WrapDestination, wrap_distance, and alternate-scale cases. *)

open Core
open Test_helpers
open Alcotest

let fixture = load_fixture "s2projections.json"

(* The generator encodes +/-inf as the tagged strings "inf" / "-inf" so we can round-trip
   polar Mercator samples. *)
let float_u_or_inf = function
  | `String "inf" -> Float_u.infinity
  | `String "-inf" -> Float_u.neg_infinity
  | j -> float_u_of_json_exn j
;;

let r2_point_or_inf j =
  match to_list j with
  | [ x; y ] -> S2.R2_point.create ~x:(float_u_or_inf x) ~y:(float_u_or_inf y)
  | _ ->
    (match failwith "expected [x, y]" with
     | (_ : Nothing.t) -> .)
;;

let s2_point_or_inf j =
  match to_list j with
  | [ x; y; z ] ->
    S2.R3_vector.create ~x:(float_u_or_inf x) ~y:(float_u_or_inf y) ~z:(float_u_or_inf z)
  | _ ->
    (match failwith "expected [x, y, z]" with
     | (_ : Nothing.t) -> .)
;;

let latlng_or_inf j =
  match to_list j with
  | [ lat; lng ] ->
    S2.S2_latlng.of_radians ~lat:(float_u_or_inf lat) ~lng:(float_u_or_inf lng)
  | _ ->
    (match failwith "expected [lat, lng]" with
     | (_ : Nothing.t) -> .)
;;

let check_r2 ?(eps = 1e-13) msg ~expected ~actual =
  check_float_u
    ~eps
    (msg ^ " x")
    ~expected:(S2.R2_point.x expected)
    ~actual:(S2.R2_point.x actual);
  check_float_u
    ~eps
    (msg ^ " y")
    ~expected:(S2.R2_point.y expected)
    ~actual:(S2.R2_point.y actual)
;;

let check_r3 ?(eps = 1e-13) msg ~expected ~actual =
  check_float_u
    ~eps
    (msg ^ " x")
    ~expected:(S2.R3_vector.x expected)
    ~actual:(S2.R3_vector.x actual);
  check_float_u
    ~eps
    (msg ^ " y")
    ~expected:(S2.R3_vector.y expected)
    ~actual:(S2.R3_vector.y actual);
  check_float_u
    ~eps
    (msg ^ " z")
    ~expected:(S2.R3_vector.z expected)
    ~actual:(S2.R3_vector.z actual)
;;

let check_latlng ?(eps = 1e-13) msg ~expected ~actual =
  check_float_u
    ~eps
    (msg ^ " lat")
    ~expected:(S2.S1_angle.radians (S2.S2_latlng.lat expected))
    ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat actual));
  check_float_u
    ~eps
    (msg ^ " lng")
    ~expected:(S2.S1_angle.radians (S2.S2_latlng.lng expected))
    ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng actual))
;;

let run_project_cases proj_name proj key =
  let cases = to_list (member key fixture) in
  List.iteri cases ~f:(fun idx c ->
    let label = sprintf "%s[%d]" proj_name idx in
    let px = r2_point_or_inf (member "px" c) in
    let x = s2_point_or_inf (member "x" c) in
    let expected_project = r2_point_or_inf (member "project" c) in
    let expected_unproject = s2_point_or_inf (member "unproject" c) in
    let expected_from_latlng = r2_point_or_inf (member "from_latlng" c) in
    let expected_to_latlng = latlng_or_inf (member "to_latlng" c) in
    check_r2
      (label ^ " project")
      ~expected:expected_project
      ~actual:(S2.S2_projections.project proj x);
    check_r3
      (label ^ " unproject")
      ~expected:expected_unproject
      ~actual:(S2.S2_projections.unproject proj px);
    check_r2
      (label ^ " from_latlng")
      ~expected:expected_from_latlng
      ~actual:(S2.S2_projections.from_latlng proj (S2.S2_latlng.of_point x));
    check_latlng
      (label ^ " to_latlng")
      ~expected:expected_to_latlng
      ~actual:(S2.S2_projections.to_latlng proj px))
;;

let test_plate_carree_project_unproject () =
  let proj = S2.S2_projections.plate_carree ~x_scale:#180.0 in
  run_project_cases "plate_carree" proj "plate_carree_project_unproject"
;;

let test_mercator_project_unproject () =
  let proj = S2.S2_projections.mercator ~max_x:#180.0 in
  run_project_cases "mercator" proj "mercator_project_unproject"
;;

let test_plate_carree_interpolate () =
  let proj = S2.S2_projections.plate_carree ~x_scale:#180.0 in
  let cases = to_list (member "plate_carree_interpolate" fixture) in
  List.iteri cases ~f:(fun idx c ->
    let label = sprintf "interpolate[%d]" idx in
    let f = float_u_of_json_exn (member "f" c) in
    let a = r2_point_or_inf (member "a" c) in
    let b = r2_point_or_inf (member "b" c) in
    let expected = r2_point_or_inf (member "result" c) in
    check_r2 label ~expected ~actual:(S2.S2_projections.interpolate proj ~f a b))
;;

let run_wrap_cases label proj key =
  let cases = to_list (member key fixture) in
  List.iteri cases ~f:(fun idx c ->
    let a = r2_point_or_inf (member "a" c) in
    let b = r2_point_or_inf (member "b" c) in
    let expected = r2_point_or_inf (member "result" c) in
    check_r2
      (sprintf "%s[%d]" label idx)
      ~expected
      ~actual:(S2.S2_projections.wrap_destination proj ~a ~b))
;;

let test_plate_carree_wrap_destination () =
  let proj = S2.S2_projections.plate_carree ~x_scale:#180.0 in
  run_wrap_cases "plate_wrap" proj "plate_carree_wrap_destination"
;;

let test_mercator_wrap_destination () =
  let proj = S2.S2_projections.mercator ~max_x:#180.0 in
  run_wrap_cases "merc_wrap" proj "mercator_wrap_destination"
;;

let test_wrap_distance () =
  let plate = S2.S2_projections.plate_carree ~x_scale:#180.0 in
  let merc = S2.S2_projections.mercator ~max_x:#180.0 in
  let obj = member "wrap_distance" fixture in
  let expected_plate = r2_point_or_inf (member "plate_carree" obj) in
  let expected_merc = r2_point_or_inf (member "mercator" obj) in
  check_r2
    "plate wrap_distance"
    ~expected:expected_plate
    ~actual:(S2.S2_projections.wrap_distance plate);
  check_r2
    "merc wrap_distance"
    ~expected:expected_merc
    ~actual:(S2.S2_projections.wrap_distance merc)
;;

let test_alternate_scales () =
  let cases = to_list (member "alternate_scales" fixture) in
  List.iter cases ~f:(fun c ->
    let kind_s = string_of_json_exn (member "kind" c) in
    let x_scale = float_u_of_json_exn (member "x_scale" c) in
    let input = s2_point_or_inf (member "input" c) in
    let expected_proj = r2_point_or_inf (member "projected" c) in
    let expected_from_ll = r2_point_or_inf (member "from_latlng" c) in
    let expected_wrap = r2_point_or_inf (member "wrap_distance" c) in
    let proj =
      match kind_s with
      | "plate_carree" -> S2.S2_projections.plate_carree ~x_scale
      | "mercator" -> S2.S2_projections.mercator ~max_x:x_scale
      | s ->
        (match failwith (sprintf "unknown kind %s" s) with
         | (_ : Nothing.t) -> .)
    in
    check_r2
      (kind_s ^ " projected")
      ~expected:expected_proj
      ~actual:(S2.S2_projections.project proj input);
    check_r2
      (kind_s ^ " from_latlng")
      ~expected:expected_from_ll
      ~actual:(S2.S2_projections.from_latlng proj (S2.S2_latlng.of_point input));
    check_r2
      (kind_s ^ " wrap_distance")
      ~expected:expected_wrap
      ~actual:(S2.S2_projections.wrap_distance proj))
;;

let () =
  run
    "s2_projections"
    [ ( "plate_carree"
      , [ test_case "project_unproject" `Quick test_plate_carree_project_unproject
        ; test_case "interpolate" `Quick test_plate_carree_interpolate
        ; test_case "wrap_destination" `Quick test_plate_carree_wrap_destination
        ] )
    ; ( "mercator"
      , [ test_case "project_unproject" `Quick test_mercator_project_unproject
        ; test_case "wrap_destination" `Quick test_mercator_wrap_destination
        ] )
    ; "wrap_distance", [ test_case "values" `Quick test_wrap_distance ]
    ; "alternate_scales", [ test_case "values" `Quick test_alternate_scales ]
    ]
;;
