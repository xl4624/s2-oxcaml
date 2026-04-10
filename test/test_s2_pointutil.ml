(* C++ test parity: s2geometry/src/s2/s2pointutil_test.cc
   -  TEST(S2, Frames)      - full parity (plus extra off-axis cases)
   -  TEST(S2, Rotate)      - deterministic subset (random property test in C++
                              omitted; covered via property-style checks here)
   -  TEST(S2, OriginTest)  - partial: we check Origin() coordinates, unit
                              length, and distance from the north pole.
                              The large-cell collinearity check is omitted
                              because it depends on s2pred::TriageSign and
                              S2Cell which are not yet ported.

   Extra coverage:
   -  is_unit_length on a dozen representative points
   -  approx_equals with explicit max_error_radians values
   -  ortho negation identity: ortho(-a) = -ortho(a)
   -  ref_dir (aliased to ortho in upstream C++) *)

open Core
open Test_helpers
open Alcotest

let fixture = load_fixture "s2pointutil.json"

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

let check_point ?(eps = 1e-15) msg ~expected ~actual =
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

let test_origin () =
  let c = member "origin" fixture in
  let expected = point_of_json (member "origin" c) in
  let actual = S2.S2_pointutil.origin () in
  check_point "origin" ~expected ~actual;
  let expected_unit = bool_of_json_exn (member "is_unit_length" c) in
  Alcotest.(check bool)
    "origin is_unit_length"
    expected_unit
    (S2.S2_pointutil.is_unit_length actual);
  let expected_norm = float_u_of_json_exn (member "norm" c) in
  check_float_u "origin norm" ~expected:expected_norm ~actual:(S2.R3_vector.norm actual);
  let expected_norm2 = float_u_of_json_exn (member "norm2" c) in
  check_float_u
    "origin norm2"
    ~expected:expected_norm2
    ~actual:(S2.R3_vector.norm2 actual);
  let expected_pole_dist =
    float_u_of_json_exn (member "distance_from_north_pole_radians" c)
  in
  let actual_pole_dist = Float_u.acos (S2.R3_vector.z actual) in
  check_float_u
    "origin distance from north pole"
    ~expected:expected_pole_dist
    ~actual:actual_pole_dist
;;

let test_is_unit_length () =
  let cases = to_list (member "is_unit_length" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let p = point_of_json (member "p" c) in
    let expected = bool_of_json_exn (member "expected" c) in
    let actual = S2.S2_pointutil.is_unit_length p in
    Alcotest.(check bool) ("is_unit_length " ^ name) expected actual)
;;

let test_approx_equals () =
  let cases = to_list (member "approx_equals" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let max_error_radians =
      Float_u.to_float (float_u_of_json_exn (member "max_error_radians" c))
    in
    let expected = bool_of_json_exn (member "expected" c) in
    let actual = S2.S2_pointutil.approx_equals ~max_error_radians a b in
    Alcotest.(check bool) ("approx_equals " ^ name) expected actual)
;;

let test_ortho () =
  let cases = to_list (member "ortho" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let expected = point_of_json (member "ortho" c) in
    let expected_neg = point_of_json (member "neg_a_ortho" c) in
    let actual = S2.S2_pointutil.ortho a in
    check_point ("ortho " ^ name) ~expected ~actual;
    let actual_neg = S2.S2_pointutil.ortho (S2.R3_vector.neg a) in
    check_point ("ortho(-a) " ^ name) ~expected:expected_neg ~actual:actual_neg;
    (* ortho(-a) = -ortho(a) identity. *)
    let neg_expected = S2.R3_vector.neg expected in
    check_point ("ortho negation " ^ name) ~expected:neg_expected ~actual:actual_neg;
    let expected_dot = float_u_of_json_exn (member "dot" c) in
    let actual_dot = S2.R3_vector.dot a actual in
    check_float_u ("ortho dot " ^ name) ~expected:expected_dot ~actual:actual_dot;
    Alcotest.(check bool)
      ("ortho is_unit " ^ name)
      (bool_of_json_exn (member "is_unit" c))
      (S2.S2_pointutil.is_unit_length actual))
;;

let test_ref_dir () =
  let cases = to_list (member "ref_dir" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let expected = point_of_json (member "ref_dir" c) in
    let actual = S2.S2_pointutil.ref_dir a in
    check_point ("ref_dir " ^ name) ~expected ~actual;
    Alcotest.(check bool)
      ("ref_dir is_unit " ^ name)
      (bool_of_json_exn (member "is_unit" c))
      (S2.S2_pointutil.is_unit_length actual))
;;

let test_rotate () =
  let cases = to_list (member "rotate" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let p = point_of_json (member "p" c) in
    let axis = point_of_json (member "axis" c) in
    let angle_radians = float_u_of_json_exn (member "angle_radians" c) in
    let expected = point_of_json (member "result" c) in
    let angle = S2.S1_angle.of_radians angle_radians in
    let actual = S2.S2_pointutil.rotate p ~axis ~angle in
    check_point ("rotate " ^ name) ~expected ~actual;
    Alcotest.(check bool)
      ("rotate is_unit " ^ name)
      (bool_of_json_exn (member "is_unit" c))
      (S2.S2_pointutil.is_unit_length actual))
;;

let test_frames () =
  let cases = to_list (member "frames" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let z = point_of_json (member "z" c) in
    let expected_col0 = point_of_json (member "col0" c) in
    let expected_col1 = point_of_json (member "col1" c) in
    let expected_col2 = point_of_json (member "col2" c) in
    let frame = S2.S2_pointutil.get_frame z in
    let #{ S2.S2_point.col0; col1; col2 } = frame in
    check_point ("col0 " ^ name) ~expected:expected_col0 ~actual:col0;
    check_point ("col1 " ^ name) ~expected:expected_col1 ~actual:col1;
    check_point ("col2 " ^ name) ~expected:expected_col2 ~actual:col2;
    (* col2 must equal z *)
    check_point ("col2 = z " ^ name) ~expected:z ~actual:col2;
    (* to_frame maps columns to the standard basis *)
    let tf0 = S2.S2_pointutil.to_frame frame col0 in
    let tf1 = S2.S2_pointutil.to_frame frame col1 in
    let tf2 = S2.S2_pointutil.to_frame frame col2 in
    check_point
      ("to_frame col0 " ^ name)
      ~expected:(point_of_json (member "to_frame_col0" c))
      ~actual:tf0;
    check_point
      ("to_frame col1 " ^ name)
      ~expected:(point_of_json (member "to_frame_col1" c))
      ~actual:tf1;
    check_point
      ("to_frame col2 " ^ name)
      ~expected:(point_of_json (member "to_frame_col2" c))
      ~actual:tf2;
    (* from_frame maps the standard basis to the columns *)
    let e0 = S2.R3_vector.create ~x:#1.0 ~y:#0.0 ~z:#0.0 in
    let e1 = S2.R3_vector.create ~x:#0.0 ~y:#1.0 ~z:#0.0 in
    let e2 = S2.R3_vector.create ~x:#0.0 ~y:#0.0 ~z:#1.0 in
    let ff0 = S2.S2_pointutil.from_frame frame e0 in
    let ff1 = S2.S2_pointutil.from_frame frame e1 in
    let ff2 = S2.S2_pointutil.from_frame frame e2 in
    check_point
      ("from_frame e0 " ^ name)
      ~expected:(point_of_json (member "from_frame_e0" c))
      ~actual:ff0;
    check_point
      ("from_frame e1 " ^ name)
      ~expected:(point_of_json (member "from_frame_e1" c))
      ~actual:ff1;
    check_point
      ("from_frame e2 " ^ name)
      ~expected:(point_of_json (member "from_frame_e2" c))
      ~actual:ff2;
    (* Round-trip a generic point. *)
    let q = point_of_json (member "q" c) in
    let q_in_frame = S2.S2_pointutil.to_frame frame q in
    let q_back = S2.S2_pointutil.from_frame frame q_in_frame in
    check_point
      ("q_in_frame " ^ name)
      ~expected:(point_of_json (member "q_in_frame" c))
      ~actual:q_in_frame;
    check_point
      ("q_back " ^ name)
      ~expected:(point_of_json (member "q_back" c))
      ~actual:q_back;
    check_point ~eps:1e-14 ("q round-trip " ^ name) ~expected:q ~actual:q_back)
;;

let () =
  run
    "s2_pointutil"
    [ "origin", [ test_case "origin" `Quick test_origin ]
    ; "is_unit_length", [ test_case "cases" `Quick test_is_unit_length ]
    ; "approx_equals", [ test_case "cases" `Quick test_approx_equals ]
    ; "ortho", [ test_case "cases" `Quick test_ortho ]
    ; "ref_dir", [ test_case "cases" `Quick test_ref_dir ]
    ; "rotate", [ test_case "cases" `Quick test_rotate ]
    ; "frames", [ test_case "cases" `Quick test_frames ]
    ]
;;
