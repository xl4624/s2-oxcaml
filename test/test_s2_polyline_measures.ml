(* C++ test parity: s2geometry/src/s2/s2polyline_measures_test.cc
   -  TEST(GetLengthAndCentroid, GreatCircles) - substituted with a set of
      deterministic great-circle cases that exercise the same invariants
      (length = 2*pi, centroid norm ~ 0).

   Extra coverage:
   -  empty, single-vertex, and two-point degenerate polylines
   -  antipodal two-point polyline
   -  quarter, half, and full great-circle subdivisions
   -  a short-edge case
   -  a tilted great circle with 32 vertices *)

open Core
open Test_helpers
open Alcotest

let fixture = load_fixture "s2polyline_measures.json"

let polyline_of_json j =
  let verts = to_list j in
  let n = List.length verts in
  if n = 0
  then [||]
  else (
    let arr = Array.create ~len:n (List.hd_exn verts |> r3_vector_of_json) in
    List.iteri verts ~f:(fun i v -> arr.(i) <- r3_vector_of_json v);
    arr)
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

let test_fixture_cases () =
  let cases = to_list (member "polylines" fixture) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let polyline = polyline_of_json (member "polyline" c) in
    let expected_length = float_u_of_json_exn (member "length_radians" c) in
    let expected_centroid = r3_vector_of_json (member "centroid" c) in
    let expected_norm = float_u_of_json_exn (member "centroid_norm" c) in
    let actual_length = S2.S1_angle.radians (S2.S2_polyline_measures.length polyline) in
    (* Length accumulates roundoff proportional to the number of edges. Use
       a slightly looser tolerance for the 32-vertex tilted great circle. *)
    let eps = if String.is_prefix label ~prefix:"great_circle" then 1e-13 else 1e-15 in
    check_float_u ~eps (label ^ " length") ~expected:expected_length ~actual:actual_length;
    let actual_centroid = S2.S2_polyline_measures.centroid polyline in
    let centroid_eps =
      if String.is_prefix label ~prefix:"great_circle" then 1e-13 else 1e-15
    in
    check_point
      ~eps:centroid_eps
      (label ^ " centroid")
      ~expected:expected_centroid
      ~actual:actual_centroid;
    let actual_norm = S2.R3_vector.norm actual_centroid in
    check_float_u
      ~eps:centroid_eps
      (label ^ " centroid_norm")
      ~expected:expected_norm
      ~actual:actual_norm)
;;

let () =
  run
    "s2_polyline_measures"
    [ "fixtures", [ test_case "cases" `Quick test_fixture_cases ] ]
;;
