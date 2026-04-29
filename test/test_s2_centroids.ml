(* C++ test parity: s2geometry/src/s2/s2centroids_test.cc
   - TEST(PlanarCentroid, SemiEquator) - full parity
   - TEST(TriangleTrueCentroid, SmallTriangles) - partial (deterministic subset)
   - TEST(EdgeTrueCentroid, SemiEquator) - full parity
   - TEST(EdgeTrueCentroid, GreatCircles) - omitted (random, verified by property)

   Extra coverage:
   - planar_centroid_cases (unit_axes, same_point)
   - edge_true_centroid_cases (same_point, antipodal, short_edge, ninety_degrees)
   - triangle_true_centroid degenerate cases (same_point, collinear) *)

open Core
open Test_helpers
open Alcotest

let fixture = load_fixture "s2centroids.json"

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

let test_planar_centroid_semi_equator () =
  let c = member "planar_centroid_semi_equator" fixture in
  let a = point_of_json (member "a" c) in
  let b = point_of_json (member "b" c) in
  let cv = point_of_json (member "c" c) in
  let expected = point_of_json (member "centroid" c) in
  let expected_normalized = point_of_json (member "centroid_normalized" c) in
  let expected_norm = float_u_of_json_exn (member "centroid_norm" c) in
  let actual = S2.S2_centroids.planar_centroid a b cv in
  check_point "centroid" ~expected ~actual;
  let actual_normalized = S2.R3_vector.normalize actual in
  check_point "normalized" ~expected:expected_normalized ~actual:actual_normalized;
  check_float_u "norm" ~expected:expected_norm ~actual:(S2.R3_vector.norm actual)
;;

let test_planar_centroid_cases () =
  let cases = to_list (member "planar_centroid_cases" fixture) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let cv = point_of_json (member "c" c) in
    let expected = point_of_json (member "centroid" c) in
    let actual = S2.S2_centroids.planar_centroid a b cv in
    check_point label ~expected ~actual)
;;

let test_triangle_true_centroid () =
  let cases = to_list (member "triangle_true_centroid" fixture) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let cv = point_of_json (member "c" c) in
    let expected = point_of_json (member "centroid" c) in
    let actual = S2.S2_centroids.true_centroid a b cv in
    check_point ~eps:1e-14 label ~expected ~actual)
;;

let test_edge_true_centroid_semi_equator () =
  let c = member "edge_true_centroid_semi_equator" fixture in
  let a = point_of_json (member "a" c) in
  let b = point_of_json (member "b" c) in
  let cv = point_of_json (member "c" c) in
  let expected_ab = point_of_json (member "centroid_ab" c) in
  let expected_bc = point_of_json (member "centroid_bc" c) in
  let expected_sum = point_of_json (member "centroid_sum" c) in
  let expected_normalized = point_of_json (member "centroid_sum_normalized" c) in
  let expected_norm = float_u_of_json_exn (member "centroid_sum_norm" c) in
  let actual_ab = S2.S2_centroids.edge_true_centroid a b in
  let actual_bc = S2.S2_centroids.edge_true_centroid b cv in
  check_point "centroid_ab" ~expected:expected_ab ~actual:actual_ab;
  check_point "centroid_bc" ~expected:expected_bc ~actual:actual_bc;
  let actual_sum = S2.R3_vector.add actual_ab actual_bc in
  check_point "centroid_sum" ~expected:expected_sum ~actual:actual_sum;
  let actual_normalized = S2.R3_vector.normalize actual_sum in
  check_point "normalized" ~expected:expected_normalized ~actual:actual_normalized;
  check_float_u "norm" ~expected:expected_norm ~actual:(S2.R3_vector.norm actual_sum)
;;

let test_edge_true_centroid_cases () =
  let cases = to_list (member "edge_true_centroid_cases" fixture) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let expected = point_of_json (member "centroid" c) in
    let actual = S2.S2_centroids.edge_true_centroid a b in
    check_point label ~expected ~actual)
;;

let () =
  run
    "s2_centroids"
    [ ( "planar_centroid"
      , [ test_case "semi_equator" `Quick test_planar_centroid_semi_equator
        ; test_case "cases" `Quick test_planar_centroid_cases
        ] )
    ; "triangle_true_centroid", [ test_case "cases" `Quick test_triangle_true_centroid ]
    ; ( "edge_true_centroid"
      , [ test_case "semi_equator" `Quick test_edge_true_centroid_semi_equator
        ; test_case "cases" `Quick test_edge_true_centroid_cases
        ] )
    ]
;;
