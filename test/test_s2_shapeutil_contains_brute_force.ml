(* C++ test parity:
   s2geometry/src/s2/s2shapeutil_contains_brute_force_test.cc.
   Golden data from test/gen/s2shapeutil_contains_brute_force.cc.

   Covered:
   -  TEST(ContainsBruteForce, NoInterior): a polyline never contains any
      point because its dimension is 1.
   -  TEST(ContainsBruteForce, ContainsReferencePoint): the shape's own
      reference point round-trips through [contains_brute_force].
   -  TEST(ContainsBruteForce, ConsistentWithS2Loop): on a 100-vertex regular
      loop, [contains_brute_force] agrees with the loop's own [contains] for
      every vertex. *)

open Core
open Alcotest
open Test_helpers

let fixture = lazy (load_fixture "s2shapeutil_contains_brute_force.json")
let point_of_json = r3_vector_of_json

let vertices_of_json j =
  let vjs = to_list j in
  let n = List.length vjs in
  let vs = Array.create ~len:n S2.S2_point.origin in
  List.iteri vjs ~f:(fun i vj -> vs.(i) <- point_of_json vj);
  vs
;;

let test_no_interior () =
  let j = member "no_interior" (Lazy.force fixture) in
  let vs = vertices_of_json (member "vertices" j) in
  let shape = S2.S2_lax_polyline.to_shape (S2.S2_lax_polyline.of_vertices vs) in
  let query = point_of_json (member "query" j) in
  let expected = bool_of_json_exn (member "expected" j) in
  check
    bool
    "polyline never contains its query point"
    expected
    (S2.S2_shapeutil_contains_brute_force.contains_brute_force shape ~point:query)
;;

let test_contains_reference_point () =
  let j = member "contains_reference_point" (Lazy.force fixture) in
  let loop = vertices_of_json (member "loop_vertices" j) in
  let polygon = S2.S2_lax_polygon.of_loops [| loop |] in
  let shape = S2.S2_lax_polygon.to_shape polygon in
  let cases = to_list (member "cases" j) in
  List.iteri cases ~f:(fun i case ->
    let query = point_of_json (member "query" case) in
    let expected = bool_of_json_exn (member "expected" case) in
    check
      bool
      (sprintf "case[%d] reference-point containment" i)
      expected
      (S2.S2_shapeutil_contains_brute_force.contains_brute_force shape ~point:query))
;;

let test_consistent_with_s2loop () =
  let j = member "consistent_with_s2loop" (Lazy.force fixture) in
  let vs = vertices_of_json (member "loop_vertices" j) in
  let loop = S2.S2_loop.of_vertices vs in
  let shape = S2.S2_loop.to_shape loop in
  let cases = to_list (member "cases" j) in
  List.iteri cases ~f:(fun i case ->
    let query = point_of_json (member "query" case) in
    let expected = bool_of_json_exn (member "expected" case) in
    check
      bool
      (sprintf "vertex[%d] brute force matches C++" i)
      expected
      (S2.S2_shapeutil_contains_brute_force.contains_brute_force shape ~point:query);
    check
      bool
      (sprintf "vertex[%d] brute force matches S2_loop.contains_point" i)
      (S2.S2_loop.contains_point loop query)
      (S2.S2_shapeutil_contains_brute_force.contains_brute_force shape ~point:query))
;;

let () =
  Alcotest.run
    "S2_shapeutil_contains_brute_force"
    [ ( "fixture"
      , [ test_case "NoInterior" `Quick test_no_interior
        ; test_case "ContainsReferencePoint" `Quick test_contains_reference_point
        ; test_case "ConsistentWithS2Loop" `Quick test_consistent_with_s2loop
        ] )
    ]
;;
