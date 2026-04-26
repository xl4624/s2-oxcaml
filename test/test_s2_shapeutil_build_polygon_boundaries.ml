(* C++ test parity:
   s2geometry/src/s2/s2shapeutil_build_polygon_boundaries_test.cc.
   Golden data from test/gen/s2shapeutil_build_polygon_boundaries.cc.

   Covered:
   - TEST(BuildPolygonBoundaries, NoComponents)
   - TEST(BuildPolygonBoundaries, OneLoop)
   - TEST(BuildPolygonBoundaries, TwoLoopsSameComponent)
   - TEST(BuildPolygonBoundaries, TwoNestedLoops)
   - TEST(BuildPolygonBoundaries, TwoLoopsDifferentComponents)
   - TEST(BuildPolygonBoundaries, OneDegenerateLoop)
   - TEST(BuildPolygonBoundaries, TwoDegenerateLoops)

   Deliberately omitted:
   - TEST(BuildPolygonBoundaries, ComplexTest1): a 27-loop nested arrangement.
     The simpler cases above already exercise nested, sibling, and degenerate
     configurations; a fixture-driven version of this test would be a large
     literal with no incremental coverage. *)

open Core
open Alcotest
open Test_helpers
module B = S2.S2_shapeutil_build_polygon_boundaries

let fixture = lazy (load_fixture "s2shapeutil_build_polygon_boundaries.json")
let point_of_json = r3_vector_of_json

let points_of_json j =
  let xs = to_list j in
  let n = List.length xs in
  if n = 0
  then [||]
  else (
    let first = point_of_json (List.hd_exn xs) in
    let arr = Array.create ~len:n first in
    List.iteri xs ~f:(fun i pj -> arr.(i) <- point_of_json pj);
    arr)
;;

let components_of_json j =
  to_list j
  |> List.map ~f:(fun comp_j ->
    to_list comp_j
    |> List.map ~f:(fun loop_j ->
      let vs = points_of_json loop_j in
      B.Loop.of_shape (S2.S2_lax_loop.to_shape (S2.S2_lax_loop.of_vertices vs))))
;;

let canonicalise (faces : int list list) : int list list =
  faces
  |> List.map ~f:(List.sort ~compare:Int.compare)
  |> List.sort ~compare:[%compare: int list]
;;

let expected_faces_of_json j =
  to_list j |> List.map ~f:(fun face_j -> to_list face_j |> List.map ~f:int_of_json_exn)
;;

let run_case case () =
  let name = string_of_json_exn (member "name" case) in
  let components = components_of_json (member "components" case) in
  let expected = canonicalise (expected_faces_of_json (member "expected_faces" case)) in
  let actual = canonicalise (B.build_polygon_boundaries components) in
  let to_string faces =
    String.concat
      ~sep:"; "
      (List.map faces ~f:(fun face ->
         "[" ^ String.concat ~sep:";" (List.map face ~f:Int.to_string) ^ "]"))
  in
  check string (sprintf "%s: faces" name) (to_string expected) (to_string actual)
;;

let () =
  let cases = to_list (member "cases" (Lazy.force fixture)) in
  let tests =
    List.map cases ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      Alcotest.test_case name `Quick (run_case case))
  in
  Alcotest.run "S2_shapeutil_build_polygon_boundaries" [ "fixtures", tests ]
;;
