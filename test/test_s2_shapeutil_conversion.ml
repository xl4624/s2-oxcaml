(* C++ test parity: s2geometry/src/s2/s2shapeutil_conversion_test.cc. Golden data from
   test/gen/s2shapeutil_conversion.cc.

   Covered:
   - TEST(S2ShapeConversionUtilTest, PointVectorShapeToPoints)
   - TEST(S2ShapeConversionUtilTest, LineToS2Polyline)
   - TEST(S2ShapeConversionUtilTest, ClosedLineToS2Polyline)
   - TEST(S2ShapeConversionUtilTest, PolygonWithHoleToS2Polygon)
   - TEST(S2ShapeConversionUtilTest, MultiPolygonToS2Polygon)
   - TEST(S2ShapeConversionUtilTest, TwoHolesToS2Polygon)
   - TEST(S2ShapeConversionUtilTest, FullPolygonToS2Polygon) *)

open Core
open Alcotest
open Test_helpers
module C = S2.S2_shapeutil_conversion

let fixture = lazy (load_fixture "s2shapeutil_conversion.json")
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

let loops_of_json j =
  let loops = to_list j in
  let n = List.length loops in
  let out = Array.create ~len:n [||] in
  List.iteri loops ~f:(fun i loop_j -> out.(i) <- points_of_json loop_j);
  out
;;

let check_points name expected actual =
  check int (sprintf "%s: count" name) (Array.length expected) (Array.length actual);
  for i = 0 to Array.length expected - 1 do
    check
      bool
      (sprintf "%s: vertex[%d]" name i)
      true
      (S2.S2_point.equal expected.(i) actual.(i))
  done
;;

let test_points name case =
  let input = points_of_json (member "input_points" case) in
  let expected = points_of_json (member "expected_points" case) in
  let shape =
    S2.S2_point_vector_shape.to_shape (S2.S2_point_vector_shape.of_points input)
  in
  let actual = C.shape_to_points shape in
  check_points name expected actual
;;

let test_polyline name case =
  let input = points_of_json (member "input_vertices" case) in
  let expected = points_of_json (member "expected_vertices" case) in
  let shape = S2.S2_lax_polyline.to_shape (S2.S2_lax_polyline.of_vertices input) in
  let polyline = C.shape_to_polyline shape in
  check
    int
    (sprintf "%s: num_vertices" name)
    (Array.length expected)
    (S2.S2_polyline.num_vertices polyline);
  for i = 0 to Array.length expected - 1 do
    check
      bool
      (sprintf "%s: vertex[%d]" name i)
      true
      (S2.S2_point.equal expected.(i) (S2.S2_polyline.vertex polyline i))
  done
;;

let test_polygon name case =
  let input_loops = loops_of_json (member "input_loops" case) in
  let lax = S2.S2_lax_polygon.of_loops input_loops in
  let shape = S2.S2_lax_polygon.to_shape lax in
  let polygon = C.shape_to_polygon shape in
  let expected_num_loops = int_of_json_exn (member "expected_num_loops" case) in
  let expected_num_vertices = int_of_json_exn (member "expected_num_vertices" case) in
  let expected_is_full = bool_of_json_exn (member "expected_is_full" case) in
  check
    int
    (sprintf "%s: num_loops" name)
    expected_num_loops
    (S2.S2_polygon.num_loops polygon);
  check
    int
    (sprintf "%s: num_vertices" name)
    expected_num_vertices
    (S2.S2_polygon.num_vertices polygon);
  check bool (sprintf "%s: is_full" name) expected_is_full (S2.S2_polygon.is_full polygon);
  let expected_oriented = to_list (member "expected_oriented_loops" case) in
  List.iteri expected_oriented ~f:(fun i loop_j ->
    let expected_pts = points_of_json loop_j in
    let loop = S2.S2_polygon.loop polygon i in
    check
      int
      (sprintf "%s: loop[%d] num_vertices" name i)
      (Array.length expected_pts)
      (S2.S2_loop.num_vertices loop);
    for j = 0 to Array.length expected_pts - 1 do
      check
        bool
        (sprintf "%s: loop[%d] oriented_vertex[%d]" name i j)
        true
        (S2.S2_point.equal expected_pts.(j) (S2.S2_loop.oriented_vertex loop j))
    done)
;;

let run_case case () =
  let name = string_of_json_exn (member "name" case) in
  let kind = string_of_json_exn (member "kind" case) in
  match kind with
  | "points" -> test_points name case
  | "polyline" -> test_polyline name case
  | "polygon" -> test_polygon name case
  | other ->
    failwithf "test_s2_shapeutil_conversion: unknown kind %S in case %S" other name ()
;;

let () =
  let cases = to_list (member "cases" (Lazy.force fixture)) in
  let tests =
    List.map cases ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      Alcotest.test_case name `Quick (run_case case))
  in
  Alcotest.run "S2_shapeutil_conversion" [ "fixtures", tests ]
;;
