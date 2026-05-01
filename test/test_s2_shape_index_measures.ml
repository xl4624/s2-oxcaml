(* Golden data produced by test/gen/s2shape_index_measures.cc.

   Upstream C++ tests covered:
   - TEST(GetDimension, Empty) - via "empty"
   - TEST(GetDimension, Points) - via "single_point" and "empty_point_set"
   - TEST(GetDimension, PointsAndLines) - via "point_and_polyline" and
     "point_and_one_vertex_polyline"
   - TEST(GetDimension, PointsLinesAndPolygons) - via "point_polyline_triangle" and
     "empty_polygon"
   - TEST(GetNumPoints, Empty) - via "empty"
   - TEST(GetNumPoints, TwoPoints) - via "two_points"
   - TEST(GetNumPoints, LineAndPolygon) - via "line_and_polygon"
   - TEST(GetLength, Empty) - via "empty"
   - TEST(GetLength, TwoLines) - via "two_lines_with_other_shapes"
   - TEST(GetPerimeter, Empty) - via "empty"
   - TEST(GetPerimeter, DegeneratePolygon) - via "degenerate_polygon_perimeter"
   - TEST(GetArea, Empty) - via "empty"
   - TEST(GetArea, TwoFullPolygons) - via "two_full_polygons"
   - TEST(GetApproxArea, Empty) - via "empty"
   - TEST(GetApproxArea, TwoFullPolygons) - via "two_full_polygons"
   - TEST(GetCentroid, Empty) - via "empty"
   - TEST(GetCentroid, Points) - via "centroid_two_points"
   - TEST(GetCentroid, Polyline) - via "centroid_polyline_with_points"
   - TEST(GetCentroid, Polygon) - via "centroid_polygon_with_others" *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2shape_index_measures.json")
let get key = member key (Lazy.force fixture)
let s2_point_of_json j = r3_vector_of_json j

let points_of_json j =
  let xs = to_list j in
  let n = List.length xs in
  if n = 0
  then [||]
  else (
    let first = s2_point_of_json (List.hd_exn xs) in
    let arr = Array.create ~len:n first in
    List.iteri xs ~f:(fun i p -> arr.(i) <- s2_point_of_json p);
    arr)
;;

let loops_of_json j =
  let xs = to_list j in
  let n = List.length xs in
  if n = 0
  then [||]
  else (
    let arr = Array.create ~len:n [||] in
    List.iteri xs ~f:(fun i l -> arr.(i) <- points_of_json l);
    arr)
;;

let shape_of_json j =
  let kind = string_of_json_exn (member "kind" j) in
  match kind with
  | "points" ->
    let points = points_of_json (member "points" j) in
    S2.S2_point_vector_shape.to_shape (S2.S2_point_vector_shape.of_points points)
  | "polyline" ->
    let vertices = points_of_json (member "vertices" j) in
    S2.S2_lax_polyline.to_shape (S2.S2_lax_polyline.of_vertices vertices)
  | "polygon" ->
    let loops = loops_of_json (member "loops" j) in
    S2.S2_lax_polygon.to_shape (S2.S2_lax_polygon.of_loops loops)
  | other ->
    (match failwithf "unknown shape kind: %s" other () with
     | (_ : Nothing.t) -> .)
;;

let index_of_json j =
  let index = S2.S2_shape_index.create () in
  List.iter (to_list j) ~f:(fun shape_json ->
    let shape = shape_of_json shape_json in
    let (_ : int) = S2.S2_shape_index.add index shape in
    ());
  S2.S2_shape_index.build index;
  index
;;

let test_cases () =
  List.iter
    (to_list (get "cases"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let index = index_of_json (member "shapes" case) in
      let expected = member "expected" case in
      let exp_dim = int_of_json_exn (member "dimension" expected) in
      let exp_num_points = int_of_json_exn (member "num_points" expected) in
      let exp_length = float_u_of_json_exn (member "length_radians" expected) in
      let exp_perimeter = float_u_of_json_exn (member "perimeter_radians" expected) in
      let exp_area = float_u_of_json_exn (member "area" expected) in
      let exp_approx_area = float_u_of_json_exn (member "approx_area" expected) in
      let exp_centroid = s2_point_of_json (member "centroid" expected) in
      (Alcotest.check Alcotest.int)
        (name ^ " dimension")
        exp_dim
        (S2.S2_shape_index_measures.dimension index);
      (Alcotest.check Alcotest.int)
        (name ^ " num_points")
        exp_num_points
        (S2.S2_shape_index_measures.num_points index);
      check_float_u
        (name ^ " length")
        ~expected:exp_length
        ~actual:(S2.S1_angle.radians (S2.S2_shape_index_measures.length index));
      check_float_u
        (name ^ " perimeter")
        ~expected:exp_perimeter
        ~actual:(S2.S1_angle.radians (S2.S2_shape_index_measures.perimeter index));
      check_float_u
        (name ^ " area")
        ~expected:exp_area
        ~actual:(S2.S2_shape_index_measures.area index);
      check_float_u
        (name ^ " approx_area")
        ~expected:exp_approx_area
        ~actual:(S2.S2_shape_index_measures.approx_area index);
      let actual_centroid = S2.S2_shape_index_measures.centroid index in
      let me = Packed_float_option.Unboxed.some #1e-14 in
      (Alcotest.check Alcotest.bool)
        (name ^ " centroid")
        true
        (S2.S2_point.approx_equal ~max_error:me actual_centroid exp_centroid))
;;

let () =
  Alcotest.run
    "S2_shape_index_measures"
    [ "cases", [ test_case "all" `Quick test_cases ] ]
;;
