(* Golden data produced by test/gen/s2shape_measures.cc.

   Upstream C++ tests covered:
   - TEST(GetLength, WrongDimension) - via "dim0_point" and "dim2_triangle"
   - TEST(GetLength, NoPolylines) - via "empty_polyline"
   - TEST(GetLength, ThreePolylinesInOneShape) - via "three_polylines_one_shape" using
     [S2_edge_vector_shape]
   - TEST(GetPerimeter, EmptyPolygon) / FullPolygon / TwoLoopPolygon
   - TEST(GetArea, TwoTinyShells) / TinyShellAndHole
   - TEST(GetApproxArea, LargeShellAndHolePolygon)
   - TEST(GetCentroid, Points) / Polyline / Polygon

   Deliberately omitted:
   - TEST(GetArea, WrongDimension), TEST(GetApproxArea, WrongDimension): redundant with
     the per-dimension wrong-dimension cases above; the shape_measures functions all key
     off [shape.dimension] uniformly. *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2shape_measures.json")
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

let edge_vector_shape_of_json j ~dimension =
  let edges = to_list (member "edges" j) in
  let shape = S2.S2_edge_vector_shape.create () in
  List.iter edges ~f:(fun pair ->
    match to_list pair with
    | [ v0_json; v1_json ] ->
      let v0 = s2_point_of_json v0_json in
      let v1 = s2_point_of_json v1_json in
      S2.S2_edge_vector_shape.add shape ~v0 ~v1
    | _ ->
      (match failwith "expected [v0, v1]" with
       | (_ : Nothing.t) -> .));
  S2.S2_edge_vector_shape.set_dimension shape dimension;
  S2.S2_edge_vector_shape.to_shape shape
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

let test_cases () =
  List.iter
    (to_list (get "cases"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let shape =
        let j = member "shape" case in
        let kind = string_of_json_exn (member "kind" j) in
        if String.equal kind "edge_vector"
        then (
          let dimension = int_of_json_exn (member "dimension" case) in
          edge_vector_shape_of_json j ~dimension)
        else shape_of_json j
      in
      let expected = member "expected" case in
      let exp_length = float_u_of_json_exn (member "length_radians" expected) in
      let exp_perimeter = float_u_of_json_exn (member "perimeter_radians" expected) in
      let exp_area = float_u_of_json_exn (member "area" expected) in
      let exp_approx_area = float_u_of_json_exn (member "approx_area" expected) in
      let exp_centroid = s2_point_of_json (member "centroid" expected) in
      check_float_u
        (name ^ " length")
        ~expected:exp_length
        ~actual:(S2.S1_angle.radians (S2.S2_shape_measures.length shape));
      check_float_u
        (name ^ " perimeter")
        ~expected:exp_perimeter
        ~actual:(S2.S1_angle.radians (S2.S2_shape_measures.perimeter shape));
      check_float_u
        (name ^ " area")
        ~expected:exp_area
        ~actual:(S2.S2_shape_measures.area shape);
      check_float_u
        (name ^ " approx_area")
        ~expected:exp_approx_area
        ~actual:(S2.S2_shape_measures.approx_area shape);
      let actual_centroid = S2.S2_shape_measures.centroid shape in
      let me = Packed_float_option.Unboxed.some #1e-14 in
      (Alcotest.check Alcotest.bool)
        (name ^ " centroid")
        true
        (S2.S2_point.approx_equal ~max_error:me actual_centroid exp_centroid))
;;

let () =
  Alcotest.run "S2_shape_measures" [ "cases", [ test_case "all" `Quick test_cases ] ]
;;
