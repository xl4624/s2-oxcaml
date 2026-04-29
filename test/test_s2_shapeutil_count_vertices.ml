(* Golden data produced by test/gen/s2shapeutil_count_vertices.cc.

   Upstream C++ tests covered:
   - TEST(CountVertices, CountsCorrectly) - the five sub-scenarios are replayed as
     "three_points", "points_and_polyline", "points_polyline_polygon",
     "degenerate_polyline", and "degenerate_polygon".

   Extra coverage:
   - "empty" pins the index result on a zero-shape index. *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2shapeutil_count_vertices.json")
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

let build_index j =
  let index = S2.S2_shape_index.create () in
  List.iter
    (to_list (member "shapes" j))
    ~f:(fun s ->
      let _id : int = S2.S2_shape_index.add index (shape_of_json s) in
      ());
  S2.S2_shape_index.build index;
  index
;;

let test_cases () =
  List.iter
    (to_list (get "cases"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let index = build_index case in
      let expected_index = int_of_json_exn (member "index_count" case) in
      (check int)
        (name ^ " of_index")
        expected_index
        (S2.S2_shapeutil_count_vertices.of_index index);
      let expected_per_shape =
        List.map (to_list (member "per_shape_counts" case)) ~f:int_of_json_exn
      in
      List.iteri expected_per_shape ~f:(fun i expected ->
        let shape = S2.S2_shape_index.shape index i in
        (check int)
          (sprintf "%s shape[%d] of_shape" name i)
          expected
          (S2.S2_shapeutil_count_vertices.of_shape shape)))
;;

let () =
  Alcotest.run
    "S2_shapeutil_count_vertices"
    [ "cases", [ test_case "all" `Quick test_cases ] ]
;;
