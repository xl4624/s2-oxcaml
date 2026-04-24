(* Golden data produced by test/gen/s2shapeutil_count_edges.cc.

   Upstream C++ tests covered:
   - TEST(CountEdgesUpTo, StopsEarly) - replayed as the "stops_early" case
     with the same four shapes and thresholds 1, 5, 6, 8.

   Extra coverage:
   - "empty" pins the behaviour on an index with no shapes.
   - "threshold_zero" confirms that [max_edges <= 0] on a non-empty index
     returns the first shape's edge count (matching C++ where the bound
     check runs after the first add). *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2shapeutil_count_edges.json")
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

let shape_of_json j =
  let kind = string_of_json_exn (member "kind" j) in
  match kind with
  | "points" ->
    let points = points_of_json (member "points" j) in
    S2.S2_point_vector_shape.to_shape (S2.S2_point_vector_shape.of_points points)
  | "polyline" ->
    let vertices = points_of_json (member "vertices" j) in
    S2.S2_polyline.to_shape (S2.S2_polyline.of_vertices vertices)
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
      let expected_total = int_of_json_exn (member "total_edges" case) in
      (check int)
        (name ^ " count_edges")
        expected_total
        (S2.S2_shapeutil_count_edges.count_edges index);
      let expected_num_edges =
        List.map (to_list (member "shape_num_edges" case)) ~f:int_of_json_exn
      in
      List.iteri expected_num_edges ~f:(fun i expected ->
        let shape = S2.S2_shape_index.shape index i in
        (check int) (sprintf "%s shape[%d] num_edges" name i) expected shape.#num_edges);
      List.iter
        (to_list (member "up_to" case))
        ~f:(fun entry ->
          let max_edges = int_of_json_exn (member "max_edges" entry) in
          let expected = int_of_json_exn (member "expected" entry) in
          (check int)
            (sprintf "%s count_edges_up_to(%d)" name max_edges)
            expected
            (S2.S2_shapeutil_count_edges.count_edges_up_to index ~max_edges)))
;;

let () =
  Alcotest.run
    "S2_shapeutil_count_edges"
    [ "cases", [ test_case "all" `Quick test_cases ] ]
;;
