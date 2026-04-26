(* C++ test parity: s2geometry/src/s2/s2shapeutil_edge_wrap_test.cc.
   Golden data from test/gen/s2shapeutil_edge_wrap.cc.

   Covered:
   - TEST(S2Shape, NextPrevEdgePointDoesNotWrap): "1:1 | 2:2 ##".
   - TEST(S2Shape, NextPrevEdgeOpenPolylineDoesNotWrap):
     "# 1:1, 2:2, 3:3 #".
   - TEST(S2Shape, NextPrevEdgeClosedPolylineWraps):
     "# 0:0, 1:1, 0:2, -1:1, 0:0 #".
   - TEST(S2Shape, NextPrevEdgePolygonWraps): "## 0:0, 1:1, 0:2, -1:1".

   Extra coverage (not in C++):
   - "polygon_two_loops_wrap_per_chain": multi-loop polygon, verifies that
     wrap stays inside [start, start + length) for each chain.
   - "single_edge_open_polyline": both endpoints of a single-edge open
     polyline return -1. *)

open Core
open Alcotest
open Test_helpers
module Edge_wrap = S2.S2_shapeutil_edge_wrap

let fixture = lazy (load_fixture "s2shapeutil_edge_wrap.json")
let point_of_json = r3_vector_of_json

let vertices_of_json j =
  let pts = to_list j in
  let n = List.length pts in
  let arr = Array.create ~len:n S2.S2_point.origin in
  List.iteri pts ~f:(fun i pj -> arr.(i) <- point_of_json pj);
  arr
;;

let loops_of_json j =
  let loops = to_list j in
  let n = List.length loops in
  let out = Array.create ~len:n [||] in
  List.iteri loops ~f:(fun i loop -> out.(i) <- vertices_of_json loop);
  out
;;

let build_shape case : S2.S2_shape.t =
  let kind = string_of_json_exn (member "kind" case) in
  match kind with
  | "points" ->
    let vs = vertices_of_json (member "vertices" case) in
    S2.S2_point_vector_shape.to_shape (S2.S2_point_vector_shape.of_points vs)
  | "polyline" ->
    let vs = vertices_of_json (member "vertices" case) in
    S2.S2_polyline.to_shape (S2.S2_polyline.of_vertices vs)
  | "polygon" ->
    let loops = loops_of_json (member "loops" case) in
    S2.S2_lax_polygon.to_shape (S2.S2_lax_polygon.of_loops loops)
  | other ->
    (match failwith (sprintf "unknown shape kind %S" other) with
     | (_ : Nothing.t) -> .)
;;

let test_cases () =
  let cases = to_list (member "cases" (Lazy.force fixture)) in
  List.iter cases ~f:(fun case ->
    let name = string_of_json_exn (member "name" case) in
    let shape = build_shape case in
    let edges = to_list (member "edges" case) in
    List.iter edges ~f:(fun e ->
      let edge_id = int_of_json_exn (member "edge_id" e) in
      let expected_next = int_of_json_exn (member "next" e) in
      let expected_prev = int_of_json_exn (member "prev" e) in
      check
        int
        (sprintf "%s edge=%d next" name edge_id)
        expected_next
        (Edge_wrap.next_edge_wrap shape ~edge_id);
      check
        int
        (sprintf "%s edge=%d prev" name edge_id)
        expected_prev
        (Edge_wrap.prev_edge_wrap shape ~edge_id)))
;;

let () =
  Alcotest.run
    "S2_shapeutil_edge_wrap"
    [ "fixture", [ test_case "all cases" `Quick test_cases ] ]
;;
