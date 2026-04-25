(* C++ test parity: s2geometry/src/s2/s2shapeutil_edge_iterator_test.cc.
   Golden data from test/gen/s2shapeutil_edge_iterator.cc.

   Covered:
   -  TEST(S2ShapeutilEdgeIteratorTest, Empty)
   -  TEST(S2ShapeutilEdgeIteratorTest, Points)
   -  TEST(S2ShapeutilEdgeIteratorTest, Lines)
   -  TEST(S2ShapeutilEdgeIteratorTest, Polygons)
   -  TEST(S2ShapeutilEdgeIteratorTest, Collection)
   -  TEST(S2ShapeutilEdgeIteratorTest, AssignmentAndEquality) - simulated
      directly in OCaml without the C++ assignment operator: the index reuse
      and stepwise advancement are exercised against [equal].

   Not covered:
   -  TEST(S2ShapeutilEdgeIteratorTest, Remove): requires
      [S2_shape_index.release], which is not yet implemented (see TODO under
      "Shape-index scope deferred"). *)

open Core
open Alcotest
open Test_helpers
module Edge_iterator = S2.S2_shapeutil_edge_iterator

let fixture = lazy (load_fixture "s2shapeutil_edge_iterator.json")
let point_of_json = r3_vector_of_json

let build_shape j : S2.S2_shape.t =
  let kind = string_of_json_exn (member "kind" j) in
  let vjs = to_list (member "vertices" j) in
  let n = List.length vjs in
  let vs = Array.create ~len:n S2.S2_point.origin in
  List.iteri vjs ~f:(fun i vj -> vs.(i) <- point_of_json vj);
  match kind with
  | "points" -> S2.S2_point_vector_shape.to_shape (S2.S2_point_vector_shape.of_points vs)
  | "polyline" -> S2.S2_polyline.to_shape (S2.S2_polyline.of_vertices vs)
  | "loop" -> S2.S2_loop.to_shape (S2.S2_loop.of_vertices vs)
  | other ->
    (match failwith (sprintf "unknown shape kind %S" other) with
     | (_ : Nothing.t) -> .)
;;

let build_index shape_jsons =
  let index = S2.S2_shape_index.create () in
  List.iter shape_jsons ~f:(fun j ->
    let (_ : int) = S2.S2_shape_index.add index (build_shape j) in
    ());
  S2.S2_shape_index.build index;
  index
;;

let check_point msg ~expected ~actual =
  let exp = S2.S2_point.to_r3 expected in
  let act = S2.S2_point.to_r3 actual in
  check_float_u_exact
    (msg ^ " x")
    ~expected:(S2.R3_vector.x exp)
    ~actual:(S2.R3_vector.x act);
  check_float_u_exact
    (msg ^ " y")
    ~expected:(S2.R3_vector.y exp)
    ~actual:(S2.R3_vector.y act);
  check_float_u_exact
    (msg ^ " z")
    ~expected:(S2.R3_vector.z exp)
    ~actual:(S2.R3_vector.z act)
;;

let run_case case =
  let name = string_of_json_exn (member "name" case) in
  let index = build_index (to_list (member "shapes" case)) in
  let it = Edge_iterator.create index in
  let edges = to_list (member "edges" case) in
  List.iteri edges ~f:(fun i e ->
    check bool (sprintf "%s[%d] not done" name i) false (Edge_iterator.is_done it);
    check
      int
      (sprintf "%s[%d] shape_id" name i)
      (int_of_json_exn (member "shape_id" e))
      (Edge_iterator.shape_id it);
    check
      int
      (sprintf "%s[%d] edge_id" name i)
      (int_of_json_exn (member "edge_id" e))
      (Edge_iterator.edge_id it);
    let edge = Edge_iterator.edge it in
    check_point
      (sprintf "%s[%d] v0" name i)
      ~expected:(point_of_json (member "v0" e))
      ~actual:edge.#v0;
    check_point
      (sprintf "%s[%d] v1" name i)
      ~expected:(point_of_json (member "v1" e))
      ~actual:edge.#v1;
    Edge_iterator.next it);
  check bool (sprintf "%s done after last edge" name) true (Edge_iterator.is_done it)
;;

let test_all_cases () =
  let cases = to_list (member "cases" (Lazy.force fixture)) in
  List.iter cases ~f:run_case
;;

let test_equality_two_indexes () =
  (* Different indexes are never [equal] even with identical contents. *)
  let case =
    List.find_exn
      (to_list (member "cases" (Lazy.force fixture)))
      ~f:(fun c -> String.equal (string_of_json_exn (member "name" c)) "collection")
  in
  let shapes = to_list (member "shapes" case) in
  let index1 = build_index shapes in
  let index2 = build_index shapes in
  let it1 = Edge_iterator.create index1 in
  let it2 = Edge_iterator.create index2 in
  check bool "different-index iterators not equal" false (Edge_iterator.equal it1 it2)
;;

let test_equality_advance () =
  let case =
    List.find_exn
      (to_list (member "cases" (Lazy.force fixture)))
      ~f:(fun c -> String.equal (string_of_json_exn (member "name" c)) "collection")
  in
  let index = build_index (to_list (member "shapes" case)) in
  let it1 = Edge_iterator.create index in
  let it2 = Edge_iterator.create index in
  check bool "same-index iterators initially equal" true (Edge_iterator.equal it1 it2);
  Edge_iterator.next it1;
  check bool "after advancing one, not equal" false (Edge_iterator.equal it1 it2);
  Edge_iterator.next it2;
  check bool "after advancing both, equal again" true (Edge_iterator.equal it1 it2)
;;

let () =
  Alcotest.run
    "S2_shapeutil_edge_iterator"
    [ "fixture", [ test_case "all upstream cases" `Quick test_all_cases ]
    ; ( "equality"
      , [ test_case "different indexes" `Quick test_equality_two_indexes
        ; test_case "advance and re-align" `Quick test_equality_advance
        ] )
    ]
;;
