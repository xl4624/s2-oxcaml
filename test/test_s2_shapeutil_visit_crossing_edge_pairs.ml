(* Golden data produced by test/gen/s2shapeutil_visit_crossing_edge_pairs.cc.

   Upstream C++ tests covered (s2shapeutil_visit_crossing_edge_pairs_test.cc):
   - TEST(GetCrossingEdgePairs, NoIntersectionsOneIndex)
   - TEST(GetCrossingEdgePairs, EdgeGridOneIndex)
   - TEST(GetCrossingEdgePairs, NoIntersectionsTwoIndexes)
   - TEST(GetCrossingEdgePairs, EdgeGridTwoIndexes)
   - TEST(FindSelfIntersection, Basic) - replayed for every cyclic
     permutation of every loop, mirroring the upstream
     TestHasCrossingPermutations driver. *)

open Core
open Test_helpers
open Alcotest
module V = S2.S2_shapeutil_visit_crossing_edge_pairs

let fixture = lazy (load_fixture "s2shapeutil_visit_crossing_edge_pairs.json")
let s2_point_of_json j = r3_vector_of_json j

let edge_of_json j =
  match to_list j with
  | [ v0_j; v1_j ] ->
    S2.S2_shape.Edge.create ~v0:(s2_point_of_json v0_j) ~v1:(s2_point_of_json v1_j)
  | _ ->
    (match failwith "expected [v0, v1]" with
     | (_ : Nothing.t) -> .)
;;

let edge_vector_shape_of_json j =
  let edges_j = to_list j in
  let shape = S2.S2_edge_vector_shape.create () in
  List.iter edges_j ~f:(fun ej ->
    let e = edge_of_json ej in
    S2.S2_edge_vector_shape.add shape ~v0:e.#v0 ~v1:e.#v1);
  S2.S2_edge_vector_shape.to_shape shape
;;

let build_edge_index shapes_j =
  let index = S2.S2_shape_index.create () in
  List.iter (to_list shapes_j) ~f:(fun sj ->
    let _id : int = S2.S2_shape_index.add index (edge_vector_shape_of_json sj) in
    ());
  S2.S2_shape_index.build index;
  index
;;

let crossing_type_alcotest =
  let crossing_type_of_string = function
    | "all" -> V.Crossing_type.All
    | "interior" -> V.Crossing_type.Interior
    | s ->
      (match failwithf "unknown crossing_type: %s" s () with
       | (_ : Nothing.t) -> .)
  in
  crossing_type_of_string
;;

(* (shape_id, edge_id, shape_id, edge_id) tuple as a comparable record. *)
module Pair = struct
  type t =
    { a_shape : int
    ; a_edge : int
    ; b_shape : int
    ; b_edge : int
    }
  [@@deriving compare, sexp_of]
end

let pair_of_json j =
  match to_list j with
  | [ a_j; b_j ] ->
    let a_shape, a_edge =
      match to_list a_j with
      | [ s; e ] -> int_of_json_exn s, int_of_json_exn e
      | _ ->
        (match failwith "expected [shape_id, edge_id]" with
         | (_ : Nothing.t) -> .)
    in
    let b_shape, b_edge =
      match to_list b_j with
      | [ s; e ] -> int_of_json_exn s, int_of_json_exn e
      | _ ->
        (match failwith "expected [shape_id, edge_id]" with
         | (_ : Nothing.t) -> .)
    in
    { Pair.a_shape; a_edge; b_shape; b_edge }
  | _ ->
    (match failwith "expected [a_pair, b_pair]" with
     | (_ : Nothing.t) -> .)
;;

let pairs_of_json j = to_list j |> List.map ~f:pair_of_json

(* Runs [V.visit_crossing_edge_pairs] (or the two-index variant) against
   [index], collects each visited pair, sorts and dedups it (matching the
   canonical comparison style used by the upstream test). *)
let collect_one_index_pairs index ~crossing_type =
  let acc = ref [] in
  let _completed =
    V.visit_crossing_edge_pairs index ~crossing_type ~visitor:(fun a b _is_interior ->
      acc
      := { Pair.a_shape = a.#id.#shape_id
         ; a_edge = a.#id.#edge_id
         ; b_shape = b.#id.#shape_id
         ; b_edge = b.#id.#edge_id
         }
         :: !acc;
      true)
  in
  List.dedup_and_sort !acc ~compare:Pair.compare
;;

let collect_two_index_pairs ~a_index ~b_index ~crossing_type =
  let acc = ref [] in
  let _completed =
    V.visit_crossing_edge_pairs_two
      ~a_index
      ~b_index
      ~crossing_type
      ~visitor:(fun a b _is_interior ->
        acc
        := { Pair.a_shape = a.#id.#shape_id
           ; a_edge = a.#id.#edge_id
           ; b_shape = b.#id.#shape_id
           ; b_edge = b.#id.#edge_id
           }
           :: !acc;
        true)
  in
  List.dedup_and_sort !acc ~compare:Pair.compare
;;

let pairs_to_string pairs =
  String.concat
    ~sep:"; "
    (List.map pairs ~f:(fun p ->
       sprintf "((%d,%d),(%d,%d))" p.Pair.a_shape p.a_edge p.b_shape p.b_edge))
;;

let check_pairs name ~expected ~actual =
  let exp_sorted = List.sort expected ~compare:Pair.compare in
  let act_sorted = List.sort actual ~compare:Pair.compare in
  (check string) name (pairs_to_string exp_sorted) (pairs_to_string act_sorted)
;;

let one_index_case case () =
  let name = string_of_json_exn (member "name" case) in
  let index = build_edge_index (member "shapes" case) in
  let expected_all = pairs_of_json (member "crossings_all" case) in
  let actual_all =
    collect_one_index_pairs index ~crossing_type:(crossing_type_alcotest "all")
  in
  check_pairs (name ^ " all") ~expected:expected_all ~actual:actual_all;
  let expected_interior = pairs_of_json (member "crossings_interior" case) in
  let actual_interior =
    collect_one_index_pairs index ~crossing_type:(crossing_type_alcotest "interior")
  in
  check_pairs (name ^ " interior") ~expected:expected_interior ~actual:actual_interior
;;

let two_index_case case () =
  let name = string_of_json_exn (member "name" case) in
  let a_index = build_edge_index (member "a_shapes" case) in
  let b_index = build_edge_index (member "b_shapes" case) in
  let expected_all = pairs_of_json (member "crossings_all" case) in
  let actual_all =
    collect_two_index_pairs
      ~a_index
      ~b_index
      ~crossing_type:(crossing_type_alcotest "all")
  in
  check_pairs (name ^ " all") ~expected:expected_all ~actual:actual_all;
  let expected_interior = pairs_of_json (member "crossings_interior" case) in
  let actual_interior =
    collect_two_index_pairs
      ~a_index
      ~b_index
      ~crossing_type:(crossing_type_alcotest "interior")
  in
  check_pairs (name ^ " interior") ~expected:expected_interior ~actual:actual_interior
;;

let lax_polygon_shape_of_loops_json loops_j =
  let loops =
    to_list loops_j
    |> List.map ~f:(fun lj ->
      let pts_j = to_list lj in
      let n = List.length pts_j in
      let arr = Array.create ~len:n S2.S2_point.origin in
      List.iteri pts_j ~f:(fun i pj -> arr.(i) <- s2_point_of_json pj);
      arr)
    |> Array.of_list
  in
  S2.S2_lax_polygon.to_shape (S2.S2_lax_polygon.of_loops loops)
;;

let find_self_intersection_case case () =
  let name = string_of_json_exn (member "name" case) in
  let expected_has_intersection =
    bool_of_json_exn (member "expected_has_intersection" case)
  in
  let index = S2.S2_shape_index.create () in
  let _id : int =
    S2.S2_shape_index.add index (lax_polygon_shape_of_loops_json (member "loops" case))
  in
  S2.S2_shape_index.build index;
  let result = V.find_self_intersection index in
  let actual_has_intersection = Option.is_some result in
  (check bool)
    (sprintf "%s: has_intersection" name)
    expected_has_intersection
    actual_has_intersection
;;

let () =
  let fx = Lazy.force fixture in
  let one_index_cases = to_list (member "one_index_cases" fx) in
  let two_index_cases = to_list (member "two_index_cases" fx) in
  let find_self_cases = to_list (member "find_self_intersection_cases" fx) in
  let one_index_tests =
    List.map one_index_cases ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      Alcotest.test_case name `Quick (one_index_case case))
  in
  let two_index_tests =
    List.map two_index_cases ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      Alcotest.test_case name `Quick (two_index_case case))
  in
  let find_self_tests =
    List.map find_self_cases ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      Alcotest.test_case name `Quick (find_self_intersection_case case))
  in
  Alcotest.run
    "S2_shapeutil_visit_crossing_edge_pairs"
    [ "one_index", one_index_tests
    ; "two_index", two_index_tests
    ; "find_self_intersection", find_self_tests
    ]
;;
