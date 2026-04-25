(* C++ test parity: s2geometry/src/s2/s2crossing_edge_query_test.cc.
   Golden data from test/gen/s2crossing_edge_query.cc.

   Covered:
   -  Empty index: every query returns empty candidates and crossings.
   -  Zigzag polyline (single shape, <= 27 edges): brute-force candidate path.
   -  Three zigzag polylines across multiple shapes.
   -  Regular 64-gon loop (> 27 edges): exercises the recursive cell-descent
      path with default [max_edges_per_cell].
   -  Regular 64-gon loop with [max_edges_per_cell = 1] to force deep
      subdivision.
   -  Mixed shapes (loop + polyline).
   -  Degenerate zero-length query edge.

   Not covered (deferred):
   -  Randomized perturbation tests (would require random generation parity).
   -  Low-level VisitCells / VisitRawCandidates visitor APIs (our OCaml
      interface only exposes GetCandidates / GetCrossingEdges). *)

open Core
open Alcotest
open Test_helpers
module Shape_edge_id = S2.S2_crossing_edge_query.Shape_edge_id

let fixture = lazy (load_fixture "s2crossing_edge_query.json")
let point_of_json = r3_vector_of_json

let shape_edge_id_of_json j : Shape_edge_id.t =
  Shape_edge_id.create
    ~shape_id:(int_of_json_exn (member "shape_id" j))
    ~edge_id:(int_of_json_exn (member "edge_id" j))
;;

let ids_of_json j =
  let xs = to_list j in
  let n = List.length xs in
  let arr = Array.create ~len:n Shape_edge_id.none in
  List.iteri xs ~f:(fun i v -> arr.(i) <- shape_edge_id_of_json v);
  arr
;;

let check_ids msg ~expected ~actual =
  let n_exp = Array.length expected in
  let n_act = Array.length actual in
  check int (msg ^ " length") n_exp n_act;
  let n = Int.min n_exp n_act in
  for i = 0 to n - 1 do
    let #{ shape_id = es; edge_id = ee } : Shape_edge_id.t = expected.(i) in
    let #{ shape_id = as_; edge_id = ae } : Shape_edge_id.t = actual.(i) in
    check int (sprintf "%s[%d] shape_id" msg i) es as_;
    check int (sprintf "%s[%d] edge_id" msg i) ee ae
  done
;;

let build_shape j : S2.S2_shape.t =
  let kind = string_of_json_exn (member "kind" j) in
  let vjs = to_list (member "vertices" j) in
  let n = List.length vjs in
  let vs = Array.create ~len:n S2.S2_point.origin in
  List.iteri vjs ~f:(fun i vj -> vs.(i) <- point_of_json vj);
  match kind with
  | "polyline" -> S2.S2_polyline.to_shape (S2.S2_polyline.of_vertices vs)
  | "loop" -> S2.S2_loop.to_shape (S2.S2_loop.of_vertices vs)
  | other ->
    (match failwith (sprintf "unknown shape kind %S" other) with
     | (_ : Nothing.t) -> .)
;;

let run_section ?(max_edges_per_cell = 10) name =
  let section = member name (Lazy.force fixture) in
  let shape_jsons = to_list (member "shapes" section) in
  let index = S2.S2_shape_index.create ~max_edges_per_cell () in
  List.iter shape_jsons ~f:(fun j ->
    let (_ : int) = S2.S2_shape_index.add index (build_shape j) in
    ());
  S2.S2_shape_index.build index;
  let query = S2.S2_crossing_edge_query.create index in
  let queries = to_list (member "queries" section) in
  List.iteri queries ~f:(fun i q ->
    let a = point_of_json (member "a" q) in
    let b = point_of_json (member "b" q) in
    let expected_candidates = ids_of_json (member "candidates_all" q) in
    let actual_candidates = S2.S2_crossing_edge_query.get_candidates query ~a ~b in
    check_ids
      (sprintf "%s queries[%d] candidates_all" name i)
      ~expected:expected_candidates
      ~actual:actual_candidates;
    let expected_all = ids_of_json (member "crossings_all" q) in
    let actual_all =
      S2.S2_crossing_edge_query.get_crossing_edges query ~a ~b ~crossing_type:All
    in
    check_ids
      (sprintf "%s queries[%d] crossings_all" name i)
      ~expected:expected_all
      ~actual:actual_all;
    let expected_int = ids_of_json (member "crossings_interior" q) in
    let actual_int =
      S2.S2_crossing_edge_query.get_crossing_edges query ~a ~b ~crossing_type:Interior
    in
    check_ids
      (sprintf "%s queries[%d] crossings_interior" name i)
      ~expected:expected_int
      ~actual:actual_int;
    let per_shape = to_list (member "per_shape" q) in
    List.iter per_shape ~f:(fun ps ->
      let shape_id = int_of_json_exn (member "shape_id" ps) in
      let shape = S2.S2_shape_index.shape index shape_id in
      let expected_cands = ids_of_json (member "candidates" ps) in
      let actual_cands =
        S2.S2_crossing_edge_query.get_candidates_for_shape query ~a ~b ~shape_id ~shape
      in
      check_ids
        (sprintf "%s queries[%d] per_shape[%d] candidates" name i shape_id)
        ~expected:expected_cands
        ~actual:actual_cands;
      let expected_all_s = ids_of_json (member "crossings_all" ps) in
      let actual_all_s =
        S2.S2_crossing_edge_query.get_crossing_edges_for_shape
          query
          ~a
          ~b
          ~shape_id
          ~shape
          ~crossing_type:All
      in
      check_ids
        (sprintf "%s queries[%d] per_shape[%d] crossings_all" name i shape_id)
        ~expected:expected_all_s
        ~actual:actual_all_s;
      let expected_int_s = ids_of_json (member "crossings_interior" ps) in
      let actual_int_s =
        S2.S2_crossing_edge_query.get_crossing_edges_for_shape
          query
          ~a
          ~b
          ~shape_id
          ~shape
          ~crossing_type:Interior
      in
      check_ids
        (sprintf "%s queries[%d] per_shape[%d] crossings_interior" name i shape_id)
        ~expected:expected_int_s
        ~actual:actual_int_s))
;;

let test_empty () = run_section "empty"
let test_zigzag_polyline () = run_section "zigzag_polyline"
let test_three_zigzag_polylines () = run_section "three_zigzag_polylines"
let test_regular_64gon () = run_section "regular_64gon"
let test_regular_64gon_fine () = run_section ~max_edges_per_cell:1 "regular_64gon_fine"
let test_loop_and_polyline () = run_section "loop_and_polyline"
let test_degenerate_query () = run_section "degenerate_query"

let () =
  run
    "s2_crossing_edge_query"
    [ "empty", [ test_case "empty" `Quick test_empty ]
    ; "zigzag_polyline", [ test_case "zigzag_polyline" `Quick test_zigzag_polyline ]
    ; ( "three_zigzag_polylines"
      , [ test_case "three_zigzag_polylines" `Quick test_three_zigzag_polylines ] )
    ; "regular_64gon", [ test_case "regular_64gon" `Quick test_regular_64gon ]
    ; ( "regular_64gon_fine"
      , [ test_case "regular_64gon_fine" `Quick test_regular_64gon_fine ] )
    ; "loop_and_polyline", [ test_case "loop_and_polyline" `Quick test_loop_and_polyline ]
    ; "degenerate_query", [ test_case "degenerate_query" `Quick test_degenerate_query ]
    ]
;;
