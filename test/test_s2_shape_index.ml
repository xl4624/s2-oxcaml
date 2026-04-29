(* C++ test parity: s2geometry/src/s2/mutable_s2shape_index_test.cc plus
   s2geometry/src/s2/s2contains_point_query_test.cc (the Semi_open subset). Golden data
   from test/gen/s2shape_index.cc.

   Covered:
   - Empty-index iteration
   - Full-loop index: six face-level cells with contains_center = true
   - Small triangle loop: cell structure and point containment queries
   - Nested squares: multiple shapes share cells and each query's per-shape containment is
     verified independently
   - Regular octagon and 64-gon: exercises recursive subdivision
   - Iterator.locate_cell_id relation (Indexed / Subdivided / Disjoint)

   Not covered (deferred until the matching porter work lands):
   - Shape removal (only add is implemented)
   - Encode / decode
   - Vertex_model.Open or Closed (only Semi_open supported) *)

open Core
open Stdlib_upstream_compatible
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2shape_index.json")
let point_of_json = r3_vector_of_json

let cell_id_of_json j =
  let s = string_of_json_exn j in
  S2.S2_cell_id.of_int64 (Int64_u.of_int64 (Int64.of_string ("0u" ^ s)))
;;

let loop_of_json j =
  let ls = to_list j in
  let n = List.length ls in
  let vs = Array.create ~len:n S2.S2_point.origin in
  List.iteri ls ~f:(fun i p -> vs.(i) <- point_of_json p);
  S2.S2_loop.of_vertices vs
;;

let build_index_from_section section =
  let index = S2.S2_shape_index.create () in
  let loops = to_list (member "shapes" section) in
  let ids =
    List.map loops ~f:(fun j ->
      let loop = loop_of_json j in
      S2.S2_shape_index.add index (S2.S2_loop.to_shape loop))
  in
  S2.S2_shape_index.build index;
  index, ids
;;

let check_cells_match msg section index =
  let expected_cells = to_list (member "cells" section) in
  let it = S2.S2_shape_index.iterator index in
  List.iteri expected_cells ~f:(fun i expected ->
    if S2.S2_shape_index.Iterator.is_done it
    then Alcotest.failf "%s: ran out of cells at index %d" msg i;
    let expected_cell_id = cell_id_of_json (member "cell_id" expected) in
    let actual_cell_id = S2.S2_shape_index.Iterator.cell_id it in
    check_cell_id (sprintf "%s cells[%d] cell_id" msg i) expected_cell_id actual_cell_id;
    let actual_cell = S2.S2_shape_index.Iterator.index_cell it in
    let expected_clipped = to_list (member "clipped" expected) in
    let n = S2.S2_shape_index.Index_cell.num_clipped actual_cell in
    check int (sprintf "%s cells[%d] num_clipped" msg i) (List.length expected_clipped) n;
    List.iteri expected_clipped ~f:(fun j exp ->
      let cs = S2.S2_shape_index.Index_cell.clipped actual_cell j in
      check
        int
        (sprintf "%s cells[%d].clipped[%d].shape_id" msg i j)
        (int_of_json_exn (member "shape_id" exp))
        (S2.S2_shape_index.Clipped_shape.shape_id cs);
      check
        bool
        (sprintf "%s cells[%d].clipped[%d].contains_center" msg i j)
        (bool_of_json_exn (member "contains_center" exp))
        (S2.S2_shape_index.Clipped_shape.contains_center cs);
      let expected_edges = to_list (member "edges" exp) |> List.map ~f:int_of_json_exn in
      let actual_edges =
        List.init (S2.S2_shape_index.Clipped_shape.num_edges cs) ~f:(fun k ->
          S2.S2_shape_index.Clipped_shape.edge cs k)
      in
      check
        (list int)
        (sprintf "%s cells[%d].clipped[%d].edges" msg i j)
        expected_edges
        actual_edges);
    S2.S2_shape_index.Iterator.next it);
  if not (S2.S2_shape_index.Iterator.is_done it)
  then
    Alcotest.failf "%s: iterator has extra cells past %d" msg (List.length expected_cells)
;;

let check_queries msg section index =
  let expected_queries = to_list (member "queries" section) in
  let num_shapes = int_of_json_exn (member "num_shape_ids" section) in
  let q = S2.S2_contains_point_query.create index () in
  List.iteri expected_queries ~f:(fun i query ->
    let p = point_of_json (member "point" query) in
    let expected_per_shape =
      to_list (member "shape_contains" query) |> List.map ~f:bool_of_json_exn
    in
    for sid = 0 to num_shapes - 1 do
      let expected = List.nth_exn expected_per_shape sid in
      let actual = S2.S2_contains_point_query.shape_contains q ~shape_id:sid p in
      check bool (sprintf "%s queries[%d] shape_contains[%d]" msg i sid) expected actual
    done)
;;

let run_section name =
  let section = member name (Lazy.force fixture) in
  let index, _ids = build_index_from_section section in
  check_cells_match name section index;
  check_queries name section index
;;

let test_empty () =
  let section = member "empty" (Lazy.force fixture) in
  let index = S2.S2_shape_index.create () in
  S2.S2_shape_index.build index;
  let it = S2.S2_shape_index.iterator index in
  check bool "empty iterator is_done" true (S2.S2_shape_index.Iterator.is_done it);
  check int "empty num_shape_ids" 0 (S2.S2_shape_index.num_shape_ids index);
  (* Sanity-check against the fixture as well. *)
  check int "empty cells" 0 (List.length (to_list (member "cells" section)))
;;

let test_full_loop () = run_section "full_loop"
let test_triangle () = run_section "triangle"
let test_nested_squares () = run_section "nested_squares"
let test_regular_octagon () = run_section "regular_octagon"
let test_regular_64gon () = run_section "regular_64gon"

let test_locate_cell_id () =
  let section = member "locate_triangle" (Lazy.force fixture) in
  let index = S2.S2_shape_index.create () in
  List.iter
    (to_list (member "shapes" section))
    ~f:(fun j ->
      let loop = loop_of_json j in
      let (_ : int) = S2.S2_shape_index.add index (S2.S2_loop.to_shape loop) in
      ());
  S2.S2_shape_index.build index;
  check_cells_match "locate_triangle" section index;
  let it = S2.S2_shape_index.iterator index in
  List.iteri
    (to_list (member "locate_cell_id" section))
    ~f:(fun i entry ->
      let target = cell_id_of_json (member "target" entry) in
      let expected_rel = string_of_json_exn (member "relation" entry) in
      let actual_rel =
        match S2.S2_shape_index.Iterator.locate_cell_id it target with
        | S2.S2_shape_index.Cell_relation.Indexed -> "indexed"
        | Subdivided -> "subdivided"
        | Disjoint -> "disjoint"
      in
      check string (sprintf "locate_cell_id[%d]" i) expected_rel actual_rel)
;;

let () =
  run
    "s2_shape_index"
    [ "empty", [ test_case "empty" `Quick test_empty ]
    ; "full_loop", [ test_case "full_loop" `Quick test_full_loop ]
    ; "triangle", [ test_case "triangle" `Quick test_triangle ]
    ; "nested_squares", [ test_case "nested_squares" `Quick test_nested_squares ]
    ; "regular_octagon", [ test_case "regular_octagon" `Quick test_regular_octagon ]
    ; "regular_64gon", [ test_case "regular_64gon" `Quick test_regular_64gon ]
    ; "locate_cell_id", [ test_case "locate_cell_id" `Quick test_locate_cell_id ]
    ]
;;
