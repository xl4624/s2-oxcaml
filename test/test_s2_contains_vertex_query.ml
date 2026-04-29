(* C++ test parity: s2geometry/src/s2/s2contains_vertex_query_test.cc Golden data from
   test/gen/s2contains_vertex_query.cc.

   Covered:
   - TEST(S2ContainsVertexQuery, Undetermined)
   - TEST(S2ContainsVertexQuery, ContainedWithDuplicates)
   - TEST(S2ContainsVertexQuery, NotContainedWithDuplicates)
   - TEST(S2ContainsVertexQuery, CompatibleWithAngleContainsVertex)
   - TEST(S2ContainsVertexQuery, CompatibleWithAngleContainsVertexDegenerate) *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2contains_vertex_query.json")
let point_of_json = r3_vector_of_json

(* Replay a sequence of AddEdge actions and check that contains_sign and duplicate_edges
   match the values the generator recorded after each step. *)
let run_action_sequence name =
  let section = member name (Lazy.force fixture) in
  let target = point_of_json (member "target" section) in
  let q = S2.S2_contains_vertex_query.create target in
  List.iteri
    (to_list (member "actions" section))
    ~f:(fun i action ->
      let v = point_of_json (member "v" action) in
      let direction = int_of_json_exn (member "direction" action) in
      let expected_sign = int_of_json_exn (member "contains_sign" action) in
      let expected_dup = bool_of_json_exn (member "duplicate_edges" action) in
      S2.S2_contains_vertex_query.add_edge q v ~direction;
      check
        int
        (sprintf "%s[%d] contains_sign" name i)
        expected_sign
        (S2.S2_contains_vertex_query.contains_sign q);
      check
        bool
        (sprintf "%s[%d] duplicate_edges" name i)
        expected_dup
        (S2.S2_contains_vertex_query.duplicate_edges q))
;;

let test_undetermined () = run_action_sequence "undetermined"
let test_contained_with_duplicates () = run_action_sequence "contained_with_duplicates"

let test_not_contained_with_duplicates () =
  run_action_sequence "not_contained_with_duplicates"
;;

let test_regular_loop_triples () =
  let f = Lazy.force fixture in
  let triples = to_list (member "regular_loop_triples" f) in
  List.iteri triples ~f:(fun i t ->
    let a = point_of_json (member "a" t) in
    let b = point_of_json (member "b" t) in
    let c = point_of_json (member "c" t) in
    let expected_sign = int_of_json_exn (member "contains_sign" t) in
    let expected_dup = bool_of_json_exn (member "duplicate_edges" t) in
    let expected_angle = bool_of_json_exn (member "angle_contains_vertex" t) in
    let q = S2.S2_contains_vertex_query.create b in
    S2.S2_contains_vertex_query.add_edge q a ~direction:(-1);
    S2.S2_contains_vertex_query.add_edge q c ~direction:1;
    let actual_sign = S2.S2_contains_vertex_query.contains_sign q in
    let actual_dup = S2.S2_contains_vertex_query.duplicate_edges q in
    let actual_angle = S2.S2_edge_crossings.angle_contains_vertex a b c in
    check int (sprintf "triple[%d] contains_sign" i) expected_sign actual_sign;
    check bool (sprintf "triple[%d] duplicate_edges" i) expected_dup actual_dup;
    check bool (sprintf "triple[%d] angle_contains_vertex" i) expected_angle actual_angle;
    check
      bool
      (sprintf "triple[%d] contains_sign > 0 == angle_contains_vertex" i)
      (actual_sign > 0)
      actual_angle)
;;

let test_degenerate_triple () =
  let f = Lazy.force fixture in
  let t = member "degenerate_triple" f in
  let a = point_of_json (member "a" t) in
  let b = point_of_json (member "b" t) in
  let expected_sign = int_of_json_exn (member "contains_sign" t) in
  let expected_dup = bool_of_json_exn (member "duplicate_edges" t) in
  let expected_angle = bool_of_json_exn (member "angle_contains_vertex" t) in
  let q = S2.S2_contains_vertex_query.create b in
  S2.S2_contains_vertex_query.add_edge q a ~direction:(-1);
  S2.S2_contains_vertex_query.add_edge q a ~direction:1;
  let actual_sign = S2.S2_contains_vertex_query.contains_sign q in
  let actual_dup = S2.S2_contains_vertex_query.duplicate_edges q in
  let actual_angle = S2.S2_edge_crossings.angle_contains_vertex a b a in
  check int "degenerate contains_sign" expected_sign actual_sign;
  check bool "degenerate duplicate_edges" expected_dup actual_dup;
  check bool "degenerate angle_contains_vertex" expected_angle actual_angle
;;

let () =
  run
    "s2_contains_vertex_query"
    [ "undetermined", [ test_case "undetermined" `Quick test_undetermined ]
    ; ( "contained_with_duplicates"
      , [ test_case "contained_with_duplicates" `Quick test_contained_with_duplicates ] )
    ; ( "not_contained_with_duplicates"
      , [ test_case
            "not_contained_with_duplicates"
            `Quick
            test_not_contained_with_duplicates
        ] )
    ; ( "regular_loop_triples"
      , [ test_case "regular_loop_triples" `Quick test_regular_loop_triples ] )
    ; "degenerate_triple", [ test_case "degenerate_triple" `Quick test_degenerate_triple ]
    ]
;;
