(* C++ test parity: s2geometry/src/s2/s2shapeutil_shape_edge_id_test.cc. Golden data from
   test/gen/s2shapeutil_shape_edge_id.cc.

   Covered:
   - Default-constructed [ShapeEdgeId] is [(-1, -1)], matching {!none}.
   - TEST(ShapeEdgeIdTest, BothFieldsEqualIsEqual)
   - TEST(ShapeEdgeIdTest, BothShapeIdUnequalIsUnequal)
   - TEST(ShapeEdgeIdTest, BothEdgeIdUnequalIsUnequal)
   - TEST(ShapeEdgeIdTest, LessThanIsLexicographicShapeIdFirst)
   - TEST(ShapeEdgeIdTest, LessEqIsLexicographicShapeIdFirst)
   - TEST(ShapeEdgeIdTest, GreaterThanIsLexicographicShapeIdFirst)
   - TEST(ShapeEdgeIdTest, GreaterEqIsLexicographicShapeIdFirst)

   Not covered:
   - AbslHashValue / streaming operators (no parity in OCaml). *)

open Core
open Alcotest
open Test_helpers
module Shape_edge_id = S2.S2_shapeutil_shape_edge_id

let fixture = lazy (load_fixture "s2shapeutil_shape_edge_id.json")

let id_of_json j : Shape_edge_id.t =
  match to_list j with
  | [ s; e ] ->
    Shape_edge_id.create ~shape_id:(int_of_json_exn s) ~edge_id:(int_of_json_exn e)
  | _ ->
    (match failwith "expected [shape_id, edge_id] pair" with
     | (_ : Nothing.t) -> .)
;;

let test_default () =
  let j = member "default" (Lazy.force fixture) in
  let expected = id_of_json j in
  check int "default shape_id" expected.#shape_id Shape_edge_id.none.#shape_id;
  check int "default edge_id" expected.#edge_id Shape_edge_id.none.#edge_id
;;

let test_relations () =
  let cases = to_list (member "relations" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i case ->
    let lhs = id_of_json (member "lhs" case) in
    let rhs = id_of_json (member "rhs" case) in
    let expected_eq = bool_of_json_exn (member "eq" case) in
    let expected_ne = bool_of_json_exn (member "ne" case) in
    let expected_lt = bool_of_json_exn (member "lt" case) in
    let expected_le = bool_of_json_exn (member "le" case) in
    let expected_gt = bool_of_json_exn (member "gt" case) in
    let expected_ge = bool_of_json_exn (member "ge" case) in
    let cmp = Shape_edge_id.compare lhs rhs in
    check bool (sprintf "case[%d] eq" i) expected_eq (Shape_edge_id.equal lhs rhs);
    check bool (sprintf "case[%d] ne" i) expected_ne (not (Shape_edge_id.equal lhs rhs));
    check bool (sprintf "case[%d] lt" i) expected_lt (cmp < 0);
    check bool (sprintf "case[%d] le" i) expected_le (cmp <= 0);
    check bool (sprintf "case[%d] gt" i) expected_gt (cmp > 0);
    check bool (sprintf "case[%d] ge" i) expected_ge (cmp >= 0))
;;

let () =
  Alcotest.run
    "S2_shapeutil_shape_edge_id"
    [ ( "fixture"
      , [ test_case "default sentinel" `Quick test_default
        ; test_case "comparison relations" `Quick test_relations
        ] )
    ]
;;
