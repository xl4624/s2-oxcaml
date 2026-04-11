(* C++ test parity: s2geometry/src/s2/s2wedge_relations_test.cc
   Golden data from test/gen/s2wedge_relations.cc.

   Covered:
   -  TEST(S2WedgeRelations, Wedges) - every sub-case from the upstream test,
      including intersection in one/two wedges, normal containment, equality
      on one/both sides, disjoint with equality, and B contains A. *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2wedge_relations.json")

let point_of_json j =
  match to_list j with
  | [ x; y; z ] ->
    S2.R3_vector.create
      ~x:(float_u_of_json_exn x)
      ~y:(float_u_of_json_exn y)
      ~z:(float_u_of_json_exn z)
  | _ ->
    (match failwith "expected [x, y, z]" with
     | (_ : Nothing.t) -> .)
;;

let relation_of_int fixture =
  let e = member "enum" fixture in
  let equals = int_of_json_exn (member "equals" e) in
  let properly_contains = int_of_json_exn (member "properly_contains" e) in
  let is_properly_contained = int_of_json_exn (member "is_properly_contained" e) in
  let properly_overlaps = int_of_json_exn (member "properly_overlaps" e) in
  let is_disjoint = int_of_json_exn (member "is_disjoint" e) in
  fun i ->
    if Int.( = ) i equals
    then S2.S2_wedge_relations.Relation.Equals
    else if Int.( = ) i properly_contains
    then S2.S2_wedge_relations.Relation.Properly_contains
    else if Int.( = ) i is_properly_contained
    then S2.S2_wedge_relations.Relation.Is_properly_contained
    else if Int.( = ) i properly_overlaps
    then S2.S2_wedge_relations.Relation.Properly_overlaps
    else if Int.( = ) i is_disjoint
    then S2.S2_wedge_relations.Relation.Is_disjoint
    else failwithf "unknown WedgeRelation int %d" i ()
;;

let relation_to_string r = Sexp.to_string (S2.S2_wedge_relations.Relation.sexp_of_t r)

let test_wedges () =
  let fixture = Lazy.force fixture in
  let to_relation = relation_of_int fixture in
  let cases = to_list (member "wedges" fixture) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a0 = point_of_json (member "a0" c) in
    let ab1 = point_of_json (member "ab1" c) in
    let a2 = point_of_json (member "a2" c) in
    let b0 = point_of_json (member "b0" c) in
    let b2 = point_of_json (member "b2" c) in
    let expected_contains = bool_of_json_exn (member "contains" c) in
    let expected_intersects = bool_of_json_exn (member "intersects" c) in
    let expected_relation = to_relation (int_of_json_exn (member "wedge_relation" c)) in
    let actual_contains = S2.S2_wedge_relations.wedge_contains ~a0 ~ab1 ~a2 ~b0 ~b2 in
    let actual_intersects = S2.S2_wedge_relations.wedge_intersects ~a0 ~ab1 ~a2 ~b0 ~b2 in
    let actual_relation = S2.S2_wedge_relations.get_wedge_relation ~a0 ~ab1 ~a2 ~b0 ~b2 in
    check bool (label ^ " contains") expected_contains actual_contains;
    check bool (label ^ " intersects") expected_intersects actual_intersects;
    if not (S2.S2_wedge_relations.Relation.equal expected_relation actual_relation)
    then
      failf
        "%s: wedge_relation expected %s, got %s"
        label
        (relation_to_string expected_relation)
        (relation_to_string actual_relation))
;;

let () = run "s2_wedge_relations" [ "wedges", [ test_case "all" `Quick test_wedges ] ]
