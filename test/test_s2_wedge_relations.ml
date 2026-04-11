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

(* ---------- Quickcheck: wedge-relation invariants ------------------------ *)

module Wedge_pair = struct
  type t =
    { a0 : S2.S2_point.t
    ; ab1 : S2.S2_point.t
    ; a2 : S2.S2_point.t
    ; b0 : S2.S2_point.t
    ; b2 : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let gen_unit_point rnd =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let rec loop () =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      if Float.((x *. x) +. (y *. y) +. (z *. z) < 1e-6)
      then loop ()
      else
        S2.S2_point.of_coords
          ~x:(Float_u.of_float x)
          ~y:(Float_u.of_float y)
          ~z:(Float_u.of_float z)
    in
    loop ()
  ;;

  (* Wedge endpoints must be distinct from the shared vertex; draw until the
     simple equality check passes. *)
  let gen_distinct rnd ~from =
    let rec loop () =
      let p = gen_unit_point rnd in
      if S2.S2_point.equal p from then loop () else p
    in
    loop ()
  ;;

  let quickcheck_generator =
    Base_quickcheck.Generator.create (fun ~size:_ ~random:rnd ->
      let ab1 = gen_unit_point rnd in
      let a0 = gen_distinct rnd ~from:ab1 in
      let a2 = gen_distinct rnd ~from:ab1 in
      let b0 = gen_distinct rnd ~from:ab1 in
      let b2 = gen_distinct rnd ~from:ab1 in
      { a0; ab1; a2; b0; b2 })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

let quickcheck_relation_exhaustive () =
  Base_quickcheck.Test.run_exn
    (module Wedge_pair)
    ~config:qc_config
    ~f:(fun { Wedge_pair.a0; ab1; a2; b0; b2 } ->
      (* Any call must return one of the five relations; pattern matching
         exhausts the tags so the match itself is the proof. *)
      match S2.S2_wedge_relations.get_wedge_relation ~a0 ~ab1 ~a2 ~b0 ~b2 with
      | Equals | Properly_contains | Is_properly_contained | Properly_overlaps
      | Is_disjoint -> ())
;;

let quickcheck_relation_reflexive_equals () =
  Base_quickcheck.Test.run_exn
    (module Wedge_pair)
    ~config:qc_config
    ~f:(fun { Wedge_pair.a0; ab1; a2; _ } ->
      let r = S2.S2_wedge_relations.get_wedge_relation ~a0 ~ab1 ~a2 ~b0:a0 ~b2:a2 in
      match r with
      | Equals -> ()
      | other ->
        Alcotest.failf
          "wedge vs itself: expected Equals, got %s"
          (Sexp.to_string (S2.S2_wedge_relations.Relation.sexp_of_t other)))
;;

let quickcheck_equals_symmetric () =
  Base_quickcheck.Test.run_exn
    (module Wedge_pair)
    ~config:qc_config
    ~f:(fun { Wedge_pair.a0; ab1; a2; b0; b2 } ->
      let ab = S2.S2_wedge_relations.get_wedge_relation ~a0 ~ab1 ~a2 ~b0 ~b2 in
      match ab with
      | Equals ->
        let ba =
          S2.S2_wedge_relations.get_wedge_relation ~a0:b0 ~ab1 ~a2:b2 ~b0:a0 ~b2:a2
        in
        (match ba with
         | Equals -> ()
         | other ->
           Alcotest.failf
             "Equals not symmetric: got %s"
             (Sexp.to_string (S2.S2_wedge_relations.Relation.sexp_of_t other)))
      | _ -> ())
;;

let () =
  run
    "s2_wedge_relations"
    [ "wedges", [ test_case "all" `Quick test_wedges ]
    ; ( "quickcheck"
      , [ test_case "relation_exhaustive" `Quick quickcheck_relation_exhaustive
        ; test_case "relation_reflexive_equals" `Quick quickcheck_relation_reflexive_equals
        ; test_case "equals_symmetric" `Quick quickcheck_equals_symmetric
        ] )
    ]
;;
