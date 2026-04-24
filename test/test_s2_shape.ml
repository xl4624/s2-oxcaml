(* S2Shape is an abstract base class in the upstream C++ library, so there is no
   dedicated s2shape_test.cc. This file exercises the concrete value types defined
   on S2Shape (Edge, Chain, ChainPosition, ReferencePoint) and its type-tag
   constants against golden data produced by test/gen/s2shape.cc.

   Coverage:
   - Edge.reversed / is_degenerate / incoming / outgoing / incident_on / compare / equal
   - Chain.equal, Chain_position.equal
   - Reference_point.contained / create / equal
   - Type_tag constants

   Deliberately omitted:
   - is_empty / is_full: these are trivially derived from a shape's num_edges,
     dimension, and num_chains. They are covered by concrete shape modules
     (s2_lax_polyline, s2_lax_polygon, ...) as they come online. *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2shape.json")
let get key = member key (Lazy.force fixture)
let s2_point_of_json j = r3_vector_of_json j
let check_point msg ~expected ~actual = check_r3_vector_exact msg ~expected ~actual

let edge_of_json j =
  let v0 = s2_point_of_json (member "v0" j) in
  let v1 = s2_point_of_json (member "v1" j) in
  S2.S2_shape.Edge.create ~v0 ~v1
;;

let check_edge msg ~(expected : S2.S2_shape.Edge.t) ~(actual : S2.S2_shape.Edge.t) =
  check_point (msg ^ " v0") ~expected:expected.#v0 ~actual:actual.#v0;
  check_point (msg ^ " v1") ~expected:expected.#v1 ~actual:actual.#v1
;;

let test_edge_helpers () =
  List.iter
    (to_list (get "edge_helpers"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let e = edge_of_json (member "edge" case) in
      let probe = s2_point_of_json (member "probe" case) in
      let expected_reversed = edge_of_json (member "reversed" case) in
      check_edge
        (name ^ " reversed")
        ~expected:expected_reversed
        ~actual:(S2.S2_shape.Edge.reversed e);
      check_bool
        (name ^ " is_degenerate")
        ~expected:(bool_of_json_exn (member "is_degenerate" case))
        ~actual:(S2.S2_shape.Edge.is_degenerate e);
      check_bool
        (name ^ " incoming")
        ~expected:(bool_of_json_exn (member "incoming" case))
        ~actual:(S2.S2_shape.Edge.incoming e probe);
      check_bool
        (name ^ " outgoing")
        ~expected:(bool_of_json_exn (member "outgoing" case))
        ~actual:(S2.S2_shape.Edge.outgoing e probe);
      check_bool
        (name ^ " incident_on")
        ~expected:(bool_of_json_exn (member "incident_on" case))
        ~actual:(S2.S2_shape.Edge.incident_on e probe))
;;

let test_edge_cmp () =
  List.iter
    (to_list (get "edge_cmp"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let a = edge_of_json (member "a" case) in
      let b = edge_of_json (member "b" case) in
      check_bool
        (name ^ " equal")
        ~expected:(bool_of_json_exn (member "equal" case))
        ~actual:(S2.S2_shape.Edge.equal a b);
      check_bool
        (name ^ " less")
        ~expected:(bool_of_json_exn (member "less" case))
        ~actual:(S2.S2_shape.Edge.compare a b < 0))
;;

let int_pair_of_json j =
  match to_list j with
  | [ a; b ] -> int_of_json_exn a, int_of_json_exn b
  | _ ->
    (match failwith "expected [a, b]" with
     | (_ : Nothing.t) -> .)
;;

let test_chain_equal () =
  List.iter
    (to_list (get "chain_equal"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let a_start, a_length = int_pair_of_json (member "a" case) in
      let b_start, b_length = int_pair_of_json (member "b" case) in
      let a = S2.S2_shape.Chain.create ~start:a_start ~length:a_length in
      let b = S2.S2_shape.Chain.create ~start:b_start ~length:b_length in
      check_bool
        (name ^ " equal")
        ~expected:(bool_of_json_exn (member "equal" case))
        ~actual:(S2.S2_shape.Chain.equal a b))
;;

let test_chain_position_equal () =
  List.iter
    (to_list (get "chain_position_equal"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let a_cid, a_off = int_pair_of_json (member "a" case) in
      let b_cid, b_off = int_pair_of_json (member "b" case) in
      let a = S2.S2_shape.Chain_position.create ~chain_id:a_cid ~offset:a_off in
      let b = S2.S2_shape.Chain_position.create ~chain_id:b_cid ~offset:b_off in
      check_bool
        (name ^ " equal")
        ~expected:(bool_of_json_exn (member "equal" case))
        ~actual:(S2.S2_shape.Chain_position.equal a b))
;;

let test_reference_point () =
  List.iter
    (to_list (get "reference_point"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let kind = string_of_json_exn (member "kind" case) in
      let expected_point = s2_point_of_json (member "point" case) in
      let expected_contained = bool_of_json_exn (member "contained" case) in
      match kind with
      | "contained" ->
        let contained_input = bool_of_json_exn (member "contained_input" case) in
        let rp = S2.S2_shape.Reference_point.contained contained_input in
        check_point (name ^ " point") ~expected:expected_point ~actual:rp.#point;
        check_bool
          (name ^ " contained")
          ~expected:expected_contained
          ~actual:rp.#contained
      | "with_point" ->
        let input_point = s2_point_of_json (member "input_point" case) in
        let input_contained = bool_of_json_exn (member "input_contained" case) in
        let rp =
          S2.S2_shape.Reference_point.create ~point:input_point ~contained:input_contained
        in
        check_point (name ^ " point") ~expected:expected_point ~actual:rp.#point;
        check_bool
          (name ^ " contained")
          ~expected:expected_contained
          ~actual:rp.#contained;
        check_bool
          (name ^ " equal_to_self")
          ~expected:(bool_of_json_exn (member "equal_to_self" case))
          ~actual:(S2.S2_shape.Reference_point.equal rp rp);
        let flipped =
          S2.S2_shape.Reference_point.create
            ~point:input_point
            ~contained:(not input_contained)
        in
        check_bool
          (name ^ " equal_to_flipped")
          ~expected:(bool_of_json_exn (member "equal_to_flipped" case))
          ~actual:(S2.S2_shape.Reference_point.equal rp flipped)
      | other ->
        (match failwith (sprintf "unknown reference_point kind: %s" other) with
         | (_ : Nothing.t) -> .))
;;

let test_type_tags () =
  let tags = get "type_tags" in
  (check int)
    "k_no_type_tag"
    (int_of_json_exn (member "k_no_type_tag" tags))
    S2.S2_shape.Type_tag.none;
  (check int)
    "k_next_available_type_tag"
    (int_of_json_exn (member "k_next_available_type_tag" tags))
    S2.S2_shape.Type_tag.next_available;
  (check int)
    "k_min_user_type_tag"
    (int_of_json_exn (member "k_min_user_type_tag" tags))
    S2.S2_shape.Type_tag.min_user
;;

let () =
  Alcotest.run
    "S2_shape"
    [ "edge", [ test_case "helpers" `Quick test_edge_helpers ]
    ; "edge_cmp", [ test_case "compare/equal" `Quick test_edge_cmp ]
    ; "chain", [ test_case "equal" `Quick test_chain_equal ]
    ; "chain_position", [ test_case "equal" `Quick test_chain_position_equal ]
    ; "reference_point", [ test_case "all" `Quick test_reference_point ]
    ; "type_tags", [ test_case "constants" `Quick test_type_tags ]
    ]
;;
