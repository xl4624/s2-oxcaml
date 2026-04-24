(* Golden data produced by test/gen/s2wrapped_shape.cc.

   Upstream C++ tests covered:
   - TEST(S2WrappedShape, Coverage) - the three-loop lax polygon case.

   Extra coverage:
   - An empty polygon (no loops) exercises the zero-edge / full-polygon
     decision in GetReferencePoint via the wrapper.
   - A multi-loop polygon (outer shell + hole) exercises multi-chain dispatch.

   Deliberately omitted:
   - Encoding: S2WrappedShape has no wire format (type_tag is kNoTypeTag).
   - Non-shape delegation: upstream has no other methods to exercise. *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2wrapped_shape.json")
let get key = member key (Lazy.force fixture)
let s2_point_of_json j = r3_vector_of_json j
let check_point msg ~expected ~actual = check_r3_vector_exact msg ~expected ~actual

let check_edge msg ~(expected : S2.S2_shape.Edge.t) ~(actual : S2.S2_shape.Edge.t) =
  check_point (msg ^ " v0") ~expected:expected.#v0 ~actual:actual.#v0;
  check_point (msg ^ " v1") ~expected:expected.#v1 ~actual:actual.#v1
;;

let edge_of_json j =
  let v0 = s2_point_of_json (member "v0" j) in
  let v1 = s2_point_of_json (member "v1" j) in
  S2.S2_shape.Edge.create ~v0 ~v1
;;

let int_pair_of_json j =
  match to_list j with
  | [ a; b ] -> int_of_json_exn a, int_of_json_exn b
  | _ ->
    (match failwith "expected [a, b]" with
     | (_ : Nothing.t) -> .)
;;

let vertices_of_json j =
  let xs = to_list j in
  let n = List.length xs in
  if n = 0
  then [||]
  else (
    let first = s2_point_of_json (List.hd_exn xs) in
    let arr = Array.create ~len:n first in
    List.iteri xs ~f:(fun i p -> arr.(i) <- s2_point_of_json p);
    arr)
;;

let loops_of_json j =
  let xs = to_list j in
  let n = List.length xs in
  if n = 0
  then [||]
  else (
    let arr = Array.create ~len:n [||] in
    List.iteri xs ~f:(fun i l -> arr.(i) <- vertices_of_json l);
    arr)
;;

let test_shapes () =
  List.iter
    (to_list (get "shapes"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let loops = loops_of_json (member "loops" case) in
      let lax = S2.S2_lax_polygon.of_loops loops in
      let wrapped = S2.S2_wrapped_shape.create (S2.S2_lax_polygon.to_shape lax) in
      let expected_num_edges = int_of_json_exn (member "num_edges" case) in
      let expected_num_chains = int_of_json_exn (member "num_chains" case) in
      let expected_dim = int_of_json_exn (member "dimension" case) in
      let expected_is_empty = bool_of_json_exn (member "is_empty" case) in
      let expected_is_full = bool_of_json_exn (member "is_full" case) in
      let expected_type_tag = int_of_json_exn (member "type_tag" case) in
      (check int)
        (name ^ " num_edges")
        expected_num_edges
        (S2.S2_wrapped_shape.num_edges wrapped);
      (check int)
        (name ^ " num_chains")
        expected_num_chains
        (S2.S2_wrapped_shape.num_chains wrapped);
      (check int)
        (name ^ " dimension")
        expected_dim
        (S2.S2_wrapped_shape.dimension wrapped);
      (check int) (name ^ " type_tag") expected_type_tag S2.S2_wrapped_shape.type_tag;
      let generic = S2.S2_wrapped_shape.to_shape wrapped in
      (check int) (name ^ " shape num_edges") expected_num_edges generic.#num_edges;
      (check int) (name ^ " shape num_chains") expected_num_chains generic.#num_chains;
      (check int) (name ^ " shape dimension") expected_dim generic.#dimension;
      (check int) (name ^ " shape type_tag") expected_type_tag generic.#type_tag;
      let is_empty = S2.S2_shape.is_empty generic in
      let is_full = S2.S2_shape.is_full generic in
      check_bool (name ^ " is_empty") ~expected:expected_is_empty ~actual:is_empty;
      check_bool (name ^ " is_full") ~expected:expected_is_full ~actual:is_full;
      let rp_json = member "reference_point" case in
      let rp = S2.S2_wrapped_shape.reference_point wrapped in
      check_bool
        (name ^ " reference_point contained")
        ~expected:(bool_of_json_exn (member "contained" rp_json))
        ~actual:rp.#contained;
      check_point
        (name ^ " reference_point point")
        ~expected:(s2_point_of_json (member "point" rp_json))
        ~actual:rp.#point;
      let edges_json = to_list (member "edges" case) in
      List.iteri edges_json ~f:(fun i e_json ->
        let expected = edge_of_json e_json in
        check_edge
          (sprintf "%s edge[%d]" name i)
          ~expected
          ~actual:(S2.S2_wrapped_shape.edge wrapped i);
        check_edge
          (sprintf "%s shape edge[%d]" name i)
          ~expected
          ~actual:(generic.#edge i));
      let chains_json = to_list (member "chains" case) in
      List.iteri chains_json ~f:(fun i c_json ->
        let s, l = int_pair_of_json c_json in
        let expected = S2.S2_shape.Chain.create ~start:s ~length:l in
        let actual = S2.S2_wrapped_shape.chain wrapped i in
        check_bool
          (sprintf "%s chain[%d] equal" name i)
          ~expected:true
          ~actual:(S2.S2_shape.Chain.equal expected actual));
      let cp_json = to_list (member "chain_positions" case) in
      List.iteri cp_json ~f:(fun e cp ->
        let cid, off = int_pair_of_json cp in
        let expected = S2.S2_shape.Chain_position.create ~chain_id:cid ~offset:off in
        let actual = S2.S2_wrapped_shape.chain_position wrapped e in
        check_bool
          (sprintf "%s chain_position[%d] equal" name e)
          ~expected:true
          ~actual:(S2.S2_shape.Chain_position.equal expected actual));
      let ce_json = to_list (member "chain_edges" case) in
      let ce_idx = ref 0 in
      for i = 0 to expected_num_chains - 1 do
        let c = S2.S2_wrapped_shape.chain wrapped i in
        for j = 0 to c.#length - 1 do
          let expected = edge_of_json (List.nth_exn ce_json !ce_idx) in
          check_edge
            (sprintf "%s chain_edge[%d,%d]" name i j)
            ~expected
            ~actual:(S2.S2_wrapped_shape.chain_edge wrapped i j);
          incr ce_idx
        done
      done)
;;

let test_type_tag () =
  let lax =
    S2.S2_lax_polygon.of_loops
      [| [| S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0
          ; S2.S2_point.of_coords ~x:#0.0 ~y:#1.0 ~z:#0.0
          ; S2.S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0
         |]
      |]
  in
  let backing_tag = S2.S2_lax_polygon.type_tag in
  check_bool
    "backing shape has a non-none type tag"
    ~expected:true
    ~actual:(backing_tag <> S2.S2_shape.Type_tag.none);
  let wrapped = S2.S2_wrapped_shape.create (S2.S2_lax_polygon.to_shape lax) in
  (check int)
    "wrapped type_tag = none"
    S2.S2_shape.Type_tag.none
    S2.S2_wrapped_shape.type_tag;
  let generic = S2.S2_wrapped_shape.to_shape wrapped in
  (check int) "wrapped shape.type_tag = none" S2.S2_shape.Type_tag.none generic.#type_tag
;;

let () =
  Alcotest.run
    "S2_wrapped_shape"
    [ "shapes", [ test_case "all" `Quick test_shapes ]
    ; "type_tag", [ test_case "wraps to none" `Quick test_type_tag ]
    ]
;;
