(* Golden data produced by test/gen/s2edge_vector_shape.cc.

   Upstream C++ tests covered:
   - TEST(S2EdgeVectorShape, Empty)
   - TEST(S2EdgeVectorShape, EdgeAccess) - via the three_edges case
   - TEST(S2EdgeVectorShape, SingletonConstructor)

   Extra coverage beyond upstream:
   - points_dim0: constructed from a vector of degenerate edges and mutated with
     set_dimension(0) - exercises the vector constructor and a non-default dimension.
   - dim2: set_dimension(2) confirms the accessor reports whatever value was configured,
     not just the default.

   Deliberately omitted:
   - Move: OCaml values are immutable at the type level; no move semantics to exercise. *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2edge_vector_shape.json")
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

(* Construct the shape from the fixture's "input_edges" plus, where applicable, a
   [set_dimension] call to mirror the generator. We key on the case name. *)
let build_shape ~name ~(input_edges : S2.S2_shape.Edge.t array) =
  let shape =
    match name with
    | "singleton" ->
      let (#{ v0; v1 } : S2.S2_shape.Edge.t) = input_edges.(0) in
      S2.S2_edge_vector_shape.singleton ~v0 ~v1
    | "points_dim0" ->
      let s = S2.S2_edge_vector_shape.of_edges input_edges in
      S2.S2_edge_vector_shape.set_dimension s 0;
      s
    | "dim2" ->
      let (#{ v0; v1 } : S2.S2_shape.Edge.t) = input_edges.(0) in
      let s = S2.S2_edge_vector_shape.singleton ~v0 ~v1 in
      S2.S2_edge_vector_shape.set_dimension s 2;
      s
    | _ ->
      let s = S2.S2_edge_vector_shape.create () in
      for i = 0 to Array.length input_edges - 1 do
        let (#{ v0; v1 } : S2.S2_shape.Edge.t) = input_edges.(i) in
        S2.S2_edge_vector_shape.add s ~v0 ~v1
      done;
      s
  in
  shape
;;

let test_shapes () =
  List.iter
    (to_list (get "shapes"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let input_edges_json = to_list (member "input_edges" case) in
      let n_input = List.length input_edges_json in
      let input_edges : S2.S2_shape.Edge.t array =
        if n_input = 0
        then [||]
        else (
          let first = edge_of_json (List.hd_exn input_edges_json) in
          let arr = Array.create ~len:n_input first in
          List.iteri input_edges_json ~f:(fun i j -> arr.(i) <- edge_of_json j);
          arr)
      in
      let shape = build_shape ~name ~input_edges in
      let expected_num_edges = int_of_json_exn (member "num_edges" case) in
      let expected_num_chains = int_of_json_exn (member "num_chains" case) in
      let expected_dim = int_of_json_exn (member "dimension" case) in
      let expected_is_empty = bool_of_json_exn (member "is_empty" case) in
      let expected_is_full = bool_of_json_exn (member "is_full" case) in
      (check int)
        (name ^ " num_edges")
        expected_num_edges
        (S2.S2_edge_vector_shape.num_edges shape);
      (check int)
        (name ^ " num_chains")
        expected_num_chains
        (S2.S2_edge_vector_shape.num_chains shape);
      (check int)
        (name ^ " dimension")
        expected_dim
        (S2.S2_edge_vector_shape.dimension shape);
      let generic = S2.S2_edge_vector_shape.to_shape shape in
      (check int) (name ^ " shape num_edges") expected_num_edges generic.#num_edges;
      (check int) (name ^ " shape num_chains") expected_num_chains generic.#num_chains;
      (check int) (name ^ " shape dimension") expected_dim generic.#dimension;
      let is_empty = S2.S2_shape.is_empty generic in
      let is_full = S2.S2_shape.is_full generic in
      check_bool (name ^ " is_empty") ~expected:expected_is_empty ~actual:is_empty;
      check_bool (name ^ " is_full") ~expected:expected_is_full ~actual:is_full;
      let rp_json = member "reference_point" case in
      let rp = S2.S2_edge_vector_shape.reference_point shape in
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
          ~actual:(S2.S2_edge_vector_shape.edge shape i);
        check_edge
          (sprintf "%s shape edge[%d]" name i)
          ~expected
          ~actual:(generic.#edge i));
      let chains_json = to_list (member "chains" case) in
      List.iteri chains_json ~f:(fun i c_json ->
        let s, l = int_pair_of_json c_json in
        let expected = S2.S2_shape.Chain.create ~start:s ~length:l in
        let actual = S2.S2_edge_vector_shape.chain shape i in
        check_bool
          (sprintf "%s chain[%d] equal" name i)
          ~expected:true
          ~actual:(S2.S2_shape.Chain.equal expected actual));
      let cp_json = to_list (member "chain_positions" case) in
      List.iteri cp_json ~f:(fun e cp ->
        let cid, off = int_pair_of_json cp in
        let expected = S2.S2_shape.Chain_position.create ~chain_id:cid ~offset:off in
        let actual = S2.S2_edge_vector_shape.chain_position shape e in
        check_bool
          (sprintf "%s chain_position[%d] equal" name e)
          ~expected:true
          ~actual:(S2.S2_shape.Chain_position.equal expected actual));
      let ce_json = to_list (member "chain_edges" case) in
      let ce_idx = ref 0 in
      for i = 0 to expected_num_chains - 1 do
        let c = S2.S2_edge_vector_shape.chain shape i in
        for j = 0 to c.#length - 1 do
          let expected = edge_of_json (List.nth_exn ce_json !ce_idx) in
          check_edge
            (sprintf "%s chain_edge[%d,%d]" name i j)
            ~expected
            ~actual:(S2.S2_edge_vector_shape.chain_edge shape i j);
          incr ce_idx
        done
      done)
;;

let test_type_tag () =
  (check int) "type_tag" S2.S2_shape.Type_tag.none S2.S2_edge_vector_shape.type_tag
;;

let test_grow_beyond_default_capacity () =
  (* Default capacity is 8; push 20 edges to exercise [grow]. *)
  let shape = S2.S2_edge_vector_shape.create () in
  for i = 0 to 19 do
    let xf = Float_u.of_int i in
    let v0 = S2.S2_point.of_coords ~x:xf ~y:#0.0 ~z:#1.0 in
    let v1 = S2.S2_point.of_coords ~x:#0.0 ~y:xf ~z:#1.0 in
    S2.S2_edge_vector_shape.add shape ~v0 ~v1
  done;
  (check int) "post-grow num_edges" 20 (S2.S2_edge_vector_shape.num_edges shape);
  (check int) "post-grow num_chains" 20 (S2.S2_edge_vector_shape.num_chains shape);
  for i = 0 to 19 do
    let e = S2.S2_edge_vector_shape.edge shape i in
    let cp = S2.S2_edge_vector_shape.chain_position shape i in
    let c = S2.S2_edge_vector_shape.chain shape i in
    (check int) (sprintf "chain[%d] start" i) i c.#start;
    (check int) (sprintf "chain[%d] length" i) 1 c.#length;
    (check int) (sprintf "chain_position[%d] chain_id" i) i cp.#chain_id;
    (check int) (sprintf "chain_position[%d] offset" i) 0 cp.#offset;
    let e_alt = S2.S2_edge_vector_shape.chain_edge shape i 0 in
    check_edge (sprintf "chain_edge=edge[%d]" i) ~expected:e ~actual:e_alt
  done
;;

let () =
  Alcotest.run
    "S2_edge_vector_shape"
    [ "shapes", [ test_case "all" `Quick test_shapes ]
    ; "type_tag", [ test_case "constant" `Quick test_type_tag ]
    ; "grow", [ test_case "beyond default" `Quick test_grow_beyond_default_capacity ]
    ]
;;
