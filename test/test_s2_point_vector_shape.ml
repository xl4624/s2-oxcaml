(* Golden data produced by test/gen/s2point_vector_shape.cc.

   Upstream C++ tests covered:
   - TEST(S2PointVectorShape, Empty)
   - TEST(S2PointVectorShape, ConstructionAndAccess) - via the grid case
   - TEST(S2PointVectorShape, ChainIteratorWorks) - via the three_points case
   - TEST(S2PointVectorShape, ChainVertexIteratorWorks) - the per-point chain/edge access
     is checked by shared shape summary assertions.

   Deliberately omitted:
   - Move: OCaml values have no move semantics to exercise.
   - Encoding / Init: encoding is not yet ported. *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2point_vector_shape.json")
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

let points_array_of_json j =
  let items = to_list j in
  match items with
  | [] -> [||]
  | first :: _ ->
    let n = List.length items in
    let arr = Array.create ~len:n (s2_point_of_json first) in
    List.iteri items ~f:(fun i p -> arr.(i) <- s2_point_of_json p);
    arr
;;

let test_shapes () =
  List.iter
    (to_list (get "shapes"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let input_points = points_array_of_json (member "input_points" case) in
      let shape = S2.S2_point_vector_shape.of_points input_points in
      let expected_num_points = int_of_json_exn (member "num_points" case) in
      let expected_num_edges = int_of_json_exn (member "num_edges" case) in
      let expected_num_chains = int_of_json_exn (member "num_chains" case) in
      let expected_dim = int_of_json_exn (member "dimension" case) in
      let expected_is_empty = bool_of_json_exn (member "is_empty" case) in
      let expected_is_full = bool_of_json_exn (member "is_full" case) in
      (check int)
        (name ^ " num_points")
        expected_num_points
        (S2.S2_point_vector_shape.num_points shape);
      (check int)
        (name ^ " num_edges")
        expected_num_edges
        (S2.S2_point_vector_shape.num_edges shape);
      (check int)
        (name ^ " num_chains")
        expected_num_chains
        (S2.S2_point_vector_shape.num_chains shape);
      (check int)
        (name ^ " dimension")
        expected_dim
        (S2.S2_point_vector_shape.dimension shape);
      let generic = S2.S2_point_vector_shape.to_shape shape in
      (check int) (name ^ " shape num_edges") expected_num_edges generic.#num_edges;
      (check int) (name ^ " shape num_chains") expected_num_chains generic.#num_chains;
      (check int) (name ^ " shape dimension") expected_dim generic.#dimension;
      let is_empty = S2.S2_shape.is_empty generic in
      let is_full = S2.S2_shape.is_full generic in
      check_bool (name ^ " is_empty") ~expected:expected_is_empty ~actual:is_empty;
      check_bool (name ^ " is_full") ~expected:expected_is_full ~actual:is_full;
      let rp_json = member "reference_point" case in
      let rp = S2.S2_point_vector_shape.reference_point shape in
      check_bool
        (name ^ " reference_point contained")
        ~expected:(bool_of_json_exn (member "contained" rp_json))
        ~actual:rp.#contained;
      check_point
        (name ^ " reference_point point")
        ~expected:(s2_point_of_json (member "point" rp_json))
        ~actual:rp.#point;
      let points_json = to_list (member "points" case) in
      List.iteri points_json ~f:(fun i p_json ->
        let expected = s2_point_of_json p_json in
        check_point
          (sprintf "%s point[%d]" name i)
          ~expected
          ~actual:(S2.S2_point_vector_shape.point shape i));
      let edges_json = to_list (member "edges" case) in
      List.iteri edges_json ~f:(fun i e_json ->
        let expected = edge_of_json e_json in
        check_edge
          (sprintf "%s edge[%d]" name i)
          ~expected
          ~actual:(S2.S2_point_vector_shape.edge shape i);
        check_edge
          (sprintf "%s shape edge[%d]" name i)
          ~expected
          ~actual:(generic.#edge i));
      let chains_json = to_list (member "chains" case) in
      List.iteri chains_json ~f:(fun i c_json ->
        let s, l = int_pair_of_json c_json in
        let expected = S2.S2_shape.Chain.create ~start:s ~length:l in
        let actual = S2.S2_point_vector_shape.chain shape i in
        check_bool
          (sprintf "%s chain[%d] equal" name i)
          ~expected:true
          ~actual:(S2.S2_shape.Chain.equal expected actual));
      let cp_json = to_list (member "chain_positions" case) in
      List.iteri cp_json ~f:(fun e cp ->
        let cid, off = int_pair_of_json cp in
        let expected = S2.S2_shape.Chain_position.create ~chain_id:cid ~offset:off in
        let actual = S2.S2_point_vector_shape.chain_position shape e in
        check_bool
          (sprintf "%s chain_position[%d] equal" name e)
          ~expected:true
          ~actual:(S2.S2_shape.Chain_position.equal expected actual));
      let ce_json = to_list (member "chain_edges" case) in
      let ce_idx = ref 0 in
      for i = 0 to expected_num_chains - 1 do
        let c = S2.S2_point_vector_shape.chain shape i in
        for j = 0 to c.#length - 1 do
          let expected = edge_of_json (List.nth_exn ce_json !ce_idx) in
          check_edge
            (sprintf "%s chain_edge[%d,%d]" name i j)
            ~expected
            ~actual:(S2.S2_point_vector_shape.chain_edge shape i j);
          incr ce_idx
        done
      done)
;;

let test_type_tag () =
  (check int) "type_tag" S2.S2_shape.Type_tag.none S2.S2_point_vector_shape.type_tag
;;

let test_create_empty () =
  let shape = S2.S2_point_vector_shape.create () in
  (check int) "empty num_points" 0 (S2.S2_point_vector_shape.num_points shape);
  (check int) "empty num_edges" 0 (S2.S2_point_vector_shape.num_edges shape);
  (check int) "empty num_chains" 0 (S2.S2_point_vector_shape.num_chains shape);
  (check int) "empty dimension" 0 (S2.S2_point_vector_shape.dimension shape);
  let generic = S2.S2_point_vector_shape.to_shape shape in
  check_bool "empty is_empty" ~expected:true ~actual:(S2.S2_shape.is_empty generic);
  check_bool "empty is_full" ~expected:false ~actual:(S2.S2_shape.is_full generic)
;;

let test_of_points_copies_input () =
  (* Mutating the caller's array must not affect the shape's view. *)
  let p0 = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let p1 = S2.S2_point.of_coords ~x:#0.0 ~y:#1.0 ~z:#0.0 in
  let replacement = S2.S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0 in
  let arr = [| p0; p1 |] in
  let shape = S2.S2_point_vector_shape.of_points arr in
  arr.(0) <- replacement;
  check_point
    "point[0] unchanged"
    ~expected:p0
    ~actual:(S2.S2_point_vector_shape.point shape 0);
  check_point
    "point[1] unchanged"
    ~expected:p1
    ~actual:(S2.S2_point_vector_shape.point shape 1)
;;

let () =
  Alcotest.run
    "S2_point_vector_shape"
    [ "shapes", [ test_case "all" `Quick test_shapes ]
    ; "type_tag", [ test_case "constant" `Quick test_type_tag ]
    ; "create", [ test_case "empty" `Quick test_create_empty ]
    ; "of_points", [ test_case "copies input" `Quick test_of_points_copies_input ]
    ]
;;
