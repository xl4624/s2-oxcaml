(* Golden data produced by test/gen/s2lax_polygon.cc.

   Upstream C++ tests covered:
   - TEST(S2LaxPolygonShape, EmptyPolygon)
   - TEST(S2LaxPolygonShape, FullPolygon)
   - TEST(S2LaxPolygonShape, SingleVertexPolygon)
   - TEST(S2LaxPolygonShape, SingleLoopPolygon)
   - TEST(S2LaxPolygonShape, MultiLoopPolygon)
   - TEST(S2LaxPolygonShape, DegenerateLoops)
   - TEST(S2LaxPolygonShape, InvertedLoops)

   The "full" case is constructed in the generator as a single zero-vertex loop,
   which is the representation S2LaxPolygonShape uses internally for a full
   polygon. The upstream test builds it through the S2Polygon constructor, but
   the resulting S2LaxPolygonShape state is identical.

   Deliberately omitted:
   - Move: OCaml values are immutable at this level.
   - MultiLoopS2Polygon: depends on S2Polygon, which is not ported.
   - ManyLoopPolygon: random, not amenable to fixture-based testing.
   - CompareToS2Loop: depends on S2Loop and fractal testing.
   - Encoding tests (S2CoderWorks, InsufficientDataInEncoder, etc.): encoding
     is not ported.
   - ChainIteratorWorks / ChainVertexIteratorWorks: iterators are not part of
     the OCaml shape API.
*)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2lax_polygon.json")
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

let int_pair_of_json j =
  match to_list j with
  | [ a; b ] -> int_of_json_exn a, int_of_json_exn b
  | _ ->
    (match failwith "expected [a, b]" with
     | (_ : Nothing.t) -> .)
;;

let test_shapes () =
  List.iter
    (to_list (get "shapes"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let loops = loops_of_json (member "loops" case) in
      let shape = S2.S2_lax_polygon.of_loops loops in
      let expected_num_loops = int_of_json_exn (member "num_loops" case) in
      let expected_num_vertices = int_of_json_exn (member "num_vertices" case) in
      let expected_num_edges = int_of_json_exn (member "num_edges" case) in
      let expected_num_chains = int_of_json_exn (member "num_chains" case) in
      let expected_dim = int_of_json_exn (member "dimension" case) in
      let expected_is_empty = bool_of_json_exn (member "is_empty" case) in
      let expected_is_full = bool_of_json_exn (member "is_full" case) in
      (check int)
        (name ^ " num_loops")
        expected_num_loops
        (S2.S2_lax_polygon.num_loops shape);
      (check int)
        (name ^ " num_vertices")
        expected_num_vertices
        (S2.S2_lax_polygon.num_vertices shape);
      (check int)
        (name ^ " num_edges")
        expected_num_edges
        (S2.S2_lax_polygon.num_edges shape);
      (check int)
        (name ^ " num_chains")
        expected_num_chains
        (S2.S2_lax_polygon.num_chains shape);
      (check int) (name ^ " dimension") expected_dim (S2.S2_lax_polygon.dimension shape);
      (* num_loop_vertices *)
      let expected_nlv = to_list (member "num_loop_vertices" case) in
      List.iteri expected_nlv ~f:(fun i nlv ->
        (check int)
          (sprintf "%s num_loop_vertices[%d]" name i)
          (int_of_json_exn nlv)
          (S2.S2_lax_polygon.num_loop_vertices shape i));
      (* loop_vertex for every (i, j) *)
      for i = 0 to expected_num_loops - 1 do
        let n = S2.S2_lax_polygon.num_loop_vertices shape i in
        for j = 0 to n - 1 do
          check_point
            (sprintf "%s loop_vertex[%d,%d]" name i j)
            ~expected:loops.(i).(j)
            ~actual:(S2.S2_lax_polygon.loop_vertex shape i j)
        done
      done;
      (* Exercise the generic shape view for the same queries. *)
      let generic = S2.S2_lax_polygon.to_shape shape in
      (check int) (name ^ " shape num_edges") expected_num_edges generic.#num_edges;
      (check int) (name ^ " shape num_chains") expected_num_chains generic.#num_chains;
      (check int) (name ^ " shape dimension") expected_dim generic.#dimension;
      let is_empty = S2.S2_shape.is_empty generic in
      let is_full = S2.S2_shape.is_full generic in
      check_bool (name ^ " is_empty") ~expected:expected_is_empty ~actual:is_empty;
      check_bool (name ^ " is_full") ~expected:expected_is_full ~actual:is_full;
      (* reference point *)
      let rp_json = member "reference_point" case in
      let rp = S2.S2_lax_polygon.reference_point shape in
      check_bool
        (name ^ " reference_point contained")
        ~expected:(bool_of_json_exn (member "contained" rp_json))
        ~actual:rp.#contained;
      check_point
        (name ^ " reference_point point")
        ~expected:(s2_point_of_json (member "point" rp_json))
        ~actual:rp.#point;
      (* edges *)
      let edges_json = to_list (member "edges" case) in
      List.iteri edges_json ~f:(fun i e_json ->
        let expected = edge_of_json e_json in
        check_edge
          (sprintf "%s edge[%d]" name i)
          ~expected
          ~actual:(S2.S2_lax_polygon.edge shape i);
        check_edge
          (sprintf "%s shape edge[%d]" name i)
          ~expected
          ~actual:(generic.#edge i));
      (* chains *)
      let chains_json = to_list (member "chains" case) in
      List.iteri chains_json ~f:(fun i c_json ->
        let s, l = int_pair_of_json c_json in
        let expected = S2.S2_shape.Chain.create ~start:s ~length:l in
        let actual = S2.S2_lax_polygon.chain shape i in
        check_bool
          (sprintf "%s chain[%d] equal" name i)
          ~expected:true
          ~actual:(S2.S2_shape.Chain.equal expected actual));
      (* chain positions *)
      let cp_json = to_list (member "chain_positions" case) in
      List.iteri cp_json ~f:(fun e cp ->
        let cid, off = int_pair_of_json cp in
        let expected = S2.S2_shape.Chain_position.create ~chain_id:cid ~offset:off in
        let actual = S2.S2_lax_polygon.chain_position shape e in
        check_bool
          (sprintf "%s chain_position[%d] equal" name e)
          ~expected:true
          ~actual:(S2.S2_shape.Chain_position.equal expected actual));
      (* chain edges: flattened across chains, one chain at a time *)
      let ce_json = to_list (member "chain_edges" case) in
      let ce_idx = ref 0 in
      for i = 0 to expected_num_chains - 1 do
        let c = S2.S2_lax_polygon.chain shape i in
        for j = 0 to c.#length - 1 do
          let expected = edge_of_json (List.nth_exn ce_json !ce_idx) in
          check_edge
            (sprintf "%s chain_edge[%d,%d]" name i j)
            ~expected
            ~actual:(S2.S2_lax_polygon.chain_edge shape i j);
          incr ce_idx
        done
      done)
;;

let test_type_tag () =
  let expected = int_of_json_exn (get "type_tag") in
  (check int) "type_tag" expected S2.S2_lax_polygon.type_tag
;;

let () =
  Alcotest.run
    "S2_lax_polygon"
    [ "shapes", [ test_case "all" `Quick test_shapes ]
    ; "type_tag", [ test_case "constant" `Quick test_type_tag ]
    ]
;;
