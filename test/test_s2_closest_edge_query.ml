(* C++ test parity: s2geometry/src/s2/s2closest_edge_query_test.cc (selected
   tests) and extra coverage.  Golden data from
   test/gen/s2closest_edge_query.cc.

   Covered:
   -  Empty index: GetDistance returns infinity / FindClosestEdge returns
      empty.
   -  Small point shape (brute-force path): closest edge, top-k, distance
      limit, predicate helpers.
   -  Polygon interior: include_interiors affects whether targets inside a
      polygon have distance zero and edge_id == -1.
   -  Edge target against a polyline index.
   -  Cell target against a loop index.
   -  Shape_index target: both nearby and far indices.
   -  Regular 64-gon loop exercising the optimized cell-descent path and
      conservative/inclusive distance limits.

   Not covered (deferred):
   -  VisitClosestEdges / VisitClosestShapes visitor APIs (our module does
      not expose them).
   -  ShapeFilter predicate (not exposed in our API).
   -  Randomized fuzz tests (would require matching random generation). *)

open Core
open Alcotest
open Test_helpers

let fixture = lazy (load_fixture "s2closest_edge_query.json")
let point_of_json = r3_vector_of_json

let build_shape j : S2.S2_shape.t =
  let kind = string_of_json_exn (member "kind" j) in
  let vjs = to_list (member "vertices" j) in
  let n = List.length vjs in
  let vs = Array.create ~len:n S2.S2_point.origin in
  List.iteri vjs ~f:(fun i vj -> vs.(i) <- point_of_json vj);
  match kind with
  | "polyline" -> S2.S2_polyline.to_shape (S2.S2_polyline.of_vertices vs)
  | "loop" -> S2.S2_loop.to_shape (S2.S2_loop.of_vertices vs)
  | "points" ->
    (* Build a degenerate "shape" from a list of points by treating each
       point as an edge with v0 = v1.  S2_polyline handles degenerate
       points poorly, so we synthesize a lax_polyline with repeated
       vertices.  The S2 library uses S2PointVectorShape for this; we
       emulate it by threading a lax_polyline where each point is both an
       edge start and end.  Alternatively, we build an [S2_shape.t]
       directly with [num_edges = length vs], [dimension = 0], and the
       edge accessor returning each point twice. *)
    let num_edges = Array.length vs in
    let dimension = 0 in
    let num_chains = num_edges in
    let edge i : S2.S2_shape.Edge.t = #{ v0 = vs.(i); v1 = vs.(i) } in
    let chain i : S2.S2_shape.Chain.t = #{ start = i; length = 1 } in
    let chain_edge i _ = edge i in
    let chain_position i : S2.S2_shape.Chain_position.t = #{ chain_id = i; offset = 0 } in
    let reference_point = S2.S2_shape.Reference_point.contained false in
    (#{ num_edges
      ; num_chains
      ; dimension
      ; type_tag = 0
      ; reference_point
      ; edge
      ; chain
      ; chain_edge
      ; chain_position
      }
     : S2.S2_shape.t)
  | other ->
    (match failwith (sprintf "unknown shape kind %S" other) with
     | (_ : Nothing.t) -> .)
;;

let build_index shape_jsons =
  let index = S2.S2_shape_index.create () in
  List.iter shape_jsons ~f:(fun j ->
    let (_ : int) = S2.S2_shape_index.add index (build_shape j) in
    ());
  S2.S2_shape_index.build index;
  index
;;

let build_target j : S2.S2_closest_edge_query.Target.t =
  let kind = string_of_json_exn (member "kind" j) in
  match kind with
  | "point" -> S2.S2_closest_edge_query.Target.point (point_of_json (member "p" j))
  | "edge" ->
    S2.S2_closest_edge_query.Target.edge
      (point_of_json (member "a" j))
      (point_of_json (member "b" j))
  | "cell" ->
    let id = S2.S2_cell_id.from_token (string_of_json_exn (member "cell_id" j)) in
    S2.S2_closest_edge_query.Target.cell (S2.S2_cell.of_cell_id id)
  | "shape_index" ->
    let sub_shapes = to_list (member "shapes" j) in
    let sub_index = build_index sub_shapes in
    let include_interiors = bool_of_json_exn (member "include_interiors" j) in
    S2.S2_closest_edge_query.Target.shape_index ~include_interiors sub_index
  | other ->
    (match failwith (sprintf "unknown target kind %S" other) with
     | (_ : Nothing.t) -> .)
;;

let apply_options j =
  let opts =
    S2.S2_closest_edge_query.Options.create
      ~include_interiors:(bool_of_json_exn (member "include_interiors" j))
      ~use_brute_force:(bool_of_json_exn (member "use_brute_force" j))
      ()
  in
  let opts =
    S2.S2_closest_edge_query.Options.with_max_results
      opts
      (int_of_json_exn (member "max_results" j))
  in
  let opts =
    match member "max_distance_deg" j with
    | `Null -> opts
    | j ->
      let deg = float_u_of_json_exn j in
      let angle = S2.S1_angle.of_degrees deg in
      let ca = S2.S1_chord_angle.of_angle angle in
      S2.S2_closest_edge_query.Options.with_max_distance opts ca
  in
  let max_err = float_u_of_json_exn (member "max_error_deg" j) in
  if Float_u.O.(max_err > #0.0)
  then (
    let ca = S2.S1_chord_angle.of_angle (S2.S1_angle.of_degrees max_err) in
    S2.S2_closest_edge_query.Options.with_max_error opts ca)
  else opts
;;

let chord_angle_of_json_or_infinity j =
  match j with
  | `Null -> S2.S1_chord_angle.infinity
  | j -> S2.S1_chord_angle.of_length2 (float_u_of_json_exn j)
;;

let result_of_json j : S2.S2_closest_edge_query.Result.t =
  { distance = chord_angle_of_json_or_infinity (member "distance" j)
  ; shape_id = int_of_json_exn (member "shape_id" j)
  ; edge_id = int_of_json_exn (member "edge_id" j)
  }
;;

let results_of_json j = to_list j |> List.map ~f:result_of_json

let result_pp ppf (r : S2.S2_closest_edge_query.Result.t) =
  Stdlib.Format.fprintf
    ppf
    "(dist^2=%.15g shape=%d edge=%d)"
    (Float_u.to_float (S2.S1_chord_angle.length2 r.distance))
    r.shape_id
    r.edge_id
;;

let result_testable : S2.S2_closest_edge_query.Result.t testable =
  let eq (a : S2.S2_closest_edge_query.Result.t) (b : S2.S2_closest_edge_query.Result.t) =
    let dist_eq =
      if S2.S1_chord_angle.is_infinity a.distance
         && S2.S1_chord_angle.is_infinity b.distance
      then true
      else
        Float.( <= )
          (Float_u.to_float
             (Float_u.abs
                (Float_u.sub
                   (S2.S1_chord_angle.length2 a.distance)
                   (S2.S1_chord_angle.length2 b.distance))))
          1e-12
    in
    Int.equal a.shape_id b.shape_id && Int.equal a.edge_id b.edge_id && dist_eq
  in
  Alcotest.testable result_pp eq
;;

let check_results msg ~expected ~actual = check (list result_testable) msg expected actual

let run_case name =
  let section = member name (Lazy.force fixture) in
  let index = build_index (to_list (member "shapes" section)) in
  let queries = to_list (member "queries" section) in
  List.iteri queries ~f:(fun i q ->
    let target = build_target (member "target" q) in
    let options = apply_options (member "options" q) in
    let query = S2.S2_closest_edge_query.create ~options index () in
    let expected_opt = results_of_json (member "results_optimized" q) in
    let actual = S2.S2_closest_edge_query.find_closest_edges query target in
    check_results
      (sprintf "%s queries[%d] find_closest_edges" name i)
      ~expected:expected_opt
      ~actual;
    let expected_single = result_of_json (member "closest_edge" q) in
    let actual_single = S2.S2_closest_edge_query.find_closest_edge query target in
    check
      result_testable
      (sprintf "%s queries[%d] find_closest_edge" name i)
      expected_single
      actual_single;
    let actual_chord = S2.S2_closest_edge_query.get_distance query target in
    match member "distance_length2" q with
    | `Null ->
      (check bool)
        (sprintf "%s queries[%d] get_distance infinite" name i)
        true
        (S2.S1_chord_angle.is_infinity actual_chord)
    | j ->
      let expected_dist = float_u_of_json_exn j in
      check_float_u
        ~eps:1e-12
        (sprintf "%s queries[%d] get_distance" name i)
        ~expected:expected_dist
        ~actual:(S2.S1_chord_angle.length2 actual_chord));
  let predicates = to_list (member "predicates" section) in
  List.iteri predicates ~f:(fun i p ->
    let target = build_target (member "target" p) in
    let limit_deg = float_u_of_json_exn (member "limit_deg" p) in
    let limit = S2.S1_chord_angle.of_angle (S2.S1_angle.of_degrees limit_deg) in
    let query = S2.S2_closest_edge_query.create index () in
    let expected_less = bool_of_json_exn (member "is_distance_less" p) in
    let actual_less = S2.S2_closest_edge_query.is_distance_less query target limit in
    (check bool)
      (sprintf "%s predicates[%d] is_distance_less" name i)
      expected_less
      actual_less;
    let expected_leq = bool_of_json_exn (member "is_distance_less_or_equal" p) in
    let actual_leq =
      S2.S2_closest_edge_query.is_distance_less_or_equal query target limit
    in
    (check bool)
      (sprintf "%s predicates[%d] is_distance_less_or_equal" name i)
      expected_leq
      actual_leq;
    let expected_cons =
      bool_of_json_exn (member "is_conservative_distance_less_or_equal" p)
    in
    let actual_cons =
      S2.S2_closest_edge_query.is_conservative_distance_less_or_equal query target limit
    in
    (check bool)
      (sprintf "%s predicates[%d] is_conservative_distance_less_or_equal" name i)
      expected_cons
      actual_cons)
;;

let test name () = run_case name

let () =
  run
    "s2_closest_edge_query"
    [ "empty", [ test_case "empty" `Quick (test "empty") ]
    ; "small_points", [ test_case "small_points" `Quick (test "small_points") ]
    ; ( "polygon_interior"
      , [ test_case "polygon_interior" `Quick (test "polygon_interior") ] )
    ; ( "edge_target_polyline"
      , [ test_case "edge_target_polyline" `Quick (test "edge_target_polyline") ] )
    ; "cell_target", [ test_case "cell_target" `Quick (test "cell_target") ]
    ; ( "shape_index_target"
      , [ test_case "shape_index_target" `Quick (test "shape_index_target") ] )
    ; "regular_loop", [ test_case "regular_loop" `Quick (test "regular_loop") ]
    ]
;;
