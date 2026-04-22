(* Golden data produced by test/gen/s2builder.cc.

   Upstream C++ tests mirrored (s2builder_test.cc):
   - (custom) unit_square, triangle, two_nested_squares - standard polygon
     construction with zero snap radius; exercise the
     [S2_builder] + [S2_polygon_layer] pipeline.
   - IdempotencySnapsIdenticalVerticesWithZeroSnapRadius (idempotency_zero_snap)
     - duplicate vertices in an input loop are merged (the duplicate edge
       becomes degenerate and is discarded by the polygon layer).

   Plus a standalone smoke test that adds unit-square edges directly
   from OCaml and checks [num_loops] and loop vertex count.

   Deliberately omitted (deferred by this port):
   - SimpleVertexMerging and all other nonzero-snap-radius tests: the OCaml
     port uses a simplified cluster-merge for snap_radius > 0, not the
     Voronoi site-selection algorithm that C++ uses. Topology is preserved
     but canonical site choice may differ.
   - MaxEdgeDeviation, Topology*, Snapping*, MinEdgeVertex* : rely on the
     full Voronoi site selection (s2builder.cc:888-1160) which is not yet
     implemented.
   - UNDIRECTED edge variants.
   - [IntLatLngSnapFunction] and [S2CellIdSnapFunction] based tests (stubs
     raise).
   - [ForceVertex], labels, [simplify_edge_chains] tests.
   - Memory tracker tests.
   - [LaxPolygonLayer], [S2PolylineLayer], [S2PolylineVectorLayer],
     [S2PointVectorLayer] tests (layer types deferred). *)

open Core
open Test_helpers
open Alcotest
module B = S2.S2_builder
module L = S2.S2_polygon_layer

let fixture = lazy (load_fixture "s2builder.json")
let point_of_json = r3_vector_of_json

let points_of_json j =
  let xs = to_list j in
  let n = List.length xs in
  if n = 0
  then [||]
  else (
    let first = point_of_json (List.hd_exn xs) in
    let arr = Array.create ~len:n first in
    List.iteri xs ~f:(fun i p -> arr.(i) <- point_of_json p);
    arr)
;;

(* Decomposed edge input: parallel arrays of v0 and v1 points. Tuples can't
   hold S2_point.t because it has an unboxed layout. *)
let edges_of_json j =
  let xs = to_list j in
  let n = List.length xs in
  if n = 0
  then [||], [||]
  else (
    let zero =
      match List.hd_exn xs with
      | pair ->
        (match to_list pair with
         | a :: _ -> point_of_json a
         | _ ->
           (match failwith "edges_of_json: expected pair" with
            | (_ : Nothing.t) -> .))
    in
    let v0s = Array.create ~len:n zero in
    let v1s = Array.create ~len:n zero in
    List.iteri xs ~f:(fun i pair ->
      match to_list pair with
      | [ a; b ] ->
        v0s.(i) <- point_of_json a;
        v1s.(i) <- point_of_json b
      | _ ->
        (match failwith "edges_of_json: expected pair" with
         | (_ : Nothing.t) -> .));
    v0s, v1s)
;;

let build_from_case case =
  let snap_r =
    S2.S1_angle.of_radians (float_u_of_json_exn (member "snap_radius_radians" case))
  in
  let split = bool_of_json_exn (member "split_crossing_edges" case) in
  let options =
    B.Options.create
      ~snap_function:(B.Snap_function.identity_with_radius snap_r)
      ~split_crossing_edges:split
      ~intersection_tolerance:
        (if split then S2.S2_edge_crossings.intersection_error else S2.S1_angle.zero)
      ()
  in
  let builder = B.create options in
  let output = L.create_output () in
  B.start_layer builder (L.layer output);
  let v0s, v1s = edges_of_json (member "input_edges" case) in
  for i = 0 to Array.length v0s - 1 do
    B.add_edge builder v0s.(i) v1s.(i)
  done;
  let err = B.build builder in
  if not (B.Error.is_ok err)
  then failwithf "build failed: %s" (B.Error.sexp_of_t err |> Sexp.to_string) ();
  L.result output
;;

let check_polygon_matches name case polygon =
  let expected = member "polygon" case in
  let expected_num_loops = int_of_json_exn (member "num_loops" expected) in
  let got_num_loops = S2.S2_polygon.num_loops polygon in
  check int (sprintf "%s: num_loops" name) expected_num_loops got_num_loops;
  let expected_is_empty = bool_of_json_exn (member "is_empty" expected) in
  let expected_is_full = bool_of_json_exn (member "is_full" expected) in
  check
    bool
    (sprintf "%s: is_empty" name)
    expected_is_empty
    (S2.S2_polygon.is_empty polygon);
  check bool (sprintf "%s: is_full" name) expected_is_full (S2.S2_polygon.is_full polygon);
  (* Vertex sets must match per loop, but cyclic order may differ. Check
     that each expected vertex appears somewhere in the corresponding loop. *)
  let loops_json = to_list (member "loops" expected) in
  List.iteri loops_json ~f:(fun li loop_json ->
    let expected_pts = points_of_json loop_json in
    let loop = S2.S2_polygon.loop polygon li in
    let n_exp = Array.length expected_pts in
    let n_got = S2.S2_loop.num_vertices loop in
    check int (sprintf "%s: loop[%d] num_vertices" name li) n_exp n_got;
    (* Check that the multisets of vertices match. *)
    let got_pts = S2.S2_loop.vertices loop in
    let contains p =
      let found = ref false in
      for i = 0 to Array.length got_pts - 1 do
        if S2.S2_point.equal p got_pts.(i) then found := true
      done;
      !found
    in
    for k = 0 to Array.length expected_pts - 1 do
      check
        bool
        (sprintf "%s: loop[%d] contains expected vertex" name li)
        true
        (contains expected_pts.(k))
    done)
;;

let fixture_cases () =
  let fx = Lazy.force fixture in
  to_list (member "cases" fx)
;;

let run_fixture_case case () =
  let name = string_of_json_exn (member "name" case) in
  let ok = bool_of_json_exn (member "ok" case) in
  if ok
  then (
    let polygon = build_from_case case in
    check_polygon_matches name case polygon)
;;

let test_smoke_unit_square () =
  (* Build a unit-square loop directly from OCaml (no fixture). Checks that
     the full pipeline from [S2_builder.create] through [S2_polygon_layer]
     produces a single 4-vertex loop. *)
  let v0 = S2.S2_latlng.to_point (S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#0.0) in
  let v1 = S2.S2_latlng.to_point (S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#1.0) in
  let v2 = S2.S2_latlng.to_point (S2.S2_latlng.of_degrees ~lat:#1.0 ~lng:#1.0) in
  let v3 = S2.S2_latlng.to_point (S2.S2_latlng.of_degrees ~lat:#1.0 ~lng:#0.0) in
  let vs = [| v0; v1; v2; v3 |] in
  let options = B.Options.default () in
  let builder = B.create options in
  let output = L.create_output () in
  B.start_layer builder (L.layer output);
  B.add_loop builder vs;
  let err = B.build builder in
  check bool "smoke: build ok" true (B.Error.is_ok err);
  let polygon = L.result output in
  check int "smoke: num_loops = 1" 1 (S2.S2_polygon.num_loops polygon);
  check int "smoke: num_vertices = 4" 4 (S2.S2_polygon.num_vertices polygon)
;;

let () =
  let cases = fixture_cases () in
  let fixture_tests =
    List.map cases ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      Alcotest.test_case name `Quick (run_fixture_case case))
  in
  let smoke_test = Alcotest.test_case "smoke_unit_square" `Quick test_smoke_unit_square in
  Alcotest.run "s2_builder" [ "fixtures", fixture_tests; "smoke", [ smoke_test ] ]
;;
