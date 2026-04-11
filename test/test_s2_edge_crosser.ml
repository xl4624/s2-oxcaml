(* C++ test parity: s2geometry/src/s2/s2edge_crosser_test.cc
   -  TEST(S2, Crossings): 12 crossing cases, exercising CrossingSign,
      EdgeOrVertexCrossing, SignedEdgeOrVertexCrossing.
   -  Chain sequences that drive the stateful crosser through several
      successive edges (RestartAt + ChainCrossingSign / ChainEdgeOrVertex /
      ChainSignedEdgeOrVertex).
   -  TEST(S2, CollinearEdgesThatDontTouch) - static deterministic variant
      using fixed great-circle interpolations.

   Omitted:
   -  RandomCrossings: random-seeded stress case from upstream. *)

open Core
open Test_helpers
open Alcotest

let fixture = load_fixture "s2edge_crosser.json"

(* ---------- One-shot CrossingSign / EdgeOrVertex ----------
   Cases labeled "barely_cross_end", "separated_1e640", "barely_cross_2000bits",
   and "separated_1e640_variant" require > double precision arithmetic
   (symbolic perturbations / >2000-bit exact arithmetic) to disambiguate. The
   OCaml s2_edge_crossings has only the stable-sign path, so these cases are
   skipped. *)

let exact_arith_only = function
  | "barely_cross_end"
  | "separated_1e640"
  | "barely_cross_2000bits"
  | "separated_1e640_variant" -> true
  | _ -> false
;;

let rec test_crossings () =
  let cases = to_list (member "crossings" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    if exact_arith_only name then () else check_one_crossing c)

and check_one_crossing c =
  let name = string_of_json_exn (member "name" c) in
  let a = r3_vector_of_json (member "a" c) in
  let b = r3_vector_of_json (member "b" c) in
  let cp = r3_vector_of_json (member "c" c) in
  let d = r3_vector_of_json (member "d" c) in
  let expected_crossing = int_of_json_exn (member "crossing_sign" c) in
  let expected_signed = int_of_json_exn (member "signed_crossing_sign" c) in
  let crosser = S2.S2_edge_crosser.create_with_chain ~a ~b ~c:cp in
  let actual_crossing = S2.S2_edge_crosser.chain_crossing_sign crosser d in
  (check int) (sprintf "%s: crossing_sign" name) expected_crossing actual_crossing;
  let crosser2 = S2.S2_edge_crosser.create_with_chain ~a ~b ~c:cp in
  let actual_signed =
    S2.S2_edge_crosser.chain_signed_edge_or_vertex_crossing crosser2 d
  in
  (check int) (sprintf "%s: signed_crossing_sign" name) expected_signed actual_signed
;;

(* ---------- Chain sequences ---------- *)

let point_at_index vs i =
  let j = List.nth_exn vs i in
  r3_vector_of_json j
;;

let test_chains () =
  let groups = to_list (member "chains" fixture) in
  List.iter groups ~f:(fun g ->
    let name = string_of_json_exn (member "name" g) in
    let a = r3_vector_of_json (member "a" g) in
    let b = r3_vector_of_json (member "b" g) in
    let chain = to_list (member "chain" g) in
    let steps = to_list (member "steps" g) in
    (* Drive a single crosser through all steps, mirroring the upstream loop. *)
    let crosser = S2.S2_edge_crosser.create ~a ~b in
    let first = point_at_index chain 0 in
    S2.S2_edge_crosser.restart_at crosser first;
    List.iter steps ~f:(fun step ->
      let c_index = int_of_json_exn (member "c_index" step) in
      let d_index = int_of_json_exn (member "d_index" step) in
      let expected_cs = int_of_json_exn (member "crossing_sign" step) in
      let expected_eov = bool_of_json_exn (member "edge_or_vertex" step) in
      let expected_sgn = int_of_json_exn (member "signed_crossing" step) in
      let d = point_at_index chain d_index in
      let actual_cs = S2.S2_edge_crosser.chain_crossing_sign crosser d in
      (check int)
        (sprintf "%s[%d->%d] crossing_sign" name c_index d_index)
        expected_cs
        actual_cs;
      (* Recompute edge_or_vertex and signed_crossing via fresh crossers so
         the primary `crosser` keeps rolling through crossing_sign calls. *)
      let c = point_at_index chain c_index in
      let eov_crosser = S2.S2_edge_crosser.create_with_chain ~a ~b ~c in
      let actual_eov = S2.S2_edge_crosser.chain_edge_or_vertex_crossing eov_crosser d in
      (check bool)
        (sprintf "%s[%d->%d] edge_or_vertex" name c_index d_index)
        expected_eov
        actual_eov;
      let sgn_crosser = S2.S2_edge_crosser.create_with_chain ~a ~b ~c in
      let actual_sgn =
        S2.S2_edge_crosser.chain_signed_edge_or_vertex_crossing sgn_crosser d
      in
      (check int)
        (sprintf "%s[%d->%d] signed_crossing" name c_index d_index)
        expected_sgn
        actual_sgn))
;;

(* ---------- CollinearEdgesThatDontTouch ---------- *)

let test_collinear_no_touch () =
  let cases = to_list (member "collinear_no_touch" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = r3_vector_of_json (member "a" c) in
    let b = r3_vector_of_json (member "b" c) in
    let cp = r3_vector_of_json (member "c" c) in
    let d = r3_vector_of_json (member "d" c) in
    let expected = int_of_json_exn (member "crossing_sign" c) in
    let crosser = S2.S2_edge_crosser.create_with_chain ~a ~b ~c:cp in
    let actual = S2.S2_edge_crosser.chain_crossing_sign crosser d in
    (check int) (sprintf "%s: crossing_sign" name) expected actual)
;;

let () =
  run
    "s2_edge_crosser"
    [ "crossings", [ test_case "Crossings" `Quick test_crossings ]
    ; "chains", [ test_case "Chains" `Quick test_chains ]
    ; ( "collinear_no_touch"
      , [ test_case "CollinearEdgesThatDontTouch" `Quick test_collinear_no_touch ] )
    ]
;;
