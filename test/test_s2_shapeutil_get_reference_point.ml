(* C++ test parity:
   s2geometry/src/s2/s2shapeutil_get_reference_point_test.cc.
   Golden data from test/gen/s2shapeutil_get_reference_point.cc.

   Covered:
   - TEST(GetReferencePoint, EmptyPolygon): a lax polygon with zero loops is
     not contained.
   - TEST(GetReferencePoint, FullPolygon): a single zero-vertex chain marks
     the full polygon and is contained.
   - TEST(GetReferencePoint, DegenerateLoops): loops whose edges form sibling
     pairs are balanced, leaving the polygon empty.
   - TEST(GetReferencePoint, InvertedLoops): two clockwise (inverted) loops;
     captured here to pin the chosen reference vertex and its containment.

   Extra coverage (not in C++):
   - "ccw_triangle": a normal counter-clockwise triangle; the algorithm
     picks edge(0).v0 because every vertex is unbalanced.
   - "cw_triangle": same triangle reversed; verifies that flipping
     orientation flips containment of the chosen reference vertex.

   Deliberately omitted:
   - TEST(GetReferencePoint, PartiallyDegenerateLoops): a randomized stress
     test that walks the S2CellId Hilbert curve. Reproducing the absl seed
     sequence in OCaml is not worth the complexity. *)

open Core
open Alcotest
open Test_helpers

let fixture = lazy (load_fixture "s2shapeutil_get_reference_point.json")
let point_of_json = r3_vector_of_json

let loops_of_json j =
  let loops = to_list j in
  let n = List.length loops in
  let out = Array.create ~len:n [||] in
  List.iteri loops ~f:(fun i loop ->
    let pts = to_list loop in
    let m = List.length pts in
    let arr = Array.create ~len:m S2.S2_point.origin in
    List.iteri pts ~f:(fun j pj -> arr.(j) <- point_of_json pj);
    out.(i) <- arr);
  out
;;

let ref_point_of_json j =
  let point = point_of_json (member "point" j) in
  let contained = bool_of_json_exn (member "contained" j) in
  S2.S2_shape.Reference_point.create ~point ~contained
;;

let test_cases () =
  let cases = to_list (member "cases" (Lazy.force fixture)) in
  List.iter cases ~f:(fun case ->
    let name = string_of_json_exn (member "name" case) in
    let loops = loops_of_json (member "loops" case) in
    let expected = ref_point_of_json (member "reference_point" case) in
    let polygon = S2.S2_lax_polygon.of_loops loops in
    let shape = S2.S2_lax_polygon.to_shape polygon in
    let actual = S2.S2_shapeutil_get_reference_point.get_reference_point shape in
    check bool (sprintf "%s: contained" name) expected.#contained actual.#contained;
    (* The point must match bit-exactly: the algorithm picks a specific
       vertex (or [S2_point.origin] in the all-balanced fallback), so any
       drift indicates a divergence from the C++ algorithm. *)
    check
      bool
      (sprintf "%s: point" name)
      true
      (S2.S2_point.equal expected.#point actual.#point);
    (* Round-trip the reference point through [contains_brute_force]: by
       construction the chosen point's containment must be preserved when
       traced from itself (zero edge crossings), so the two answers must
       agree. *)
    check
      bool
      (sprintf "%s: brute force agrees with reference point" name)
      actual.#contained
      (S2.S2_shapeutil_contains_brute_force.contains_brute_force
         shape
         ~point:actual.#point))
;;

let () =
  Alcotest.run
    "S2_shapeutil_get_reference_point"
    [ "fixture", [ test_case "all cases" `Quick test_cases ] ]
;;
