(* C++ test parity: s2geometry/src/s2/s2predicates_test.cc
   Golden data from test/gen/s2predicates.cc.

   Covered:
   -  TEST(Sign, StableSignUnderflow)
   -  TEST(Sign, CollinearPoints) - collinear_tangent and proportional triples
   -  TEST(Sign, SymbolicPerturbationCodeCoverage) - all 13 M1..M13 cases
   -  TEST(CompareDistances, Coverage) - triage, exact, and symbolic paths
   -  TEST(CompareDistance, Coverage)  - triage and exact paths
   -  TEST(SignDotProd, Orthogonal / NearlyOrthogonal) - triage and exact
   -  Basic Sign and OrderedCCW cases.

   Deliberately omitted:
   -  TEST(CompareDistances, Consistency) / TEST(CompareDistance, Consistency)
      - randomized cross-precision consistency tests.
   -  TEST(CircleEdgeIntersectionOrdering) - C++-only predicate, not ported.
   -  TEST(CompareEdgeDistance) / CompareEdgePairDistance / CompareEdgeDirections
   -  TEST(EdgeCircumcenterSign) / VoronoiSiteExclusion *)

open Core
open Test_helpers

let fixture = lazy (load_fixture "s2predicates.json")

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

(* ---------- Sign ---------- *)

let test_sign_section section_name () =
  let cases = to_list (member section_name (Lazy.force fixture)) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let c_pt = point_of_json (member "c" c) in
    let expected_sign = int_of_json_exn (member "sign" c) in
    let expected_dir = S2.S2_predicates.Direction.of_int expected_sign in
    let actual_dir = S2.S2_predicates.robust_sign a b c_pt in
    let actual_sign = S2.S2_predicates.Direction.to_int actual_dir in
    if Int.( <> ) actual_sign expected_sign
    then
      Alcotest.failf
        "%s/%s: robust_sign expected %d, got %d"
        section_name
        label
        expected_sign
        actual_sign;
    (* Consistency: rotating the arguments preserves the sign, reversing
       flips it.  Every fixture case must satisfy this. *)
    let sign_rot =
      S2.S2_predicates.Direction.to_int (S2.S2_predicates.robust_sign b c_pt a)
    in
    let sign_rev =
      S2.S2_predicates.Direction.to_int (S2.S2_predicates.robust_sign c_pt b a)
    in
    if Int.( <> ) sign_rot actual_sign
    then Alcotest.failf "%s/%s: rotation changed sign" section_name label;
    if Int.( <> ) sign_rev (-actual_sign)
    then Alcotest.failf "%s/%s: reversal did not negate sign" section_name label;
    ignore expected_dir)
;;

let test_sign_basic () = test_sign_section "sign_basic" ()
let test_sign_collinear () = test_sign_section "sign_collinear" ()
let test_sign_symbolic () = test_sign_section "sign_symbolic" ()

let test_sign_underflow () =
  let cases = to_list (member "sign_underflow" (Lazy.force fixture)) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let c_pt = point_of_json (member "c" c) in
    let expected_sign = int_of_json_exn (member "sign" c) in
    let actual_sign =
      S2.S2_predicates.Direction.to_int (S2.S2_predicates.robust_sign a b c_pt)
    in
    if Int.( <> ) actual_sign expected_sign
    then
      Alcotest.failf "underflow/%s: expected %d, got %d" label expected_sign actual_sign)
;;

(* ---------- OrderedCCW ---------- *)

let test_ordered_ccw () =
  let cases = to_list (member "ordered_ccw" (Lazy.force fixture)) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let c_pt = point_of_json (member "c" c) in
    let o = point_of_json (member "o" c) in
    let expected = bool_of_json_exn (member "ordered" c) in
    let actual = S2.S2_predicates.ordered_ccw a b c_pt o in
    if Bool.( <> ) actual expected
    then Alcotest.failf "ordered_ccw/%s: expected %b, got %b" label expected actual)
;;

(* ---------- CompareDistances ---------- *)

let test_compare_distances () =
  let cases = to_list (member "compare_distances" (Lazy.force fixture)) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let x = point_of_json (member "x" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let expected_sign = int_of_json_exn (member "sign" c) in
    let actual_sign = S2.S2_predicates.compare_distances x a b in
    if Int.( <> ) actual_sign expected_sign
    then
      Alcotest.failf
        "compare_distances/%s: expected %d, got %d"
        label
        expected_sign
        actual_sign)
;;

(* ---------- CompareDistance ---------- *)

let test_compare_distance () =
  let cases = to_list (member "compare_distance" (Lazy.force fixture)) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let x = point_of_json (member "x" c) in
    let y = point_of_json (member "y" c) in
    let r_length2 = float_u_of_json_exn (member "r_length2" c) in
    let expected_sign = int_of_json_exn (member "sign" c) in
    let r = S2.S1_chord_angle.of_length2 r_length2 in
    let actual_sign = S2.S2_predicates.compare_distance x y r in
    if Int.( <> ) actual_sign expected_sign
    then
      Alcotest.failf
        "compare_distance/%s: expected %d, got %d"
        label
        expected_sign
        actual_sign)
;;

(* ---------- SignDotProd ---------- *)

let test_sign_dot_prod () =
  let cases = to_list (member "sign_dot_prod" (Lazy.force fixture)) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let expected_sign = int_of_json_exn (member "sign" c) in
    let actual_sign = S2.S2_predicates.sign_dot_prod a b in
    if Int.( <> ) actual_sign expected_sign
    then
      Alcotest.failf
        "sign_dot_prod/%s: expected %d, got %d"
        label
        expected_sign
        actual_sign)
;;

(* ---------- Alcotest suite ---------- *)

let () =
  Alcotest.run
    "S2_predicates"
    [ ( "sign"
      , [ Alcotest.test_case "basic" `Quick test_sign_basic
        ; Alcotest.test_case "underflow" `Quick test_sign_underflow
        ; Alcotest.test_case "collinear" `Quick test_sign_collinear
        ; Alcotest.test_case "symbolic" `Quick test_sign_symbolic
        ] )
    ; "ordered_ccw", [ Alcotest.test_case "ordered_ccw" `Quick test_ordered_ccw ]
    ; ( "compare_distances"
      , [ Alcotest.test_case "compare_distances" `Quick test_compare_distances ] )
    ; ( "compare_distance"
      , [ Alcotest.test_case "compare_distance" `Quick test_compare_distance ] )
    ; "sign_dot_prod", [ Alcotest.test_case "sign_dot_prod" `Quick test_sign_dot_prod ]
    ]
;;
