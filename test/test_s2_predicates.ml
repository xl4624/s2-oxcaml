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

(* ---------- Quickcheck: predicate invariants on random S2 points ---------- *)

module S2_point_triple = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    ; c : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let gen_point rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      S2.S2_point.of_coords
        ~x:(Float_u.of_float x)
        ~y:(Float_u.of_float y)
        ~z:(Float_u.of_float z)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = gen_point rnd in
      let b = gen_point rnd in
      let c = gen_point rnd in
      { a; b; c })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module S2_point_quad = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    ; c : S2.S2_point.t
    ; o : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let gen_point rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      S2.S2_point.of_coords
        ~x:(Float_u.of_float x)
        ~y:(Float_u.of_float y)
        ~z:(Float_u.of_float z)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = gen_point rnd in
      let b = gen_point rnd in
      let c = gen_point rnd in
      let o = gen_point rnd in
      { a; b; c; o })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

let sign_int a b c =
  S2.S2_predicates.Direction.to_int (S2.S2_predicates.robust_sign a b c)
;;

let quickcheck_sign_antisymmetric () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let s1 = sign_int a b c in
      let s2 = sign_int b a c in
      assert (Int.( = ) s1 (-s2)))
;;

let quickcheck_sign_cyclic () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let s1 = sign_int a b c in
      let s2 = sign_int b c a in
      let s3 = sign_int c a b in
      assert (Int.( = ) s1 s2);
      assert (Int.( = ) s2 s3))
;;

let quickcheck_sign_anticyclic_swap () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let s1 = sign_int a b c in
      let s2 = sign_int a c b in
      assert (Int.( = ) s1 (-s2)))
;;

let quickcheck_robust_sign_never_indeterminate () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      (* Random unit points generated from independent coordinates will never
         be exactly equal (the only case where robust_sign returns
         Indeterminate).  Skip the vanishingly unlikely collision case
         defensively. *)
      if S2.S2_point.equal a b || S2.S2_point.equal b c || S2.S2_point.equal a c
      then ()
      else (
        let d = S2.S2_predicates.robust_sign a b c in
        match d with
        | S2.S2_predicates.Direction.Indeterminate ->
          assert false
        | Clockwise | Counter_clockwise -> ()))
;;

let quickcheck_sign_agrees_with_robust_sign () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      (* For non-degenerate triples, [sign] and [robust_sign] must agree: the
         boolean returns true iff robust_sign is Counter_clockwise.  Skip
         near-collinear triples where the plain-float determinant sign is
         unreliable; those are exercised by the fixture-driven tests. *)
      let det =
        S2.R3_vector.dot (S2.R3_vector.cross a b) c
      in
      if Float_u.O.(Float_u.abs det < #1e-10)
      then ()
      else (
        let boolean = S2.S2_predicates.sign a b c in
        let robust = Int.( > ) (sign_int a b c) 0 in
        assert (Bool.( = ) boolean robust)))
;;

let quickcheck_ordered_ccw_definition () =
  Base_quickcheck.Test.run_exn
    (module S2_point_quad)
    ~config:qc_config
    ~f:(fun { S2_point_quad.a; b; c; o } ->
      (* Cross-check ordered_ccw against its definition in terms of
         robust_sign.  The Go reference:
            sum := 0
            if robustSign(b, o, a) != Clockwise { sum++ }
            if robustSign(c, o, b) != Clockwise { sum++ }
            if robustSign(a, o, c) == CounterClockwise { sum++ }
            return sum >= 2
         This test is a tautology against the current implementation but
         protects against accidental regressions that swap arguments. *)
      let nonneg x y z =
        match S2.S2_predicates.robust_sign x y z with
        | Clockwise -> 0
        | Counter_clockwise | Indeterminate -> 1
      in
      let pos x y z =
        match S2.S2_predicates.robust_sign x y z with
        | Counter_clockwise -> 1
        | Clockwise | Indeterminate -> 0
      in
      let sum = nonneg b o a + nonneg c o b + pos a o c in
      let expected = Int.( >= ) sum 2 in
      let actual = S2.S2_predicates.ordered_ccw a b c o in
      assert (Bool.( = ) expected actual))
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
    ; ( "quickcheck"
      , [ Alcotest.test_case "sign_antisymmetric" `Quick quickcheck_sign_antisymmetric
        ; Alcotest.test_case "sign_cyclic" `Quick quickcheck_sign_cyclic
        ; Alcotest.test_case
            "sign_anticyclic_swap"
            `Quick
            quickcheck_sign_anticyclic_swap
        ; Alcotest.test_case
            "robust_sign_never_indeterminate"
            `Quick
            quickcheck_robust_sign_never_indeterminate
        ; Alcotest.test_case
            "sign_agrees_with_robust_sign"
            `Quick
            quickcheck_sign_agrees_with_robust_sign
        ; Alcotest.test_case
            "ordered_ccw_definition"
            `Quick
            quickcheck_ordered_ccw_definition
        ] )
    ]
;;
