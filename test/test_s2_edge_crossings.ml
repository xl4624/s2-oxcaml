(* C++ test parity: s2geometry/src/s2/s2edge_crossings_test.cc
   and s2geometry/src/s2/s2edge_crosser_test.cc
   Golden data from test/gen/s2edge_crossings.cc.

   Covered:
   -  TEST(S2, Crossings) (from s2edge_crosser_test.cc)
   -  TEST(S2, AngleContainsVertex)
   -  TEST(S2, VertexCrossing) - manual cases
   -  TEST(S2, GetIntersection) - selected deterministic cases
   -  TEST(S2, RobustSign / Sign) - basic cases
   -  TEST(S2, CompareEdgesOrderInvariant)
   -  TEST(S2, RobustCrossProdCoverage) - the EXACT and SYMBOLIC cases that
      reach our arbitrary-precision fallback; the DOUBLE-vs-LONG_DOUBLE
      precision-selection cases are omitted since OCaml has no long double.
   -  TEST(S2, RobustCrossProdMagnitude) - regression for the underflow fix.
   -  TEST(S2, SymbolicCrossProdConsistentWithSign) - 27-point unit-vector
      sweep checking that [robust_cross_prod] is CCW-consistent with [sign]
      whenever symbolic perturbations are required.

   Deliberately omitted:
   -  TEST(S2, RobustCrossProdError) - statistical/randomized
   -  TEST(S2, IntersectionError) - statistical/randomized
   -  TEST(S2, GrazingIntersections) - statistical/randomized
   -  TEST(S2, GetIntersectionInvariants) - statistical/randomized
   -  TEST(S2, CollinearEdgesThatDontTouch) - statistical/randomized
   -  TEST(S2, CoincidentZeroLengthEdgesThatDontTouch) - statistical *)

open Core
open Test_helpers

let fixture = lazy (load_fixture "s2edge_crossings.json")

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

(* ---------- crossing_sign and edge_or_vertex_crossing --------------------- *)
(* Cases 9-12 (barely_cross_end, separated_1e640, barely_cross_2000bits,
   separated_1e640_variant) use denormalized coordinates (1e-323) that require
   arbitrary-precision arithmetic not yet available. Skip them for now. *)
let denorm_cases =
  Set.of_list
    (module String)
    [ "barely_cross_end"
    ; "separated_1e640"
    ; "barely_cross_2000bits"
    ; "separated_1e640_variant"
    ]
;;

let test_crossings () =
  let data = Lazy.force fixture in
  let cases = to_list (member "crossings" data) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    if not (Set.mem denorm_cases name)
    then (
      let a = point_of_json (member "a" c) in
      let b = point_of_json (member "b" c) in
      let cc = point_of_json (member "c" c) in
      let d = point_of_json (member "d" c) in
      let expected_cs = int_of_json_exn (member "crossing_sign" c) in
      let expected_eov = bool_of_json_exn (member "edge_or_vertex_crossing" c) in
      let actual_cs = S2.S2_edge_crossings.crossing_sign a b cc d in
      Alcotest.(check int) (name ^ " crossing_sign") expected_cs actual_cs;
      let actual_eov = S2.S2_edge_crossings.edge_or_vertex_crossing a b cc d in
      Alcotest.(check bool) (name ^ " edge_or_vertex") expected_eov actual_eov))
;;

(* ---------- angle_contains_vertex ----------------------------------------- *)
let test_angle_contains_vertex () =
  let data = Lazy.force fixture in
  let cases = member "angle_contains_vertex" data in
  let a = point_of_json (member "a" cases) in
  let b = point_of_json (member "b" cases) in
  let ref_b = point_of_json (member "ref_b" cases) in
  let expected_aba = bool_of_json_exn (member "degenerate_aba" cases) in
  let expected_ref = bool_of_json_exn (member "a_eq_refdir_b" cases) in
  let expected_c_ref = bool_of_json_exn (member "c_eq_refdir_b" cases) in
  check_bool
    "degenerate ABA"
    ~expected:expected_aba
    ~actual:(S2.S2_edge_crossings.angle_contains_vertex a b a);
  check_bool
    "A == RefDir(B)"
    ~expected:expected_ref
    ~actual:(S2.S2_edge_crossings.angle_contains_vertex ref_b b a);
  check_bool
    "C == RefDir(B)"
    ~expected:expected_c_ref
    ~actual:(S2.S2_edge_crossings.angle_contains_vertex a b ref_b)
;;

(* ---------- vertex_crossing ----------------------------------------------- *)
let test_vertex_crossing () =
  let data = Lazy.force fixture in
  let cases = to_list (member "vertex_crossing" data) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let cc = point_of_json (member "c" c) in
    let d = point_of_json (member "d" c) in
    let expected = bool_of_json_exn (member "expected" c) in
    let actual = S2.S2_edge_crossings.vertex_crossing a b cc d in
    Alcotest.(check bool) name expected actual)
;;

(* ---------- sign ---------------------------------------------------------- *)
let test_sign () =
  let data = Lazy.force fixture in
  let cases = to_list (member "sign" data) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let cc = point_of_json (member "c" c) in
    let expected = int_of_json_exn (member "sign" c) in
    let actual = S2.S2_edge_crossings.sign a b cc in
    Alcotest.(check int) name expected actual)
;;

(* ---------- get_intersection ---------------------------------------------- *)
(* exact_underflow and exact_sign require arbitrary-precision GetIntersectionExact. *)
let intersection_skip = Set.of_list (module String) [ "exact_underflow"; "exact_sign" ]

let test_intersection () =
  let data = Lazy.force fixture in
  let cases = to_list (member "intersection" data) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    if not (Set.mem intersection_skip name)
    then (
      let a = point_of_json (member "a" c) in
      let b = point_of_json (member "b" c) in
      let cc = point_of_json (member "c" c) in
      let d = point_of_json (member "d" c) in
      let expected = point_of_json (member "intersection" c) in
      let actual = S2.S2_edge_crossings.get_intersection a b cc d in
      check_r3_vector (name ^ " intersection") ~expected ~actual))
;;

(* ---------- compare_edges ------------------------------------------------- *)
let test_compare_edges () =
  let data = Lazy.force fixture in
  let cases = member "compare_edges" data in
  let v0 = point_of_json (member "v0" cases) in
  let v1 = point_of_json (member "v1" cases) in
  let expected_00 = bool_of_json_exn (member "v0_v1_v0_v1" cases) in
  let expected_10 = bool_of_json_exn (member "v1_v0_v0_v1" cases) in
  let expected_01 = bool_of_json_exn (member "v0_v1_v1_v0" cases) in
  let expected_11 = bool_of_json_exn (member "v1_v0_v1_v0" cases) in
  check_bool
    "v0,v1 < v0,v1"
    ~expected:expected_00
    ~actual:(S2.S2_edge_crossings.compare_edges v0 v1 v0 v1);
  check_bool
    "v1,v0 < v0,v1"
    ~expected:expected_10
    ~actual:(S2.S2_edge_crossings.compare_edges v1 v0 v0 v1);
  check_bool
    "v0,v1 < v1,v0"
    ~expected:expected_01
    ~actual:(S2.S2_edge_crossings.compare_edges v0 v1 v1 v0);
  check_bool
    "v1,v0 < v1,v0"
    ~expected:expected_11
    ~actual:(S2.S2_edge_crossings.compare_edges v1 v0 v1 v0)
;;

(* ---------- robust_cross_prod coverage ----------------------------------- *)

let p x y z =
  S2.R3_vector.create
    ~x:(Float_u.of_float x)
    ~y:(Float_u.of_float y)
    ~z:(Float_u.of_float z)
;;

(* Like EXPECT_EQ on [result.Normalize()] in C++: after [Normalize], the
   direction is exactly what we expect. Uses [S2_point.approx_equal] with a
   tight tolerance so ulp-level differences from rescaling pass but any
   direction error fails loudly. *)
let check_direction label ~expected ~actual =
  let normalized = S2.R3_vector.normalize actual in
  let max_error =
    Packed_float_option.Unboxed.some Float_u.O.(#2.0 * Float_u.epsilon_float ())
  in
  if not (S2.S2_point.approx_equal ~max_error normalized expected)
  then
    Alcotest.failf
      "%s: expected direction %s, got %s (pre-normalize %s)"
      label
      (S2.R3_vector.to_string expected)
      (S2.R3_vector.to_string normalized)
      (S2.R3_vector.to_string actual)
;;

(* Subnormal inputs that force the exact-arithmetic fallback. Mirrors the
   EXACT-precision rows of C++ [RobustCrossProdCoverage] at
   s2edge_crossings_test.cc:217-222. *)
let test_robust_cross_prod_subnormal () =
  (* Exact result is scaled up when the direct [ExactCrossProd] cast would
     underflow. Inputs: (5e-324, 1, 0) x (0, 1, 0). *)
  check_direction
    "subnormal vs (0,1,0)"
    ~expected:(p 0.0 0.0 1.0)
    ~actual:(S2.S2_point.robust_cross_prod (p 5e-324 1.0 0.0) (p 0.0 1.0 0.0));
  (* Even when the exact cross product underflows in double precision: the z
     component of the real cross product is [-5e-324 * DBL_ERR ~= 2^-1127],
     well below the smallest subnormal. *)
  let dbl_err = Float.ldexp 1.0 (-53) in
  check_direction
    "subnormal vs (5e-324, 1-DBL_ERR, 0)"
    ~expected:(p 0.0 0.0 (-1.0))
    ~actual:
      (S2.S2_point.robust_cross_prod (p 5e-324 1.0 0.0) (p 5e-324 (1.0 -. dbl_err) 0.0))
;;

(* Symbolic-perturbation cases: [a] and [b] are exactly collinear in the
   reals, so the exact cross product is zero and [robust_cross_prod] falls
   through to [SymbolicCrossProd]. Mirrors s2edge_crossings_test.cc:225-230
   (after [Normalize]). *)
let test_robust_cross_prod_symbolic () =
  let dbl_epsilon = Float.ldexp 1.0 (-52) in
  check_direction
    "collinear x axis"
    ~expected:(p 0.0 1.0 0.0)
    ~actual:
      (S2.S2_point.robust_cross_prod (p 1.0 0.0 0.0) (p (1.0 +. dbl_epsilon) 0.0 0.0));
  check_direction
    "collinear y axis"
    ~expected:(p 1.0 0.0 0.0)
    ~actual:
      (S2.S2_point.robust_cross_prod (p 0.0 (1.0 +. dbl_epsilon) 0.0) (p 0.0 1.0 0.0));
  check_direction
    "antipodal z axis"
    ~expected:(p (-1.0) 0.0 0.0)
    ~actual:(S2.S2_point.robust_cross_prod (p 0.0 0.0 1.0) (p 0.0 0.0 (-1.0)))
;;

(* At 1e-100 scale, simply ensuring the [robust_cross_prod] result is
   normalizable is not sufficient: [Angle] between two such results would
   still underflow without the direction-preserving scaling that
   [ensure_normalizable] / [normalizable_from_exact] provide. Mirrors
   s2edge_crossings_test.cc:264-283. *)
let test_robust_cross_prod_magnitude () =
  let pi_2 = Float.pi /. 2.0 in
  let angle_between u v =
    Float_u.to_float (S2.S1_angle.radians (S2.R3_vector.angle u v))
  in
  let check label ~a ~b ~c ~d =
    let ab = S2.S2_point.robust_cross_prod a b in
    let cd = S2.S2_point.robust_cross_prod c d in
    let got = angle_between ab cd in
    if not (Float.equal got pi_2)
    then Alcotest.failf "%s: expected angle pi/2 (%.17g), got %.17g" label pi_2 got
  in
  (* Underflow in the double cross-product magnitude. *)
  check
    "underflow direction preserved"
    ~a:(p 1.0 0.0 0.0)
    ~b:(p 1.0 1e-100 0.0)
    ~c:(p 1.0 0.0 0.0)
    ~d:(p 1.0 0.0 1e-100);
  (* Same but the exact cross product itself underflows in double after the
     cast, forcing the [normalizable_from_exact] scaling path. *)
  check
    "exact-underflow direction preserved"
    ~a:(p (-1e-100) 0.0 1.0)
    ~b:(p 1e-100 0.0 (-1.0))
    ~c:(p 0.0 (-1e-100) 1.0)
    ~d:(p 0.0 1e-100 (-1.0))
;;

(* Sweep over the 27 points in {-1, 0, 1}^3 (excluding the origin after
   normalization) paired with three scalars that produce linearly-dependent
   non-degenerate pairs; for each pair [robust_cross_prod] must land on the
   CCW side of [a, b] per [S2_predicates.sign]. Mirrors
   s2edge_crossings_test.cc:242-262. *)
let test_symbolic_cross_prod_consistent_with_sign () =
  let dbl_err = Float.ldexp 1.0 (-53) in
  let coord_values = [ -1.0; 0.0; 1.0 ] in
  let scale_values = [ -1.0; 1.0 -. dbl_err; 1.0 +. (2.0 *. dbl_err) ] in
  List.iter coord_values ~f:(fun x ->
    List.iter coord_values ~f:(fun y ->
      List.iter coord_values ~f:(fun z ->
        let a_raw = p x y z in
        if not (S2.R3_vector.equal a_raw S2.R3_vector.zero)
        then (
          let a = S2.R3_vector.normalize a_raw in
          List.iter scale_values ~f:(fun scale ->
            let b = S2.R3_vector.mul a (Float_u.of_float scale) in
            let rcp = S2.S2_point.robust_cross_prod a b in
            let c = S2.R3_vector.normalize rcp in
            match S2.S2_predicates.robust_sign a b c with
            | Counter_clockwise -> ()
            | Clockwise | Indeterminate ->
              Alcotest.failf
                "robust_sign a b robust_cross_prod(a,b) not CCW: a=%s scale=%g"
                (S2.R3_vector.to_string a)
                scale)))))
;;

let () =
  Alcotest.run
    "s2_edge_crossings"
    [ "crossing_sign", [ Alcotest.test_case "crossings" `Quick test_crossings ]
    ; ( "angle_contains_vertex"
      , [ Alcotest.test_case "angle_contains_vertex" `Quick test_angle_contains_vertex ] )
    ; ( "vertex_crossing"
      , [ Alcotest.test_case "vertex_crossing" `Quick test_vertex_crossing ] )
    ; "sign", [ Alcotest.test_case "sign" `Quick test_sign ]
    ; "intersection", [ Alcotest.test_case "intersection" `Quick test_intersection ]
    ; "compare_edges", [ Alcotest.test_case "compare_edges" `Quick test_compare_edges ]
    ; ( "robust_cross_prod"
      , [ Alcotest.test_case "subnormal" `Quick test_robust_cross_prod_subnormal
        ; Alcotest.test_case "symbolic" `Quick test_robust_cross_prod_symbolic
        ; Alcotest.test_case "magnitude" `Quick test_robust_cross_prod_magnitude
        ; Alcotest.test_case
            "consistent_with_sign"
            `Quick
            test_symbolic_cross_prod_consistent_with_sign
        ] )
    ]
;;
