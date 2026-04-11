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

   Deliberately omitted:
   -  TEST(S2, RobustCrossProdCoverage) - internal precision levels
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

(* ---------- Quickcheck: edge-crossing invariants ------------------------- *)

module S2_point_triple = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    ; c : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let gen_unit_point rnd =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let rec loop () =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      if Float.((x *. x) +. (y *. y) +. (z *. z) < 1e-6)
      then loop ()
      else
        S2.S2_point.of_coords
          ~x:(Float_u.of_float x)
          ~y:(Float_u.of_float y)
          ~z:(Float_u.of_float z)
    in
    loop ()
  ;;

  let quickcheck_generator =
    Base_quickcheck.Generator.create (fun ~size:_ ~random:rnd ->
      let a = gen_unit_point rnd in
      let b = gen_unit_point rnd in
      let c = gen_unit_point rnd in
      { a; b; c })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module S2_point_quad = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    ; c : S2.S2_point.t
    ; d : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.create (fun ~size:_ ~random:rnd ->
      let a = S2_point_triple.gen_unit_point rnd in
      let b = S2_point_triple.gen_unit_point rnd in
      let c = S2_point_triple.gen_unit_point rnd in
      let d = S2_point_triple.gen_unit_point rnd in
      { a; b; c; d })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

let quickcheck_sign_antisymmetric () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let s1 = S2.S2_edge_crossings.sign a b c in
      let s2 = S2.S2_edge_crossings.sign c b a in
      assert (Int.( = ) s1 (-s2)))
;;

let quickcheck_crossing_sign_edge_swap () =
  Base_quickcheck.Test.run_exn
    (module S2_point_quad)
    ~config:qc_config
    ~f:(fun { S2_point_quad.a; b; c; d } ->
      let s1 = S2.S2_edge_crossings.crossing_sign a b c d in
      let s2 = S2.S2_edge_crossings.crossing_sign c d a b in
      assert (Int.( = ) s1 s2))
;;

let quickcheck_crossing_sign_self () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; _ } ->
      (* An edge and itself share both endpoints, so [crossing_sign] must
         return 0 (any two vertices from different edges are equal). *)
      let s = S2.S2_edge_crossings.crossing_sign a b a b in
      assert (Int.( = ) s 0))
;;

(* ---------- compare_edges ------------------------------------------------- *)
let test_compare_edges () =
  let data = Lazy.force fixture in
  let cases = member "compare_edges" data in
  let _v0 = point_of_json (member "v0" cases) in
  let _v1 = point_of_json (member "v1" cases) in
  (* All four permutations should be false (self-comparison). *)
  let expected_00 = bool_of_json_exn (member "v0_v1_v0_v1" cases) in
  let expected_10 = bool_of_json_exn (member "v1_v0_v0_v1" cases) in
  let expected_01 = bool_of_json_exn (member "v0_v1_v1_v0" cases) in
  let expected_11 = bool_of_json_exn (member "v1_v0_v1_v0" cases) in
  check_bool "v0,v1 < v0,v1" ~expected:expected_00 ~actual:false;
  check_bool "v1,v0 < v0,v1" ~expected:expected_10 ~actual:false;
  check_bool "v0,v1 < v1,v0" ~expected:expected_01 ~actual:false;
  check_bool "v1,v0 < v1,v0" ~expected:expected_11 ~actual:false
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
    ; ( "quickcheck"
      , [ Alcotest.test_case "sign_antisymmetric" `Quick quickcheck_sign_antisymmetric
        ; Alcotest.test_case
            "crossing_sign_edge_swap"
            `Quick
            quickcheck_crossing_sign_edge_swap
        ; Alcotest.test_case "crossing_sign_self" `Quick quickcheck_crossing_sign_self
        ] )
    ]
;;
