(* C++ test parity: s2geometry/src/s2/s1interval_test.cc

   Golden vectors from test/gen/s1interval.cc (same named intervals as
   [S1IntervalTestBase] in the C++ file):

   -  TEST_F(S1IntervalTestBase, ConstructorsAndAccessors) - constructors
   -  TEST_F(S1IntervalTestBase, SimplePredicates) - accessors (lo/hi/predicates)
   -  TEST_F(..., GetCenter), GetLength         - accessors
   -  TEST_F(..., Complement)                   - complement (+ complement_center)
   -  TEST_F(..., Contains)                     - contains_point
   -  TEST_F(..., IntervalOps)                  - interval_ops
   -  TEST_F(..., AddPoint)                     - add_point
   -  TEST_F(..., Project)                      - project
   -  TEST_F(..., FromPointPair)                - constructors (from_point_pair)
   -  TEST_F(..., Expanded)                     - expanded
   -  TEST_F(..., ApproxEquals)                 - approx_equal
   -  TEST_F(..., GetDirectedHausdorffDistance) - directed_hausdorff
   -  TEST(S1Interval, IsValidPoint)            - is_valid_point
   -  TEST_F(..., AlmostEmptyOrFull)            - almost_empty_full

   [IntervalOps] in C++ includes additional mid*/quad* combinations; the generator uses a
   Cartesian grid of 11 interval kinds (see test/gen/s1interval.cc).

   Deliberately omitted (C++-only API): [operator[]], [bounds()], mutable endpoints;
   [OperatorEquals] is redundant with structural checks. *)

open Core
open Test_helpers
open Alcotest

module S1_pair = struct
  type t =
    { a : S2.S1_interval.t
    ; b : S2.S1_interval.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let angle = float_inclusive (-.Float.pi) Float.pi in
    create (fun ~size:_ ~random:rnd ->
      let p1 = Float_u.of_float (generate angle ~size:30 ~random:rnd) in
      let p2 = Float_u.of_float (generate angle ~size:30 ~random:rnd) in
      let q1 = Float_u.of_float (generate angle ~size:30 ~random:rnd) in
      let q2 = Float_u.of_float (generate angle ~size:30 ~random:rnd) in
      { a = S2.S1_interval.from_point_pair p1 p2
      ; b = S2.S1_interval.from_point_pair q1 q2
      })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module S1_interval_gen = struct
  type t = { interval : S2.S1_interval.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let angle = float_inclusive (-.Float.pi) Float.pi in
    create (fun ~size:_ ~random:rnd ->
      let p1 = Float_u.of_float (generate angle ~size:30 ~random:rnd) in
      let p2 = Float_u.of_float (generate angle ~size:30 ~random:rnd) in
      { interval = S2.S1_interval.from_point_pair p1 p2 })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

let quickcheck_intersection_subset () =
  Base_quickcheck.Test.run_exn
    (module S1_pair)
    ~config:qc_config
    ~f:(fun { S1_pair.a; b } ->
      let c = S2.S1_interval.intersection a b in
      if S2.S1_interval.is_empty c
      then ()
      else (
        assert (S2.S1_interval.contains_interval a c);
        assert (S2.S1_interval.contains_interval b c)))
;;

let quickcheck_union_superset () =
  Base_quickcheck.Test.run_exn
    (module S1_pair)
    ~config:qc_config
    ~f:(fun { S1_pair.a; b } ->
      let u = S2.S1_interval.union a b in
      assert (S2.S1_interval.contains_interval u a);
      assert (S2.S1_interval.contains_interval u b))
;;

let quickcheck_add_point_contains () =
  Base_quickcheck.Test.run_exn
    (module S1_interval_gen)
    ~config:qc_config
    ~f:(fun { S1_interval_gen.interval } ->
      let p = Float_u.of_float 1.23 in
      let added = S2.S1_interval.add_point interval p in
      assert (S2.S1_interval.contains added p))
;;

let quickcheck_expanded_contains () =
  Base_quickcheck.Test.run_exn
    (module S1_interval_gen)
    ~config:qc_config
    ~f:(fun { S1_interval_gen.interval } ->
      if not (S2.S1_interval.is_empty interval)
      then (
        let margin = Float_u.of_float 0.5 in
        let expanded = S2.S1_interval.expanded interval margin in
        assert (S2.S1_interval.contains_interval expanded interval)))
;;

let quickcheck_length_nonneg () =
  Base_quickcheck.Test.run_exn
    (module S1_interval_gen)
    ~config:qc_config
    ~f:(fun { S1_interval_gen.interval } ->
      let len = Float_u.to_float (S2.S1_interval.length interval) in
      assert (Float.( >= ) len 0.0))
;;

let quickcheck_equal_reflexive () =
  Base_quickcheck.Test.run_exn
    (module S1_interval_gen)
    ~config:qc_config
    ~f:(fun { S1_interval_gen.interval } ->
      assert (S2.S1_interval.equal interval interval))
;;

let quickcheck_project_in_range () =
  Base_quickcheck.Test.run_exn
    (module S1_interval_gen)
    ~config:qc_config
    ~f:(fun { S1_interval_gen.interval } ->
      if not (S2.S1_interval.is_empty interval)
      then (
        let p = Float_u.of_float 2.34 in
        let proj = S2.S1_interval.project interval p in
        assert (S2.S1_interval.contains interval proj)))
;;

let test_empty_full_sexp () =
  (check string)
    "sexp_of empty"
    "((lo 3.1415926535897931)(hi -3.1415926535897931))"
    (Sexp.to_string (S2.S1_interval.sexp_of_t S2.S1_interval.empty));
  (check string)
    "sexp_of full"
    "((lo -3.1415926535897931)(hi 3.1415926535897931))"
    (Sexp.to_string (S2.S1_interval.sexp_of_t S2.S1_interval.full))
;;

let named_interval =
  let open Float_u.O in
  let pi = Float_u.pi () in
  let mipi = Float_u.neg (Float_u.pi ()) in
  function
  | "empty" -> S2.S1_interval.empty
  | "full" -> S2.S1_interval.full
  | "zero" -> S2.S1_interval.create ~lo:#0.0 ~hi:#0.0
  | "pi" -> S2.S1_interval.create ~lo:pi ~hi:pi
  | "mipi" -> S2.S1_interval.create ~lo:mipi ~hi:mipi
  | "pi2" -> S2.S1_interval.create ~lo:(Float_u.pi () / #2.) ~hi:(Float_u.pi () / #2.)
  | "mipi2" ->
    S2.S1_interval.create
      ~lo:(Float_u.neg (Float_u.pi () / #2.))
      ~hi:(Float_u.neg (Float_u.pi () / #2.))
  | "quad12" -> S2.S1_interval.create ~lo:#0.0 ~hi:pi
  | "quad23" ->
    S2.S1_interval.create
      ~lo:(Float_u.pi () / #2.)
      ~hi:(Float_u.neg (Float_u.pi () / #2.))
  | "quad34" -> S2.S1_interval.create ~lo:pi ~hi:#0.0
  | "quad123" -> S2.S1_interval.create ~lo:#0.0 ~hi:(Float_u.neg (Float_u.pi () / #2.))
  | name ->
    (match failwith (sprintf "unknown named interval: %s" name) with
     | (_ : Nothing.t) -> .)
;;

let test_constructors fixture () =
  let cases = to_list (member "constructors" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let expected = s1_interval_of_json (member "expected" c) in
    let result =
      match op with
      | "default" -> S2.S1_interval.empty
      | "empty" -> S2.S1_interval.empty
      | "full" -> S2.S1_interval.full
      | "from_point" -> S2.S1_interval.from_point (float_u_of_json_exn (member "p" c))
      | "from_point_pair" ->
        S2.S1_interval.from_point_pair
          (float_u_of_json_exn (member "p1" c))
          (float_u_of_json_exn (member "p2" c))
      | _ ->
        (match failwith (sprintf "unknown op: %s" op) with
         | (_ : Nothing.t) -> .)
    in
    let label = sprintf "constructor %s" op in
    check_s1_interval_exact label ~expected ~actual:result)
;;

let test_accessors fixture () =
  let cases = to_list (member "accessors" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let i = named_interval name in
    let label = sprintf "accessor %s" name in
    check_float_u_exact
      (label ^ " lo")
      ~expected:(float_u_of_json_exn (member "lo" c))
      ~actual:(S2.S1_interval.lo i);
    check_float_u_exact
      (label ^ " hi")
      ~expected:(float_u_of_json_exn (member "hi" c))
      ~actual:(S2.S1_interval.hi i);
    (check bool)
      (label ^ " is_valid")
      (bool_of_json_exn (member "is_valid" c))
      (S2.S1_interval.is_valid i);
    (check bool)
      (label ^ " is_empty")
      (bool_of_json_exn (member "is_empty" c))
      (S2.S1_interval.is_empty i);
    (check bool)
      (label ^ " is_full")
      (bool_of_json_exn (member "is_full" c))
      (S2.S1_interval.is_full i);
    (check bool)
      (label ^ " is_inverted")
      (bool_of_json_exn (member "is_inverted" c))
      (S2.S1_interval.is_inverted i);
    check_float_u
      (label ^ " center")
      ~expected:(float_u_of_json_exn (member "center" c))
      ~actual:(S2.S1_interval.center i);
    check_float_u_exact
      (label ^ " length")
      ~expected:(float_u_of_json_exn (member "length" c))
      ~actual:(S2.S1_interval.length i))
;;

let test_complement fixture () =
  let cases = to_list (member "complement" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let i = s1_interval_of_json (member "interval" c) in
    let expected = s1_interval_of_json (member "complement" c) in
    let result = S2.S1_interval.complement i in
    let label = sprintf "complement %s" name in
    check_s1_interval_exact label ~expected ~actual:result;
    check_float_u
      (label ^ " center")
      ~expected:(float_u_of_json_exn (member "complement_center" c))
      ~actual:(S2.S1_interval.complement_center i))
;;

let test_contains_point fixture () =
  let cases = to_list (member "contains_point" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let i = s1_interval_of_json (member "interval" c) in
    let p = float_u_of_json_exn (member "p" c) in
    let label = sprintf "%s contains %g" name (Float_u.to_float p) in
    (check bool)
      label
      (bool_of_json_exn (member "contains" c))
      (S2.S1_interval.contains i p);
    (check bool)
      (label ^ " interior")
      (bool_of_json_exn (member "interior_contains" c))
      (S2.S1_interval.interior_contains i p))
;;

let test_interval_ops fixture () =
  let cases = to_list (member "interval_ops" fixture) in
  List.iter cases ~f:(fun c ->
    let x = s1_interval_of_json (member "x" c) in
    let y = s1_interval_of_json (member "y" c) in
    let xn = string_of_json_exn (member "x_name" c) in
    let yn = string_of_json_exn (member "y_name" c) in
    let label = sprintf "%s vs %s" xn yn in
    (check bool)
      (label ^ " contains")
      (bool_of_json_exn (member "contains" c))
      (S2.S1_interval.contains_interval x y);
    (check bool)
      (label ^ " interior_contains")
      (bool_of_json_exn (member "interior_contains" c))
      (S2.S1_interval.interior_contains_interval x y);
    (check bool)
      (label ^ " intersects")
      (bool_of_json_exn (member "intersects" c))
      (S2.S1_interval.intersects x y);
    (check bool)
      (label ^ " interior_intersects")
      (bool_of_json_exn (member "interior_intersects" c))
      (S2.S1_interval.interior_intersects x y);
    let eu = s1_interval_of_json (member "union" c) in
    let au = S2.S1_interval.union x y in
    check_s1_interval_exact (label ^ " union") ~expected:eu ~actual:au;
    let ei = s1_interval_of_json (member "intersection" c) in
    let ai = S2.S1_interval.intersection x y in
    check_s1_interval_exact (label ^ " intersection") ~expected:ei ~actual:ai [@nontail])
;;

let test_add_point fixture () =
  let cases = to_list (member "add_point" fixture) in
  List.iter cases ~f:(fun c ->
    match member "op" c with
    | `String "add_pi_then_mipi" ->
      let result =
        S2.S1_interval.(
          add_point (add_point empty (Float_u.pi ())) (Float_u.neg (Float_u.pi ())))
      in
      let expected = s1_interval_of_json (member "expected" c) in
      check_float_u_exact
        "add pi then mipi lo"
        ~expected:(S2.S1_interval.lo expected)
        ~actual:(S2.S1_interval.lo result);
      check_float_u_exact
        "add pi then mipi hi"
        ~expected:(S2.S1_interval.hi expected)
        ~actual:(S2.S1_interval.hi result)
    | `String "add_mipi_then_pi" ->
      let result =
        S2.S1_interval.(
          add_point (add_point empty (Float_u.neg (Float_u.pi ()))) (Float_u.pi ()))
      in
      let expected = s1_interval_of_json (member "expected" c) in
      check_float_u_exact
        "add mipi then pi lo"
        ~expected:(S2.S1_interval.lo expected)
        ~actual:(S2.S1_interval.lo result);
      check_float_u_exact
        "add mipi then pi hi"
        ~expected:(S2.S1_interval.hi expected)
        ~actual:(S2.S1_interval.hi result)
    | _ ->
      let i = s1_interval_of_json (member "interval" c) in
      let p = float_u_of_json_exn (member "p" c) in
      let result = S2.S1_interval.add_point i p in
      let expected = s1_interval_of_json (member "expected" c) in
      let label = sprintf "add_point %s" (Float_u.to_string p) in
      check_float_u_exact
        (label ^ " lo")
        ~expected:(S2.S1_interval.lo expected)
        ~actual:(S2.S1_interval.lo result);
      check_float_u_exact
        (label ^ " hi")
        ~expected:(S2.S1_interval.hi expected)
        ~actual:(S2.S1_interval.hi result))
;;

let test_project fixture () =
  let cases = to_list (member "project" fixture) in
  List.iter cases ~f:(fun c ->
    let i = s1_interval_of_json (member "interval" c) in
    let p = float_u_of_json_exn (member "p" c) in
    check_float_u_exact
      (sprintf "project %s" (Float_u.to_string p))
      ~expected:(float_u_of_json_exn (member "projected" c))
      ~actual:(S2.S1_interval.project i p))
;;

let test_expanded fixture () =
  let cases = to_list (member "expanded" fixture) in
  List.iter cases ~f:(fun c ->
    let i = s1_interval_of_json (member "interval" c) in
    let margin = float_u_of_json_exn (member "margin" c) in
    let result = S2.S1_interval.expanded i margin in
    let expected = s1_interval_of_json (member "expected" c) in
    let label = sprintf "expanded %s" (Float_u.to_string margin) in
    check_float_u
      (label ^ " lo")
      ~expected:(S2.S1_interval.lo expected)
      ~actual:(S2.S1_interval.lo result);
    check_float_u
      (label ^ " hi")
      ~expected:(S2.S1_interval.hi expected)
      ~actual:(S2.S1_interval.hi result))
;;

let test_approx_equal fixture () =
  let cases = to_list (member "approx_equal" fixture) in
  List.iter cases ~f:(fun c ->
    let x = s1_interval_of_json (member "x" c) in
    let y = s1_interval_of_json (member "y" c) in
    let expected = bool_of_json_exn (member "approx_equal" c) in
    (check bool)
      (sprintf "approx %s %s" (S2.S1_interval.to_string x) (S2.S1_interval.to_string y))
      expected
      (S2.S1_interval.approx_equal x y))
;;

let test_directed_hausdorff fixture () =
  let cases = to_list (member "directed_hausdorff" fixture) in
  List.iter cases ~f:(fun c ->
    let x = s1_interval_of_json (member "x" c) in
    let y = s1_interval_of_json (member "y" c) in
    let result = S2.S1_interval.directed_hausdorff_distance x y in
    check_float_u
      "hausdorff"
      ~expected:(float_u_of_json_exn (member "distance" c))
      ~actual:result)
;;

let test_is_valid_point fixture () =
  let cases = to_list (member "is_valid_point" fixture) in
  List.iter cases ~f:(fun c ->
    let p = float_u_of_json_exn (member "p" c) in
    (check bool)
      (sprintf "is_valid_point %g" (Float_u.to_float p))
      (bool_of_json_exn (member "valid" c))
      (S2.S1_interval.is_valid_point p))
;;

let test_almost_empty_full fixture () =
  let cases = to_list (member "almost_empty_full" fixture) in
  List.iter cases ~f:(fun c ->
    let i = s1_interval_of_json (member "interval" c) in
    let label = S2.S1_interval.to_string i in
    (check bool)
      (label ^ " is_full")
      (bool_of_json_exn (member "is_full" c))
      (S2.S1_interval.is_full i);
    (check bool)
      (label ^ " is_empty")
      (bool_of_json_exn (member "is_empty" c))
      (S2.S1_interval.is_empty i))
;;

let () =
  let fixture = load_fixture "s1interval.json" in
  Alcotest.run
    "S1_interval"
    [ ( "constructors"
      , [ test_case "ConstructorsAndAccessors" `Quick (test_constructors fixture) ] )
    ; "accessors", [ test_case "SimplePredicates" `Quick (test_accessors fixture) ]
    ; "complement", [ test_case "Complement" `Quick (test_complement fixture) ]
    ; "contains_point", [ test_case "Contains" `Quick (test_contains_point fixture) ]
    ; "interval_ops", [ test_case "IntervalOps" `Quick (test_interval_ops fixture) ]
    ; "add_point", [ test_case "AddPoint" `Quick (test_add_point fixture) ]
    ; "project", [ test_case "Project" `Quick (test_project fixture) ]
    ; "expanded", [ test_case "Expanded" `Quick (test_expanded fixture) ]
    ; "approx_equal", [ test_case "ApproxEquals" `Quick (test_approx_equal fixture) ]
    ; ( "directed_hausdorff"
      , [ test_case
            "GetDirectedHausdorffDistance"
            `Quick
            (test_directed_hausdorff fixture)
        ] )
    ; "is_valid_point", [ test_case "IsValidPoint" `Quick (test_is_valid_point fixture) ]
    ; ( "almost_empty_full"
      , [ test_case "AlmostEmptyOrFull" `Quick (test_almost_empty_full fixture) ] )
    ; "sexp", [ test_case "empty_full" `Quick test_empty_full_sexp ]
    ; ( "quickcheck"
      , [ test_case "intersection_subset" `Quick quickcheck_intersection_subset
        ; test_case "union_superset" `Quick quickcheck_union_superset
        ; test_case "add_point_contains" `Quick quickcheck_add_point_contains
        ; test_case "expanded_contains" `Quick quickcheck_expanded_contains
        ; test_case "length_nonneg" `Quick quickcheck_length_nonneg
        ; test_case "equal_reflexive" `Quick quickcheck_equal_reflexive
        ; test_case "project_in_range" `Quick quickcheck_project_in_range
        ] )
    ]
;;
