(* C++ test parity: s2geometry/src/s2/r2rect_test.cc
   -  TEST(R2Rect, EmptyRectangles)           - full parity (empty_rect)
   -  TEST(R2Rect, ConstructorsAndAccessors)  - full parity (constructors + accessors)
   -  TEST(R2Rect, FromCenterSize)            - full parity (constructors: both cases)
   -  TEST(R2Rect, FromPoint)                 - full parity (constructors)
   -  TEST(R2Rect, SimplePredicates)          - full parity (accessors, contains_point, ccw_vertices)
   -  TEST(R2Rect, IntervalOperations)        - full parity (interval_ops: all 9 C++ pairs)
   -  TEST(R2Rect, AddPoint)                  - full parity (add_point: same 4-point sequence)
   -  TEST(R2Rect, Project)                   - full parity (project: all 9 C++ cases)
   -  TEST(R2Rect, Expanded)                  - full parity (expanded: all 7 C++ cases)
   -  TEST(R2Rect, Expanded) uses ApproxEquals - approx_equal golden (no separate macro)

   Deliberately omitted (C++-only mutable API, no OCaml equivalent):
   - r[0] = R1Interval(3, 4), r[1][0] = 5 (mutable index assignment)
   - default constructor producing empty (OCaml has R2_rect.empty) *)

open Core
open Test_helpers

module R2_rect_pair = struct
  type t =
    { a : S2.R2_rect.t
    ; b : S2.R2_rect.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let finite = filter float ~f:Float.is_finite in
    let pt_gen rnd =
      let x = generate finite ~size:30 ~random:rnd in
      let y = generate finite ~size:30 ~random:rnd in
      S2.R2_point.create ~x:(Float_u.of_float x) ~y:(Float_u.of_float y)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = pt_gen rnd in
      let b = pt_gen rnd in
      let r1 = S2.R2_rect.from_point_pair a b in
      let r2 = S2.R2_rect.from_point_pair (pt_gen rnd) (pt_gen rnd) in
      { a = r1; b = r2 })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Rect_gen = struct
  type t = { rect : S2.R2_rect.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let finite = filter float ~f:Float.is_finite in
    let pt_gen rnd =
      let x = generate finite ~size:30 ~random:rnd in
      let y = generate finite ~size:30 ~random:rnd in
      S2.R2_point.create ~x:(Float_u.of_float x) ~y:(Float_u.of_float y)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = pt_gen rnd in
      let b = pt_gen rnd in
      { rect = S2.R2_rect.from_point_pair a b })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

let quickcheck_add_point_contains () =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      let p = S2.R2_point.create ~x:(Float_u.of_float 1.23) ~y:(Float_u.of_float 4.56) in
      let added = S2.R2_rect.add_point rect p in
      assert (S2.R2_rect.contains_point added p))
;;

let quickcheck_expanded_contains () =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      if not (S2.R2_rect.is_empty rect)
      then (
        let expanded =
          S2.R2_rect.expanded
            rect
            (S2.R2_point.create ~x:(Float_u.of_float 0.5) ~y:(Float_u.of_float 0.5))
        in
        assert (S2.R2_rect.contains_rect expanded rect)))
;;

let quickcheck_volume_nonneg () =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      if not (S2.R2_rect.is_empty rect)
      then (
        let dx = Float_u.to_float (S2.R1_interval.length (S2.R2_rect.x rect)) in
        let dy = Float_u.to_float (S2.R1_interval.length (S2.R2_rect.y rect)) in
        let vol = dx *. dy in
        assert (Float.( >= ) vol 0.0)))
;;

let quickcheck_project_in_rect () =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      if not (S2.R2_rect.is_empty rect)
      then (
        let p =
          S2.R2_point.create ~x:(Float_u.of_float 7.89) ~y:(Float_u.of_float 0.12)
        in
        let proj = S2.R2_rect.project rect p in
        assert (S2.R2_rect.contains_point rect proj)))
;;

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

let quickcheck_intersection_subset () =
  Base_quickcheck.Test.run_exn
    (module R2_rect_pair)
    ~config:qc_config
    ~f:(fun { R2_rect_pair.a; b } ->
      let c = S2.R2_rect.intersection a b in
      if S2.R2_rect.is_empty c
      then ()
      else (
        assert (S2.R2_rect.contains_rect a c);
        assert (S2.R2_rect.contains_rect b c)))
;;

let quickcheck_union_superset () =
  Base_quickcheck.Test.run_exn
    (module R2_rect_pair)
    ~config:qc_config
    ~f:(fun { R2_rect_pair.a; b } ->
      let u = S2.R2_rect.union a b in
      assert (S2.R2_rect.contains_rect u a);
      assert (S2.R2_rect.contains_rect u b))
;;

let test_empty_sexp () =
  Alcotest.(check string)
    "sexp_of empty"
    "((x((lo 1)(hi 0)))(y((lo 1)(hi 0))))"
    (Sexp.to_string_mach (S2.R2_rect.sexp_of_t S2.R2_rect.empty))
;;

let check_rect msg (expected : S2.R2_rect.t) actual =
  let ex = S2.R2_rect.x expected in
  let ey = S2.R2_rect.y expected in
  let ax = S2.R2_rect.x actual in
  let ay = S2.R2_rect.y actual in
  check_float_u_exact
    (msg ^ " x.lo")
    ~expected:(S2.R1_interval.lo ex)
    ~actual:(S2.R1_interval.lo ax);
  check_float_u_exact
    (msg ^ " x.hi")
    ~expected:(S2.R1_interval.hi ex)
    ~actual:(S2.R1_interval.hi ax);
  check_float_u_exact
    (msg ^ " y.lo")
    ~expected:(S2.R1_interval.lo ey)
    ~actual:(S2.R1_interval.lo ay);
  check_float_u_exact
    (msg ^ " y.hi")
    ~expected:(S2.R1_interval.hi ey)
    ~actual:(S2.R1_interval.hi ay)
;;

let test_empty_rect fixture () =
  let c = member "empty_rect" fixture in
  Alcotest.(check bool)
    "is_valid"
    (bool_of_json_exn (member "is_valid" c))
    (S2.R2_rect.is_valid S2.R2_rect.empty);
  Alcotest.(check bool)
    "is_empty"
    (bool_of_json_exn (member "is_empty" c))
    (S2.R2_rect.is_empty S2.R2_rect.empty);
  Alcotest.(check bool)
    "equal_self"
    (bool_of_json_exn (member "equal_self" c))
    (S2.R2_rect.equal S2.R2_rect.empty S2.R2_rect.empty)
;;

let test_constructors fixture () =
  let cases = to_list (member "constructors" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let expected = r2_rect_of_json (member "expected" c) in
    let result =
      match op with
      | "create" ->
        let lo = r2_point_of_json (member "lo" c) in
        let hi = r2_point_of_json (member "hi" c) in
        S2.R2_rect.create_exn ~lo ~hi
      | "from_center_size" ->
        let center = r2_point_of_json (member "center" c) in
        let size = r2_point_of_json (member "size" c) in
        S2.R2_rect.from_center_size ~center ~size
      | "from_point" ->
        let p = r2_point_of_json (member "p" c) in
        S2.R2_rect.from_point p
      | "from_point_pair" ->
        let p1 = r2_point_of_json (member "p1" c) in
        let p2 = r2_point_of_json (member "p2" c) in
        S2.R2_rect.from_point_pair p1 p2
      | _ ->
        (match failwith (sprintf "unknown op: %s" op) with
         | (_ : Nothing.t) -> .)
    in
    check_rect (sprintf "constructor %s" op) expected result [@nontail];
    match op with
    | "create" ->
      check_float_u_exact
        "x_lo"
        ~expected:(float_u_of_json_exn (member "x_lo" c))
        ~actual:(S2.R1_interval.lo (S2.R2_rect.x result));
      check_float_u_exact
        "x_hi"
        ~expected:(float_u_of_json_exn (member "x_hi" c))
        ~actual:(S2.R1_interval.hi (S2.R2_rect.x result));
      check_float_u_exact
        "y_lo"
        ~expected:(float_u_of_json_exn (member "y_lo" c))
        ~actual:(S2.R1_interval.lo (S2.R2_rect.y result));
      check_float_u_exact
        "y_hi"
        ~expected:(float_u_of_json_exn (member "y_hi" c))
        ~actual:(S2.R1_interval.hi (S2.R2_rect.y result))
    | _ -> ())
;;

let test_accessors fixture () =
  let cases = to_list (member "accessors" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let r = r2_rect_of_json (member "rect" c) in
    check_r2_point_exact
      (name ^ " lo")
      ~expected:(r2_point_of_json (member "lo" c))
      ~actual:(S2.R2_rect.lo r);
    check_r2_point_exact
      (name ^ " hi")
      ~expected:(r2_point_of_json (member "hi" c))
      ~actual:(S2.R2_rect.hi r);
    check_r2_point_exact
      (name ^ " center")
      ~expected:(r2_point_of_json (member "center" c))
      ~actual:(S2.R2_rect.center r);
    check_r2_point_exact
      (name ^ " size")
      ~expected:(r2_point_of_json (member "size" c))
      ~actual:(S2.R2_rect.size r);
    Alcotest.(check bool)
      (name ^ " is_empty")
      (bool_of_json_exn (member "is_empty" c))
      (S2.R2_rect.is_empty r);
    Alcotest.(check bool)
      (name ^ " is_valid")
      (bool_of_json_exn (member "is_valid" c))
      (S2.R2_rect.is_valid r);
    check_r2_point_exact
      (name ^ " v0")
      ~expected:(r2_point_of_json (member "v0" c))
      ~actual:(S2.R2_rect.get_vertex r 0);
    check_r2_point_exact
      (name ^ " v1")
      ~expected:(r2_point_of_json (member "v1" c))
      ~actual:(S2.R2_rect.get_vertex r 1);
    check_r2_point_exact
      (name ^ " v2")
      ~expected:(r2_point_of_json (member "v2" c))
      ~actual:(S2.R2_rect.get_vertex r 2);
    check_r2_point_exact
      (name ^ " v3")
      ~expected:(r2_point_of_json (member "v3" c))
      ~actual:(S2.R2_rect.get_vertex r 3) [@nontail])
;;

let test_contains_point fixture () =
  let cases = to_list (member "contains_point" fixture) in
  List.iter cases ~f:(fun c ->
    let r = r2_rect_of_json (member "rect" c) in
    let p = r2_point_of_json (member "p" c) in
    let label = S2.R2_point.to_string p in
    Alcotest.(check bool)
      (label ^ " contains")
      (bool_of_json_exn (member "contains" c))
      (S2.R2_rect.contains_point r p);
    Alcotest.(check bool)
      (label ^ " interior_contains")
      (bool_of_json_exn (member "interior_contains" c))
      (S2.R2_rect.interior_contains_point r p))
;;

let test_ccw_vertices _fixture () =
  let r =
    S2.R2_rect.create_exn
      ~lo:(S2.R2_point.create ~x:#0.0 ~y:#0.25)
      ~hi:(S2.R2_point.create ~x:#0.5 ~y:#0.75)
  in
  for k = 0 to 3 do
    let a = S2.R2_rect.get_vertex r (k - 1) in
    let b = S2.R2_rect.get_vertex r k in
    let c = S2.R2_rect.get_vertex r (k + 1) in
    let ortho_x = Float_u.neg Float_u.(S2.R2_point.y b - S2.R2_point.y a) in
    let ortho_y = Float_u.(S2.R2_point.x b - S2.R2_point.x a) in
    let dx = Float_u.(S2.R2_point.x c - S2.R2_point.x a) in
    let dy = Float_u.(S2.R2_point.y c - S2.R2_point.y a) in
    let cross = Float_u.((ortho_x * dx) + (ortho_y * dy)) in
    Alcotest.(check bool) (sprintf "ccw k=%d" k) true Float_u.(cross > #0.0)
  done
;;

let test_interval_ops fixture () =
  let cases = to_list (member "interval_ops" fixture) in
  List.iteri cases ~f:(fun i c ->
    let x = r2_rect_of_json (member "x" c) in
    let y = r2_rect_of_json (member "y" c) in
    let label =
      sprintf "pair %d: %s vs %s" i (S2.R2_rect.to_string x) (S2.R2_rect.to_string y)
    in
    Alcotest.(check bool)
      (label ^ " contains")
      (bool_of_json_exn (member "contains" c))
      (S2.R2_rect.contains_rect x y);
    Alcotest.(check bool)
      (label ^ " interior_contains")
      (bool_of_json_exn (member "interior_contains" c))
      (S2.R2_rect.interior_contains_rect x y);
    Alcotest.(check bool)
      (label ^ " intersects")
      (bool_of_json_exn (member "intersects" c))
      (S2.R2_rect.intersects x y);
    Alcotest.(check bool)
      (label ^ " interior_intersects")
      (bool_of_json_exn (member "interior_intersects" c))
      (S2.R2_rect.interior_intersects x y);
    check_rect
      (label ^ " union")
      (r2_rect_of_json (member "union" c))
      (S2.R2_rect.union x y);
    check_rect
      (label ^ " intersection")
      (r2_rect_of_json (member "intersection" c))
      (S2.R2_rect.intersection x y);
    check_rect
      (label ^ " add_rect")
      (r2_rect_of_json (member "add_rect" c))
      (S2.R2_rect.add_rect x y);
    (* C++ invariants *)
    Alcotest.(check bool)
      (label ^ " contains_iff_union_eq")
      (bool_of_json_exn (member "contains" c))
      (S2.R2_rect.equal (S2.R2_rect.union x y) x);
    Alcotest.(check bool)
      (label ^ " intersects_iff_nonempty_intersection")
      (bool_of_json_exn (member "intersects" c))
      (not (S2.R2_rect.is_empty (S2.R2_rect.intersection x y))))
;;

let test_add_point fixture () =
  let cases = to_list (member "add_point" fixture) in
  let module Acc = struct
    type t = { mutable r : S2.R2_rect.t }
  end
  in
  let acc = { Acc.r = S2.R2_rect.empty } in
  List.iter cases ~f:(fun c ->
    match member "p" c with
    | `Null -> ()
    | _ ->
      let p = r2_point_of_json (member "p" c) in
      let expected = r2_rect_of_json (member "after" c) in
      let result = S2.R2_rect.add_point acc.r p in
      let label = sprintf "add %s" (S2.R2_point.to_string p) in
      check_rect label expected result;
      acc.r <- result);
  let target =
    S2.R2_rect.create_exn
      ~lo:(S2.R2_point.create ~x:#0.0 ~y:#0.25)
      ~hi:(S2.R2_point.create ~x:#0.5 ~y:#0.75)
  in
  Alcotest.(check bool) "final rect equals target" true (S2.R2_rect.equal acc.r target)
;;

let test_project fixture () =
  let cases = to_list (member "project" fixture) in
  List.iter cases ~f:(fun c ->
    let r = r2_rect_of_json (member "rect" c) in
    let p = r2_point_of_json (member "p" c) in
    let expected = r2_point_of_json (member "projected" c) in
    let label =
      sprintf "project %s onto %s" (S2.R2_point.to_string p) (S2.R2_rect.to_string r)
    in
    let actual = S2.R2_rect.project r p in
    check_r2_point_exact label ~expected ~actual)
;;

let test_expanded fixture () =
  let cases = to_list (member "expanded" fixture) in
  List.iter cases ~f:(fun c ->
    let r = r2_rect_of_json (member "rect" c) in
    let expected_empty = bool_of_json_exn (member "expected_empty" c) in
    let result =
      match member "margin_scalar" c with
      | `Float s -> S2.R2_rect.expanded_scalar r s
      | `Int s -> S2.R2_rect.expanded_scalar r (Float.of_int s)
      | _ ->
        let margin = r2_point_of_json (member "margin" c) in
        S2.R2_rect.expanded r margin
    in
    Alcotest.(check bool) "is_empty" expected_empty (S2.R2_rect.is_empty result);
    if not expected_empty
    then (
      let expected = r2_rect_of_json (member "expected" c) in
      Alcotest.(check bool) "approx_equal" true (S2.R2_rect.approx_equal expected result)))
;;

let test_approx_equal fixture () =
  let cases = to_list (member "approx_equal" fixture) in
  List.iter cases ~f:(fun c ->
    let r1 = r2_rect_of_json (member "r1" c) in
    let r2 = r2_rect_of_json (member "r2" c) in
    let expected = bool_of_json_exn (member "expected" c) in
    Alcotest.(check bool) "approx_equal" expected (S2.R2_rect.approx_equal r1 r2))
;;

let () =
  let fixture = load_fixture "r2rect.json" in
  Alcotest.run
    "R2_rect"
    [ ( "empty_rect"
      , [ Alcotest.test_case "EmptyRectangles" `Quick (test_empty_rect fixture) ] )
    ; ( "constructors"
      , [ Alcotest.test_case "ConstructorsAndAccessors" `Quick (test_constructors fixture)
        ] )
    ; ( "accessors"
      , [ Alcotest.test_case "ConstructorsAndAccessors" `Quick (test_accessors fixture) ]
      )
    ; ( "contains_point"
      , [ Alcotest.test_case "SimplePredicates" `Quick (test_contains_point fixture) ] )
    ; ( "ccw_vertices"
      , [ Alcotest.test_case "SimplePredicates" `Quick (test_ccw_vertices fixture) ] )
    ; ( "interval_ops"
      , [ Alcotest.test_case "IntervalOperations" `Quick (test_interval_ops fixture) ] )
    ; "add_point", [ Alcotest.test_case "AddPoint" `Quick (test_add_point fixture) ]
    ; "project", [ Alcotest.test_case "Project" `Quick (test_project fixture) ]
    ; "expanded", [ Alcotest.test_case "Expanded" `Quick (test_expanded fixture) ]
    ; ( "approx_equal"
      , [ Alcotest.test_case "ApproxEquals" `Quick (test_approx_equal fixture) ] )
    ; "sexp", [ Alcotest.test_case "empty" `Quick test_empty_sexp ]
    ; ( "quickcheck"
      , [ Alcotest.test_case "intersection_subset" `Quick quickcheck_intersection_subset
        ; Alcotest.test_case "union_superset" `Quick quickcheck_union_superset
        ; Alcotest.test_case "add_point_contains" `Quick quickcheck_add_point_contains
        ; Alcotest.test_case "expanded_contains" `Quick quickcheck_expanded_contains
        ; Alcotest.test_case "volume_nonneg" `Quick quickcheck_volume_nonneg
        ; Alcotest.test_case "project_in_rect" `Quick quickcheck_project_in_rect
        ] )
    ]
;;
