(* Reference: no dedicated r2point_test.cc upstream; Vector2<double> (util/math/vector.h).
   Go coverage in r2/rect_test.go (Alcotest case names mirror these where applicable):
   -  TEST(Go, TestOrtho)      - ortho
   -  TEST(Go, TestDot)        - arithmetic (dot)
   -  TEST(Go, TestCross)      - arithmetic (cross)
   -  TEST(Go, TestNorm)       - norm
   -  TEST(Go, TestNormalize)  - normalize

   Golden-only (no direct Go/C++ macro); Alcotest case name in parentheses:
   -  GoldenConstructors - constructors (zero, create, negative-zero)
   -  GoldenMul, GoldenDiv, GoldenNeg - mul, div, neg
   -  GoldenAngle, GoldenFabs, GoldenEquality - angle, fabs, equality
   -  SexpOf_t - sexp *)

open Core
open Test_helpers

module R2_pair = struct
  type t =
    { a : S2.R2_point.t
    ; b : S2.R2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1e6) 1e6 in
    let gen_pt rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      S2.R2_point.create ~x:(Float_u.of_float x) ~y:(Float_u.of_float y)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = gen_pt rnd in
      let b = gen_pt rnd in
      { a; b })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module R2_vec = struct
  type t = { v : S2.R2_point.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1e6) 1e6 in
    let gen_pt rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      S2.R2_point.create ~x:(Float_u.of_float x) ~y:(Float_u.of_float y)
    in
    create (fun ~size:_ ~random:rnd -> { v = gen_pt rnd })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module R2_point_scalar = struct
  type t =
    { v : S2.R2_point.t
    ; k : float
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1e6) 1e6 in
    let k_gen = float_inclusive (-1e3) 1e3 in
    let gen_pt rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      S2.R2_point.create ~x:(Float_u.of_float x) ~y:(Float_u.of_float y)
    in
    create (fun ~size:_ ~random:rnd ->
      let v = gen_pt rnd in
      let k = generate k_gen ~size:30 ~random:rnd in
      { v; k })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

let quickcheck_add_commutative () =
  Base_quickcheck.Test.run_exn
    (module R2_pair)
    ~config:qc_config
    ~f:(fun { R2_pair.a; b } ->
      assert (S2.R2_point.equal (S2.R2_point.add a b) (S2.R2_point.add b a)))
;;

let quickcheck_dot_commutative () =
  Base_quickcheck.Test.run_exn
    (module R2_pair)
    ~config:qc_config
    ~f:(fun { R2_pair.a; b } ->
      let u = S2.R2_point.dot a b in
      let v = S2.R2_point.dot b a in
      let scale = Float_u.(max #1.0 (max (abs u) (abs v))) |> Float_u.to_float in
      assert (Float.( <= ) (Float.abs (Float_u.to_float Float_u.(u - v))) (1e-10 *. scale)))
;;

let quickcheck_ortho_perpendicular () =
  Base_quickcheck.Test.run_exn (module R2_vec) ~config:qc_config ~f:(fun { R2_vec.v } ->
    let d = S2.R2_point.dot v (S2.R2_point.ortho v) in
    let scale =
      Float_u.(max #1.0 (max (abs (S2.R2_point.x v)) (abs (S2.R2_point.y v))))
      |> Float_u.to_float
    in
    assert (Float.( <= ) (Float.abs (Float_u.to_float d)) (1e-14 *. scale)))
;;

let quickcheck_norm2_mul () =
  Base_quickcheck.Test.run_exn
    (module R2_point_scalar)
    ~config:qc_config
    ~f:(fun { R2_point_scalar.v; k } ->
      let ku = Float_u.of_float k in
      let lhs = S2.R2_point.norm2 (S2.R2_point.mul v ku) in
      let rhs = Float_u.(ku * ku * S2.R2_point.norm2 v) in
      let scale = Float_u.(max (abs lhs) (abs rhs) |> max #1.0) |> Float_u.to_float in
      assert (
        Float.( <= ) (Float.abs (Float_u.to_float Float_u.(lhs - rhs))) (1e-10 *. scale)))
;;

let quickcheck_neg_involution () =
  Base_quickcheck.Test.run_exn
    (module R2_vec)
    ~config:qc_config
    ~f:(fun { R2_vec.v = p } ->
      let neg_p = S2.R2_point.neg p in
      let neg_neg_p = S2.R2_point.neg neg_p in
      assert (S2.R2_point.equal neg_neg_p p))
;;

let quickcheck_sub_self_zero () =
  Base_quickcheck.Test.run_exn
    (module R2_vec)
    ~config:qc_config
    ~f:(fun { R2_vec.v = p } ->
      let sub_p = S2.R2_point.sub p p in
      assert (S2.R2_point.equal sub_p S2.R2_point.zero))
;;

let quickcheck_norm_nonneg () =
  Base_quickcheck.Test.run_exn
    (module R2_vec)
    ~config:qc_config
    ~f:(fun { R2_vec.v = p } ->
      let norm = Float_u.to_float (S2.R2_point.norm p) in
      assert (Float.( >= ) norm 0.0))
;;

let quickcheck_normalize_unit_or_zero () =
  Base_quickcheck.Test.run_exn
    (module R2_vec)
    ~config:qc_config
    ~f:(fun { R2_vec.v = p } ->
      let n = S2.R2_point.normalize p in
      if S2.R2_point.equal p S2.R2_point.zero
      then assert (S2.R2_point.equal n S2.R2_point.zero)
      else (
        let norm = Float_u.to_float (S2.R2_point.norm n) in
        assert (Float.( <= ) (Float.abs (norm -. 1.0)) 1e-15)))
;;

let quickcheck_cross_antisymmetric () =
  Base_quickcheck.Test.run_exn
    (module R2_pair)
    ~config:qc_config
    ~f:(fun { R2_pair.a; b } ->
      let cross_ab = Float_u.to_float (S2.R2_point.cross a b) in
      let cross_ba = Float_u.to_float (S2.R2_point.cross b a) in
      assert (Float.( <= ) (Float.abs (cross_ab +. cross_ba)) 1e-15))
;;

let quickcheck_fabs_nonneg () =
  Base_quickcheck.Test.run_exn
    (module R2_vec)
    ~config:qc_config
    ~f:(fun { R2_vec.v = p } ->
      let f = S2.R2_point.fabs p in
      let x = Float_u.to_float (S2.R2_point.x f) in
      let y = Float_u.to_float (S2.R2_point.y f) in
      assert (Float.( >= ) x 0.0);
      assert (Float.( >= ) y 0.0))
;;

let test_constructors fixture () =
  let cases = to_list (member "constructors" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let expected = r2_point_of_json (member "point" c) in
    let actual =
      match op with
      | "zero" -> S2.R2_point.zero
      | "create" ->
        S2.R2_point.create
          ~x:(float_u_of_json_exn (member "x" c))
          ~y:(float_u_of_json_exn (member "y" c))
      | _ ->
        (match failwith (sprintf "unknown constructor op: %s" op) with
         | (_ : Nothing.t) -> .)
    in
    check_r2_point_exact (sprintf "constructor %s" op) ~expected ~actual)
;;

let test_arithmetic fixture () =
  List.iter
    (to_list (member "arithmetic" fixture))
    ~f:(fun c ->
      let p1 = r2_point_of_json (member "p1" c) in
      let p2 = r2_point_of_json (member "p2" c) in
      let expected_add = r2_point_of_json (member "add" c) in
      let expected_sub = r2_point_of_json (member "sub" c) in
      let expected_dot = float_u_of_json_exn (member "dot" c) in
      let expected_cross = float_u_of_json_exn (member "cross" c) in
      let label =
        sprintf "%s vs %s" (S2.R2_point.to_string p1) (S2.R2_point.to_string p2)
      in
      check_r2_point_exact
        (label ^ " add")
        ~expected:expected_add
        ~actual:(S2.R2_point.add p1 p2);
      check_r2_point_exact
        (label ^ " sub")
        ~expected:expected_sub
        ~actual:(S2.R2_point.sub p1 p2);
      check_float_u_exact
        (label ^ " dot")
        ~expected:expected_dot
        ~actual:(S2.R2_point.dot p1 p2);
      check_float_u_exact
        (label ^ " cross")
        ~expected:expected_cross
        ~actual:(S2.R2_point.cross p1 p2))
;;

let test_mul fixture () =
  List.iter
    (to_list (member "mul" fixture))
    ~f:(fun c ->
      let p = r2_point_of_json (member "point" c) in
      let ku = float_u_of_json_exn (member "scalar" c) in
      let expected = r2_point_of_json (member "result" c) in
      let label = sprintf "%s * %s" (S2.R2_point.to_string p) (Float_u.to_string ku) in
      check_r2_point_exact label ~expected ~actual:(S2.R2_point.mul p ku) [@nontail])
;;

let test_div fixture () =
  List.iter
    (to_list (member "div" fixture))
    ~f:(fun c ->
      let p = r2_point_of_json (member "point" c) in
      let ku = float_u_of_json_exn (member "scalar" c) in
      let expected = r2_point_of_json (member "result" c) in
      let label = sprintf "%s / %s" (S2.R2_point.to_string p) (Float_u.to_string ku) in
      check_r2_point_exact label ~expected ~actual:(S2.R2_point.div p ku) [@nontail])
;;

let test_neg fixture () =
  List.iter
    (to_list (member "neg" fixture))
    ~f:(fun c ->
      let p = r2_point_of_json (member "point" c) in
      let expected = r2_point_of_json (member "result" c) in
      let label = sprintf "neg %s" (S2.R2_point.to_string p) in
      check_r2_point_exact label ~expected ~actual:(S2.R2_point.neg p) [@nontail])
;;

let test_ortho fixture () =
  List.iter
    (to_list (member "ortho" fixture))
    ~f:(fun c ->
      let p = r2_point_of_json (member "point" c) in
      let expected = r2_point_of_json (member "ortho" c) in
      let label = sprintf "ortho %s" (S2.R2_point.to_string p) in
      check_r2_point_exact label ~expected ~actual:(S2.R2_point.ortho p) [@nontail])
;;

let test_norm fixture () =
  List.iter
    (to_list (member "norm" fixture))
    ~f:(fun c ->
      let p = r2_point_of_json (member "point" c) in
      let expected_norm = float_u_of_json_exn (member "norm" c) in
      let expected_norm2 = float_u_of_json_exn (member "norm2" c) in
      let label = S2.R2_point.to_string p in
      check_float_u_exact
        (label ^ " norm")
        ~expected:expected_norm
        ~actual:(S2.R2_point.norm p);
      check_float_u_exact
        (label ^ " norm2")
        ~expected:expected_norm2
        ~actual:(S2.R2_point.norm2 p))
;;

let test_normalize fixture () =
  List.iter
    (to_list (member "normalize" fixture))
    ~f:(fun c ->
      let p = r2_point_of_json (member "point" c) in
      let expected = r2_point_of_json (member "normalized" c) in
      let label = sprintf "normalize %s" (S2.R2_point.to_string p) in
      check_r2_point_exact label ~expected ~actual:(S2.R2_point.normalize p) [@nontail])
;;

let test_angle fixture () =
  List.iter
    (to_list (member "angle" fixture))
    ~f:(fun c ->
      let a = r2_point_of_json (member "a" c) in
      let b = r2_point_of_json (member "b" c) in
      let expected = float_u_of_json_exn (member "angle" c) in
      let label =
        sprintf "angle %s %s" (S2.R2_point.to_string a) (S2.R2_point.to_string b)
      in
      check_float_u label ~expected ~actual:(S2.R2_point.angle a b))
;;

let test_fabs fixture () =
  List.iter
    (to_list (member "fabs" fixture))
    ~f:(fun c ->
      let p = r2_point_of_json (member "point" c) in
      let expected = r2_point_of_json (member "fabs" c) in
      let label = sprintf "fabs %s" (S2.R2_point.to_string p) in
      check_r2_point_exact label ~expected ~actual:(S2.R2_point.fabs p) [@nontail])
;;

let test_equality fixture () =
  let cases = to_list (member "equality" fixture) in
  List.iter cases ~f:(fun c ->
    let p1 = r2_point_of_json (member "p1" c) in
    let p2 = r2_point_of_json (member "p2" c) in
    let expected = bool_of_json_exn (member "equal" c) in
    let label = sprintf "%s = %s" (S2.R2_point.to_string p1) (S2.R2_point.to_string p2) in
    Alcotest.(check bool) label expected (S2.R2_point.equal p1 p2))
;;

let test_sexp () =
  let p = S2.R2_point.create ~x:#1. ~y:#2. in
  Alcotest.(check string)
    "sexp_of sample point"
    "((x 1)(y 2))"
    (Sexp.to_string_mach (S2.R2_point.sexp_of_t p))
;;

let test_zero_sexp () =
  Alcotest.(check string)
    "sexp_of zero"
    "((x 0)(y 0))"
    (Sexp.to_string_mach (S2.R2_point.sexp_of_t S2.R2_point.zero))
;;

let () =
  let fixture = load_fixture "r2point.json" in
  Alcotest.run
    "R2_point"
    [ ( "constructors"
      , [ Alcotest.test_case "GoldenConstructors" `Quick (test_constructors fixture) ] )
    ; "arithmetic", [ Alcotest.test_case "TestDot" `Quick (test_arithmetic fixture) ]
    ; "mul", [ Alcotest.test_case "GoldenMul" `Quick (test_mul fixture) ]
    ; "div", [ Alcotest.test_case "GoldenDiv" `Quick (test_div fixture) ]
    ; "neg", [ Alcotest.test_case "GoldenNeg" `Quick (test_neg fixture) ]
    ; "ortho", [ Alcotest.test_case "TestOrtho" `Quick (test_ortho fixture) ]
    ; "norm", [ Alcotest.test_case "TestNorm" `Quick (test_norm fixture) ]
    ; "normalize", [ Alcotest.test_case "TestNormalize" `Quick (test_normalize fixture) ]
    ; "angle", [ Alcotest.test_case "GoldenAngle" `Quick (test_angle fixture) ]
    ; "fabs", [ Alcotest.test_case "GoldenFabs" `Quick (test_fabs fixture) ]
    ; "equality", [ Alcotest.test_case "GoldenEquality" `Quick (test_equality fixture) ]
    ; ( "sexp"
      , [ Alcotest.test_case "SexpOf_t" `Quick test_sexp
        ; Alcotest.test_case "zero" `Quick test_zero_sexp
        ] )
    ; ( "quickcheck"
      , [ Alcotest.test_case "add_commutative" `Quick quickcheck_add_commutative
        ; Alcotest.test_case "dot_commutative" `Quick quickcheck_dot_commutative
        ; Alcotest.test_case "ortho_perpendicular" `Quick quickcheck_ortho_perpendicular
        ; Alcotest.test_case "norm2_mul" `Quick quickcheck_norm2_mul
        ; Alcotest.test_case "neg_involution" `Quick quickcheck_neg_involution
        ; Alcotest.test_case "sub_self_zero" `Quick quickcheck_sub_self_zero
        ; Alcotest.test_case "norm_nonneg" `Quick quickcheck_norm_nonneg
        ; Alcotest.test_case
            "normalize_unit_or_zero"
            `Quick
            quickcheck_normalize_unit_or_zero
        ; Alcotest.test_case "cross_antisymmetric" `Quick quickcheck_cross_antisymmetric
        ; Alcotest.test_case "fabs_nonneg" `Quick quickcheck_fabs_nonneg
        ] )
    ]
;;
