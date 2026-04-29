(* Reference: no dedicated r2point_test.cc upstream; Vector2<double> (util/math/vector.h).
   Go coverage in r2/rect_test.go (Alcotest case names mirror these where applicable):
   - TEST(Go, TestOrtho) - ortho
   - TEST(Go, TestDot) - arithmetic (dot)
   - TEST(Go, TestCross) - arithmetic (cross)
   - TEST(Go, TestNorm) - norm
   - TEST(Go, TestNormalize) - normalize

   Golden-only (no direct Go/C++ macro); Alcotest case name in parentheses:
   - GoldenConstructors - constructors (zero, create, negative-zero)
   - GoldenMul, GoldenDiv, GoldenNeg - mul, div, neg
   - GoldenAngle, GoldenFabs, GoldenEquality - angle, fabs, equality
   - SexpOf_t - sexp *)

open Core
open Test_helpers
open Alcotest

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
    (check bool) label expected (S2.R2_point.equal p1 p2))
;;

let test_sexp () =
  let p = S2.R2_point.create ~x:#1. ~y:#2. in
  (check string)
    "sexp_of sample point"
    "((x 1)(y 2))"
    (Sexp.to_string_mach (S2.R2_point.sexp_of_t p))
;;

let test_zero_sexp () =
  (check string)
    "sexp_of zero"
    "((x 0)(y 0))"
    (Sexp.to_string_mach (S2.R2_point.sexp_of_t S2.R2_point.zero))
;;

let () =
  let fixture = load_fixture "r2point.json" in
  Alcotest.run
    "R2_point"
    [ ( "constructors"
      , [ test_case "GoldenConstructors" `Quick (test_constructors fixture) ] )
    ; "arithmetic", [ test_case "TestDot" `Quick (test_arithmetic fixture) ]
    ; "mul", [ test_case "GoldenMul" `Quick (test_mul fixture) ]
    ; "div", [ test_case "GoldenDiv" `Quick (test_div fixture) ]
    ; "neg", [ test_case "GoldenNeg" `Quick (test_neg fixture) ]
    ; "ortho", [ test_case "TestOrtho" `Quick (test_ortho fixture) ]
    ; "norm", [ test_case "TestNorm" `Quick (test_norm fixture) ]
    ; "normalize", [ test_case "TestNormalize" `Quick (test_normalize fixture) ]
    ; "angle", [ test_case "GoldenAngle" `Quick (test_angle fixture) ]
    ; "fabs", [ test_case "GoldenFabs" `Quick (test_fabs fixture) ]
    ; "equality", [ test_case "GoldenEquality" `Quick (test_equality fixture) ]
    ; ( "sexp"
      , [ test_case "SexpOf_t" `Quick test_sexp; test_case "zero" `Quick test_zero_sexp ]
      )
    ]
;;
