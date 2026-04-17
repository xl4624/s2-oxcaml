(* C++ test parity: s2geometry/src/s2/r1interval_test.cc
   -  TEST(R1Interval, TestBasic)  - constructors, accessors, contains_point,
      interval_ops, add_point, project, expanded, expanded_chain, equal
   -  TEST(R1Interval, ApproxEquals) - approx_equal, approx_equal_custom

   Deliberately omitted (C++-only mutable API, no OCaml equivalent):
   - set_hi, operator[], bounds(), mutable_bounds()
   - AddInterval (equivalent to union; the union invariant is verified) *)

open Core
open Test_helpers
open Alcotest

let named_interval = function
  | "unit" -> S2.R1_interval.create ~lo:#0.0 ~hi:#1.0
  | "negunit" -> S2.R1_interval.create ~lo:(-#1.0) ~hi:#0.0
  | "half" -> S2.R1_interval.create ~lo:#0.5 ~hi:#0.5
  | "empty" -> S2.R1_interval.empty
  | name ->
    (match failwith (sprintf "unknown named interval: %s" name) with
     | (_ : Nothing.t) -> .)
;;

let test_constructors fixture () =
  let cases = to_list (member "constructors" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let expected = r1_interval_of_json (member "expected" c) in
    let result =
      match op with
      | "from_point" -> S2.R1_interval.from_point (float_u_of_json_exn (member "input" c))
      | "from_point_pair" ->
        S2.R1_interval.from_point_pair
          (float_u_of_json_exn (member "p1" c))
          (float_u_of_json_exn (member "p2" c))
      | "create" ->
        S2.R1_interval.create
          ~lo:(float_u_of_json_exn (member "lo" c))
          ~hi:(float_u_of_json_exn (member "hi" c))
      | _ ->
        (match failwith (sprintf "unknown op: %s" op) with
         | (_ : Nothing.t) -> .)
    in
    check_r1_interval_exact op ~expected ~actual:result)
;;

let test_accessors fixture () =
  let cases = to_list (member "accessors" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "interval" c) in
    let i = named_interval name in
    check_float_u_exact
      (name ^ " lo")
      ~expected:(float_u_of_json_exn (member "lo" c))
      ~actual:(S2.R1_interval.lo i);
    check_float_u_exact
      (name ^ " hi")
      ~expected:(float_u_of_json_exn (member "hi" c))
      ~actual:(S2.R1_interval.hi i);
    check_bool
      (name ^ " is_empty")
      ~expected:(bool_of_json_exn (member "is_empty" c))
      ~actual:(S2.R1_interval.is_empty i);
    check_float_u
      (name ^ " center")
      ~expected:(float_u_of_json_exn (member "center" c))
      ~actual:(S2.R1_interval.center i);
    check_float_u_exact
      (name ^ " length")
      ~expected:(float_u_of_json_exn (member "length" c))
      ~actual:(S2.R1_interval.length i))
;;

let test_contains_point fixture () =
  let cases = to_list (member "contains_point" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "interval" c) in
    let i = named_interval name in
    let pu = float_u_of_json_exn (member "point" c) in
    let label = sprintf "%s contains %s" name (Float_u.to_string pu) in
    (check bool)
      label
      (bool_of_json_exn (member "contains" c))
      (S2.R1_interval.contains i pu);
    (check bool)
      (label ^ " interior")
      (bool_of_json_exn (member "interior_contains" c))
      (S2.R1_interval.interior_contains i pu))
;;

let test_interval_ops fixture () =
  let cases = to_list (member "interval_ops" fixture) in
  List.iter cases ~f:(fun c ->
    let x = r1_interval_of_json (member "x_val" c) in
    let y = r1_interval_of_json (member "y_val" c) in
    let xn = string_of_json_exn (member "x" c) in
    let yn = string_of_json_exn (member "y" c) in
    let label =
      sprintf
        "%s(%s) vs %s(%s)"
        xn
        (S2.R1_interval.to_string x)
        yn
        (S2.R1_interval.to_string y)
    in
    (check bool)
      (label ^ " contains")
      (bool_of_json_exn (member "contains" c))
      (S2.R1_interval.contains_interval x y);
    (check bool)
      (label ^ " interior_contains")
      (bool_of_json_exn (member "interior_contains" c))
      (S2.R1_interval.interior_contains_interval x y);
    (check bool)
      (label ^ " intersects")
      (bool_of_json_exn (member "intersects" c))
      (S2.R1_interval.intersects x y);
    (check bool)
      (label ^ " interior_intersects")
      (bool_of_json_exn (member "interior_intersects" c))
      (S2.R1_interval.interior_intersects x y);
    let eu = r1_interval_of_json (member "union" c) in
    let au = S2.R1_interval.union x y in
    check_r1_interval_exact (label ^ " union") ~expected:eu ~actual:au;
    let ei = r1_interval_of_json (member "intersection" c) in
    let ai = S2.R1_interval.intersection x y in
    check_r1_interval_exact (label ^ " intersection") ~expected:ei ~actual:ai;
    (* C++ TestIntervalOps: Contains(y) <=> Union(x,y)==x; Intersects <=> non-empty intersection. *)
    (check bool)
      (label ^ " invariant contains_interval/union")
      (bool_of_json_exn (member "contains" c))
      (S2.R1_interval.equal (S2.R1_interval.union x y) x);
    (check bool)
      (label ^ " invariant intersects/intersection")
      (bool_of_json_exn (member "intersects" c))
      (not (S2.R1_interval.is_empty (S2.R1_interval.intersection x y))))
;;

let test_add_point fixture () =
  let cases = Array.of_list (to_list (member "add_point" fixture)) in
  let total_cases = Array.length cases in
  let mutable acc = S2.R1_interval.empty in
  for i = 0 to total_cases - 1 do
    let c = cases.(i) in
    let pu = float_u_of_json_exn (member "after_adding" c) in
    let result = S2.R1_interval.add_point acc pu in
    check_float_u_exact
      (sprintf "add %s lo" (Float_u.to_string pu))
      ~expected:(float_u_of_json_exn (member "lo" c))
      ~actual:(S2.R1_interval.lo result);
    check_float_u_exact
      (sprintf "add %s hi" (Float_u.to_string pu))
      ~expected:(float_u_of_json_exn (member "hi" c))
      ~actual:(S2.R1_interval.hi result);
    acc <- result
  done
;;

let test_project fixture () =
  let cases = to_list (member "project" fixture) in
  List.iter cases ~f:(fun c ->
    let i = r1_interval_of_json (member "interval" c) in
    let pu = float_u_of_json_exn (member "point" c) in
    check_float_u_exact
      (sprintf "project %s onto %s" (Float_u.to_string pu) (S2.R1_interval.to_string i))
      ~expected:(float_u_of_json_exn (member "projected" c))
      ~actual:(S2.R1_interval.project_exn i pu))
;;

let test_expanded fixture () =
  let cases = to_list (member "expanded" fixture) in
  List.iter cases ~f:(fun c ->
    let i = r1_interval_of_json (member "interval_val" c) in
    let marginu = float_u_of_json_exn (member "margin" c) in
    let result = S2.R1_interval.expanded i marginu in
    let expected = r1_interval_of_json (member "result" c) in
    let label =
      sprintf "expanded %s by %s" (S2.R1_interval.to_string i) (Float_u.to_string marginu)
    in
    check_float_u
      (label ^ " lo")
      ~expected:(S2.R1_interval.lo expected)
      ~actual:(S2.R1_interval.lo result);
    check_float_u
      (label ^ " hi")
      ~expected:(S2.R1_interval.hi expected)
      ~actual:(S2.R1_interval.hi result);
    (check bool)
      (label ^ " is_empty")
      (bool_of_json_exn (member "result_is_empty" c))
      (S2.R1_interval.is_empty result))
;;

let test_directed_hausdorff fixture () =
  let cases = to_list (member "directed_hausdorff" fixture) in
  List.iter cases ~f:(fun c ->
    let x = r1_interval_of_json (member "x" c) in
    let y = r1_interval_of_json (member "y" c) in
    let result = S2.R1_interval.directed_hausdorff_distance x y in
    let label =
      sprintf
        "hausdorff %s -> %s"
        (S2.R1_interval.to_string x)
        (S2.R1_interval.to_string y)
    in
    match member "distance" c with
    | `Null -> (check bool) (label ^ " infinity") true (Float_u.is_inf result)
    | j -> check_float_u_exact label ~expected:(float_u_of_json_exn j) ~actual:result)
;;

let test_approx_equal fixture () =
  let cases = to_list (member "approx_equal" fixture) in
  List.iter cases ~f:(fun c ->
    let x = r1_interval_of_json (member "x" c) in
    let y = r1_interval_of_json (member "y" c) in
    let expected = bool_of_json_exn (member "approx_equal" c) in
    (check bool)
      (sprintf
         "approx %s vs %s"
         (S2.R1_interval.to_string x)
         (S2.R1_interval.to_string y))
      expected
      (S2.R1_interval.approx_equal ~max_error:(Packed_float_option.Unboxed.none ()) x y))
;;

let test_approx_equal_custom fixture () =
  let cases = to_list (member "approx_equal_custom" fixture) in
  List.iter cases ~f:(fun c ->
    let x = r1_interval_of_json (member "x" c) in
    let y = r1_interval_of_json (member "y" c) in
    let me = float_u_of_json_exn (member "max_error" c) in
    let expected = bool_of_json_exn (member "approx_equal" c) in
    let label =
      sprintf
        "approx_custom max_error=%s %s vs %s"
        (Float_u.to_string me)
        (S2.R1_interval.to_string x)
        (S2.R1_interval.to_string y)
    in
    (check bool)
      label
      expected
      (S2.R1_interval.approx_equal ~max_error:(Packed_float_option.Unboxed.some me) x y))
;;

let test_equal fixture () =
  let cases = to_list (member "equal" fixture) in
  List.iter cases ~f:(fun c ->
    let x = r1_interval_of_json (member "x" c) in
    let y = r1_interval_of_json (member "y" c) in
    let expected = bool_of_json_exn (member "equal" c) in
    (check bool)
      (sprintf "equal %s vs %s" (S2.R1_interval.to_string x) (S2.R1_interval.to_string y))
      expected
      (S2.R1_interval.equal x y))
;;

let test_expanded_chain fixture () =
  let cases = to_list (member "expanded_chain" fixture) in
  List.iter cases ~f:(fun c ->
    let i = S2.R1_interval.create ~lo:#0.0 ~hi:#1.0 in
    let m1 = float_u_of_json_exn (member "first_margin" c) in
    let m2 = float_u_of_json_exn (member "second_margin" c) in
    let after_first = S2.R1_interval.expanded i m1 in
    let expected_mid = r1_interval_of_json (member "after_first" c) in
    let label =
      sprintf
        "expanded_chain %s by %s then %s"
        (S2.R1_interval.to_string i)
        (Float_u.to_string m1)
        (Float_u.to_string m2)
    in
    check_float_u
      (label ^ " mid lo")
      ~expected:(S2.R1_interval.lo expected_mid)
      ~actual:(S2.R1_interval.lo after_first);
    check_float_u
      (label ^ " mid hi")
      ~expected:(S2.R1_interval.hi expected_mid)
      ~actual:(S2.R1_interval.hi after_first);
    (check bool)
      (label ^ " mid is_empty")
      (bool_of_json_exn (member "after_first_empty" c))
      (S2.R1_interval.is_empty after_first);
    let final = S2.R1_interval.expanded after_first m2 in
    let expected_final = r1_interval_of_json (member "final" c) in
    check_float_u
      (label ^ " final lo")
      ~expected:(S2.R1_interval.lo expected_final)
      ~actual:(S2.R1_interval.lo final);
    check_float_u
      (label ^ " final hi")
      ~expected:(S2.R1_interval.hi expected_final)
      ~actual:(S2.R1_interval.hi final);
    (check bool)
      (label ^ " final is_empty")
      (bool_of_json_exn (member "final_empty" c))
      (S2.R1_interval.is_empty final))
;;

let test_sexp () =
  let i = S2.R1_interval.create ~lo:#0. ~hi:#1. in
  (check string)
    "sexp_of unit interval"
    "((lo 0)(hi 1))"
    (Sexp.to_string_mach (S2.R1_interval.sexp_of_t i))
;;

let test_empty_sexp () =
  (check string)
    "sexp_of empty"
    "((lo 1)(hi 0))"
    (Sexp.to_string_mach (S2.R1_interval.sexp_of_t S2.R1_interval.empty))
;;

let () =
  let fixture = load_fixture "r1interval.json" in
  Alcotest.run
    "R1_interval"
    [ "constructors", [ test_case "TestBasic" `Quick (test_constructors fixture) ]
    ; "accessors", [ test_case "TestBasic" `Quick (test_accessors fixture) ]
    ; "contains_point", [ test_case "TestBasic" `Quick (test_contains_point fixture) ]
    ; "interval_ops", [ test_case "TestBasic" `Quick (test_interval_ops fixture) ]
    ; "add_point", [ test_case "TestBasic" `Quick (test_add_point fixture) ]
    ; "project", [ test_case "TestBasic" `Quick (test_project fixture) ]
    ; "expanded", [ test_case "TestBasic" `Quick (test_expanded fixture) ]
    ; "expanded_chain", [ test_case "TestBasic" `Quick (test_expanded_chain fixture) ]
    ; ( "directed_hausdorff"
      , [ test_case "DirectedHausdorff" `Quick (test_directed_hausdorff fixture) ] )
    ; "approx_equal", [ test_case "ApproxEquals" `Quick (test_approx_equal fixture) ]
    ; ( "approx_equal_custom"
      , [ test_case "ApproxEqualsCustom" `Quick (test_approx_equal_custom fixture) ] )
    ; "equal", [ test_case "TestBasic" `Quick (test_equal fixture) ]
    ; ( "sexp"
      , [ test_case "SexpOf_t" `Quick test_sexp
        ; test_case "empty" `Quick test_empty_sexp
        ] )
    ]
;;
