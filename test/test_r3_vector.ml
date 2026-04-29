(* Reference: no dedicated r3vector_test.cc; R3Vector is Vector3<double>
   (util/math/vector.h). Golden vectors from test/gen/r3vector.cc. Alcotest case names
   (TEST-style labels):

   - TEST(Vector3Golden, Constructors) - constructors
   - TEST(Vector3Golden, Accessors) - accessors
   - TEST(Vector3Golden, Arithmetic) - arithmetic
   - TEST(Vector3Golden, ComponentWise) - component_wise
   - TEST(Vector3Golden, Normalization) - normalization
   - TEST(Vector3Golden, Ortho) - ortho
   - TEST(Vector3Golden, ApproxEquals) - approx_equal
   - TEST(Vector3Golden, CompareEqual) - compare_equal

   Deliberately omitted (no OCaml equivalent or not part of this port’s API):
   - Cast, Set, operator[], mutable Data(), Clear
   - ComponentOrder, Floor, Ceil, FRound, IRound, Sqrt
   - IsNaN, NaN(), stream formatting (covered only in broader C++ tests) *)

open Core
open Test_helpers
open Alcotest

let test_constructors fixture () =
  let cases = to_list (member "constructors" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let expected = r3_vector_of_json (member "expected" c) in
    let result =
      match op with
      | "default" -> S2.R3_vector.create ~x:#0.0 ~y:#0.0 ~z:#0.0
      | "zero" -> S2.R3_vector.zero
      | "create" | "init" ->
        S2.R3_vector.create
          ~x:(float_u_of_json_exn (member "x" c))
          ~y:(float_u_of_json_exn (member "y" c))
          ~z:(float_u_of_json_exn (member "z" c))
      | _ ->
        (match failwith ("unknown op: " ^ op) with
         | (_ : Nothing.t) -> .)
    in
    check_r3_vector_exact op ~expected ~actual:result)
;;

let test_accessors fixture () =
  let cases = to_list (member "accessors" fixture) in
  List.iter cases ~f:(fun c ->
    let v = r3_vector_of_json (member "val" c) in
    let name = string_of_json_exn (member "name" c) in
    check_float_u_exact
      (name ^ " x")
      ~expected:(float_u_of_json_exn (member "x" c))
      ~actual:(S2.R3_vector.x v);
    check_float_u_exact
      (name ^ " y")
      ~expected:(float_u_of_json_exn (member "y" c))
      ~actual:(S2.R3_vector.y v);
    check_float_u_exact
      (name ^ " z")
      ~expected:(float_u_of_json_exn (member "z" c))
      ~actual:(S2.R3_vector.z v);
    check_float_u
      (name ^ " norm")
      ~expected:(float_u_of_json_exn (member "norm" c))
      ~actual:(S2.R3_vector.norm v);
    check_float_u
      (name ^ " norm2")
      ~expected:(float_u_of_json_exn (member "norm2" c))
      ~actual:(S2.R3_vector.norm2 v);
    (check int)
      (name ^ " largest_abs_component")
      (int_of_json_exn (member "largest_abs_component" c))
      (S2.R3_vector.largest_abs_component v);
    (check int)
      (name ^ " smallest_abs_component")
      (int_of_json_exn (member "smallest_abs_component" c))
      (S2.R3_vector.smallest_component v))
;;

let test_arithmetic fixture () =
  let cases = to_list (member "arithmetic" fixture) in
  List.iter cases ~f:(fun c ->
    let a = r3_vector_of_json (member "a" c) in
    let b = r3_vector_of_json (member "b" c) in
    let label =
      sprintf "%s op %s" (S2.R3_vector.to_string a) (S2.R3_vector.to_string b)
    in
    check_r3_vector_exact
      (label ^ " add")
      ~expected:(r3_vector_of_json (member "add" c))
      ~actual:(S2.R3_vector.add a b);
    check_r3_vector_exact
      (label ^ " sub")
      ~expected:(r3_vector_of_json (member "sub" c))
      ~actual:(S2.R3_vector.sub a b);
    check_r3_vector_exact
      (label ^ " mul_scalar")
      ~expected:(r3_vector_of_json (member "mul_scalar" c))
      ~actual:(S2.R3_vector.mul a #2.5);
    (* C++ [a / 2.0]: uniform scale by [1/2]. *)
    check_r3_vector_exact
      (label ^ " div_scalar")
      ~expected:(r3_vector_of_json (member "div_scalar" c))
      ~actual:(S2.R3_vector.mul a #0.5);
    check_r3_vector_exact
      (label ^ " neg")
      ~expected:(r3_vector_of_json (member "neg" c))
      ~actual:(S2.R3_vector.neg a);
    check_float_u
      (label ^ " dot")
      ~expected:(float_u_of_json_exn (member "dot" c))
      ~actual:(S2.R3_vector.dot a b);
    check_r3_vector_exact
      (label ^ " cross")
      ~expected:(r3_vector_of_json (member "cross" c))
      ~actual:(S2.R3_vector.cross a b);
    check_float_u
      (label ^ " dist")
      ~expected:(float_u_of_json_exn (member "dist" c))
      ~actual:(S2.R3_vector.distance a b);
    check_float_u
      (label ^ " angle")
      ~expected:(float_u_of_json_exn (member "angle" c))
      ~actual:(S2.S1_angle.radians (S2.R3_vector.angle a b)))
;;

let test_component_wise fixture () =
  let cases = to_list (member "component_wise" fixture) in
  List.iter cases ~f:(fun c ->
    let a = r3_vector_of_json (member "a" c) in
    let b = r3_vector_of_json (member "b" c) in
    let label =
      sprintf "%s cmp %s" (S2.R3_vector.to_string a) (S2.R3_vector.to_string b)
    in
    check_r3_vector_exact
      (label ^ " mul_components")
      ~expected:(r3_vector_of_json (member "mul" c))
      ~actual:(S2.R3_vector.mul_components a b);
    check_r3_vector_exact
      (label ^ " div_components")
      ~expected:(r3_vector_of_json (member "div" c))
      ~actual:(S2.R3_vector.div_components a b);
    check_r3_vector_exact
      (label ^ " max")
      ~expected:(r3_vector_of_json (member "max" c))
      ~actual:(S2.R3_vector.max a b);
    check_r3_vector_exact
      (label ^ " min")
      ~expected:(r3_vector_of_json (member "min" c))
      ~actual:(S2.R3_vector.min a b);
    check_r3_vector_exact
      (label ^ " abs")
      ~expected:(r3_vector_of_json (member "abs" c))
      ~actual:(S2.R3_vector.abs a))
;;

let test_normalization fixture () =
  let open Float_u.O in
  let cases = to_list (member "normalization" fixture) in
  List.iter cases ~f:(fun c ->
    let v = r3_vector_of_json (member "v" c) in
    let label = sprintf "normalize %s" (S2.R3_vector.to_string v) in
    check_r3_vector_exact
      (label ^ " normalized")
      ~expected:(r3_vector_of_json (member "normalized" c))
      ~actual:(S2.R3_vector.normalize v);
    (check bool)
      (label ^ " is_unit")
      (bool_of_json_exn (member "is_unit" c))
      (let n = S2.R3_vector.normalize v in
       if S2.R3_vector.norm v = #0.0
       then false
       else Float_u.abs (S2.R3_vector.norm n - #1.0) <= #1e-15))
;;

let test_ortho fixture () =
  let cases = to_list (member "ortho" fixture) in
  List.iter cases ~f:(fun c ->
    let v = r3_vector_of_json (member "v" c) in
    let label = sprintf "ortho %s" (S2.R3_vector.to_string v) in
    let ortho = S2.R3_vector.ortho v in
    check_r3_vector_exact
      (label ^ " ortho")
      ~expected:(r3_vector_of_json (member "ortho" c))
      ~actual:ortho;
    check_float_u
      (label ^ " dot_product")
      ~expected:(float_u_of_json_exn (member "dot_product" c))
      ~actual:(S2.R3_vector.dot v ortho);
    check_float_u
      (label ^ " ortho_norm")
      ~expected:(float_u_of_json_exn (member "ortho_norm" c))
      ~actual:(S2.R3_vector.norm ortho))
;;

let test_approx_equal fixture () =
  let cases = to_list (member "approx_equal" fixture) in
  List.iter cases ~f:(fun c ->
    let a = r3_vector_of_json (member "a" c) in
    let b = r3_vector_of_json (member "b" c) in
    let margin = float_u_of_json_exn (member "margin" c) in
    (check bool)
      "approx_equal vs C++ aequal"
      (bool_of_json_exn (member "aequal" c))
      (S2.R3_vector.approx_equal ~max_error:(Packed_float_option.Unboxed.some margin) a b))
;;

let test_compare_equal fixture () =
  let cases = to_list (member "compare_equal" fixture) in
  List.iter cases ~f:(fun c ->
    let a = r3_vector_of_json (member "a" c) in
    let b = r3_vector_of_json (member "b" c) in
    let label =
      sprintf "cmp %s %s" (S2.R3_vector.to_string a) (S2.R3_vector.to_string b)
    in
    (check int)
      (label ^ " compare")
      (int_of_json_exn (member "compare" c))
      (S2.R3_vector.compare a b);
    (check bool)
      (label ^ " equal")
      (bool_of_json_exn (member "equal" c))
      (S2.R3_vector.equal a b))
;;

let () =
  let fixture = load_fixture "r3vector.json" in
  Alcotest.run
    "R3_vector"
    [ "constructors", [ test_case "Constructors" `Quick (test_constructors fixture) ]
    ; "accessors", [ test_case "Accessors" `Quick (test_accessors fixture) ]
    ; "arithmetic", [ test_case "Arithmetic" `Quick (test_arithmetic fixture) ]
    ; "component_wise", [ test_case "ComponentWise" `Quick (test_component_wise fixture) ]
    ; "normalization", [ test_case "Normalization" `Quick (test_normalization fixture) ]
    ; "ortho", [ test_case "Ortho" `Quick (test_ortho fixture) ]
    ; "approx_equal", [ test_case "ApproxEquals" `Quick (test_approx_equal fixture) ]
    ; "compare_equal", [ test_case "CompareEqual" `Quick (test_compare_equal fixture) ]
    ]
;;
