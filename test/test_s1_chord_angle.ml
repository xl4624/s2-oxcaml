(* C++ test parity: s2geometry/src/s2/s1chord_angle_test.cc
   -  TEST(S1ChordAngle, ToFromS1Angle)               - constructors
   -  TEST(S1ChordAngle, FromLength2)                  - constructors
   -  TEST(S1ChordAngle, Zero)                         - constructors
   -  TEST(S1ChordAngle, Right)                        - constructors
   -  TEST(S1ChordAngle, Straight)                     - constructors
   -  TEST(S1ChordAngle, Infinity)                     - constructors
   -  TEST(S1ChordAngle, Negative)                     - constructors
   -  TEST(S1ChordAngle, Predicates)                   - predicates
   -  TEST(S1ChordAngle, Successor)                    - successor_predecessor, successor_chain
   -  TEST(S1ChordAngle, Predecessor)                  - successor_predecessor, predecessor_chain
   -  TEST(S1ChordAngle, Arithmetic)                   - arithmetic
   -  TEST(S1ChordAngle, Trigonometry)                 - trigonometry
   -  TEST(S1ChordAngle, PlusError)                    - plus_error
   -  TEST(S1ChordAngle, GetS2PointConstructorMaxError) - error_bounds

   Additional coverage beyond C++ tests:
   -  comparison: explicit compare/equal checks
   -  to_angle: explicit round-trip conversion checks
   -  convenience: Radians, Degrees, E5, E6, E7 convenience constructors

   Deliberately omitted (require S2Point or randomized testing):
   -  TEST(S1ChordAngle, TwoPointConstructor)
   -  TEST(S1ChordAngle, ArithmeticPrecision)
   -  TEST(S1ChordAngle, ConstexprFunctionsWork)
   -  TEST(S1ChordAngle, DefaultConstructor)
   -  TEST(S1ChordAngle, GetS2PointConstructorMaxError) randomized loop *)

open Core
open Test_helpers
open Alcotest

(* -- Quickcheck generators ------------------------------------------------ *)

module Chord_angle_valid = struct
  type t = { degrees : float } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    map (float_inclusive 0.0 180.0) ~f:(fun d -> { degrees = d })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  let to_chord t = S2.S1_chord_angle.of_degrees t.degrees
end

module Chord_angle_pair = struct
  type t =
    { a : float
    ; b : float
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let deg = float_inclusive 0.0 180.0 in
    map (both deg deg) ~f:(fun (a, b) -> { a; b })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

let quickcheck_add_commutative () =
  Base_quickcheck.Test.run_exn
    (module Chord_angle_pair)
    ~config:qc_config
    ~f:(fun { Chord_angle_pair.a; b } ->
      let ca = S2.S1_chord_angle.of_degrees a in
      let cb = S2.S1_chord_angle.of_degrees b in
      let sum_ab = S2.S1_chord_angle.add ca cb in
      let sum_ba = S2.S1_chord_angle.add cb ca in
      assert (S2.S1_chord_angle.equal sum_ab sum_ba))
;;

let quickcheck_monotone_with_angle () =
  Base_quickcheck.Test.run_exn
    (module Chord_angle_pair)
    ~config:qc_config
    ~f:(fun { Chord_angle_pair.a; b } ->
      let ca = S2.S1_chord_angle.of_degrees a in
      let cb = S2.S1_chord_angle.of_degrees b in
      (* If a ≤ b as chord angles, then to_angle(a) ≤ to_angle(b) *)
      if S2.S1_chord_angle.compare ca cb <= 0
      then
        assert (
          S2.S1_angle.compare
            (S2.S1_chord_angle.to_angle ca)
            (S2.S1_chord_angle.to_angle cb)
          <= 0))
;;

let quickcheck_successor_predecessor_inverse () =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let a = Chord_angle_valid.to_chord t in
    (* For valid non-boundary angles, predecessor(successor(a)) = a *)
    if (not (S2.S1_chord_angle.is_special a))
       && not (S2.S1_chord_angle.equal a S2.S1_chord_angle.straight)
    then (
      let s = S2.S1_chord_angle.successor a in
      let ps = S2.S1_chord_angle.predecessor s in
      assert (S2.S1_chord_angle.equal ps a)))
;;

let quickcheck_of_angle_roundtrip () =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let ca = Chord_angle_valid.to_chord t in
    let angle = S2.S1_chord_angle.to_angle ca in
    let ca2 = S2.S1_chord_angle.of_angle angle in
    (* Round-trip should be close *)
    let d1 = Float_u.to_float (S2.S1_chord_angle.length2 ca) in
    let d2 = Float_u.to_float (S2.S1_chord_angle.length2 ca2) in
    let scale = Float.max 1.0 (Float.max (Float.abs d1) (Float.abs d2)) in
    assert (Float.( <= ) (Float.abs (d1 -. d2)) (1e-13 *. scale)))
;;

let quickcheck_sin2_cos2_identity () =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let ca = Chord_angle_valid.to_chord t in
    if not (S2.S1_chord_angle.is_special ca)
    then (
      let s = Float_u.to_float (S2.S1_chord_angle.sin ca) in
      let c = Float_u.to_float (S2.S1_chord_angle.cos ca) in
      let sum = (s *. s) +. (c *. c) in
      assert (Float.( <= ) (Float.abs (sum -. 1.0)) 1e-14)))
;;

let quickcheck_compare_consistent () =
  Base_quickcheck.Test.run_exn
    (module Chord_angle_pair)
    ~config:qc_config
    ~f:(fun { Chord_angle_pair.a; b } ->
      let ca = S2.S1_chord_angle.of_degrees a in
      let cb = S2.S1_chord_angle.of_degrees b in
      let cmp = S2.S1_chord_angle.compare ca cb in
      let l2_cmp =
        Float.compare
          (Float_u.to_float (S2.S1_chord_angle.length2 ca))
          (Float_u.to_float (S2.S1_chord_angle.length2 cb))
      in
      assert (Sign.equal (Sign.of_int cmp) (Sign.of_int l2_cmp)))
;;

(* -- End quickcheck ------------------------------------------------------- *)

let chord_angle_of_name = function
  | "negative" -> S2.S1_chord_angle.negative
  | "zero" -> S2.S1_chord_angle.zero
  | "right" -> S2.S1_chord_angle.right
  | "straight" -> S2.S1_chord_angle.straight
  | "infinity" -> S2.S1_chord_angle.infinity
  | "d30" -> S2.S1_chord_angle.of_degrees 30.0
  | "d60" -> S2.S1_chord_angle.of_degrees 60.0
  | "d90" -> S2.S1_chord_angle.of_degrees 90.0
  | "d120" -> S2.S1_chord_angle.of_degrees 120.0
  | "d180" -> S2.S1_chord_angle.straight
  | "degrees_1" -> S2.S1_chord_angle.of_degrees 1.0
  | "degrees_45" -> S2.S1_chord_angle.of_degrees 45.0
  | "from_radians_1" -> S2.S1_chord_angle.of_radians 1.0
  | name ->
    (match failwith (sprintf "unknown chord angle name: %s" name) with
     | (_ : Nothing.t) -> .)
;;

let float_or_infinity_of_json_exn = function
  | `String "infinity" -> Float.infinity
  | `String "-infinity" -> Float.neg_infinity
  | j -> float_of_json_exn j
;;

let test_constructors fixture () =
  let cases = to_list (member "constructors" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let result =
      match op with
      | "from_angle" ->
        let input = float_or_infinity_of_json_exn (member "input_radians" c) in
        S2.S1_chord_angle.of_radians input
      | "from_length2" ->
        S2.S1_chord_angle.of_length2 (float_u_of_json_exn (member "input" c))
      | "zero" -> S2.S1_chord_angle.zero
      | "right" -> S2.S1_chord_angle.right
      | "straight" -> S2.S1_chord_angle.straight
      | _ ->
        (match failwith (sprintf "unknown op: %s" op) with
         | (_ : Nothing.t) -> .)
    in
    check_float_u_or_infinity
      (op ^ " length2")
      (member "length2" c)
      (S2.S1_chord_angle.length2 result);
    (match member "radians" c with
     | `Null -> ()
     | j ->
       check_float
         (op ^ " radians")
         ~expected:(float_of_json_exn j)
         ~actual:(S2.S1_chord_angle.radians result));
    (match member "degrees" c with
     | `Null -> ()
     | j ->
       check_float
         (op ^ " degrees")
         ~expected:(float_of_json_exn j)
         ~actual:(S2.S1_chord_angle.degrees result));
    match member "is_infinity" c with
    | `Null -> ()
    | j ->
      (check bool)
        (op ^ " is_infinity")
        (bool_of_json_exn j)
        (S2.S1_chord_angle.is_infinity result))
;;

let test_convenience fixture () =
  let cases = to_list (member "convenience" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let input = float_of_json_exn (member "input" c) in
    let result =
      match op with
      | "radians" -> S2.S1_chord_angle.of_radians input
      | "degrees" -> S2.S1_chord_angle.of_degrees input
      | "e5" -> S2.S1_chord_angle.of_e5 (int_of_json_exn (member "input" c))
      | "e6" -> S2.S1_chord_angle.of_e6 (int_of_json_exn (member "input" c))
      | "e7" -> S2.S1_chord_angle.of_e7 (int_of_json_exn (member "input" c))
      | _ ->
        (match failwith (sprintf "unknown op: %s" op) with
         | (_ : Nothing.t) -> .)
    in
    check_float_u
      (op ^ " length2")
      ~expected:(float_u_of_json_exn (member "length2" c))
      ~actual:(S2.S1_chord_angle.length2 result))
;;

let test_predicates fixture () =
  let cases = to_list (member "predicates" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let angle = chord_angle_of_name name in
    (check bool)
      (name ^ " is_zero")
      (bool_of_json_exn (member "is_zero" c))
      (S2.S1_chord_angle.is_zero angle);
    (check bool)
      (name ^ " is_negative")
      (bool_of_json_exn (member "is_negative" c))
      (S2.S1_chord_angle.is_negative angle);
    (check bool)
      (name ^ " is_infinity")
      (bool_of_json_exn (member "is_infinity" c))
      (S2.S1_chord_angle.is_infinity angle);
    (check bool)
      (name ^ " is_special")
      (bool_of_json_exn (member "is_special" c))
      (S2.S1_chord_angle.is_special angle);
    (check bool)
      (name ^ " is_valid")
      (bool_of_json_exn (member "is_valid" c))
      (S2.S1_chord_angle.is_valid angle))
;;

let test_comparison fixture () =
  let cases = to_list (member "comparison" fixture) in
  List.iter cases ~f:(fun c ->
    let a = chord_angle_of_name (string_of_json_exn (member "a" c)) in
    let b = chord_angle_of_name (string_of_json_exn (member "b" c)) in
    (check int)
      "compare"
      (int_of_json_exn (member "compare" c))
      (S2.S1_chord_angle.compare a b);
    (check bool)
      "equal"
      (bool_of_json_exn (member "equal" c))
      (S2.S1_chord_angle.equal a b))
;;

let test_to_angle fixture () =
  let cases = to_list (member "to_angle" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let angle = chord_angle_of_name name in
    let result = S2.S1_chord_angle.to_angle angle in
    check_float_u_or_infinity
      (name ^ " radians")
      (member "radians" c)
      (S2.S1_angle.radians result);
    check_float_u_or_infinity
      (name ^ " degrees")
      (member "degrees" c)
      (S2.S1_angle.degrees result);
    (check bool)
      (name ^ " is_inf")
      (bool_of_json_exn (member "is_inf" c))
      (S2.S1_angle.is_inf result))
;;

let test_successor_predecessor fixture () =
  let cases = to_list (member "successor_predecessor" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let angle = chord_angle_of_name name in
    let succ = S2.S1_chord_angle.successor angle in
    let pred = S2.S1_chord_angle.predecessor angle in
    check_float_u_or_infinity
      (name ^ " successor")
      (member "successor_length2" c)
      (S2.S1_chord_angle.length2 succ);
    check_float_u_or_infinity
      (name ^ " predecessor")
      (member "predecessor_length2" c)
      (S2.S1_chord_angle.length2 pred))
;;

let test_successor_chain fixture () =
  let cases = to_list (member "successor_chain" fixture) in
  List.iter cases ~f:(fun c ->
    let current = float_of_json_exn (member "length2" c) in
    let angle =
      if Float.( = ) current (-1.0)
      then S2.S1_chord_angle.negative
      else S2.S1_chord_angle.of_length2 (Float_u.of_float current)
    in
    let result = S2.S1_chord_angle.successor angle in
    check_float_u_exact
      (sprintf "successor chain %.17g" current)
      ~expected:(float_u_of_json_exn (member "successor_length2" c))
      ~actual:(S2.S1_chord_angle.length2 result))
;;

let test_predecessor_chain fixture () =
  let cases = to_list (member "predecessor_chain" fixture) in
  List.iter cases ~f:(fun c ->
    let current = member "length2" c in
    let angle =
      match current with
      | `Null -> S2.S1_chord_angle.infinity
      | j -> S2.S1_chord_angle.of_length2 (float_u_of_json_exn j)
    in
    let result = S2.S1_chord_angle.predecessor angle in
    check_float_u_or_infinity
      (sprintf "predecessor chain %s" (Yojson.Safe.to_string current))
      (member "predecessor_length2" c)
      (S2.S1_chord_angle.length2 result))
;;

let test_arithmetic fixture () =
  let cases = to_list (member "arithmetic" fixture) in
  List.iter cases ~f:(fun c ->
    let a = chord_angle_of_name (string_of_json_exn (member "a" c)) in
    let b = chord_angle_of_name (string_of_json_exn (member "b" c)) in
    let sum = S2.S1_chord_angle.add a b in
    let diff = S2.S1_chord_angle.sub a b in
    check_float_u
      "sum length2"
      ~expected:(float_u_of_json_exn (member "sum_length2" c))
      ~actual:(S2.S1_chord_angle.length2 sum);
    check_float
      "sum degrees"
      ~expected:(float_of_json_exn (member "sum_degrees" c))
      ~actual:(S2.S1_chord_angle.degrees sum);
    check_float_u
      "diff length2"
      ~expected:(float_u_of_json_exn (member "diff_length2" c))
      ~actual:(S2.S1_chord_angle.length2 diff);
    check_float
      "diff degrees"
      ~expected:(float_of_json_exn (member "diff_degrees" c))
      ~actual:(S2.S1_chord_angle.degrees diff))
;;

let test_trigonometry fixture () =
  let cases = to_list (member "trigonometry" fixture) in
  List.iter cases ~f:(fun c ->
    let angle = S2.S1_chord_angle.of_radians (float_of_json_exn (member "radians" c)) in
    let label = sprintf "trig(%g)" (float_of_json_exn (member "radians" c)) in
    check_float_u
      (label ^ " sin")
      ~expected:(float_u_of_json_exn (member "sin" c))
      ~actual:(S2.S1_chord_angle.sin angle);
    check_float_u
      (label ^ " sin2")
      ~expected:(float_u_of_json_exn (member "sin2" c))
      ~actual:(S2.S1_chord_angle.sin2 angle);
    check_float_u
      (label ^ " cos")
      ~expected:(float_u_of_json_exn (member "cos" c))
      ~actual:(S2.S1_chord_angle.cos angle);
    let expected_tan =
      match member "tan" c with
      | `Null -> Float.infinity
      | j -> float_of_json_exn j
    in
    check_float
      (label ^ " atan(tan)")
      ~expected:(Float.atan expected_tan)
      ~actual:(Float.atan (Float_u.to_float (S2.S1_chord_angle.tan angle))))
;;

let test_plus_error fixture () =
  let cases = to_list (member "plus_error" fixture) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let angle =
      let length2 = member "length2" c in
      match length2 with
      | `Null -> S2.S1_chord_angle.infinity
      | j -> S2.S1_chord_angle.of_length2 (float_u_of_json_exn j)
    in
    let error = float_of_json_exn (member "error" c) in
    let result = S2.S1_chord_angle.plus_error angle error in
    check_float_u_or_infinity
      (name ^ " result_length2")
      (member "result_length2" c)
      (S2.S1_chord_angle.length2 result))
;;

let test_error_bounds fixture () =
  let cases = to_list (member "error_bounds" fixture) in
  List.iter cases ~f:(fun c ->
    let angle = S2.S1_chord_angle.of_length2 (float_u_of_json_exn (member "length2" c)) in
    check_float
      (sprintf
         "error_bounds length2=%g point"
         (Float_u.to_float (S2.S1_chord_angle.length2 angle)))
      ~expected:(float_of_json_exn (member "max_point_error" c))
      ~actual:(S2.S1_chord_angle.max_point_error angle);
    check_float
      (sprintf
         "error_bounds length2=%g angle"
         (Float_u.to_float (S2.S1_chord_angle.length2 angle)))
      ~expected:(float_of_json_exn (member "max_angle_error" c))
      ~actual:(S2.S1_chord_angle.max_angle_error angle))
;;

let () =
  let fixture = load_fixture "s1chordangle.json" in
  Alcotest.run
    "S1_chord_angle"
    [ "constructors", [ test_case "Constructors" `Quick (test_constructors fixture) ]
    ; "convenience", [ test_case "Convenience" `Quick (test_convenience fixture) ]
    ; "predicates", [ test_case "Predicates" `Quick (test_predicates fixture) ]
    ; "comparison", [ test_case "Comparison" `Quick (test_comparison fixture) ]
    ; "to_angle", [ test_case "ToAngle" `Quick (test_to_angle fixture) ]
    ; ( "successor_predecessor"
      , [ test_case "SuccessorPredecessor" `Quick (test_successor_predecessor fixture) ] )
    ; ( "successor_chain"
      , [ test_case "SuccessorChain" `Quick (test_successor_chain fixture) ] )
    ; ( "predecessor_chain"
      , [ test_case "PredecessorChain" `Quick (test_predecessor_chain fixture) ] )
    ; "arithmetic", [ test_case "Arithmetic" `Quick (test_arithmetic fixture) ]
    ; "trigonometry", [ test_case "Trigonometry" `Quick (test_trigonometry fixture) ]
    ; "plus_error", [ test_case "PlusError" `Quick (test_plus_error fixture) ]
    ; "error_bounds", [ test_case "ErrorBounds" `Quick (test_error_bounds fixture) ]
    ; ( "quickcheck"
      , [ test_case "add_commutative" `Quick quickcheck_add_commutative
        ; test_case "monotone_with_angle" `Quick quickcheck_monotone_with_angle
        ; test_case
            "successor_predecessor_inverse"
            `Quick
            quickcheck_successor_predecessor_inverse
        ; test_case "of_angle_roundtrip" `Quick quickcheck_of_angle_roundtrip
        ; test_case "sin2_cos2_identity" `Quick quickcheck_sin2_cos2_identity
        ; test_case "compare_consistent" `Quick quickcheck_compare_consistent
        ] )
    ]
;;
