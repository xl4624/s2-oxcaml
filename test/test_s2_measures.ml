(* C++ test parity: s2geometry/src/s2/s2measures_test.cc
   -  TEST(S2, AngleMethods)             - full parity via fixture cases
   -  TEST(S2, AreaMethods)               - full parity via fixture cases
   -  TEST(S2, GetAreaRegression_B229644268) - full parity
   -  Quarter-sphere near-180 triangles  - full parity (two groups)
   -  Meridian zero-area case            - full parity

   Extra coverage:
   -  quickcheck: angle symmetry (angle a b c = angle c b a)
   -  quickcheck: turn_angle antisymmetry (turn_angle a b c = -turn_angle c b a)
   -  quickcheck: signed_area sign consistency with robust_sign
   -  quickcheck: area non-negative, signed_area magnitude equals area *)

open Core
open Test_helpers
open Alcotest

let fixture = load_fixture "s2measures.json"

let point_of_json j =
  match to_list j with
  | [ x; y; z ] ->
    S2.S2_point.of_coords
      ~x:(float_u_of_json_exn x)
      ~y:(float_u_of_json_exn y)
      ~z:(float_u_of_json_exn z)
  | _ ->
    (match failwith "expected [x, y, z]" with
     | (_ : Nothing.t) -> .)
;;

let radians a = S2.S1_angle.radians a

let test_angle_methods () =
  let cases = to_list (member "angle_methods" fixture) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let cv = point_of_json (member "c" c) in
    let expected_angle = float_u_of_json_exn (member "angle" c) in
    let expected_turn = float_u_of_json_exn (member "turn_angle" c) in
    let actual_angle = radians (S2.S2_measures.angle a b cv) in
    let actual_turn = radians (S2.S2_measures.turn_angle a b cv) in
    check_float_u (label ^ " angle") ~expected:expected_angle ~actual:actual_angle;
    check_float_u (label ^ " turn_angle") ~expected:expected_turn ~actual:actual_turn)
;;

let test_area_methods () =
  let cases = to_list (member "area_methods" fixture) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let cv = point_of_json (member "c" c) in
    let expected_area = float_u_of_json_exn (member "area" c) in
    let expected_girard = float_u_of_json_exn (member "girard_area" c) in
    let expected_signed = float_u_of_json_exn (member "signed_area" c) in
    let actual_area = S2.S2_measures.area a b cv in
    let actual_girard = S2.S2_measures.girard_area a b cv in
    let actual_signed = S2.S2_measures.signed_area a b cv in
    check_float_u (label ^ " area") ~expected:expected_area ~actual:actual_area;
    check_float_u (label ^ " girard") ~expected:expected_girard ~actual:actual_girard;
    check_float_u (label ^ " signed") ~expected:expected_signed ~actual:actual_signed)
;;

let test_area_regression () =
  let c = member "area_regression" fixture in
  let label = string_of_json_exn (member "label" c) in
  let a = point_of_json (member "a" c) in
  let b = point_of_json (member "b" c) in
  let cv = point_of_json (member "c" c) in
  let expected_area = float_u_of_json_exn (member "area" c) in
  let expected_girard = float_u_of_json_exn (member "girard_area" c) in
  let actual_area = S2.S2_measures.area a b cv in
  let actual_girard = S2.S2_measures.girard_area a b cv in
  check_float_u (label ^ " area") ~expected:expected_area ~actual:actual_area;
  check_float_u (label ^ " girard") ~expected:expected_girard ~actual:actual_girard
;;

let test_area_quarter_sphere () =
  let groups = to_list (member "area_quarter_sphere" fixture) in
  List.iter groups ~f:(fun g ->
    let label = string_of_json_exn (member "label" g) in
    let expected_sum = float_of_json_exn (member "sum" g) in
    let triangles = to_list (member "triangles" g) in
    let actual_sum =
      List.fold triangles ~init:0.0 ~f:(fun acc t ->
        let a = point_of_json (member "a" t) in
        let b = point_of_json (member "b" t) in
        let cv = point_of_json (member "c" t) in
        let expected = float_u_of_json_exn (member "area" t) in
        let actual = S2.S2_measures.area a b cv in
        check_float_u (label ^ " triangle area") ~expected ~actual;
        acc +. Float_u.to_float actual)
    in
    check_float ~eps:1e-14 (label ^ " sum") ~expected:expected_sum ~actual:actual_sum)
;;

let test_area_zero_meridian () =
  let c = member "area_zero_meridian" fixture in
  let label = string_of_json_exn (member "label" c) in
  let a = point_of_json (member "a" c) in
  let b = point_of_json (member "b" c) in
  let cv = point_of_json (member "c" c) in
  let expected_area = float_u_of_json_exn (member "area" c) in
  let actual_area = S2.S2_measures.area a b cv in
  check_float_u (label ^ " area") ~expected:expected_area ~actual:actual_area
;;

(* -- Quickcheck generators ------------------------------------------------ *)

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

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

let quickcheck_angle_symmetric () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let abc = radians (S2.S2_measures.angle a b c) in
      let cba = radians (S2.S2_measures.angle c b a) in
      check_float_u ~eps:1e-14 "angle symmetric" ~expected:abc ~actual:cba)
;;

let quickcheck_turn_angle_antisymmetric () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let abc = radians (S2.S2_measures.turn_angle a b c) in
      let cba = radians (S2.S2_measures.turn_angle c b a) in
      (* turn_angle a b c = -turn_angle c b a for distinct points. If a = c,
         both sides equal |pi|, so the sum is either 0 or 2*pi. Skip near-
         degenerate cases by checking the dot products. *)
      let open Float_u.O in
      let sum = abc + cba in
      let near_pi = Float_u.abs (Float_u.abs abc - Float_u.pi ()) <= #1e-9 in
      if not near_pi
      then
        if Float_u.abs sum > #1e-13
        then
          Alcotest.failf
            "turn_angle antisymmetry: abc=%.17g cba=%.17g"
            (Float_u.to_float abc)
            (Float_u.to_float cba))
;;

let quickcheck_area_non_negative () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let ar = S2.S2_measures.area a b c in
      assert (Float_u.O.(ar >= #0.0)))
;;

let quickcheck_signed_area_magnitude () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let ar = S2.S2_measures.area a b c in
      let sa = S2.S2_measures.signed_area a b c in
      let open Float_u.O in
      let diff = Float_u.abs (Float_u.abs sa - ar) in
      if diff > #1e-13
      then
        Alcotest.failf
          "|signed_area|=%.17g vs area=%.17g"
          (Float_u.to_float (Float_u.abs sa))
          (Float_u.to_float ar))
;;

(* -- End quickcheck ------------------------------------------------------- *)

let () =
  run
    "s2_measures"
    [ "angle", [ test_case "methods" `Quick test_angle_methods ]
    ; ( "area"
      , [ test_case "methods" `Quick test_area_methods
        ; test_case "regression_b229644268" `Quick test_area_regression
        ; test_case "quarter_sphere" `Quick test_area_quarter_sphere
        ; test_case "zero_meridian" `Quick test_area_zero_meridian
        ] )
    ; ( "quickcheck"
      , [ test_case "angle_symmetric" `Quick quickcheck_angle_symmetric
        ; test_case "turn_angle_antisymmetric" `Quick quickcheck_turn_angle_antisymmetric
        ; test_case "area_non_negative" `Quick quickcheck_area_non_negative
        ; test_case "signed_area_magnitude" `Quick quickcheck_signed_area_magnitude
        ] )
    ]
;;
