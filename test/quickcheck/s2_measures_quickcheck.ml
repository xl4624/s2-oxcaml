(* Quickcheck property tests for S2_measures. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

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

let radians a = S2.S1_angle.radians a

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "angle_symmetric" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let abc = radians (S2.S2_measures.angle a b c) in
      let cba = radians (S2.S2_measures.angle c b a) in
      check_float_u ~eps:1e-14 "angle symmetric" ~expected:abc ~actual:cba)
;;

let%test_unit "turn_angle_antisymmetric" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let abc = radians (S2.S2_measures.turn_angle a b c) in
      let cba = radians (S2.S2_measures.turn_angle c b a) in
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

let%test_unit "area_non_negative" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let ar = S2.S2_measures.area a b c in
      assert (Float_u.O.(ar >= #0.0)))
;;

let%test_unit "signed_area_magnitude" =
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
