(* Quickcheck property tests for S1_chord_angle. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

module Chord_angle_valid = struct
  type t = { degrees : float } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    map (float_inclusive 0.0 180.0) ~f:(fun d -> { degrees = d })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  let to_chord t = S2.S1_chord_angle.of_degrees (Float_u.of_float t.degrees)
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

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "add_commutative" =
  Base_quickcheck.Test.run_exn
    (module Chord_angle_pair)
    ~config:qc_config
    ~f:(fun { Chord_angle_pair.a; b } ->
      let ca = S2.S1_chord_angle.of_degrees (Float_u.of_float a) in
      let cb = S2.S1_chord_angle.of_degrees (Float_u.of_float b) in
      let sum_ab = S2.S1_chord_angle.add ca cb in
      let sum_ba = S2.S1_chord_angle.add cb ca in
      assert (S2.S1_chord_angle.equal sum_ab sum_ba))
;;

let%test_unit "monotone_with_angle" =
  Base_quickcheck.Test.run_exn
    (module Chord_angle_pair)
    ~config:qc_config
    ~f:(fun { Chord_angle_pair.a; b } ->
      let ca = S2.S1_chord_angle.of_degrees (Float_u.of_float a) in
      let cb = S2.S1_chord_angle.of_degrees (Float_u.of_float b) in
      if S2.S1_chord_angle.compare ca cb <= 0
      then
        assert (
          S2.S1_angle.compare
            (S2.S1_chord_angle.to_angle ca)
            (S2.S1_chord_angle.to_angle cb)
          <= 0))
;;

let%test_unit "successor_predecessor_inverse" =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let a = Chord_angle_valid.to_chord t in
    if (not (S2.S1_chord_angle.is_special a))
       && not (S2.S1_chord_angle.equal a S2.S1_chord_angle.straight)
    then (
      let s = S2.S1_chord_angle.successor a in
      let ps = S2.S1_chord_angle.predecessor s in
      assert (S2.S1_chord_angle.equal ps a)))
;;

let%test_unit "of_angle_roundtrip" =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let ca = Chord_angle_valid.to_chord t in
    let angle = S2.S1_chord_angle.to_angle ca in
    let ca2 = S2.S1_chord_angle.of_angle angle in
    let d1 = S2.S1_chord_angle.length2 ca in
    let d2 = S2.S1_chord_angle.length2 ca2 in
    let scale = Float_u.max #1.0 (Float_u.max (Float_u.abs d1) (Float_u.abs d2)) in
    assert (Float_u.abs (d1 - d2) <= #1e-13 * scale))
;;

let%test_unit "sin2_cos2_identity" =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let ca = Chord_angle_valid.to_chord t in
    if not (S2.S1_chord_angle.is_special ca)
    then (
      let s = S2.S1_chord_angle.sin ca in
      let c = S2.S1_chord_angle.cos ca in
      let sum = (s * s) + (c * c) in
      assert (Float_u.abs (sum - #1.0) <= #1e-14)))
;;

let%test_unit "compare_consistent" =
  Base_quickcheck.Test.run_exn
    (module Chord_angle_pair)
    ~config:qc_config
    ~f:(fun { Chord_angle_pair.a; b } ->
      let ca = S2.S1_chord_angle.of_degrees (Float_u.of_float a) in
      let cb = S2.S1_chord_angle.of_degrees (Float_u.of_float b) in
      let cmp = S2.S1_chord_angle.compare ca cb in
      let l2_cmp =
        Float_u.compare (S2.S1_chord_angle.length2 ca) (S2.S1_chord_angle.length2 cb)
      in
      assert (Sign.equal (Sign.of_int cmp) (Sign.of_int l2_cmp)))
;;

let%test_unit "successor_strictly_greater" =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let a = Chord_angle_valid.to_chord t in
    if not (S2.S1_chord_angle.equal a S2.S1_chord_angle.straight)
    then (
      let s = S2.S1_chord_angle.successor a in
      assert (S2.S1_chord_angle.compare s a > 0)))
;;

let%test_unit "predecessor_strictly_less" =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let a = Chord_angle_valid.to_chord t in
    if not (S2.S1_chord_angle.is_zero a)
    then (
      let p = S2.S1_chord_angle.predecessor a in
      assert (S2.S1_chord_angle.compare p a < 0)))
;;

let%test_unit "sub_self_zero" =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let a = Chord_angle_valid.to_chord t in
    if not (S2.S1_chord_angle.is_special a)
    then (
      let diff = S2.S1_chord_angle.sub a a in
      assert (S2.S1_chord_angle.is_zero diff)))
;;

let%test_unit "add_zero_identity" =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let a = Chord_angle_valid.to_chord t in
    let sum = S2.S1_chord_angle.add a S2.S1_chord_angle.zero in
    assert (S2.S1_chord_angle.equal sum a))
;;

let%test_unit "plus_error_zero_identity" =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let a = Chord_angle_valid.to_chord t in
    let a' = S2.S1_chord_angle.plus_error a #0.0 in
    assert (S2.S1_chord_angle.equal a' a))
;;

let%test_unit "length2_nonneg" =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let a = Chord_angle_valid.to_chord t in
    assert (Float_u.O.(S2.S1_chord_angle.length2 a >= #0.0)))
;;

let%test_unit "tan_matches_sin_over_cos" =
  Base_quickcheck.Test.run_exn (module Chord_angle_valid) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let a = Chord_angle_valid.to_chord t in
    if not (S2.S1_chord_angle.is_special a)
    then (
      let c = S2.S1_chord_angle.cos a in
      if Float_u.abs c > #1e-6
      then (
        let tn = S2.S1_chord_angle.tan a in
        let s = S2.S1_chord_angle.sin a in
        let expect = s / c in
        let scale = Float_u.max #1.0 (Float_u.abs expect) in
        assert (Float_u.abs (tn - expect) <= #1e-12 * scale))))
;;
