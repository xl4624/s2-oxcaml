(* Quickcheck property tests for S1_angle. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

module S1_angle_only = struct
  type t = { radians : float } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    map (float_inclusive (-10.0) 10.0) ~f:(fun r -> { radians = r })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  let to_angle t = S2.S1_angle.of_radians (Float_u.of_float t.radians)
end

module S1_angle_pair = struct
  type t =
    { a : float
    ; b : float
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let rad = float_inclusive (-10.0) 10.0 in
    map (both rad rad) ~f:(fun (a, b) -> { a; b })
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
    (module S1_angle_pair)
    ~config:qc_config
    ~f:(fun { S1_angle_pair.a; b } ->
      let a = S2.S1_angle.of_radians (Float_u.of_float a) in
      let b = S2.S1_angle.of_radians (Float_u.of_float b) in
      assert (S2.S1_angle.equal (S2.S1_angle.add a b) (S2.S1_angle.add b a)))
;;

let%test_unit "neg_involution" =
  Base_quickcheck.Test.run_exn (module S1_angle_only) ~config:qc_config ~f:(fun t ->
    let a = S1_angle_only.to_angle t in
    assert (S2.S1_angle.equal (S2.S1_angle.neg (S2.S1_angle.neg a)) a))
;;

let%test_unit "normalized_idempotent" =
  Base_quickcheck.Test.run_exn (module S1_angle_only) ~config:qc_config ~f:(fun t ->
    let a = S1_angle_only.to_angle t in
    let n = S2.S1_angle.normalized a in
    assert (S2.S1_angle.equal (S2.S1_angle.normalized n) n))
;;

let%test_unit "sub_self_zero" =
  Base_quickcheck.Test.run_exn (module S1_angle_only) ~config:qc_config ~f:(fun t ->
    let a = S1_angle_only.to_angle t in
    let sub_a = S2.S1_angle.sub a a in
    assert (S2.S1_angle.equal sub_a S2.S1_angle.zero))
;;

let%test_unit "abs_nonneg" =
  Base_quickcheck.Test.run_exn (module S1_angle_only) ~config:qc_config ~f:(fun t ->
    let a = S1_angle_only.to_angle t in
    let abs_a = S2.S1_angle.abs a in
    assert (Float_u.( >= ) (S2.S1_angle.radians abs_a) #0.0))
;;

let%test_unit "mul_identity" =
  Base_quickcheck.Test.run_exn (module S1_angle_only) ~config:qc_config ~f:(fun t ->
    let a = S1_angle_only.to_angle t in
    let mul_a = S2.S1_angle.mul a #1.0 in
    assert (S2.S1_angle.equal mul_a a))
;;

let%test_unit "div_self_one" =
  Base_quickcheck.Test.run_exn
    (module S1_angle_only)
    ~config:qc_config
    ~f:(fun { S1_angle_only.radians = r } ->
      let open Float_u.O in
      let a_rad = Float.abs r in
      if Float.( > ) a_rad 1e-10
      then (
        let a_rad_u = Float_u.of_float a_rad in
        let a = S2.S1_angle.of_radians a_rad_u in
        let div_a = S2.S1_angle.div a a_rad_u in
        let diff = Float_u.abs (S2.S1_angle.radians div_a - #1.0) in
        assert (diff <= #1e-14)))
;;

let%test_unit "degrees_radians" =
  Base_quickcheck.Test.run_exn (module S1_angle_only) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let a = S1_angle_only.to_angle t in
    let deg = S2.S1_angle.degrees a in
    let back = S2.S1_angle.of_degrees deg in
    let diff = Float_u.abs (S2.S1_angle.radians a - S2.S1_angle.radians back) in
    assert (diff <= #1e-13))
;;

let%test_unit "sub_add_inverse" =
  Base_quickcheck.Test.run_exn
    (module S1_angle_pair)
    ~config:qc_config
    ~f:(fun { S1_angle_pair.a; b } ->
      let open Float_u.O in
      let a = S2.S1_angle.of_radians (Float_u.of_float a) in
      let b = S2.S1_angle.of_radians (Float_u.of_float b) in
      let roundtrip = S2.S1_angle.add (S2.S1_angle.sub a b) b in
      let diff = Float_u.abs (S2.S1_angle.radians a - S2.S1_angle.radians roundtrip) in
      let scale = Float_u.max #1.0 (Float_u.abs (S2.S1_angle.radians a)) in
      assert (diff <= #1e-13 * scale))
;;

let%test_unit "add_associative" =
  Base_quickcheck.Test.run_exn
    (module S1_angle_pair)
    ~config:qc_config
    ~f:(fun { S1_angle_pair.a; b } ->
      let open Float_u.O in
      let a = S2.S1_angle.of_radians (Float_u.of_float a) in
      let b = S2.S1_angle.of_radians (Float_u.of_float b) in
      let c = S2.S1_angle.of_radians #0.5 in
      let lhs = S2.S1_angle.add (S2.S1_angle.add a b) c in
      let rhs = S2.S1_angle.add a (S2.S1_angle.add b c) in
      let diff = Float_u.abs (S2.S1_angle.radians lhs - S2.S1_angle.radians rhs) in
      let scale =
        Float_u.max
          #1.0
          (Float_u.abs (S2.S1_angle.radians a) + Float_u.abs (S2.S1_angle.radians b))
      in
      assert (diff <= #1e-13 * scale))
;;

let%test_unit "mul_distributive_over_add" =
  Base_quickcheck.Test.run_exn
    (module S1_angle_pair)
    ~config:qc_config
    ~f:(fun { S1_angle_pair.a; b } ->
      let open Float_u.O in
      let k = #2.5 in
      let a = S2.S1_angle.of_radians (Float_u.of_float a) in
      let b = S2.S1_angle.of_radians (Float_u.of_float b) in
      let lhs = S2.S1_angle.mul (S2.S1_angle.add a b) k in
      let rhs = S2.S1_angle.add (S2.S1_angle.mul a k) (S2.S1_angle.mul b k) in
      let diff = Float_u.abs (S2.S1_angle.radians lhs - S2.S1_angle.radians rhs) in
      let scale =
        Float_u.max
          #1.0
          (k * (Float_u.abs (S2.S1_angle.radians a) + Float_u.abs (S2.S1_angle.radians b)))
      in
      assert (diff <= #1e-13 * scale))
;;

let%test_unit "sin_cos_identity" =
  Base_quickcheck.Test.run_exn (module S1_angle_only) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let a = S1_angle_only.to_angle t in
    let s = S2.S1_angle.sin a in
    let c = S2.S1_angle.cos a in
    let sum = (s * s) + (c * c) in
    assert (Float_u.abs (sum - #1.0) <= #1e-14))
;;

let%test_unit "sin_cos_pair_matches" =
  Base_quickcheck.Test.run_exn (module S1_angle_only) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let a = S1_angle_only.to_angle t in
    let #(s_pair, c_pair) = S2.S1_angle.sin_cos a in
    let s = S2.S1_angle.sin a in
    let c = S2.S1_angle.cos a in
    assert (Float_u.abs (s_pair - s) <= #1e-15);
    assert (Float_u.abs (c_pair - c) <= #1e-15))
;;

let%test_unit "normalized_in_range" =
  Base_quickcheck.Test.run_exn (module S1_angle_only) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let a = S1_angle_only.to_angle t in
    let n = S2.S1_angle.radians (S2.S1_angle.normalized a) in
    let neg_pi = Float_u.neg Float_u.pi in
    assert (n > neg_pi);
    assert (n <= Float_u.pi))
;;
