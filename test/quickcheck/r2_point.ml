(* Quickcheck property tests for R2_point. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

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

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "add_commutative" =
  Base_quickcheck.Test.run_exn
    (module R2_pair)
    ~config:qc_config
    ~f:(fun { R2_pair.a; b } ->
      assert (S2.R2_point.equal (S2.R2_point.add a b) (S2.R2_point.add b a)))
;;

let%test_unit "dot_commutative" =
  Base_quickcheck.Test.run_exn
    (module R2_pair)
    ~config:qc_config
    ~f:(fun { R2_pair.a; b } ->
      let u = S2.R2_point.dot a b in
      let v = S2.R2_point.dot b a in
      let open Float_u.O in
      let scale = Float_u.(max #1.0 (max (abs u) (abs v))) in
      assert (Float_u.abs (u - v) <= #1e-10 * scale))
;;

let%test_unit "ortho_perpendicular" =
  Base_quickcheck.Test.run_exn (module R2_vec) ~config:qc_config ~f:(fun { R2_vec.v } ->
    let d = S2.R2_point.dot v (S2.R2_point.ortho v) in
    let open Float_u.O in
    let scale =
      Float_u.(max #1.0 (max (abs (S2.R2_point.x v)) (abs (S2.R2_point.y v))))
    in
    assert (Float_u.abs d <= #1e-14 * scale))
;;

let%test_unit "norm2_mul" =
  Base_quickcheck.Test.run_exn
    (module R2_point_scalar)
    ~config:qc_config
    ~f:(fun { R2_point_scalar.v; k } ->
      let ku = Float_u.of_float k in
      let lhs = S2.R2_point.norm2 (S2.R2_point.mul v ku) in
      let rhs = Float_u.(ku * ku * S2.R2_point.norm2 v) in
      let open Float_u.O in
      let scale = Float_u.(max (abs lhs) (abs rhs) |> max #1.0) in
      assert (Float_u.abs (lhs - rhs) <= #1e-10 * scale))
;;

let%test_unit "neg_involution" =
  Base_quickcheck.Test.run_exn
    (module R2_vec)
    ~config:qc_config
    ~f:(fun { R2_vec.v = p } ->
      let neg_p = S2.R2_point.neg p in
      let neg_neg_p = S2.R2_point.neg neg_p in
      assert (S2.R2_point.equal neg_neg_p p))
;;

let%test_unit "sub_self_zero" =
  Base_quickcheck.Test.run_exn
    (module R2_vec)
    ~config:qc_config
    ~f:(fun { R2_vec.v = p } ->
      let sub_p = S2.R2_point.sub p p in
      assert (S2.R2_point.equal sub_p S2.R2_point.zero))
;;

let%test_unit "norm_nonneg" =
  Base_quickcheck.Test.run_exn
    (module R2_vec)
    ~config:qc_config
    ~f:(fun { R2_vec.v = p } -> assert (Float_u.O.(S2.R2_point.norm p >= #0.0)))
;;

let%test_unit "normalize_unit_or_zero" =
  Base_quickcheck.Test.run_exn
    (module R2_vec)
    ~config:qc_config
    ~f:(fun { R2_vec.v = p } ->
      let n = S2.R2_point.normalize p in
      if S2.R2_point.equal p S2.R2_point.zero
      then assert (S2.R2_point.equal n S2.R2_point.zero)
      else
        let open Float_u.O in
        assert (Float_u.abs (S2.R2_point.norm n - #1.0) <= #1e-15))
;;

let%test_unit "cross_antisymmetric" =
  Base_quickcheck.Test.run_exn
    (module R2_pair)
    ~config:qc_config
    ~f:(fun { R2_pair.a; b } ->
      let open Float_u.O in
      assert (Float_u.abs (S2.R2_point.cross a b + S2.R2_point.cross b a) <= #1e-15))
;;

let%test_unit "fabs_nonneg" =
  Base_quickcheck.Test.run_exn
    (module R2_vec)
    ~config:qc_config
    ~f:(fun { R2_vec.v = p } ->
      let f = S2.R2_point.fabs p in
      let open Float_u.O in
      assert (S2.R2_point.x f >= #0.0);
      assert (S2.R2_point.y f >= #0.0))
;;

let%test_unit "sub_antisymmetric" =
  Base_quickcheck.Test.run_exn
    (module R2_pair)
    ~config:qc_config
    ~f:(fun { R2_pair.a; b } ->
      let ab = S2.R2_point.sub a b in
      let ba = S2.R2_point.sub b a in
      assert (S2.R2_point.equal ab (S2.R2_point.neg ba)))
;;

let%test_unit "triangle_inequality" =
  Base_quickcheck.Test.run_exn
    (module R2_pair)
    ~config:qc_config
    ~f:(fun { R2_pair.a; b } ->
      let open Float_u.O in
      let na = S2.R2_point.norm a in
      let nb = S2.R2_point.norm b in
      let nab = S2.R2_point.norm (S2.R2_point.add a b) in
      let scale = Float_u.max #1.0 (na + nb) in
      assert (nab <= na + nb + (#1e-12 * scale)))
;;

let%test_unit "dot_distributive" =
  Base_quickcheck.Test.run_exn
    (module R2_pair)
    ~config:qc_config
    ~f:(fun { R2_pair.a; b } ->
      let open Float_u.O in
      let c = S2.R2_point.create ~x:#3.14 ~y:(-#1.59) in
      let lhs = S2.R2_point.dot a (S2.R2_point.add b c) in
      let rhs = S2.R2_point.dot a b + S2.R2_point.dot a c in
      let scale = Float_u.max #1.0 (Float_u.max (Float_u.abs lhs) (Float_u.abs rhs)) in
      assert (Float_u.abs (lhs - rhs) <= #1e-8 * scale))
;;

let%test_unit "add_associative" =
  Base_quickcheck.Test.run_exn
    (module R2_pair)
    ~config:qc_config
    ~f:(fun { R2_pair.a; b } ->
      let c = S2.R2_point.create ~x:#0.5 ~y:(-#1.25) in
      let lhs = S2.R2_point.add (S2.R2_point.add a b) c in
      let rhs = S2.R2_point.add a (S2.R2_point.add b c) in
      let d = S2.R2_point.sub lhs rhs in
      let open Float_u.O in
      let m =
        Float_u.max (Float_u.abs (S2.R2_point.x d)) (Float_u.abs (S2.R2_point.y d))
      in
      assert (m <= #1e-9))
;;

let%test_unit "norm2_is_norm_squared" =
  Base_quickcheck.Test.run_exn
    (module R2_vec)
    ~config:qc_config
    ~f:(fun { R2_vec.v = p } ->
      let open Float_u.O in
      let n = S2.R2_point.norm p in
      let n2 = S2.R2_point.norm2 p in
      let scale = Float_u.max #1.0 n2 in
      assert (Float_u.abs ((n * n) - n2) <= #1e-10 * scale))
;;

let%test_unit "dot_self_is_norm2" =
  Base_quickcheck.Test.run_exn
    (module R2_vec)
    ~config:qc_config
    ~f:(fun { R2_vec.v = p } ->
      let open Float_u.O in
      let d = S2.R2_point.dot p p in
      let n2 = S2.R2_point.norm2 p in
      let scale = Float_u.max #1.0 n2 in
      assert (Float_u.abs (d - n2) <= #1e-12 * scale))
;;
