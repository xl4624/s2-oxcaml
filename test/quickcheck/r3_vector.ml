(* Quickcheck property tests for R3_vector. *)

(* --- Generators ----------------------------------------------------------- *)

module R3_pair = struct
  type t =
    { a : S2.R3_vector.t
    ; b : S2.R3_vector.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1e6) 1e6 in
    create (fun ~size:_ ~random:rnd ->
      let gen () = Float_u.of_float (generate coord ~size:30 ~random:rnd) in
      let v () = S2.R3_vector.create ~x:(gen ()) ~y:(gen ()) ~z:(gen ()) in
      { a = v (); b = v () })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module R3_vec = struct
  type t = { v : S2.R3_vector.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1e6) 1e6 in
    create (fun ~size:_ ~random:rnd ->
      let gen () = Float_u.of_float (generate coord ~size:30 ~random:rnd) in
      { v = S2.R3_vector.create ~x:(gen ()) ~y:(gen ()) ~z:(gen ()) })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "dot_commutative" =
  Base_quickcheck.Test.run_exn
    (module R3_pair)
    ~config:qc_config
    ~f:(fun { R3_pair.a; b } ->
      let open Float_u.O in
      let u = S2.R3_vector.dot a b in
      let v = S2.R3_vector.dot b a in
      let scale = Float_u.max #1.0 (Float_u.max (Float_u.abs u) (Float_u.abs v)) in
      assert (Float_u.abs (u - v) <= #1e-10 * scale))
;;

let%test_unit "cross_antisymmetric" =
  Base_quickcheck.Test.run_exn
    (module R3_pair)
    ~config:qc_config
    ~f:(fun { R3_pair.a; b } ->
      assert (
        S2.R3_vector.approx_equal
          ~max_error:(Packed_float_option.Unboxed.some #1e-10)
          (S2.R3_vector.cross b a)
          (S2.R3_vector.neg (S2.R3_vector.cross a b))))
;;

let%test_unit "norm2_nonneg" =
  Base_quickcheck.Test.run_exn (module R3_vec) ~config:qc_config ~f:(fun { R3_vec.v } ->
    assert (Float_u.O.(S2.R3_vector.norm2 v >= #0.0)))
;;

let%test_unit "add_commutative" =
  Base_quickcheck.Test.run_exn
    (module R3_pair)
    ~config:qc_config
    ~f:(fun { R3_pair.a; b } ->
      assert (S2.R3_vector.equal (S2.R3_vector.add a b) (S2.R3_vector.add b a)))
;;

let%test_unit "sub_self_zero" =
  Base_quickcheck.Test.run_exn
    (module R3_vec)
    ~config:qc_config
    ~f:(fun { R3_vec.v = p } ->
      let sub_p = S2.R3_vector.sub p p in
      assert (S2.R3_vector.equal sub_p S2.R3_vector.zero))
;;

let%test_unit "neg_involution" =
  Base_quickcheck.Test.run_exn
    (module R3_vec)
    ~config:qc_config
    ~f:(fun { R3_vec.v = p } ->
      let neg_p = S2.R3_vector.neg p in
      let neg_neg_p = S2.R3_vector.neg neg_p in
      assert (S2.R3_vector.equal neg_neg_p p))
;;

let%test_unit "abs_nonneg" =
  Base_quickcheck.Test.run_exn
    (module R3_vec)
    ~config:qc_config
    ~f:(fun { R3_vec.v = p } ->
      let open Float_u.O in
      let f = S2.R3_vector.abs p in
      assert (S2.R3_vector.x f >= #0.0);
      assert (S2.R3_vector.y f >= #0.0);
      assert (S2.R3_vector.z f >= #0.0))
;;

let%test_unit "sub_antisymmetric" =
  Base_quickcheck.Test.run_exn
    (module R3_pair)
    ~config:qc_config
    ~f:(fun { R3_pair.a; b } ->
      let ab = S2.R3_vector.sub a b in
      let ba = S2.R3_vector.sub b a in
      assert (S2.R3_vector.equal ab (S2.R3_vector.neg ba)))
;;

let%test_unit "cross_perpendicular_to_operands" =
  Base_quickcheck.Test.run_exn
    (module R3_pair)
    ~config:qc_config
    ~f:(fun { R3_pair.a; b } ->
      let open Float_u.O in
      let c = S2.R3_vector.cross a b in
      let na = S2.R3_vector.norm a in
      let nb = S2.R3_vector.norm b in
      let scale = Float_u.max #1.0 (na * na * nb) in
      let da = Float_u.abs (S2.R3_vector.dot c a) in
      let db = Float_u.abs (S2.R3_vector.dot c b) in
      assert (da <= #1e-10 * scale);
      assert (db <= #1e-10 * scale))
;;

let%test_unit "lagrange_identity" =
  (* |a x b|^2 = |a|^2 * |b|^2 - (a . b)^2. *)
  Base_quickcheck.Test.run_exn
    (module R3_pair)
    ~config:qc_config
    ~f:(fun { R3_pair.a; b } ->
      let open Float_u.O in
      let cr = S2.R3_vector.cross a b in
      let lhs = S2.R3_vector.norm2 cr in
      let dot = S2.R3_vector.dot a b in
      let rhs = (S2.R3_vector.norm2 a * S2.R3_vector.norm2 b) - (dot * dot) in
      let scale = Float_u.max #1.0 (Float_u.max (Float_u.abs lhs) (Float_u.abs rhs)) in
      assert (Float_u.abs (lhs - rhs) <= #1e-8 * scale))
;;

let%test_unit "triangle_inequality" =
  Base_quickcheck.Test.run_exn
    (module R3_pair)
    ~config:qc_config
    ~f:(fun { R3_pair.a; b } ->
      let open Float_u.O in
      let na = S2.R3_vector.norm a in
      let nb = S2.R3_vector.norm b in
      let nab = S2.R3_vector.norm (S2.R3_vector.add a b) in
      let scale = Float_u.max #1.0 (na + nb) in
      assert (nab <= na + nb + (#1e-12 * scale)))
;;

let%test_unit "dot_distributive" =
  (* a . (b + c) = a . b + a . c with c fixed to a constant vector. *)
  Base_quickcheck.Test.run_exn
    (module R3_pair)
    ~config:qc_config
    ~f:(fun { R3_pair.a; b } ->
      let open Float_u.O in
      let c = S2.R3_vector.create ~x:#1.5 ~y:(-#2.25) ~z:#0.75 in
      let lhs = S2.R3_vector.dot a (S2.R3_vector.add b c) in
      let rhs = S2.R3_vector.dot a b + S2.R3_vector.dot a c in
      let scale = Float_u.max #1.0 (Float_u.max (Float_u.abs lhs) (Float_u.abs rhs)) in
      assert (Float_u.abs (lhs - rhs) <= #1e-8 * scale))
;;

let%test_unit "norm_squared_consistent" =
  Base_quickcheck.Test.run_exn (module R3_vec) ~config:qc_config ~f:(fun { R3_vec.v } ->
    let open Float_u.O in
    let n = S2.R3_vector.norm v in
    let n2 = S2.R3_vector.norm2 v in
    assert (n >= #0.0);
    let scale = Float_u.max #1.0 n2 in
    assert (Float_u.abs ((n * n) - n2) <= #1e-10 * scale))
;;
