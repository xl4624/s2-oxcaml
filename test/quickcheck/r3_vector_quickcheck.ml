(* Quickcheck property tests for R3_vector. *)
open Core
open Test_helpers

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
