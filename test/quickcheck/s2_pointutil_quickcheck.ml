(* Quickcheck property tests for S2_pointutil. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

module Unit_point = struct
  type t = { p : S2.S2_point.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    create (fun ~size:_ ~random:rnd ->
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      { p =
          S2.S2_point.of_coords
            ~x:(Float_u.of_float x)
            ~y:(Float_u.of_float y)
            ~z:(Float_u.of_float z)
      })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "is_unit_length" =
  Base_quickcheck.Test.run_exn
    (module Unit_point)
    ~config:qc_config
    ~f:(fun { Unit_point.p } -> assert (S2.S2_pointutil.is_unit_length p))
;;

let%test_unit "neg_is_unit_length" =
  Base_quickcheck.Test.run_exn
    (module Unit_point)
    ~config:qc_config
    ~f:(fun { Unit_point.p } ->
      assert (S2.S2_pointutil.is_unit_length (S2.R3_vector.neg p)))
;;

let%test_unit "ortho_perpendicular" =
  Base_quickcheck.Test.run_exn
    (module Unit_point)
    ~config:qc_config
    ~f:(fun { Unit_point.p } ->
      let o = S2.S2_pointutil.ortho p in
      let dot = S2.R3_vector.dot p o in
      assert (Float_u.O.(Float_u.abs dot <= #1e-14)))
;;

let%test_unit "ortho_unit" =
  Base_quickcheck.Test.run_exn
    (module Unit_point)
    ~config:qc_config
    ~f:(fun { Unit_point.p } ->
      let o = S2.S2_pointutil.ortho p in
      assert (S2.S2_pointutil.is_unit_length o))
;;

let%test_unit "ortho_negation" =
  Base_quickcheck.Test.run_exn
    (module Unit_point)
    ~config:qc_config
    ~f:(fun { Unit_point.p } ->
      let o = S2.S2_pointutil.ortho p in
      let o_neg = S2.S2_pointutil.ortho (S2.R3_vector.neg p) in
      let sum = S2.R3_vector.add o o_neg in
      let norm = Float_u.to_float (S2.R3_vector.norm sum) in
      assert (Float.(norm <= 1e-14)))
;;

let%test_unit "approx_equals_self" =
  Base_quickcheck.Test.run_exn
    (module Unit_point)
    ~config:qc_config
    ~f:(fun { Unit_point.p } ->
      assert (
        S2.S2_pointutil.approx_equals
          ~max_error_radians:(Packed_float_option.Unboxed.none ())
          p
          p))
;;

let%test_unit "frame_roundtrip" =
  Base_quickcheck.Test.run_exn
    (module Unit_point)
    ~config:qc_config
    ~f:(fun { Unit_point.p } ->
      let frame = S2.S2_pointutil.get_frame p in
      let q = S2.S2_point.of_coords ~x:#0.6 ~y:#0.8 ~z:#0.0 in
      let local = S2.S2_pointutil.to_frame frame q in
      let back = S2.S2_pointutil.from_frame frame local in
      assert (
        S2.S2_pointutil.approx_equals
          ~max_error_radians:(Packed_float_option.Unboxed.some #1e-14)
          q
          back))
;;

let%test_unit "frame_col2_equals_z" =
  Base_quickcheck.Test.run_exn
    (module Unit_point)
    ~config:qc_config
    ~f:(fun { Unit_point.p } ->
      let frame = S2.S2_pointutil.get_frame p in
      let #{ S2.S2_point.col0 = _; col1 = _; col2 } = frame in
      assert (
        S2.S2_pointutil.approx_equals
          ~max_error_radians:(Packed_float_option.Unboxed.some #1e-14)
          col2
          p))
;;
