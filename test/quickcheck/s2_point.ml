(* Quickcheck property tests for S2_point. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

module S2_point_gen = struct
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

module S2_point_pair = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let gen_point rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      S2.S2_point.of_coords
        ~x:(Float_u.of_float x)
        ~y:(Float_u.of_float y)
        ~z:(Float_u.of_float z)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = gen_point rnd in
      let b = gen_point rnd in
      { a; b })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "distance_nonneg" =
  Base_quickcheck.Test.run_exn
    (module S2_point_pair)
    ~config:qc_config
    ~f:(fun { S2_point_pair.a; b } ->
      assert (Float_u.O.(S2.S1_angle.radians (S2.S2_point.distance a b) >= #0.0)))
;;

let%test_unit "distance_symmetric" =
  Base_quickcheck.Test.run_exn
    (module S2_point_pair)
    ~config:qc_config
    ~f:(fun { S2_point_pair.a; b } ->
      let d1 = S2.S1_angle.radians (S2.S2_point.distance a b) in
      let d2 = S2.S1_angle.radians (S2.S2_point.distance b a) in
      assert (Float_u.O.(Float_u.abs (d1 - d2) <= #1e-15)))
;;

let%test_unit "distance_self_zero" =
  Base_quickcheck.Test.run_exn
    (module S2_point_gen)
    ~config:qc_config
    ~f:(fun { S2_point_gen.p } ->
      let d = S2.S1_angle.radians (S2.S2_point.distance p p) in
      assert (Float_u.O.(Float_u.abs d <= #1e-15)))
;;

let%test_unit "ortho_perpendicular" =
  Base_quickcheck.Test.run_exn
    (module S2_point_gen)
    ~config:qc_config
    ~f:(fun { S2_point_gen.p } ->
      let o = S2.S2_point.ortho p in
      let dot = S2.R3_vector.dot p o in
      assert (Float_u.O.(Float_u.abs dot <= #1e-14)))
;;

let%test_unit "ortho_unit" =
  Base_quickcheck.Test.run_exn
    (module S2_point_gen)
    ~config:qc_config
    ~f:(fun { S2_point_gen.p } ->
      let o = S2.S2_point.ortho p in
      assert (S2.S2_point.is_unit_length o))
;;

let%test_unit "rotate_preserves_length" =
  Base_quickcheck.Test.run_exn
    (module S2_point_pair)
    ~config:qc_config
    ~f:(fun { S2_point_pair.a; b } ->
      let angle = S2.S1_angle.of_radians #1.23 in
      let rotated = S2.S2_point.rotate a ~axis:b ~angle in
      assert (S2.S2_point.is_unit_length rotated))
;;

let%test_unit "frame_roundtrip" =
  Base_quickcheck.Test.run_exn
    (module S2_point_gen)
    ~config:qc_config
    ~f:(fun { S2_point_gen.p } ->
      let frame = S2.S2_point.get_frame p in
      let q = S2.S2_point.of_coords ~x:#0.6 ~y:#0.8 ~z:#0.0 in
      let local = S2.S2_point.to_frame frame q in
      let back = S2.S2_point.from_frame frame local in
      assert (
        S2.S2_point.approx_equal
          ~max_error:(Packed_float_option.Unboxed.some #1e-14)
          q
          back))
;;

let%test_unit "distance_triangle_inequality" =
  (* d(a,c) <= d(a,b) + d(b,c) on the unit sphere. *)
  Base_quickcheck.Test.run_exn
    (module S2_point_pair)
    ~config:qc_config
    ~f:(fun { S2_point_pair.a; b } ->
      (* Use a fixed third point to keep the generator simple. *)
      let c = S2.S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0 in
      let open Float_u.O in
      let dab = S2.S1_angle.radians (S2.S2_point.distance a b) in
      let dbc = S2.S1_angle.radians (S2.S2_point.distance b c) in
      let dac = S2.S1_angle.radians (S2.S2_point.distance a c) in
      assert (dac <= dab + dbc + #1e-12))
;;

let%test_unit "distance_to_antipodal_is_pi" =
  Base_quickcheck.Test.run_exn
    (module S2_point_gen)
    ~config:qc_config
    ~f:(fun { S2_point_gen.p } ->
      let open Float_u.O in
      if not (S2.S2_point.equal p S2.R3_vector.zero)
      then (
        let p_unit = S2.R3_vector.normalize p in
        let antipode = S2.R3_vector.neg p_unit in
        let d = S2.S1_angle.radians (S2.S2_point.distance p_unit antipode) in
        assert (Float_u.abs (d - Float_u.pi) <= #1e-9)))
;;

let%test_unit "normalize_is_unit_length" =
  Base_quickcheck.Test.run_exn
    (module S2_point_gen)
    ~config:qc_config
    ~f:(fun { S2_point_gen.p } ->
      if not (S2.S2_point.equal p S2.R3_vector.zero)
      then (
        let p_unit = S2.R3_vector.normalize p in
        assert (S2.S2_point.is_unit_length p_unit)))
;;

let%test_unit "rotate_by_full_turn_identity" =
  Base_quickcheck.Test.run_exn
    (module S2_point_pair)
    ~config:qc_config
    ~f:(fun { S2_point_pair.a; b } ->
      let open Float_u.O in
      if not
           (S2.S2_point.equal a S2.R3_vector.zero || S2.S2_point.equal b S2.R3_vector.zero)
      then (
        let a = S2.R3_vector.normalize a in
        let axis = S2.R3_vector.normalize b in
        let full = S2.S1_angle.of_radians (#2.0 * Float_u.pi) in
        let rotated = S2.S2_point.rotate a ~axis ~angle:full in
        assert (
          S2.S2_point.approx_equal
            ~max_error:(Packed_float_option.Unboxed.some #1e-10)
            rotated
            a)))
;;
