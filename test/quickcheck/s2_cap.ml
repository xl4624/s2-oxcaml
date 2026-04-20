(* Quickcheck property tests for S2_cap. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

module Cap_gen = struct
  type t = { cap : S2.S2_cap.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let radius = float_inclusive (-0.1) 4.0 in
    create (fun ~size:_ ~random:rnd ->
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      let r = generate radius ~size:30 ~random:rnd in
      let center =
        S2.S2_point.of_coords
          ~x:(Float_u.of_float x)
          ~y:(Float_u.of_float y)
          ~z:(Float_u.of_float z)
      in
      let chord_angle = S2.S1_chord_angle.of_length2 (Float_u.of_float r) in
      { cap = S2.S2_cap.of_center_chord_angle center chord_angle })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Cap_pair = struct
  type t =
    { a : S2.S2_cap.t
    ; b : S2.S2_cap.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let radius = float_inclusive (-0.1) 4.0 in
    let gen_cap rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      let r = generate radius ~size:30 ~random:rnd in
      let center =
        S2.S2_point.of_coords
          ~x:(Float_u.of_float x)
          ~y:(Float_u.of_float y)
          ~z:(Float_u.of_float z)
      in
      let chord_angle = S2.S1_chord_angle.of_length2 (Float_u.of_float r) in
      S2.S2_cap.of_center_chord_angle center chord_angle
    in
    create (fun ~size:_ ~random:rnd -> { a = gen_cap rnd; b = gen_cap rnd })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "contains_center" =
  Base_quickcheck.Test.run_exn
    (module Cap_gen)
    ~config:qc_config
    ~f:(fun { Cap_gen.cap } ->
      if not (S2.S2_cap.is_empty cap)
      then assert (S2.S2_cap.contains_point cap (S2.S2_cap.center cap)))
;;

let%test_unit "complement_complement" =
  Base_quickcheck.Test.run_exn
    (module Cap_gen)
    ~config:qc_config
    ~f:(fun { Cap_gen.cap } ->
      let c = S2.S2_cap.complement cap in
      let cc = S2.S2_cap.complement c in
      assert (
        S2.S2_cap.approx_equal ~max_error:(Packed_float_option.Unboxed.some #1e-14) cap cc))
;;

let%test_unit "union_contains_both" =
  Base_quickcheck.Test.run_exn
    (module Cap_pair)
    ~config:qc_config
    ~f:(fun { Cap_pair.a; b } ->
      let u = S2.S2_cap.union a b in
      if not (S2.S2_cap.is_empty u)
      then (
        let margin = S2.S1_angle.of_radians #1e-14 in
        let u_expanded = S2.S2_cap.expanded_exn u margin in
        assert (S2.S2_cap.contains_cap u_expanded a);
        assert (S2.S2_cap.contains_cap u_expanded b)))
;;

let%test_unit "add_point_contains" =
  Base_quickcheck.Test.run_exn
    (module Cap_gen)
    ~config:qc_config
    ~f:(fun { Cap_gen.cap } ->
      let p = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
      let added = S2.S2_cap.add_point cap p in
      assert (S2.S2_cap.contains_point added p))
;;

let%test_unit "empty_subset_any" =
  Base_quickcheck.Test.run_exn
    (module Cap_gen)
    ~config:qc_config
    ~f:(fun { Cap_gen.cap } -> assert (S2.S2_cap.contains_cap cap S2.S2_cap.empty))
;;

let%test_unit "any_subset_full" =
  Base_quickcheck.Test.run_exn
    (module Cap_gen)
    ~config:qc_config
    ~f:(fun { Cap_gen.cap } -> assert (S2.S2_cap.contains_cap S2.S2_cap.full cap))
;;

let%test_unit "union_commutative" =
  Base_quickcheck.Test.run_exn
    (module Cap_pair)
    ~config:qc_config
    ~f:(fun { Cap_pair.a; b } ->
      let ab = S2.S2_cap.union a b in
      let ba = S2.S2_cap.union b a in
      assert (
        S2.S2_cap.approx_equal ~max_error:(Packed_float_option.Unboxed.some #1e-14) ab ba))
;;

let%test_unit "contains_self" =
  Base_quickcheck.Test.run_exn
    (module Cap_gen)
    ~config:qc_config
    ~f:(fun { Cap_gen.cap } -> assert (S2.S2_cap.contains_cap cap cap))
;;

let%test_unit "add_point_superset" =
  Base_quickcheck.Test.run_exn
    (module Cap_gen)
    ~config:qc_config
    ~f:(fun { Cap_gen.cap } ->
      let p = S2.S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0 in
      let added = S2.S2_cap.add_point cap p in
      assert (S2.S2_cap.contains_cap added cap))
;;

let%test_unit "intersects_iff_not_disjoint" =
  Base_quickcheck.Test.run_exn
    (module Cap_pair)
    ~config:qc_config
    ~f:(fun { Cap_pair.a; b } ->
      let i1 = S2.S2_cap.intersects a b in
      let i2 = S2.S2_cap.intersects b a in
      assert (Bool.equal i1 i2))
;;

let%test_unit "empty_area_zero" =
  let open Float_u.O in
  let a = S2.S2_cap.area S2.S2_cap.empty in
  assert (a <= #1e-14)
;;

let%test_unit "area_nonneg" =
  Base_quickcheck.Test.run_exn
    (module Cap_gen)
    ~config:qc_config
    ~f:(fun { Cap_gen.cap } ->
      let open Float_u.O in
      let a = S2.S2_cap.area cap in
      assert (a >= #0.0))
;;
