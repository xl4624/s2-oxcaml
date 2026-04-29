(* Quickcheck property tests for S2_latlng_rect. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

module Rect_gen = struct
  type t = { rect : S2.S2_latlng_rect.t } [@@deriving sexp_of]

  let latlng_of_floats lat_r lng_r =
    S2.S2_latlng.of_radians ~lat:(Float_u.of_float lat_r) ~lng:(Float_u.of_float lng_r)
  ;;

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let lat_gen = float_inclusive (-.Float.pi /. 2.0) (Float.pi /. 2.0) in
    let lng_gen = float_inclusive (-.Float.pi) Float.pi in
    create (fun ~size:_ ~random:rnd ->
      let lat1 = generate lat_gen ~size:30 ~random:rnd in
      let lat2 = generate lat_gen ~size:30 ~random:rnd in
      let lng1 = generate lng_gen ~size:30 ~random:rnd in
      let lng2 = generate lng_gen ~size:30 ~random:rnd in
      let p1 = latlng_of_floats lat1 lng1 in
      let p2 = latlng_of_floats lat2 lng2 in
      { rect = S2.S2_latlng_rect.of_point_pair p1 p2 })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Rect_pair = struct
  type t =
    { a : S2.S2_latlng_rect.t
    ; b : S2.S2_latlng_rect.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    create (fun ~size ~random:rnd ->
      let { Rect_gen.rect = a } =
        generate Rect_gen.quickcheck_generator ~size ~random:rnd
      in
      let { Rect_gen.rect = b } =
        generate Rect_gen.quickcheck_generator ~size ~random:rnd
      in
      { a; b })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "contains_reflexive" =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } -> assert (S2.S2_latlng_rect.contains rect rect))
;;

let%test_unit "union_superset" =
  Base_quickcheck.Test.run_exn
    (module Rect_pair)
    ~config:qc_config
    ~f:(fun { Rect_pair.a; b } ->
      let u = S2.S2_latlng_rect.union a b in
      assert (S2.S2_latlng_rect.contains u a);
      assert (S2.S2_latlng_rect.contains u b))
;;

let%test_unit "intersection_subset" =
  Base_quickcheck.Test.run_exn
    (module Rect_pair)
    ~config:qc_config
    ~f:(fun { Rect_pair.a; b } ->
      let i = S2.S2_latlng_rect.intersection a b in
      if not (S2.S2_latlng_rect.is_empty i)
      then (
        assert (S2.S2_latlng_rect.contains a i);
        assert (S2.S2_latlng_rect.contains b i)))
;;

let%test_unit "area_nonneg" =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      let area = Float_u.to_float (S2.S2_latlng_rect.area rect) in
      assert (Float.(area >= 0.0)))
;;

let%test_unit "empty_full_area" =
  let empty_area = Float_u.to_float (S2.S2_latlng_rect.area S2.S2_latlng_rect.empty) in
  let full_area = Float_u.to_float (S2.S2_latlng_rect.area S2.S2_latlng_rect.full) in
  assert (Float.equal empty_area 0.0);
  let expected_full = 4.0 *. Float.pi in
  assert (Float.(abs (full_area - expected_full) < 1e-12))
;;

let%test_unit "intersection_commutative" =
  Base_quickcheck.Test.run_exn
    (module Rect_pair)
    ~config:qc_config
    ~f:(fun { Rect_pair.a; b } ->
      let ab = S2.S2_latlng_rect.intersection a b in
      let ba = S2.S2_latlng_rect.intersection b a in
      assert (S2.S2_latlng_rect.equal ab ba))
;;

let%test_unit "union_commutative" =
  Base_quickcheck.Test.run_exn
    (module Rect_pair)
    ~config:qc_config
    ~f:(fun { Rect_pair.a; b } ->
      let ab = S2.S2_latlng_rect.union a b in
      let ba = S2.S2_latlng_rect.union b a in
      assert (S2.S2_latlng_rect.equal ab ba))
;;

let%test_unit "expanded_zero_identity" =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      if not (S2.S2_latlng_rect.is_empty rect)
      then (
        let zero = S2.S2_latlng.of_radians ~lat:#0.0 ~lng:#0.0 in
        let e = S2.S2_latlng_rect.expanded rect zero in
        assert (
          S2.S2_latlng_rect.approx_equal
            ~max_error:(Packed_float_option.Unboxed.none)
            rect
            e)))
;;

let%test_unit "full_contains_any" =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      assert (S2.S2_latlng_rect.contains S2.S2_latlng_rect.full rect))
;;

let%test_unit "intersects_iff_nonempty_intersection" =
  Base_quickcheck.Test.run_exn
    (module Rect_pair)
    ~config:qc_config
    ~f:(fun { Rect_pair.a; b } ->
      let inter = S2.S2_latlng_rect.intersection a b in
      let intersects = S2.S2_latlng_rect.intersects a b in
      assert (Bool.equal intersects (not (S2.S2_latlng_rect.is_empty inter))))
;;

let%test_unit "union_is_idempotent" =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      let u = S2.S2_latlng_rect.union rect rect in
      assert (S2.S2_latlng_rect.equal u rect))
;;

let%test_unit "intersection_is_idempotent" =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      let i = S2.S2_latlng_rect.intersection rect rect in
      assert (S2.S2_latlng_rect.equal i rect))
;;
