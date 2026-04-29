(* Quickcheck property tests for S2_latlng. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

module Latlng_gen = struct
  type t =
    { lat : float
    ; lng : float
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let lat_gen = float_inclusive (-90.0) 90.0 in
    let lng_gen = float_inclusive (-180.0) 180.0 in
    map (both lat_gen lng_gen) ~f:(fun (lat, lng) -> { lat; lng })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

  let to_latlng t =
    S2.S2_latlng.of_degrees ~lat:(Float_u.of_float t.lat) ~lng:(Float_u.of_float t.lng)
  ;;
end

module Latlng_pair = struct
  type t =
    { a_lat : float
    ; a_lng : float
    ; b_lat : float
    ; b_lng : float
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let lat_gen = float_inclusive (-90.0) 90.0 in
    let lng_gen = float_inclusive (-180.0) 180.0 in
    create (fun ~size:_ ~random:rnd ->
      { a_lat = generate lat_gen ~size:30 ~random:rnd
      ; a_lng = generate lng_gen ~size:30 ~random:rnd
      ; b_lat = generate lat_gen ~size:30 ~random:rnd
      ; b_lng = generate lng_gen ~size:30 ~random:rnd
      })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Latlng_any = struct
  type t =
    { lat : float
    ; lng : float
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let gen = float_inclusive (-1000.0) 1000.0 in
    map (both gen gen) ~f:(fun (lat, lng) -> { lat; lng })
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
    (module Latlng_pair)
    ~config:qc_config
    ~f:(fun { Latlng_pair.a_lat; a_lng; b_lat; b_lng } ->
      let a =
        S2.S2_latlng.of_degrees
          ~lat:(Float_u.of_float a_lat)
          ~lng:(Float_u.of_float a_lng)
      in
      let b =
        S2.S2_latlng.of_degrees
          ~lat:(Float_u.of_float b_lat)
          ~lng:(Float_u.of_float b_lng)
      in
      let ab = S2.S2_latlng.add a b in
      let ba = S2.S2_latlng.add b a in
      assert (S2.S2_latlng.equal ab ba))
;;

let%test_unit "point_roundtrip" =
  Base_quickcheck.Test.run_exn (module Latlng_gen) ~config:qc_config ~f:(fun t ->
    let ll = Latlng_gen.to_latlng t in
    let p = S2.S2_latlng.to_point ll in
    let back = S2.S2_latlng.of_point p in
    assert (
      S2.S2_latlng.approx_equal
        ~max_error:(Packed_float_option.Unboxed.some #1e-14)
        ll
        back))
;;

let%test_unit "normalized_is_valid" =
  Base_quickcheck.Test.run_exn
    (module Latlng_any)
    ~config:qc_config
    ~f:(fun { Latlng_any.lat; lng } ->
      let ll =
        S2.S2_latlng.of_degrees ~lat:(Float_u.of_float lat) ~lng:(Float_u.of_float lng)
      in
      let n = S2.S2_latlng.normalized ll in
      assert (S2.S2_latlng.is_valid n))
;;

let%test_unit "normalized_idempotent" =
  Base_quickcheck.Test.run_exn
    (module Latlng_any)
    ~config:qc_config
    ~f:(fun { Latlng_any.lat; lng } ->
      let ll =
        S2.S2_latlng.of_degrees ~lat:(Float_u.of_float lat) ~lng:(Float_u.of_float lng)
      in
      let n = S2.S2_latlng.normalized ll in
      let nn = S2.S2_latlng.normalized n in
      assert (S2.S2_latlng.equal n nn))
;;

let%test_unit "distance_nonneg" =
  Base_quickcheck.Test.run_exn
    (module Latlng_pair)
    ~config:qc_config
    ~f:(fun { Latlng_pair.a_lat; a_lng; b_lat; b_lng } ->
      let a =
        S2.S2_latlng.of_degrees
          ~lat:(Float_u.of_float a_lat)
          ~lng:(Float_u.of_float a_lng)
      in
      let b =
        S2.S2_latlng.of_degrees
          ~lat:(Float_u.of_float b_lat)
          ~lng:(Float_u.of_float b_lng)
      in
      let open Float_u.O in
      let d = S2.S1_angle.radians (S2.S2_latlng.distance a b) in
      assert (d >= #0.0))
;;

let%test_unit "distance_symmetric" =
  Base_quickcheck.Test.run_exn
    (module Latlng_pair)
    ~config:qc_config
    ~f:(fun { Latlng_pair.a_lat; a_lng; b_lat; b_lng } ->
      let a =
        S2.S2_latlng.of_degrees
          ~lat:(Float_u.of_float a_lat)
          ~lng:(Float_u.of_float a_lng)
      in
      let b =
        S2.S2_latlng.of_degrees
          ~lat:(Float_u.of_float b_lat)
          ~lng:(Float_u.of_float b_lng)
      in
      let open Float_u.O in
      let d1 = S2.S1_angle.radians (S2.S2_latlng.distance a b) in
      let d2 = S2.S1_angle.radians (S2.S2_latlng.distance b a) in
      assert (Float_u.abs (d1 - d2) <= #1e-15))
;;

let%test_unit "distance_self_zero" =
  Base_quickcheck.Test.run_exn (module Latlng_gen) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let a = Latlng_gen.to_latlng t in
    let d = S2.S1_angle.radians (S2.S2_latlng.distance a a) in
    assert (Float_u.abs d <= #1e-14))
;;

let%test_unit "distance_triangle_inequality" =
  Base_quickcheck.Test.run_exn
    (module Latlng_pair)
    ~config:qc_config
    ~f:(fun { Latlng_pair.a_lat; a_lng; b_lat; b_lng } ->
      let open Float_u.O in
      let a =
        S2.S2_latlng.of_degrees
          ~lat:(Float_u.of_float a_lat)
          ~lng:(Float_u.of_float a_lng)
      in
      let b =
        S2.S2_latlng.of_degrees
          ~lat:(Float_u.of_float b_lat)
          ~lng:(Float_u.of_float b_lng)
      in
      let c = S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#0.0 in
      let dab = S2.S1_angle.radians (S2.S2_latlng.distance a b) in
      let dbc = S2.S1_angle.radians (S2.S2_latlng.distance b c) in
      let dac = S2.S1_angle.radians (S2.S2_latlng.distance a c) in
      assert (dac <= dab + dbc + #1e-12))
;;

let%test_unit "sub_self_zero" =
  Base_quickcheck.Test.run_exn (module Latlng_gen) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let a = Latlng_gen.to_latlng t in
    let d = S2.S2_latlng.sub a a in
    assert (Float_u.abs (S2.S1_angle.radians (S2.S2_latlng.lat d)) <= #1e-14);
    assert (Float_u.abs (S2.S1_angle.radians (S2.S2_latlng.lng d)) <= #1e-14))
;;
