(* Quickcheck property tests for Earth. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

module Length_gen = struct
  type t = { meters : float } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    map (float_inclusive (-5e7) 5e7) ~f:(fun meters -> { meters })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Steradians_gen = struct
  type t = { s : float } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    map (float_inclusive 0.0 (4.0 *. Float.pi)) ~f:(fun s -> { s })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Latlng_pair_gen = struct
  type t =
    { a_lat : float
    ; a_lng : float
    ; b_lat : float
    ; b_lng : float
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let lat_gen = float_inclusive (-89.0) 89.0 in
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

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "length_angle_roundtrip" =
  Base_quickcheck.Test.run_exn
    (module Length_gen)
    ~config:qc_config
    ~f:(fun { Length_gen.meters } ->
      let meters_u = Float_u.of_float meters in
      let angle = S2.Earth.angle_from_length meters_u in
      let back = S2.Earth.length_from_angle angle in
      let diff = Float_u.to_float (Float_u.abs (Float_u.sub back meters_u)) in
      let scale = Float.max 1.0 (Float.abs meters) in
      assert (Float.( <= ) diff (1e-9 *. scale)))
;;

let%test_unit "area_steradian_roundtrip" =
  Base_quickcheck.Test.run_exn
    (module Steradians_gen)
    ~config:qc_config
    ~f:(fun { Steradians_gen.s } ->
      let s_u = Float_u.of_float s in
      let area = S2.Earth.area_from_steradians s_u in
      let back = S2.Earth.steradians_from_area area in
      let diff = Float_u.to_float (Float_u.abs (Float_u.sub back s_u)) in
      assert (Float.(diff <= 1e-14 +. (1e-15 *. s))))
;;

let%test_unit "length_from_latlngs_symmetric" =
  Base_quickcheck.Test.run_exn
    (module Latlng_pair_gen)
    ~config:qc_config
    ~f:(fun { Latlng_pair_gen.a_lat; a_lng; b_lat; b_lng } ->
      let a_ll =
        S2.S2_latlng.of_degrees
          ~lat:(Float_u.of_float a_lat)
          ~lng:(Float_u.of_float a_lng)
      in
      let b_ll =
        S2.S2_latlng.of_degrees
          ~lat:(Float_u.of_float b_lat)
          ~lng:(Float_u.of_float b_lng)
      in
      let ab = S2.Earth.length_from_latlngs a_ll b_ll in
      let ba = S2.Earth.length_from_latlngs b_ll a_ll in
      let diff = Float_u.to_float (Float_u.abs (Float_u.sub ab ba)) in
      assert (Float.(diff <= 1e-6)))
;;

let%test_unit "length_from_angle_nonneg_for_nonneg_angle" =
  Base_quickcheck.Test.run_exn
    (module Length_gen)
    ~config:qc_config
    ~f:(fun { Length_gen.meters } ->
      let m = Float.abs meters in
      let m_u = Float_u.of_float m in
      let ang = S2.Earth.angle_from_length m_u in
      let back = S2.Earth.length_from_angle ang in
      let back_f = Float_u.to_float back in
      assert (Float.(back_f >= -1e-6)))
;;

let%test_unit "points_vs_latlngs" =
  Base_quickcheck.Test.run_exn
    (module Latlng_pair_gen)
    ~config:qc_config
    ~f:(fun { Latlng_pair_gen.a_lat; a_lng; b_lat; b_lng } ->
      let a_ll =
        S2.S2_latlng.of_degrees
          ~lat:(Float_u.of_float a_lat)
          ~lng:(Float_u.of_float a_lng)
      in
      let b_ll =
        S2.S2_latlng.of_degrees
          ~lat:(Float_u.of_float b_lat)
          ~lng:(Float_u.of_float b_lng)
      in
      let a_pt = S2.S2_latlng.to_point a_ll in
      let b_pt = S2.S2_latlng.to_point b_ll in
      let by_points = S2.Earth.length_from_points a_pt b_pt in
      let by_latlngs = S2.Earth.length_from_latlngs a_ll b_ll in
      let diff = Float_u.to_float (Float_u.abs (Float_u.sub by_points by_latlngs)) in
      (* Allow a few millimeters of slack: the two formulations use different
         numerical paths (chord-based vs. haversine). *)
      assert (Float.(diff <= 1e-2)))
;;
