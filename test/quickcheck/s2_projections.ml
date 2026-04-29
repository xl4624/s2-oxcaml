(* Quickcheck property tests for S2_projections. *)
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
    let lat_gen = float_inclusive (-85.0) 85.0 in
    let lng_gen = float_inclusive (-179.0) 179.0 in
    map (both lat_gen lng_gen) ~f:(fun (lat, lng) -> { lat; lng })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

  let to_latlng t =
    S2.S2_latlng.of_degrees ~lat:(Float_u.of_float t.lat) ~lng:(Float_u.of_float t.lng)
  ;;
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

let plate_carree = S2.S2_projections.plate_carree ~x_scale:Float_u.pi
let mercator = S2.S2_projections.mercator ~max_x:Float_u.pi

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "plate_carree_project_unproject_roundtrip" =
  Base_quickcheck.Test.run_exn (module Latlng_gen) ~config:qc_config ~f:(fun t ->
    let ll = Latlng_gen.to_latlng t in
    let p = S2.S2_latlng.to_point ll in
    let xy = S2.S2_projections.project plate_carree p in
    let back = S2.S2_projections.unproject plate_carree xy in
    assert (
      S2.S2_point.approx_equal ~max_error:(Packed_float_option.Unboxed.some #1e-12) p back))
;;

let%test_unit "mercator_project_unproject_roundtrip" =
  Base_quickcheck.Test.run_exn (module Latlng_gen) ~config:qc_config ~f:(fun t ->
    let ll = Latlng_gen.to_latlng t in
    let p = S2.S2_latlng.to_point ll in
    let xy = S2.S2_projections.project mercator p in
    let back = S2.S2_projections.unproject mercator xy in
    assert (
      S2.S2_point.approx_equal ~max_error:(Packed_float_option.Unboxed.some #1e-12) p back))
;;

let%test_unit "plate_carree_from_latlng_matches_project" =
  Base_quickcheck.Test.run_exn (module Latlng_gen) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let ll = Latlng_gen.to_latlng t in
    let p = S2.S2_latlng.to_point ll in
    let via_project = S2.S2_projections.project plate_carree p in
    let via_latlng = S2.S2_projections.from_latlng plate_carree ll in
    let dx = Float_u.abs (S2.R2_point.x via_project - S2.R2_point.x via_latlng) in
    let dy = Float_u.abs (S2.R2_point.y via_project - S2.R2_point.y via_latlng) in
    assert (dx <= #1e-12);
    assert (dy <= #1e-12))
;;

let%test_unit "mercator_from_latlng_matches_project" =
  Base_quickcheck.Test.run_exn (module Latlng_gen) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let ll = Latlng_gen.to_latlng t in
    let p = S2.S2_latlng.to_point ll in
    let via_project = S2.S2_projections.project mercator p in
    let via_latlng = S2.S2_projections.from_latlng mercator ll in
    let dx = Float_u.abs (S2.R2_point.x via_project - S2.R2_point.x via_latlng) in
    let dy = Float_u.abs (S2.R2_point.y via_project - S2.R2_point.y via_latlng) in
    assert (dx <= #1e-12);
    assert (dy <= #1e-12))
;;

let%test_unit "plate_carree_to_latlng_matches_unproject" =
  Base_quickcheck.Test.run_exn (module Latlng_gen) ~config:qc_config ~f:(fun t ->
    let ll = Latlng_gen.to_latlng t in
    let p = S2.S2_latlng.to_point ll in
    let xy = S2.S2_projections.project plate_carree p in
    let via_latlng = S2.S2_projections.to_latlng plate_carree xy in
    let via_point = S2.S2_latlng.of_point (S2.S2_projections.unproject plate_carree xy) in
    assert (
      S2.S2_latlng.approx_equal
        ~max_error:(Packed_float_option.Unboxed.some #1e-12)
        via_latlng
        via_point))
;;

let%test_unit "interpolate_zero_is_a" =
  Base_quickcheck.Test.run_exn (module Latlng_gen) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let ll = Latlng_gen.to_latlng t in
    let a = S2.S2_projections.from_latlng plate_carree ll in
    let b =
      S2.S2_projections.from_latlng
        plate_carree
        (S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#0.0)
    in
    let p = S2.S2_projections.interpolate plate_carree ~f:#0.0 a b in
    let dx = Float_u.abs (S2.R2_point.x p - S2.R2_point.x a) in
    let dy = Float_u.abs (S2.R2_point.y p - S2.R2_point.y a) in
    assert (dx <= #1e-12);
    assert (dy <= #1e-12))
;;

let%test_unit "interpolate_one_is_b" =
  Base_quickcheck.Test.run_exn (module Latlng_gen) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let ll = Latlng_gen.to_latlng t in
    let a = S2.S2_projections.from_latlng plate_carree ll in
    let b =
      S2.S2_projections.from_latlng
        plate_carree
        (S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#0.0)
    in
    let p = S2.S2_projections.interpolate plate_carree ~f:#1.0 a b in
    let dx = Float_u.abs (S2.R2_point.x p - S2.R2_point.x b) in
    let dy = Float_u.abs (S2.R2_point.y p - S2.R2_point.y b) in
    assert (dx <= #1e-12);
    assert (dy <= #1e-12))
;;

let%test_unit "kind_matches_constructor" =
  let pc = S2.S2_projections.plate_carree ~x_scale:#180.0 in
  let m = S2.S2_projections.mercator ~max_x:#180.0 in
  (match S2.S2_projections.kind pc with
   | Plate_carree -> ()
   | Mercator -> assert false);
  match S2.S2_projections.kind m with
  | Mercator -> ()
  | Plate_carree -> assert false
;;
