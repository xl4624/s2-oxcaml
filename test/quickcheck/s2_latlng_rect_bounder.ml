(* Quickcheck property tests for S2_latlng_rect_bounder. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

module Point_list = struct
  type t = { latlngs : (float * float) list } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let lat_gen = float_inclusive (-.Float.pi /. 2.0) (Float.pi /. 2.0) in
    let lng_gen = float_inclusive (-.Float.pi) Float.pi in
    let latlng_gen = map2 lat_gen lng_gen ~f:(fun lat lng -> lat, lng) in
    let list_gen = list_with_length latlng_gen ~length:5 in
    map list_gen ~f:(fun latlngs -> { latlngs })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 10 }
;;

module Cell = struct
  type t = { mutable v : S2.S2_latlng_rect_bounder.t }
end

let bound_of_latlngs latlngs =
  let cell = { Cell.v = S2.S2_latlng_rect_bounder.create () } in
  List.iter latlngs ~f:(fun (lat, lng) ->
    let ll =
      S2.S2_latlng.of_radians ~lat:(Float_u.of_float lat) ~lng:(Float_u.of_float lng)
    in
    cell.v <- S2.S2_latlng_rect_bounder.add_latlng cell.v ll);
  S2.S2_latlng_rect_bounder.get_bound cell.v
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "bound_contains_points" =
  Base_quickcheck.Test.run_exn
    (module Point_list)
    ~config:qc_config
    ~f:(fun { Point_list.latlngs } ->
      let bound = bound_of_latlngs latlngs in
      List.iter latlngs ~f:(fun (lat, lng) ->
        let ll =
          S2.S2_latlng.of_radians ~lat:(Float_u.of_float lat) ~lng:(Float_u.of_float lng)
        in
        assert (S2.S2_latlng_rect.contains_latlng bound ll)))
;;

let%test_unit "expand_for_subregions_contains" =
  Base_quickcheck.Test.run_exn
    (module Point_list)
    ~config:qc_config
    ~f:(fun { Point_list.latlngs } ->
      let bound = bound_of_latlngs latlngs in
      let expanded = S2.S2_latlng_rect_bounder.expand_for_subregions bound in
      assert (S2.S2_latlng_rect.contains expanded bound))
;;

let%test_unit "empty_bounder" =
  let bounder = S2.S2_latlng_rect_bounder.create () in
  let bound = S2.S2_latlng_rect_bounder.get_bound bounder in
  assert (S2.S2_latlng_rect.is_empty bound)
;;
