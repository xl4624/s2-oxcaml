(* Quickcheck property tests for S2_loop. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

(* Generate loops from S2 cells: a fast, reliable way to produce a guaranteed
   valid (simple, normalized) non-degenerate loop from random input. *)
module Cell_loop = struct
  type t = { id : Int64.t }

  let sexp_of_t { id } = Sexp.Atom (Int64.to_string id)

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let rec descend depth id =
      let cell = cid_of_int64 id in
      if depth = 0 || S2.S2_cell_id.is_leaf cell
      then return id
      else
        bind (int_uniform_inclusive 0 3) ~f:(fun k ->
          descend (depth - 1) (int64_of_cid (S2.S2_cell_id.child_exn cell k)))
    in
    bind (int_uniform_inclusive 0 5) ~f:(fun f ->
      bind (int_uniform_inclusive 1 10) ~f:(fun depth ->
        map
          (descend depth (int64_of_cid (S2.S2_cell_id.from_face_exn f)))
          ~f:(fun id -> { id })))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

  let to_loop t =
    let cell = S2.S2_cell.of_cell_id (cid_of_int64 t.id) in
    S2.S2_loop.of_cell cell
  ;;
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 100; shrink_count = 25 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "cell_loop_has_four_vertices" =
  Base_quickcheck.Test.run_exn (module Cell_loop) ~config:qc_config ~f:(fun t ->
    let loop = Cell_loop.to_loop t in
    assert (S2.S2_loop.num_vertices loop = 4))
;;

let%test_unit "cell_loop_is_valid" =
  Base_quickcheck.Test.run_exn (module Cell_loop) ~config:qc_config ~f:(fun t ->
    let loop = Cell_loop.to_loop t in
    assert (S2.S2_loop.is_valid loop))
;;

let%test_unit "cell_loop_is_normalized" =
  Base_quickcheck.Test.run_exn (module Cell_loop) ~config:qc_config ~f:(fun t ->
    let loop = Cell_loop.to_loop t in
    assert (S2.S2_loop.is_normalized loop))
;;

let%test_unit "cell_loop_not_empty_or_full" =
  Base_quickcheck.Test.run_exn (module Cell_loop) ~config:qc_config ~f:(fun t ->
    let loop = Cell_loop.to_loop t in
    assert (not (S2.S2_loop.is_empty_or_full loop)))
;;

let%test_unit "invert_involution" =
  Base_quickcheck.Test.run_exn (module Cell_loop) ~config:qc_config ~f:(fun t ->
    let loop = Cell_loop.to_loop t in
    let ii = S2.S2_loop.invert (S2.S2_loop.invert loop) in
    assert (S2.S2_loop.equal ii loop))
;;

let%test_unit "invert_flips_normalization" =
  Base_quickcheck.Test.run_exn (module Cell_loop) ~config:qc_config ~f:(fun t ->
    let loop = Cell_loop.to_loop t in
    let inverted = S2.S2_loop.invert loop in
    assert (
      Bool.( <> ) (S2.S2_loop.is_normalized loop) (S2.S2_loop.is_normalized inverted)))
;;

let%test_unit "area_plus_curvature_equals_2pi" =
  (* area + curvature = 2*pi for any finite normalized loop. *)
  Base_quickcheck.Test.run_exn (module Cell_loop) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let loop = Cell_loop.to_loop t in
    let a = S2.S2_loop.area loop in
    let c = S2.S2_loop.curvature loop in
    let sum = a + c in
    let expected = #2.0 * Float_u.pi () in
    assert (Float_u.abs (sum - expected) <= #1e-10))
;;

let%test_unit "area_nonneg_and_bounded" =
  Base_quickcheck.Test.run_exn (module Cell_loop) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let loop = Cell_loop.to_loop t in
    let a = S2.S2_loop.area loop in
    let four_pi = #4.0 * Float_u.pi () in
    assert (a >= #0.0);
    assert (a <= four_pi + #1e-10))
;;

let%test_unit "invert_area_complement" =
  (* area loop + area (invert loop) = 4*pi. *)
  Base_quickcheck.Test.run_exn (module Cell_loop) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let loop = Cell_loop.to_loop t in
    let a = S2.S2_loop.area loop in
    let ainv = S2.S2_loop.area (S2.S2_loop.invert loop) in
    let four_pi = #4.0 * Float_u.pi () in
    assert (Float_u.abs (a + ainv - four_pi) <= #1e-10))
;;

let%test_unit "cell_loop_contains_center" =
  Base_quickcheck.Test.run_exn
    (module Cell_loop)
    ~config:qc_config
    ~f:(fun { Cell_loop.id } ->
      let cell = S2.S2_cell.of_cell_id (cid_of_int64 id) in
      let loop = S2.S2_loop.of_cell cell in
      let center = S2.S2_cell.center cell in
      assert (S2.S2_loop.contains_point loop center))
;;

let%test_unit "empty_full_special" =
  let e = S2.S2_loop.empty () in
  let f = S2.S2_loop.full () in
  assert (S2.S2_loop.is_empty e);
  assert (S2.S2_loop.is_full f);
  assert (S2.S2_loop.is_empty_or_full e);
  assert (S2.S2_loop.is_empty_or_full f);
  (* invert swaps them. *)
  assert (S2.S2_loop.is_full (S2.S2_loop.invert e));
  assert (S2.S2_loop.is_empty (S2.S2_loop.invert f))
;;
