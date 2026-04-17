(* Quickcheck property tests for S2_cell. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

module Cell_id_int = struct
  type t = Int64.t [@@deriving sexp_of]

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
      bind (int_uniform_inclusive 0 24) ~f:(fun depth ->
        descend depth (int64_of_cid (S2.S2_cell_id.from_face_exn f))))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "subdivide_matches_cell_id_hierarchy" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cell_id = cid_of_int64 id in
    let cell = S2.S2_cell.of_cell_id cell_id in
    if S2.S2_cell.is_leaf cell
    then ()
    else
      for k = 0 to 3 do
        let child = S2.S2_cell.child cell ~pos:k in
        assert (
          S2.S2_cell_id.equal (S2.S2_cell.id child) (S2.S2_cell_id.child_exn cell_id k));
        assert (S2.S2_cell.contains_cell cell child);
        assert (S2.S2_cell.intersects_cell cell child)
      done)
;;

let%test_unit "consistent_with_cell_id_from_point" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cell = S2.S2_cell.of_cell_id (cid_of_int64 id) in
    for k = 0 to 3 do
      let v = S2.S2_cell.vertex cell k in
      let cell_from_v = S2.S2_cell.of_point v in
      assert (S2.S2_cell.contains_point cell_from_v v)
    done)
;;

let%test_unit "center_and_vertices_contained" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cell = S2.S2_cell.of_cell_id (cid_of_int64 id) in
    assert (S2.S2_cell.contains_point cell (S2.S2_cell.center_raw cell));
    for k = 0 to 3 do
      assert (S2.S2_cell.contains_point cell (S2.S2_cell.vertex_raw cell k))
    done)
;;
