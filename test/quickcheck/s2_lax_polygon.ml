(* Quickcheck property tests for S2_lax_polygon. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

module Cell_polygon = struct
  type t =
    { id1 : Int64.t
    ; id2 : Int64.t
    }
  [@@deriving sexp_of]

  let cell_id_gen =
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
      bind (int_uniform_inclusive 2 10) ~f:(fun depth ->
        descend depth (int64_of_cid (S2.S2_cell_id.from_face_exn f))))
  ;;

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    bind cell_id_gen ~f:(fun id1 -> map cell_id_gen ~f:(fun id2 -> { id1; id2 }))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

  let cell_to_loop_vertices cell =
    let n = 4 in
    let arr = Array.create ~len:n (S2.S2_cell.vertex cell 0) in
    for i = 1 to n - 1 do
      arr.(i) <- S2.S2_cell.vertex cell i
    done;
    arr
  ;;

  let to_loops t =
    let cell1 = S2.S2_cell.of_cell_id (cid_of_int64 t.id1) in
    let cell2 = S2.S2_cell.of_cell_id (cid_of_int64 t.id2) in
    [| cell_to_loop_vertices cell1; cell_to_loop_vertices cell2 |]
  ;;
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 100; shrink_count = 25 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "num_loops_matches_input" =
  Base_quickcheck.Test.run_exn (module Cell_polygon) ~config:qc_config ~f:(fun t ->
    let loops = Cell_polygon.to_loops t in
    let poly = S2.S2_lax_polygon.of_loops loops in
    assert (S2.S2_lax_polygon.num_loops poly = Array.length loops))
;;

let%test_unit "num_vertices_is_sum_over_loops" =
  Base_quickcheck.Test.run_exn (module Cell_polygon) ~config:qc_config ~f:(fun t ->
    let loops = Cell_polygon.to_loops t in
    let poly = S2.S2_lax_polygon.of_loops loops in
    let expected = Array.fold loops ~init:0 ~f:(fun acc l -> acc + Array.length l) in
    assert (S2.S2_lax_polygon.num_vertices poly = expected))
;;

let%test_unit "num_edges_equals_num_vertices" =
  Base_quickcheck.Test.run_exn (module Cell_polygon) ~config:qc_config ~f:(fun t ->
    let loops = Cell_polygon.to_loops t in
    let poly = S2.S2_lax_polygon.of_loops loops in
    assert (S2.S2_lax_polygon.num_edges poly = S2.S2_lax_polygon.num_vertices poly))
;;

let%test_unit "dimension_is_two" =
  Base_quickcheck.Test.run_exn (module Cell_polygon) ~config:qc_config ~f:(fun t ->
    let loops = Cell_polygon.to_loops t in
    let poly = S2.S2_lax_polygon.of_loops loops in
    assert (S2.S2_lax_polygon.dimension poly = 2))
;;

let%test_unit "num_chains_equals_num_loops" =
  Base_quickcheck.Test.run_exn (module Cell_polygon) ~config:qc_config ~f:(fun t ->
    let loops = Cell_polygon.to_loops t in
    let poly = S2.S2_lax_polygon.of_loops loops in
    assert (S2.S2_lax_polygon.num_chains poly = S2.S2_lax_polygon.num_loops poly))
;;

let%test_unit "loop_vertex_matches_input" =
  Base_quickcheck.Test.run_exn (module Cell_polygon) ~config:qc_config ~f:(fun t ->
    let loops = Cell_polygon.to_loops t in
    let poly = S2.S2_lax_polygon.of_loops loops in
    for i = 0 to Array.length loops - 1 do
      let li = loops.(i) in
      for j = 0 to Array.length li - 1 do
        assert (S2.S2_point.equal (S2.S2_lax_polygon.loop_vertex poly i j) li.(j))
      done
    done)
;;

let%test_unit "num_loop_vertices_matches_input" =
  Base_quickcheck.Test.run_exn (module Cell_polygon) ~config:qc_config ~f:(fun t ->
    let loops = Cell_polygon.to_loops t in
    let poly = S2.S2_lax_polygon.of_loops loops in
    for i = 0 to Array.length loops - 1 do
      assert (S2.S2_lax_polygon.num_loop_vertices poly i = Array.length loops.(i))
    done)
;;
