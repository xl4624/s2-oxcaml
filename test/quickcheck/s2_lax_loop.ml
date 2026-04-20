(* Quickcheck property tests for S2_lax_loop. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

module Cell_loop_vertices = struct
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

  let to_vertices t =
    let cell = S2.S2_cell.of_cell_id (cid_of_int64 t.id) in
    let n = 4 in
    let arr = Array.create ~len:n (S2.S2_cell.vertex cell 0) in
    for i = 1 to n - 1 do
      arr.(i) <- S2.S2_cell.vertex cell i
    done;
    arr
  ;;
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 100; shrink_count = 25 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "num_vertices_matches_input" =
  Base_quickcheck.Test.run_exn (module Cell_loop_vertices) ~config:qc_config ~f:(fun t ->
    let vs = Cell_loop_vertices.to_vertices t in
    let loop = S2.S2_lax_loop.of_vertices vs in
    assert (S2.S2_lax_loop.num_vertices loop = Array.length vs))
;;

let%test_unit "num_edges_equals_num_vertices" =
  Base_quickcheck.Test.run_exn (module Cell_loop_vertices) ~config:qc_config ~f:(fun t ->
    let vs = Cell_loop_vertices.to_vertices t in
    let loop = S2.S2_lax_loop.of_vertices vs in
    assert (S2.S2_lax_loop.num_edges loop = S2.S2_lax_loop.num_vertices loop))
;;

let%test_unit "vertices_preserved" =
  Base_quickcheck.Test.run_exn (module Cell_loop_vertices) ~config:qc_config ~f:(fun t ->
    let vs = Cell_loop_vertices.to_vertices t in
    let loop = S2.S2_lax_loop.of_vertices vs in
    for i = 0 to Array.length vs - 1 do
      assert (S2.S2_point.equal (S2.S2_lax_loop.vertex loop i) vs.(i))
    done)
;;

let%test_unit "num_chains_is_one" =
  Base_quickcheck.Test.run_exn (module Cell_loop_vertices) ~config:qc_config ~f:(fun t ->
    let vs = Cell_loop_vertices.to_vertices t in
    let loop = S2.S2_lax_loop.of_vertices vs in
    assert (S2.S2_lax_loop.num_chains loop = 1))
;;

let%test_unit "dimension_is_two" =
  Base_quickcheck.Test.run_exn (module Cell_loop_vertices) ~config:qc_config ~f:(fun t ->
    let vs = Cell_loop_vertices.to_vertices t in
    let loop = S2.S2_lax_loop.of_vertices vs in
    assert (S2.S2_lax_loop.dimension loop = 2))
;;
