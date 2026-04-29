(* Quickcheck property tests for S2_padded_cell. *)
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
      bind (int_uniform_inclusive 0 20) ~f:(fun depth ->
        descend depth (int64_of_cid (S2.S2_cell_id.from_face_exn f))))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

(* --- Properties ----------------------------------------------------------- *)

let check_point_exact msg ~expected ~actual = check_r3_vector_exact msg ~expected ~actual

(* Quickcheck: the bound of a padded cell with padding 0 equals the (u, v)-bound of the
   corresponding S2Cell, and padding > 0 expands it symmetrically by the padding. *)
let%test_unit "bound_matches_s2cell" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cid = cid_of_int64 id in
    let cell = S2.S2_cell.of_cell_id cid in
    let expected = S2.S2_cell.bound_uv cell in
    let pc = S2.S2_padded_cell.create cid ~padding:#0.0 in
    let got = S2.S2_padded_cell.bound pc in
    assert (
      S2.R2_rect.approx_equal
        ~max_error:(Packed_float_option.Unboxed.none)
        expected
        got);
    let padding = #0.01 in
    let expanded = S2.R2_rect.expanded_scalar expected padding in
    let pc2 = S2.S2_padded_cell.create cid ~padding in
    let got2 = S2.S2_padded_cell.bound pc2 in
    assert (
      S2.R2_rect.approx_equal
        ~max_error:(Packed_float_option.Unboxed.none)
        expanded
        got2))
;;

(* Quickcheck: the padded cell center equals the S2Cell center. *)
let%test_unit "center_matches_s2cell" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cid = cid_of_int64 id in
    let cell = S2.S2_cell.of_cell_id cid in
    let pc = S2.S2_padded_cell.create cid ~padding:#0.0 in
    let expected = S2.S2_cell.center cell in
    let actual = S2.S2_padded_cell.center pc in
    check_point_exact "center_match" ~expected ~actual)
;;

(* Quickcheck: exit_vertex of one cell equals entry_vertex of its Hilbert successor. *)
let%test_unit "exit_equals_next_entry" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cid = cid_of_int64 id in
    let pc = S2.S2_padded_cell.create cid ~padding:#0.0 in
    let pc_next = S2.S2_padded_cell.create (S2.S2_cell_id.next_wrap cid) ~padding:#0.0 in
    check_point_exact
      "exit->next_entry"
      ~expected:(S2.S2_padded_cell.exit_vertex pc)
      ~actual:(S2.S2_padded_cell.entry_vertex pc_next))
;;

(* Quickcheck: child_ij_of_pos inverts child_ij (up to the orientation-dependent
   kPosToIJ / kIJtoPos tables). *)
let%test_unit "child_ij_roundtrip" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cid = cid_of_int64 id in
    if not (S2.S2_cell_id.is_leaf cid)
    then (
      let parent = S2.S2_padded_cell.create cid ~padding:#0.0 in
      for pos = 0 to 3 do
        let #(i, j) = S2.S2_padded_cell.child_ij_of_pos parent ~pos in
        let child = S2.S2_padded_cell.child_ij parent ~i ~j in
        let expected_id = S2.S2_cell_id.child_exn cid pos in
        assert (S2.S2_cell_id.equal (S2.S2_padded_cell.id child) expected_id)
      done))
;;
