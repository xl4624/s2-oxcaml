(* Quickcheck property tests for S2_cell_id. *)
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

module Leaf_cell_id_int = struct
  type t = Int64.t [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let rec descend depth id =
      let cell = cid_of_int64 id in
      if S2.S2_cell_id.is_leaf cell
      then return id
      else if depth = 0
      then return (int64_of_cid (S2.S2_cell_id.child_exn cell 0))
      else
        bind (int_uniform_inclusive 0 3) ~f:(fun k ->
          descend (depth - 1) (int64_of_cid (S2.S2_cell_id.child_exn cell k)))
    in
    bind (int_uniform_inclusive 0 5) ~f:(fun f ->
      descend 30 (int64_of_cid (S2.S2_cell_id.from_face_exn f)))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "inverses" =
  Base_quickcheck.Test.run_exn
    (module Leaf_cell_id_int)
    ~config:{ qc_config with test_count = 1000 }
    ~f:(fun id ->
      let cell = cid_of_int64 id in
      assert (S2.S2_cell_id.is_leaf cell);
      assert (S2.S2_cell_id.level cell = S2.S2_cell_id.max_level);
      let center = S2.S2_latlng.of_point (S2.S2_cell_id.to_point cell) in
      let roundtripped = S2.S2_cell_id.from_latlng center in
      assert (S2.S2_cell_id.equal cell roundtripped))
;;

let%test_unit "token_roundtrip" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let t = cid_of_int64 id in
    assert (S2.S2_cell_id.equal (S2.S2_cell_id.from_token (S2.S2_cell_id.to_token t)) t))
;;

let%test_unit "contains_immediate_children" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let t = cid_of_int64 id in
    if S2.S2_cell_id.is_leaf t
    then ()
    else
      for k = 0 to 3 do
        assert (S2.S2_cell_id.contains t (S2.S2_cell_id.child_exn t k))
      done)
;;

let%test_unit "parent_of_child" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let t = cid_of_int64 id in
    if S2.S2_cell_id.is_leaf t
    then ()
    else
      for k = 0 to 3 do
        assert (
          S2.S2_cell_id.equal (S2.S2_cell_id.parent_exn (S2.S2_cell_id.child_exn t k)) t)
      done)
;;
