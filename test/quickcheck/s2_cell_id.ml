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

let%test_unit "next_prev_inverse" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let t = cid_of_int64 id in
    let nxt = S2.S2_cell_id.next t in
    if S2.S2_cell_id.is_valid nxt
    then (
      let back = S2.S2_cell_id.prev nxt in
      assert (S2.S2_cell_id.is_valid back);
      assert (S2.S2_cell_id.equal back t)))
;;

let%test_unit "parent_level_decreases" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let t = cid_of_int64 id in
    if S2.S2_cell_id.is_face t
    then ()
    else (
      let p = S2.S2_cell_id.parent_exn t in
      assert (S2.S2_cell_id.level p = S2.S2_cell_id.level t - 1);
      assert (S2.S2_cell_id.contains p t)))
;;

let%test_unit "child_level_increases" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let t = cid_of_int64 id in
    if S2.S2_cell_id.is_leaf t
    then ()
    else
      for k = 0 to 3 do
        let c = S2.S2_cell_id.child_exn t k in
        assert (S2.S2_cell_id.level c = S2.S2_cell_id.level t + 1)
      done)
;;

let%test_unit "range_min_max_contained" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let t = cid_of_int64 id in
    let lo = S2.S2_cell_id.range_min t in
    let hi = S2.S2_cell_id.range_max t in
    assert (S2.S2_cell_id.contains t lo);
    assert (S2.S2_cell_id.contains t hi))
;;

let%test_unit "next_wrap_prev_wrap_inverse" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let t = cid_of_int64 id in
    let back = S2.S2_cell_id.prev_wrap (S2.S2_cell_id.next_wrap t) in
    assert (S2.S2_cell_id.equal back t))
;;

let%test_unit "common_ancestor_with_own_child" =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let a = cid_of_int64 id in
    if S2.S2_cell_id.is_leaf a
    then ()
    else (
      let b = S2.S2_cell_id.child_exn a 0 in
      let lvl = S2.S2_cell_id.get_common_ancestor_level a b in
      assert (lvl = S2.S2_cell_id.level a)))
;;
