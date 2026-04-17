(* Quickcheck property tests for S2_cell_union. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

module Cell_union_sample = struct
  type t = { ids : Int64.t array } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let rec descend depth id =
      let cell = cid_of_int64 id in
      if depth = 0
      then return id
      else
        bind (int_uniform_inclusive 0 3) ~f:(fun k ->
          descend (depth - 1) (int64_of_cid (S2.S2_cell_id.child_exn cell k)))
    in
    let id_gen =
      bind (int_uniform_inclusive 0 5) ~f:(fun f ->
        bind (int_uniform_inclusive 0 10) ~f:(fun depth ->
          descend depth (int64_of_cid (S2.S2_cell_id.from_face_exn f))))
    in
    bind (int_uniform_inclusive 0 4) ~f:(fun n ->
      map (list_with_length ~length:n id_gen) ~f:(fun ids -> { ids = Array.of_list ids }))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Cell_union_pair = struct
  type t =
    { a : Cell_union_sample.t
    ; b : Cell_union_sample.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    bind Cell_union_sample.quickcheck_generator ~f:(fun a ->
      map Cell_union_sample.quickcheck_generator ~f:(fun b -> { a; b }))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

let cu_of_sample (s : Cell_union_sample.t) =
  let n = Array.length s.ids in
  let cids = Array.create ~len:n S2.S2_cell_id.none in
  for i = 0 to n - 1 do
    cids.(i) <- cid_of_int64 s.ids.(i)
  done;
  S2.S2_cell_union.create cids
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "normalize_idempotent" =
  Base_quickcheck.Test.run_exn (module Cell_union_sample) ~config:qc_config ~f:(fun s ->
    let u = cu_of_sample s in
    let n1 = S2.S2_cell_union.normalize u in
    let n2 = S2.S2_cell_union.normalize n1 in
    assert (S2.S2_cell_union.equal n1 n2))
;;

let%test_unit "union_is_superset" =
  Base_quickcheck.Test.run_exn
    (module Cell_union_pair)
    ~config:qc_config
    ~f:(fun { Cell_union_pair.a; b } ->
      let u = cu_of_sample a in
      let v = cu_of_sample b in
      let w = S2.S2_cell_union.union u v in
      assert (S2.S2_cell_union.contains_union w u);
      assert (S2.S2_cell_union.contains_union w v))
;;

let%test_unit "union_self" =
  Base_quickcheck.Test.run_exn (module Cell_union_sample) ~config:qc_config ~f:(fun s ->
    let u = cu_of_sample s in
    let w = S2.S2_cell_union.union u u in
    assert (S2.S2_cell_union.equal w u))
;;

let%test_unit "intersection_self" =
  Base_quickcheck.Test.run_exn (module Cell_union_sample) ~config:qc_config ~f:(fun s ->
    let u = cu_of_sample s in
    let w = S2.S2_cell_union.intersection u u in
    assert (S2.S2_cell_union.equal w u))
;;

let%test_unit "difference_self" =
  Base_quickcheck.Test.run_exn (module Cell_union_sample) ~config:qc_config ~f:(fun s ->
    let u = cu_of_sample s in
    let d = S2.S2_cell_union.difference u u in
    assert (S2.S2_cell_union.is_empty d))
;;

let%test_unit "union_contains_intersection" =
  Base_quickcheck.Test.run_exn
    (module Cell_union_pair)
    ~config:qc_config
    ~f:(fun { Cell_union_pair.a; b } ->
      let u = cu_of_sample a in
      let v = cu_of_sample b in
      let union = S2.S2_cell_union.union u v in
      let inter = S2.S2_cell_union.intersection u v in
      assert (S2.S2_cell_union.contains_union union inter))
;;

let%test_unit "intersects_iff_nonempty_intersection" =
  Base_quickcheck.Test.run_exn
    (module Cell_union_pair)
    ~config:qc_config
    ~f:(fun { Cell_union_pair.a; b } ->
      let u = cu_of_sample a in
      let v = cu_of_sample b in
      let inter = S2.S2_cell_union.intersection u v in
      let intersects = S2.S2_cell_union.intersects_union u v in
      assert (Bool.equal intersects (not (S2.S2_cell_union.is_empty inter))))
;;
