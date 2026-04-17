(* Quickcheck property tests for S1_interval. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

module S1_pair = struct
  type t =
    { a : S2.S1_interval.t
    ; b : S2.S1_interval.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let angle = float_inclusive (-.Float.pi) Float.pi in
    create (fun ~size:_ ~random:rnd ->
      let p1 = Float_u.of_float (generate angle ~size:30 ~random:rnd) in
      let p2 = Float_u.of_float (generate angle ~size:30 ~random:rnd) in
      let q1 = Float_u.of_float (generate angle ~size:30 ~random:rnd) in
      let q2 = Float_u.of_float (generate angle ~size:30 ~random:rnd) in
      { a = S2.S1_interval.from_point_pair p1 p2
      ; b = S2.S1_interval.from_point_pair q1 q2
      })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module S1_interval_gen = struct
  type t = { interval : S2.S1_interval.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let angle = float_inclusive (-.Float.pi) Float.pi in
    create (fun ~size:_ ~random:rnd ->
      let p1 = Float_u.of_float (generate angle ~size:30 ~random:rnd) in
      let p2 = Float_u.of_float (generate angle ~size:30 ~random:rnd) in
      { interval = S2.S1_interval.from_point_pair p1 p2 })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "intersection_subset" =
  Base_quickcheck.Test.run_exn
    (module S1_pair)
    ~config:qc_config
    ~f:(fun { S1_pair.a; b } ->
      let c = S2.S1_interval.intersection a b in
      if S2.S1_interval.is_empty c
      then ()
      else (
        assert (S2.S1_interval.contains_interval a c);
        assert (S2.S1_interval.contains_interval b c)))
;;

let%test_unit "union_superset" =
  Base_quickcheck.Test.run_exn
    (module S1_pair)
    ~config:qc_config
    ~f:(fun { S1_pair.a; b } ->
      let u = S2.S1_interval.union a b in
      assert (S2.S1_interval.contains_interval u a);
      assert (S2.S1_interval.contains_interval u b))
;;

let%test_unit "add_point_contains" =
  Base_quickcheck.Test.run_exn
    (module S1_interval_gen)
    ~config:qc_config
    ~f:(fun { S1_interval_gen.interval } ->
      let p = #1.23 in
      let added = S2.S1_interval.add_point interval p in
      assert (S2.S1_interval.contains added p))
;;

let%test_unit "expanded_contains" =
  Base_quickcheck.Test.run_exn
    (module S1_interval_gen)
    ~config:qc_config
    ~f:(fun { S1_interval_gen.interval } ->
      if not (S2.S1_interval.is_empty interval)
      then (
        let margin = #0.5 in
        let expanded = S2.S1_interval.expanded interval margin in
        assert (S2.S1_interval.contains_interval expanded interval)))
;;

let%test_unit "length_nonneg" =
  Base_quickcheck.Test.run_exn
    (module S1_interval_gen)
    ~config:qc_config
    ~f:(fun { S1_interval_gen.interval } ->
      assert (Float_u.O.(S2.S1_interval.length interval >= #0.0)))
;;

let%test_unit "equal_reflexive" =
  Base_quickcheck.Test.run_exn
    (module S1_interval_gen)
    ~config:qc_config
    ~f:(fun { S1_interval_gen.interval } ->
      assert (S2.S1_interval.equal interval interval))
;;

let%test_unit "project_in_range" =
  Base_quickcheck.Test.run_exn
    (module S1_interval_gen)
    ~config:qc_config
    ~f:(fun { S1_interval_gen.interval } ->
      if not (S2.S1_interval.is_empty interval)
      then (
        let p = #2.34 in
        let proj = S2.S1_interval.project interval p in
        assert (S2.S1_interval.contains interval proj)))
;;
