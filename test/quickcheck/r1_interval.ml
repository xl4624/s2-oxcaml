(* Quickcheck property tests for R1_interval. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

module R1_pair = struct
  type t =
    { a : S2.R1_interval.t
    ; b : S2.R1_interval.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let finite = filter float ~f:Float.is_finite in
    let gen_unboxed_interval rnd =
      let lo = generate finite ~size:30 ~random:rnd in
      let hi = generate finite ~size:30 ~random:rnd in
      S2.R1_interval.create ~lo:(Float_u.of_float lo) ~hi:(Float_u.of_float hi)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = gen_unboxed_interval rnd in
      let b = gen_unboxed_interval rnd in
      { a; b })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Interval_gen = struct
  type t = { interval : S2.R1_interval.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let finite = filter float ~f:Float.is_finite in
    create (fun ~size:_ ~random:rnd ->
      let lo = generate finite ~size:30 ~random:rnd in
      let hi = generate finite ~size:30 ~random:rnd in
      { interval =
          S2.R1_interval.create ~lo:(Float_u.of_float lo) ~hi:(Float_u.of_float hi)
      })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Finite_float = struct
  type t = float [@@deriving sexp_of]

  let quickcheck_generator = Base_quickcheck.Generator.(filter float ~f:Float.is_finite)
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "add_point_contains" =
  Base_quickcheck.Test.run_exn
    (module Interval_gen)
    ~config:qc_config
    ~f:(fun { Interval_gen.interval } ->
      let p = #1.23 in
      let added = S2.R1_interval.add_point interval p in
      assert (S2.R1_interval.contains added p))
;;

let%test_unit "from_point_pair_contains" =
  Base_quickcheck.Test.run_exn (module Finite_float) ~config:qc_config ~f:(fun p1_f ->
    let p1 = Float_u.of_float p1_f in
    let p2 = #4.56 in
    let i = S2.R1_interval.from_point_pair p1 p2 in
    assert (S2.R1_interval.contains i p1);
    assert (S2.R1_interval.contains i p2))
;;

let%test_unit "project_in_range" =
  Base_quickcheck.Test.run_exn
    (module Interval_gen)
    ~config:qc_config
    ~f:(fun { Interval_gen.interval } ->
      if not (S2.R1_interval.is_empty interval)
      then (
        let p = #7.89 in
        let proj = S2.R1_interval.project_exn interval p in
        assert (S2.R1_interval.contains interval proj)))
;;

let%test_unit "expanded_contains" =
  Base_quickcheck.Test.run_exn
    (module Interval_gen)
    ~config:qc_config
    ~f:(fun { Interval_gen.interval } ->
      if not (S2.R1_interval.is_empty interval)
      then (
        let expanded = S2.R1_interval.expanded interval #0.5 in
        assert (S2.R1_interval.contains_interval expanded interval)))
;;

let%test_unit "length_nonneg" =
  Base_quickcheck.Test.run_exn
    (module Interval_gen)
    ~config:qc_config
    ~f:(fun { Interval_gen.interval } ->
      if not (S2.R1_interval.is_empty interval)
      then assert (Float_u.O.(S2.R1_interval.length interval >= #0.0)))
;;

let%test_unit "equal_reflexive" =
  Base_quickcheck.Test.run_exn
    (module Interval_gen)
    ~config:qc_config
    ~f:(fun { Interval_gen.interval } -> assert (S2.R1_interval.equal interval interval))
;;

let%test_unit "intersection_subset" =
  Base_quickcheck.Test.run_exn
    (module R1_pair)
    ~config:qc_config
    ~f:(fun { R1_pair.a; b } ->
      let c = S2.R1_interval.intersection a b in
      if S2.R1_interval.is_empty c
      then ()
      else (
        assert (S2.R1_interval.contains_interval a c);
        assert (S2.R1_interval.contains_interval b c)))
;;

let%test_unit "union_superset" =
  Base_quickcheck.Test.run_exn
    (module R1_pair)
    ~config:qc_config
    ~f:(fun { R1_pair.a; b } ->
      let u = S2.R1_interval.union a b in
      assert (S2.R1_interval.contains_interval u a);
      assert (S2.R1_interval.contains_interval u b))
;;

let%test_unit "union_commutative" =
  Base_quickcheck.Test.run_exn
    (module R1_pair)
    ~config:qc_config
    ~f:(fun { R1_pair.a; b } ->
      let ab = S2.R1_interval.union a b in
      let ba = S2.R1_interval.union b a in
      assert (S2.R1_interval.equal ab ba))
;;

let%test_unit "intersection_commutative" =
  Base_quickcheck.Test.run_exn
    (module R1_pair)
    ~config:qc_config
    ~f:(fun { R1_pair.a; b } ->
      let ab = S2.R1_interval.intersection a b in
      let ba = S2.R1_interval.intersection b a in
      assert (S2.R1_interval.equal ab ba))
;;

let%test_unit "intersects_iff_nonempty_intersection" =
  Base_quickcheck.Test.run_exn
    (module R1_pair)
    ~config:qc_config
    ~f:(fun { R1_pair.a; b } ->
      let inter = S2.R1_interval.intersection a b in
      let intersects = S2.R1_interval.intersects a b in
      assert (Bool.equal intersects (not (S2.R1_interval.is_empty inter))))
;;

let%test_unit "contains_self" =
  Base_quickcheck.Test.run_exn
    (module Interval_gen)
    ~config:qc_config
    ~f:(fun { Interval_gen.interval } ->
      assert (S2.R1_interval.contains_interval interval interval))
;;

let%test_unit "add_point_superset" =
  Base_quickcheck.Test.run_exn
    (module Interval_gen)
    ~config:qc_config
    ~f:(fun { Interval_gen.interval } ->
      if not (S2.R1_interval.is_empty interval)
      then (
        let added = S2.R1_interval.add_point interval #3.14 in
        assert (S2.R1_interval.contains_interval added interval)))
;;

let%test_unit "from_point_pair_symmetric" =
  Base_quickcheck.Test.run_exn (module Finite_float) ~config:qc_config ~f:(fun p1_f ->
    let p1 = Float_u.of_float p1_f in
    let p2 = #2.71 in
    let ab = S2.R1_interval.from_point_pair p1 p2 in
    let ba = S2.R1_interval.from_point_pair p2 p1 in
    assert (S2.R1_interval.equal ab ba))
;;

let%test_unit "expanded_zero_identity" =
  Base_quickcheck.Test.run_exn
    (module Interval_gen)
    ~config:qc_config
    ~f:(fun { Interval_gen.interval } ->
      if not (S2.R1_interval.is_empty interval)
      then (
        let e = S2.R1_interval.expanded interval #0.0 in
        assert (S2.R1_interval.equal e interval)))
;;
