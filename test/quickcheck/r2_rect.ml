(* Quickcheck property tests for R2_rect. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

module R2_rect_pair = struct
  type t =
    { a : S2.R2_rect.t
    ; b : S2.R2_rect.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let finite = filter float ~f:Float.is_finite in
    let pt_gen rnd =
      let x = generate finite ~size:30 ~random:rnd in
      let y = generate finite ~size:30 ~random:rnd in
      S2.R2_point.create ~x:(Float_u.of_float x) ~y:(Float_u.of_float y)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = pt_gen rnd in
      let b = pt_gen rnd in
      let r1 = S2.R2_rect.from_point_pair a b in
      let r2 = S2.R2_rect.from_point_pair (pt_gen rnd) (pt_gen rnd) in
      { a = r1; b = r2 })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Rect_gen = struct
  type t = { rect : S2.R2_rect.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let finite = filter float ~f:Float.is_finite in
    let pt_gen rnd =
      let x = generate finite ~size:30 ~random:rnd in
      let y = generate finite ~size:30 ~random:rnd in
      S2.R2_point.create ~x:(Float_u.of_float x) ~y:(Float_u.of_float y)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = pt_gen rnd in
      let b = pt_gen rnd in
      { rect = S2.R2_rect.from_point_pair a b })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "add_point_contains" =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      let p = S2.R2_point.create ~x:#1.23 ~y:#4.56 in
      let added = S2.R2_rect.add_point rect p in
      assert (S2.R2_rect.contains_point added p))
;;

let%test_unit "expanded_contains" =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      if not (S2.R2_rect.is_empty rect)
      then (
        let expanded = S2.R2_rect.expanded rect (S2.R2_point.create ~x:#0.5 ~y:#0.5) in
        assert (S2.R2_rect.contains_rect expanded rect)))
;;

let%test_unit "volume_nonneg" =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      let open Float_u.O in
      if not (S2.R2_rect.is_empty rect)
      then (
        let dx = S2.R1_interval.length (S2.R2_rect.x rect) in
        let dy = S2.R1_interval.length (S2.R2_rect.y rect) in
        let vol = dx * dy in
        assert (vol >= #0.0)))
;;

let%test_unit "project_in_rect" =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } ->
      if not (S2.R2_rect.is_empty rect)
      then (
        let p = S2.R2_point.create ~x:#7.89 ~y:#0.12 in
        let proj = S2.R2_rect.project rect p in
        assert (S2.R2_rect.contains_point rect proj)))
;;

let%test_unit "intersection_subset" =
  Base_quickcheck.Test.run_exn
    (module R2_rect_pair)
    ~config:qc_config
    ~f:(fun { R2_rect_pair.a; b } ->
      let c = S2.R2_rect.intersection a b in
      if S2.R2_rect.is_empty c
      then ()
      else (
        assert (S2.R2_rect.contains_rect a c);
        assert (S2.R2_rect.contains_rect b c)))
;;

let%test_unit "union_superset" =
  Base_quickcheck.Test.run_exn
    (module R2_rect_pair)
    ~config:qc_config
    ~f:(fun { R2_rect_pair.a; b } ->
      let u = S2.R2_rect.union a b in
      assert (S2.R2_rect.contains_rect u a);
      assert (S2.R2_rect.contains_rect u b))
;;

let%test_unit "union_commutative" =
  Base_quickcheck.Test.run_exn
    (module R2_rect_pair)
    ~config:qc_config
    ~f:(fun { R2_rect_pair.a; b } ->
      let ab = S2.R2_rect.union a b in
      let ba = S2.R2_rect.union b a in
      assert (S2.R2_rect.equal ab ba))
;;

let%test_unit "intersection_commutative" =
  Base_quickcheck.Test.run_exn
    (module R2_rect_pair)
    ~config:qc_config
    ~f:(fun { R2_rect_pair.a; b } ->
      let ab = S2.R2_rect.intersection a b in
      let ba = S2.R2_rect.intersection b a in
      assert (S2.R2_rect.equal ab ba))
;;

let%test_unit "contains_self" =
  Base_quickcheck.Test.run_exn
    (module Rect_gen)
    ~config:qc_config
    ~f:(fun { Rect_gen.rect } -> assert (S2.R2_rect.contains_rect rect rect))
;;

let%test_unit "intersects_iff_nonempty_intersection" =
  Base_quickcheck.Test.run_exn
    (module R2_rect_pair)
    ~config:qc_config
    ~f:(fun { R2_rect_pair.a; b } ->
      let inter = S2.R2_rect.intersection a b in
      let intersects = S2.R2_rect.intersects a b in
      assert (Bool.equal intersects (not (S2.R2_rect.is_empty inter))))
;;
