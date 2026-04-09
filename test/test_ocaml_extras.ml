open Core

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
      let a = S2.R2_rect.from_point_pair (pt_gen rnd) (pt_gen rnd) in
      let b = S2.R2_rect.from_point_pair (pt_gen rnd) (pt_gen rnd) in
      { a; b })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module R2_pair = struct
  type t =
    { a : S2.R2_point.t
    ; b : S2.R2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1e6) 1e6 in
    let pt_gen rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      S2.R2_point.create ~x:(Float_u.of_float x) ~y:(Float_u.of_float y)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = pt_gen rnd in
      let b = pt_gen rnd in
      { a; b })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module R2_point_scalar = struct
  type t =
    { v : S2.R2_point.t
    ; k : float
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1e6) 1e6 in
    let k_gen = float_inclusive (-1e3) 1e3 in
    let pt_gen rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      S2.R2_point.create ~x:(Float_u.of_float x) ~y:(Float_u.of_float y)
    in
    create (fun ~size:_ ~random:rnd ->
      let v = pt_gen rnd in
      let k = generate k_gen ~size:30 ~random:rnd in
      { v; k })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module R3_pair = struct
  type t =
    { a : S2.R3_vector.t
    ; b : S2.R3_vector.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1e6) 1e6 in
    create (fun ~size:_ ~random:rnd ->
      let gen () = Float_u.of_float (generate coord ~size:30 ~random:rnd) in
      let v () = S2.R3_vector.create ~x:(gen ()) ~y:(gen ()) ~z:(gen ()) in
      { a = v (); b = v () })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module R2_vec = struct
  type t = { v : S2.R2_point.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1e6) 1e6 in
    let pt_gen rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      S2.R2_point.create ~x:(Float_u.of_float x) ~y:(Float_u.of_float y)
    in
    create (fun ~size:_ ~random:rnd -> { v = pt_gen rnd })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module R3_vec = struct
  type t = { v : S2.R3_vector.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1e6) 1e6 in
    create (fun ~size:_ ~random:rnd ->
      let gen () = Float_u.of_float (generate coord ~size:30 ~random:rnd) in
      { v = S2.R3_vector.create ~x:(gen ()) ~y:(gen ()) ~z:(gen ()) })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

let quickcheck_r1_intersection_subset () =
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

let quickcheck_r1_union_superset () =
  Base_quickcheck.Test.run_exn
    (module R1_pair)
    ~config:qc_config
    ~f:(fun { R1_pair.a; b } ->
      let u = S2.R1_interval.union a b in
      assert (S2.R1_interval.contains_interval u a);
      assert (S2.R1_interval.contains_interval u b))
;;

let quickcheck_s1_intersection_subset () =
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

let quickcheck_s1_union_superset () =
  Base_quickcheck.Test.run_exn
    (module S1_pair)
    ~config:qc_config
    ~f:(fun { S1_pair.a; b } ->
      let u = S2.S1_interval.union a b in
      assert (S2.S1_interval.contains_interval u a);
      assert (S2.S1_interval.contains_interval u b))
;;

let quickcheck_r2_rect_intersection_subset () =
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

let quickcheck_r2_rect_union_superset () =
  Base_quickcheck.Test.run_exn
    (module R2_rect_pair)
    ~config:qc_config
    ~f:(fun { R2_rect_pair.a; b } ->
      let u = S2.R2_rect.union a b in
      assert (S2.R2_rect.contains_rect u a);
      assert (S2.R2_rect.contains_rect u b))
;;

let quickcheck_r2_add_commutative () =
  Base_quickcheck.Test.run_exn
    (module R2_pair)
    ~config:qc_config
    ~f:(fun { R2_pair.a; b } ->
      assert (S2.R2_point.equal (S2.R2_point.add a b) (S2.R2_point.add b a)))
;;

let quickcheck_r2_dot_commutative () =
  Base_quickcheck.Test.run_exn
    (module R2_pair)
    ~config:qc_config
    ~f:(fun { R2_pair.a; b } ->
      let u = S2.R2_point.dot a b in
      let v = S2.R2_point.dot b a in
      let open Float_u.O in
      let scale = Float_u.max #1.0 (Float_u.max (Float_u.abs u) (Float_u.abs v)) in
      assert (Float_u.abs (u - v) <= #1e-10 * scale))
;;

let quickcheck_r2_ortho_perpendicular () =
  Base_quickcheck.Test.run_exn (module R2_vec) ~config:qc_config ~f:(fun { R2_vec.v } ->
    let d = S2.R2_point.dot v (S2.R2_point.ortho v) in
    let open Float_u.O in
    let scale =
      Float_u.max
        #1.0
        (Float_u.max (Float_u.abs (S2.R2_point.x v)) (Float_u.abs (S2.R2_point.y v)))
    in
    assert (Float_u.abs d <= #1e-14 * scale))
;;

let quickcheck_r2_norm2_mul () =
  Base_quickcheck.Test.run_exn
    (module R2_point_scalar)
    ~config:qc_config
    ~f:(fun { R2_point_scalar.v; k } ->
      let ku = Float_u.of_float k in
      let lhs = S2.R2_point.norm2 (S2.R2_point.mul v ku) in
      let rhs = Float_u.(ku * ku * S2.R2_point.norm2 v) in
      let open Float_u.O in
      let scale = Float_u.max #1.0 (Float_u.max (Float_u.abs lhs) (Float_u.abs rhs)) in
      assert (Float_u.abs (lhs - rhs) <= #1e-10 * scale))
;;

let quickcheck_r3_dot_commutative () =
  Base_quickcheck.Test.run_exn
    (module R3_pair)
    ~config:qc_config
    ~f:(fun { R3_pair.a; b } ->
      let u = S2.R3_vector.dot a b in
      let v = S2.R3_vector.dot b a in
      let open Float_u.O in
      let scale = Float_u.max #1.0 (Float_u.max (Float_u.abs u) (Float_u.abs v)) in
      assert (Float_u.abs (u - v) <= #1e-10 * scale))
;;

let quickcheck_r3_cross_antisymmetric () =
  Base_quickcheck.Test.run_exn
    (module R3_pair)
    ~config:qc_config
    ~f:(fun { R3_pair.a; b } ->
      assert (
        S2.R3_vector.approx_equal
          ~max_error:1e-10
          (S2.R3_vector.cross b a)
          (S2.R3_vector.neg (S2.R3_vector.cross a b))))
;;

let quickcheck_r3_norm2_nonneg () =
  Base_quickcheck.Test.run_exn (module R3_vec) ~config:qc_config ~f:(fun { R3_vec.v } ->
    assert (Float_u.O.(S2.R3_vector.norm2 v >= #0.0)))
;;

let () =
  Alcotest.run
    "ocaml_extras"
    [ ( "r1_interval"
      , [ Alcotest.test_case
            "intersection_subset"
            `Quick
            quickcheck_r1_intersection_subset
        ; Alcotest.test_case "union_superset" `Quick quickcheck_r1_union_superset
        ] )
    ; ( "s1_interval"
      , [ Alcotest.test_case
            "intersection_subset"
            `Quick
            quickcheck_s1_intersection_subset
        ; Alcotest.test_case "union_superset" `Quick quickcheck_s1_union_superset
        ] )
    ; ( "r2_rect"
      , [ Alcotest.test_case
            "intersection_subset"
            `Quick
            quickcheck_r2_rect_intersection_subset
        ; Alcotest.test_case "union_superset" `Quick quickcheck_r2_rect_union_superset
        ] )
    ; ( "r2_point"
      , [ Alcotest.test_case "add_commutative" `Quick quickcheck_r2_add_commutative
        ; Alcotest.test_case "dot_commutative" `Quick quickcheck_r2_dot_commutative
        ; Alcotest.test_case
            "ortho_perpendicular"
            `Quick
            quickcheck_r2_ortho_perpendicular
        ; Alcotest.test_case "norm2_mul" `Quick quickcheck_r2_norm2_mul
        ] )
    ; ( "r3_vector"
      , [ Alcotest.test_case "dot_commutative" `Quick quickcheck_r3_dot_commutative
        ; Alcotest.test_case
            "cross_antisymmetric"
            `Quick
            quickcheck_r3_cross_antisymmetric
        ; Alcotest.test_case "norm2_nonneg" `Quick quickcheck_r3_norm2_nonneg
        ] )
    ]
;;
