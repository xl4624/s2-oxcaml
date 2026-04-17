(* Quickcheck property tests for S2_predicates. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

module S2_point_triple = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    ; c : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let gen_point rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      S2.S2_point.of_coords
        ~x:(Float_u.of_float x)
        ~y:(Float_u.of_float y)
        ~z:(Float_u.of_float z)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = gen_point rnd in
      let b = gen_point rnd in
      let c = gen_point rnd in
      { a; b; c })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module S2_point_quad = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    ; c : S2.S2_point.t
    ; o : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let gen_point rnd =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      S2.S2_point.of_coords
        ~x:(Float_u.of_float x)
        ~y:(Float_u.of_float y)
        ~z:(Float_u.of_float z)
    in
    create (fun ~size:_ ~random:rnd ->
      let a = gen_point rnd in
      let b = gen_point rnd in
      let c = gen_point rnd in
      let o = gen_point rnd in
      { a; b; c; o })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

let sign_int a b c =
  S2.S2_predicates.Direction.to_int (S2.S2_predicates.robust_sign a b c)
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "sign_antisymmetric" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let s1 = sign_int a b c in
      let s2 = sign_int b a c in
      assert (Int.( = ) s1 (-s2)))
;;

let%test_unit "sign_cyclic" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let s1 = sign_int a b c in
      let s2 = sign_int b c a in
      let s3 = sign_int c a b in
      assert (Int.( = ) s1 s2);
      assert (Int.( = ) s2 s3))
;;

let%test_unit "sign_anticyclic_swap" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let s1 = sign_int a b c in
      let s2 = sign_int a c b in
      assert (Int.( = ) s1 (-s2)))
;;

let%test_unit "robust_sign_never_indeterminate" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      (* Random unit points generated from independent coordinates will never
         be exactly equal (the only case where robust_sign returns
         Indeterminate).  Skip the vanishingly unlikely collision case
         defensively. *)
      if S2.S2_point.equal a b || S2.S2_point.equal b c || S2.S2_point.equal a c
      then ()
      else (
        let d = S2.S2_predicates.robust_sign a b c in
        match d with
        | S2.S2_predicates.Direction.Indeterminate -> assert false
        | Clockwise | Counter_clockwise -> ()))
;;

let%test_unit "sign_agrees_with_robust_sign" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      (* For non-degenerate triples, [sign] and [robust_sign] must agree: the
         boolean returns true iff robust_sign is Counter_clockwise.  Skip
         near-collinear triples where the plain-float determinant sign is
         unreliable; those are exercised by the fixture-driven tests. *)
      let det = S2.R3_vector.dot (S2.R3_vector.cross a b) c in
      if Float_u.O.(Float_u.abs det < #1e-10)
      then ()
      else (
        let boolean = S2.S2_predicates.sign a b c in
        let robust = Int.( > ) (sign_int a b c) 0 in
        assert (Bool.( = ) boolean robust)))
;;

let%test_unit "ordered_ccw_definition" =
  Base_quickcheck.Test.run_exn
    (module S2_point_quad)
    ~config:qc_config
    ~f:(fun { S2_point_quad.a; b; c; o } ->
      (* Cross-check ordered_ccw against its definition in terms of
         robust_sign.  The Go reference:
            sum := 0
            if robustSign(b, o, a) != Clockwise { sum++ }
            if robustSign(c, o, b) != Clockwise { sum++ }
            if robustSign(a, o, c) == CounterClockwise { sum++ }
            return sum >= 2
         This test is a tautology against the current implementation but
         protects against accidental regressions that swap arguments. *)
      let nonneg x y z =
        match S2.S2_predicates.robust_sign x y z with
        | Clockwise -> 0
        | Counter_clockwise | Indeterminate -> 1
      in
      let pos x y z =
        match S2.S2_predicates.robust_sign x y z with
        | Counter_clockwise -> 1
        | Clockwise | Indeterminate -> 0
      in
      let sum = nonneg b o a + nonneg c o b + pos a o c in
      let expected = Int.( >= ) sum 2 in
      let actual = S2.S2_predicates.ordered_ccw a b c o in
      assert (Bool.( = ) expected actual))
;;
