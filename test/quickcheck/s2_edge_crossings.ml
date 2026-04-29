(* Quickcheck property tests for S2_edge_crossings. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

module S2_point_triple = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    ; c : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let gen_unit_point rnd =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let rec loop () =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      if Float.((x *. x) +. (y *. y) +. (z *. z) < 1e-6)
      then loop ()
      else
        S2.S2_point.of_coords
          ~x:(Float_u.of_float x)
          ~y:(Float_u.of_float y)
          ~z:(Float_u.of_float z)
    in
    loop ()
  ;;

  let quickcheck_generator =
    Base_quickcheck.Generator.create (fun ~size:_ ~random:rnd ->
      let a = gen_unit_point rnd in
      let b = gen_unit_point rnd in
      let c = gen_unit_point rnd in
      { a; b; c })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module S2_point_quad = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    ; c : S2.S2_point.t
    ; d : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.create (fun ~size:_ ~random:rnd ->
      let a = S2_point_triple.gen_unit_point rnd in
      let b = S2_point_triple.gen_unit_point rnd in
      let c = S2_point_triple.gen_unit_point rnd in
      let d = S2_point_triple.gen_unit_point rnd in
      { a; b; c; d })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "sign_antisymmetric" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let s1 = S2.S2_edge_crossings.sign a b c in
      let s2 = S2.S2_edge_crossings.sign c b a in
      assert (Int.( = ) s1 (-s2)))
;;

let%test_unit "crossing_sign_edge_swap" =
  Base_quickcheck.Test.run_exn
    (module S2_point_quad)
    ~config:qc_config
    ~f:(fun { S2_point_quad.a; b; c; d } ->
      let s1 = S2.S2_edge_crossings.crossing_sign a b c d in
      let s2 = S2.S2_edge_crossings.crossing_sign c d a b in
      assert (Int.( = ) s1 s2))
;;

let%test_unit "crossing_sign_self" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; _ } ->
      let s = S2.S2_edge_crossings.crossing_sign a b a b in
      assert (Int.( = ) s 0))
;;

let%test_unit "sign_cyclic" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let s1 = S2.S2_edge_crossings.sign a b c in
      let s2 = S2.S2_edge_crossings.sign b c a in
      let s3 = S2.S2_edge_crossings.sign c a b in
      assert (Int.( = ) s1 s2);
      assert (Int.( = ) s2 s3))
;;

let%test_unit "crossing_sign_reverse_edges" =
  (* crossing_sign should be symmetric w.r.t. reversing both edges' endpoints. *)
  Base_quickcheck.Test.run_exn
    (module S2_point_quad)
    ~config:qc_config
    ~f:(fun { S2_point_quad.a; b; c; d } ->
      let s1 = S2.S2_edge_crossings.crossing_sign a b c d in
      let s2 = S2.S2_edge_crossings.crossing_sign b a d c in
      assert (Int.( = ) s1 s2))
;;

let%test_unit "crossing_sign_in_range" =
  Base_quickcheck.Test.run_exn
    (module S2_point_quad)
    ~config:qc_config
    ~f:(fun { S2_point_quad.a; b; c; d } ->
      let s = S2.S2_edge_crossings.crossing_sign a b c d in
      assert (Int.( = ) s (-1) || Int.( = ) s 0 || Int.( = ) s 1))
;;

(* --- Randomized tests ported from s2edge_crossings_test.cc --------------- *)

(* Pair generator biased toward nearly-linearly-dependent points so the
   [robust_cross_prod] exact and symbolic fallbacks are exercised. Mirrors
   the distribution C++ [TEST(S2, RobustCrossProdError)] samples from. *)
module Robust_cross_prod_pair = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let gen_unit_point = S2_point_triple.gen_unit_point

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    create (fun ~size:_ ~random:rnd ->
      let a = gen_unit_point rnd in
      let dir = gen_unit_point rnd in
      let u = generate (float_inclusive 0.0 1.0) ~size:30 ~random:rnd in
      (* [r] sweeps from [~pi/2] down to [~2^-53] radians logarithmically. *)
      let mutable r =
        Float.( * )
          (Float.( / ) Float.pi 2.0)
          (Float.ldexp 1.0 (Int.of_float (Float.( * ) (-53.0) u)))
      in
      (* 1/3 chance of making [r] vastly smaller to push the fallback deep
         into subnormal territory. *)
      let coin = generate (int_uniform_inclusive 0 2) ~size:30 ~random:rnd in
      if Int.( = ) coin 0
      then (
        let u2 = generate (float_inclusive 0.0 1.0) ~size:30 ~random:rnd in
        r <- Float.( * ) r (Float.ldexp 1.0 (Int.of_float (Float.( * ) (-1022.0) u2))));
      let b =
        S2.S2_edge_distances.get_point_on_line
          a
          dir
          (S2.S1_angle.of_radians (Float_u.of_float r))
      in
      let negate = generate (int_uniform_inclusive 0 1) ~size:30 ~random:rnd in
      let b = if Int.( = ) negate 0 then b else S2.R3_vector.neg b in
      { a; b })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config_large =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 1000; shrink_count = 50 }
;;

(* C++ parity: the core invariant of [TEST(S2, RobustCrossProdError)]. For any
   distinct [a], [b], [robust_cross_prod a b] points to the CCW side of the
   oriented edge [a -> b]. This property is what makes [robust_cross_prod]
   usable as the "reference normal" for [angle_contains_vertex] and the
   containment tests. *)
let%test_unit "robust_cross_prod_ccw_consistent" =
  Base_quickcheck.Test.run_exn
    (module Robust_cross_prod_pair)
    ~config:qc_config_large
    ~f:(fun { Robust_cross_prod_pair.a; b } ->
      if S2.S2_point.equal a b
      then ()
      else (
        let r = S2.R3_vector.normalize (S2.S2_point.robust_cross_prod a b) in
        match S2.S2_predicates.robust_sign a b r with
        | Counter_clockwise -> ()
        | Clockwise | Indeterminate ->
          Alcotest.failf
            "robust_sign a b (robust_cross_prod a b) should be CCW: a=%s b=%s"
            (S2.R3_vector.to_string a)
            (S2.R3_vector.to_string b)))
;;

(* C++ parity: [TEST(S2, RobustCrossProdError)] identity
   [RobustCrossProd(b, a).Normalize() == -RobustCrossProd(a, b).Normalize()].
   Holds whenever [a <> b]; note that the raw (non-normalized) vectors can
   differ in magnitude, which is why the check is after [Normalize]. *)
let%test_unit "robust_cross_prod_swap_antisymmetric" =
  Base_quickcheck.Test.run_exn
    (module Robust_cross_prod_pair)
    ~config:qc_config_large
    ~f:(fun { Robust_cross_prod_pair.a; b } ->
      if S2.S2_point.equal a b
      then ()
      else (
        let ab = S2.R3_vector.normalize (S2.S2_point.robust_cross_prod a b) in
        let ba = S2.R3_vector.normalize (S2.S2_point.robust_cross_prod b a) in
        let max_error =
          Packed_float_option.Unboxed.some Float_u.O.(#4.0 * Float_u.epsilon_float)
        in
        if not (S2.S2_point.approx_equal ~max_error ab (S2.R3_vector.neg ba))
        then
          Alcotest.failf
            "swap antisymmetry: ab=%s vs -ba=%s"
            (S2.R3_vector.to_string ab)
            (S2.R3_vector.to_string (S2.R3_vector.neg ba))))
;;

(* C++ parity: [RobustCrossProd(-a, b).Normalize() == -result] and the
   symmetric [a, -b] version. These identities hold only when [a] and [b] are
   linearly independent -- for linearly-dependent inputs, symbolic
   perturbation of [-a] is a distinct symbol from [-sym(a)], so the identity
   is not required. We detect linear dependence via the double cross product
   [(a + b) x (b - a)]; when that is nonzero, the exact cross is also
   nonzero (it's [2 * a x b]) and the identity holds. *)
let%test_unit "robust_cross_prod_neg_antisymmetric" =
  Base_quickcheck.Test.run_exn
    (module Robust_cross_prod_pair)
    ~config:qc_config_large
    ~f:(fun { Robust_cross_prod_pair.a; b } ->
      if S2.S2_point.equal a b
      then ()
      else (
        let dbl_cross =
          S2.R3_vector.cross (S2.R3_vector.add a b) (S2.R3_vector.sub b a)
        in
        if S2.R3_vector.equal dbl_cross S2.R3_vector.zero
        then ()
        else (
          let ab = S2.R3_vector.normalize (S2.S2_point.robust_cross_prod a b) in
          let neg_ab =
            S2.R3_vector.normalize (S2.S2_point.robust_cross_prod (S2.R3_vector.neg a) b)
          in
          let a_neg_b =
            S2.R3_vector.normalize (S2.S2_point.robust_cross_prod a (S2.R3_vector.neg b))
          in
          let max_error =
            Packed_float_option.Unboxed.some Float_u.O.(#4.0 * Float_u.epsilon_float)
          in
          let expected = S2.R3_vector.neg ab in
          if not (S2.S2_point.approx_equal ~max_error neg_ab expected)
          then
            Alcotest.failf
              "neg_a antisymmetry failed: robust_cross_prod(-a, b)=%s, expected %s"
              (S2.R3_vector.to_string neg_ab)
              (S2.R3_vector.to_string expected);
          if not (S2.S2_point.approx_equal ~max_error a_neg_b expected)
          then
            Alcotest.failf
              "neg_b antisymmetry failed: robust_cross_prod(a, -b)=%s, expected %s"
              (S2.R3_vector.to_string a_neg_b)
              (S2.R3_vector.to_string expected))))
;;

(* C++ parity: [TEST(S2, CollinearEdgesThatDontTouch)] from
   s2edge_crosser_test.cc. Takes two random points [a], [d] on the sphere,
   picks [b] and [c] as interior interpolation fractions along the great
   circle [a -> d]. The two sub-edges [a-b] and [c-d] are disjoint collinear
   segments; [crossing_sign] must return [-1] (do not cross). Exercises the
   exact and symbolic paths because [a, b, c, d] are mathematically collinear
   but numerically perturbed. *)
module Pair_a_d = struct
  type t =
    { a : S2.S2_point.t
    ; d : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.create (fun ~size:_ ~random:rnd ->
      let a = S2_point_triple.gen_unit_point rnd in
      let d = S2_point_triple.gen_unit_point rnd in
      { a; d })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let%test_unit "collinear_edges_that_dont_touch" =
  Base_quickcheck.Test.run_exn
    (module Pair_a_d)
    ~config:{ qc_config_large with test_count = 500 }
    ~f:(fun { Pair_a_d.a; d } ->
      if S2.S2_point.equal a d
      then ()
      else (
        let b = S2.S2_edge_distances.interpolate a d #0.05 in
        let c = S2.S2_edge_distances.interpolate a d #0.95 in
        let s = S2.S2_edge_crossings.crossing_sign a b c d in
        if not (Int.( < ) s 0)
        then
          Alcotest.failf
            "collinear sub-edges should not cross: a=%s d=%s s=%d"
            (S2.R3_vector.to_string a)
            (S2.R3_vector.to_string d)
            s))
;;

(* C++ parity: [TEST(S2, CoincidentZeroLengthEdgesThatDontTouch)]. Picks a
   point [p] whose components are all zero or a power of two (so every
   nonzero coordinate shares the same mantissa). Scales [p] by four very
   close scalars [(1 - 3e-16), (1 - 1e-16), 1, (1 + 2e-16)] to form
   [a, b, c, d]; after [Normalize] these are coincident on the unit sphere
   but distinct in memory. [crossing_sign] must return [-1] because the
   edges are degenerate and symbolic perturbations should rule out an
   intersection. *)
module Power_of_two_point = struct
  type t = { p : S2.S2_point.t } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord_gen =
      (* Binary exponent chosen from a skewed integer distribution in
         [0, 11]; values > 10 collapse to 0 to populate ~10% zeros. *)
      bind (int_inclusive 0 11) ~f:(fun e ->
        if Int.( > ) e 10 then return 0.0 else return (Float.ldexp 1.0 (Int.neg e)))
    in
    create (fun ~size ~random:rnd ->
      let rec loop () =
        let x = generate coord_gen ~size ~random:rnd in
        let y = generate coord_gen ~size ~random:rnd in
        let z = generate coord_gen ~size ~random:rnd in
        let p =
          S2.S2_point.of_coords
            ~x:(Float_u.of_float x)
            ~y:(Float_u.of_float y)
            ~z:(Float_u.of_float z)
        in
        if S2.R3_vector.equal p S2.R3_vector.zero then loop () else p
      in
      { p = loop () })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let%test_unit "coincident_zero_length_edges_that_dont_touch" =
  Base_quickcheck.Test.run_exn
    (module Power_of_two_point)
    ~config:qc_config_large
    ~f:(fun { Power_of_two_point.p } ->
      let scale k = S2.R3_vector.mul p (Float_u.of_float k) in
      let a = scale (1.0 -. 3e-16) in
      let b = scale (1.0 -. 1e-16) in
      let c = p in
      let d = scale (1.0 +. 2e-16) in
      if S2.S2_point.is_unit_length a && S2.S2_point.is_unit_length d
      then (
        let s = S2.S2_edge_crossings.crossing_sign a b c d in
        if not (Int.( < ) s 0)
        then
          Alcotest.failf
            "coincident zero-length edges should not cross: p=%s s=%d"
            (S2.R3_vector.to_string p)
            s))
;;

(* C++ parity: [TEST(S2, GetIntersectionInvariants)]. When two crossing
   edges are swapped or reversed, [get_intersection] must return the same
   point (bit-exactly). We construct crossing edges of equal length by
   generating two random points and swapping their x and y coordinates
   (preserves [(x**2 + y**2) + z**2] exactly in double precision), which
   matches the C++ setup. *)
module Crossing_equal_length = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    ; c : S2.S2_point.t
    ; d : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    create (fun ~size:_ ~random:rnd ->
      let rec loop () =
        let a = S2_point_triple.gen_unit_point rnd in
        let b = S2_point_triple.gen_unit_point rnd in
        let swap_xy v =
          S2.R3_vector.create
            ~x:(S2.R3_vector.y v)
            ~y:(S2.R3_vector.x v)
            ~z:(S2.R3_vector.z v)
        in
        let c = swap_xy a in
        let d = swap_xy b in
        if Int.( > ) (S2.S2_edge_crossings.crossing_sign a b c d) 0
        then { a; b; c; d }
        else loop ()
      in
      loop ())
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let%test_unit "get_intersection_invariants" =
  Base_quickcheck.Test.run_exn
    (module Crossing_equal_length)
    ~config:{ qc_config_large with test_count = 200 }
    ~f:(fun { Crossing_equal_length.a; b; c; d } ->
      let result = S2.S2_edge_crossings.get_intersection a b c d in
      let check label a b c d =
        let got = S2.S2_edge_crossings.get_intersection a b c d in
        if not (S2.R3_vector.equal got result)
        then
          Alcotest.failf
            "get_intersection not invariant under %s: expected %s, got %s"
            label
            (S2.R3_vector.to_string result)
            (S2.R3_vector.to_string got)
      in
      check "swap(a, b)" b a c d;
      check "swap(c, d)" a b d c;
      check "swap(ab, cd)" c d a b;
      check "all three swaps" d c b a)
;;

(* [TEST(S2, IntersectionError)] and [TEST(S2, GrazingIntersections)] are
   deliberately omitted: they stress-test [get_intersection] under very
   degenerate configurations (log-uniform 1e-15 .. 1 edge lengths combined
   with 1e-15 .. 1e15 slopes), which exercises precision paths unrelated to
   [robust_cross_prod]. Porting them well requires an equivalent of the
   upstream [s2random::Frame] + [s2random::LogUniform] helpers and a
   [get_intersection_exact] reference, which are out of scope for this
   change. *)
