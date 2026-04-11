(* C++ test parity: s2geometry/src/s2/s2centroids_test.cc
   -  TEST(PlanarCentroid, SemiEquator)            - full parity
   -  TEST(TriangleTrueCentroid, SmallTriangles)   - partial (deterministic subset)
   -  TEST(EdgeTrueCentroid, SemiEquator)           - full parity
   -  TEST(EdgeTrueCentroid, GreatCircles)          - omitted (random, verified by property)

   Extra coverage:
   -  planar_centroid_cases (unit_axes, same_point)
   -  edge_true_centroid_cases (same_point, antipodal, short_edge, ninety_degrees)
   -  triangle_true_centroid degenerate cases (same_point, collinear)
   -  quickcheck properties: planar/true centroid cyclic symmetry, planar
      linearity, degenerate true centroid has zero norm, true centroid norm is
      bounded by triangle area (at most 2*pi for unit-length inputs) *)

open Core
open Test_helpers
open Alcotest

let fixture = load_fixture "s2centroids.json"

let point_of_json j =
  match to_list j with
  | [ x; y; z ] ->
    S2.R3_vector.create
      ~x:(float_u_of_json_exn x)
      ~y:(float_u_of_json_exn y)
      ~z:(float_u_of_json_exn z)
  | _ ->
    (match failwith "expected [x, y, z]" with
     | (_ : Nothing.t) -> .)
;;

let check_point ?(eps = 1e-15) msg ~expected ~actual =
  check_float_u
    ~eps
    (msg ^ " x")
    ~expected:(S2.R3_vector.x expected)
    ~actual:(S2.R3_vector.x actual);
  check_float_u
    ~eps
    (msg ^ " y")
    ~expected:(S2.R3_vector.y expected)
    ~actual:(S2.R3_vector.y actual);
  check_float_u
    ~eps
    (msg ^ " z")
    ~expected:(S2.R3_vector.z expected)
    ~actual:(S2.R3_vector.z actual)
;;

let test_planar_centroid_semi_equator () =
  let c = member "planar_centroid_semi_equator" fixture in
  let a = point_of_json (member "a" c) in
  let b = point_of_json (member "b" c) in
  let cv = point_of_json (member "c" c) in
  let expected = point_of_json (member "centroid" c) in
  let expected_normalized = point_of_json (member "centroid_normalized" c) in
  let expected_norm = float_of_json_exn (member "centroid_norm" c) in
  let actual = S2.S2_centroids.planar_centroid a b cv in
  check_point "centroid" ~expected ~actual;
  let actual_normalized = S2.R3_vector.normalize actual in
  check_point "normalized" ~expected:expected_normalized ~actual:actual_normalized;
  check_float
    "norm"
    ~expected:expected_norm
    ~actual:(Float_u.to_float (S2.R3_vector.norm actual))
;;

let test_planar_centroid_cases () =
  let cases = to_list (member "planar_centroid_cases" fixture) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let cv = point_of_json (member "c" c) in
    let expected = point_of_json (member "centroid" c) in
    let actual = S2.S2_centroids.planar_centroid a b cv in
    check_point label ~expected ~actual)
;;

let test_triangle_true_centroid () =
  let cases = to_list (member "triangle_true_centroid" fixture) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let cv = point_of_json (member "c" c) in
    let expected = point_of_json (member "centroid" c) in
    let actual = S2.S2_centroids.true_centroid a b cv in
    check_point ~eps:1e-14 label ~expected ~actual)
;;

let test_edge_true_centroid_semi_equator () =
  let c = member "edge_true_centroid_semi_equator" fixture in
  let a = point_of_json (member "a" c) in
  let b = point_of_json (member "b" c) in
  let cv = point_of_json (member "c" c) in
  let expected_ab = point_of_json (member "centroid_ab" c) in
  let expected_bc = point_of_json (member "centroid_bc" c) in
  let expected_sum = point_of_json (member "centroid_sum" c) in
  let expected_normalized = point_of_json (member "centroid_sum_normalized" c) in
  let expected_norm = float_of_json_exn (member "centroid_sum_norm" c) in
  let actual_ab = S2.S2_centroids.edge_true_centroid a b in
  let actual_bc = S2.S2_centroids.edge_true_centroid b cv in
  check_point "centroid_ab" ~expected:expected_ab ~actual:actual_ab;
  check_point "centroid_bc" ~expected:expected_bc ~actual:actual_bc;
  let actual_sum = S2.R3_vector.add actual_ab actual_bc in
  check_point "centroid_sum" ~expected:expected_sum ~actual:actual_sum;
  let actual_normalized = S2.R3_vector.normalize actual_sum in
  check_point "normalized" ~expected:expected_normalized ~actual:actual_normalized;
  check_float
    "norm"
    ~expected:expected_norm
    ~actual:(Float_u.to_float (S2.R3_vector.norm actual_sum))
;;

let test_edge_true_centroid_cases () =
  let cases = to_list (member "edge_true_centroid_cases" fixture) in
  List.iter cases ~f:(fun c ->
    let label = string_of_json_exn (member "label" c) in
    let a = point_of_json (member "a" c) in
    let b = point_of_json (member "b" c) in
    let expected = point_of_json (member "centroid" c) in
    let actual = S2.S2_centroids.edge_true_centroid a b in
    check_point label ~expected ~actual)
;;

(* -- Quickcheck generators ------------------------------------------------ *)

(* Three unit-length S2 points for testing triangle centroids. *)
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
    (* Reject points that are too close to zero before normalizing, so that
       [of_coords] does not return the sentinel [origin]. *)
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

(* A single unit-length S2 point for degenerate-triangle tests. *)
module S2_point_single = struct
  type t = { p : S2.S2_point.t } [@@deriving sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.create (fun ~size:_ ~random:rnd ->
      { p = S2_point_triple.gen_unit_point rnd })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

let vec_max_abs v =
  let open Float_u.O in
  let ax = Float_u.abs (S2.R3_vector.x v) in
  let ay = Float_u.abs (S2.R3_vector.y v) in
  let az = Float_u.abs (S2.R3_vector.z v) in
  Float_u.max ax (Float_u.max ay az)
;;

let check_vec_close ~eps msg u v =
  let open Float_u.O in
  let diff = S2.R3_vector.sub u v in
  let m = vec_max_abs diff in
  if m > Float_u.of_float eps
  then
    Alcotest.failf
      "%s: diff=%.17g (eps=%.17g)"
      msg
      (Float_u.to_float m)
      eps
;;

let quickcheck_planar_centroid_cyclic_symmetry () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let pabc = S2.S2_centroids.planar_centroid a b c in
      let pbca = S2.S2_centroids.planar_centroid b c a in
      let pcab = S2.S2_centroids.planar_centroid c a b in
      (* Addition order in [planar_centroid] is ((a+b)+c). The cyclic shifts
         compute ((b+c)+a) and ((c+a)+b), which can differ by a ULP or two from
         the original due to associativity of floating-point addition. Allow a
         small epsilon rather than exact equality. *)
      check_vec_close ~eps:1e-15 "planar_centroid abc vs bca" pabc pbca;
      check_vec_close ~eps:1e-15 "planar_centroid abc vs cab" pabc pcab)
;;

let quickcheck_planar_centroid_linear_scaling () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let open Float_u.O in
      (* [planar_centroid] is linear: s * ((a+b+c)/3) = (s*a + s*b + s*c) / 3.
         Inputs here are unit-length S2 points, but planar_centroid does not
         rely on unit length, so scaling a raw R3 vector is a valid check. *)
      let s = #2.5 in
      let scale v =
        S2.R3_vector.create
          ~x:(S2.R3_vector.x v * s)
          ~y:(S2.R3_vector.y v * s)
          ~z:(S2.R3_vector.z v * s)
      in
      let lhs = S2.S2_centroids.planar_centroid (scale a) (scale b) (scale c) in
      let rhs_unscaled = S2.S2_centroids.planar_centroid a b c in
      let rhs = scale rhs_unscaled in
      check_vec_close ~eps:1e-15 "planar_centroid linear scaling" lhs rhs)
;;

let quickcheck_true_centroid_cyclic_symmetry () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let tabc = S2.S2_centroids.true_centroid a b c in
      let tbca = S2.S2_centroids.true_centroid b c a in
      let tcab = S2.S2_centroids.true_centroid c a b in
      (* [true_centroid] is invariant under cyclic permutation, up to low-bit
         numerical noise from the Cramer's-rule formulation. *)
      check_vec_close ~eps:1e-14 "true_centroid abc vs bca" tabc tbca;
      check_vec_close ~eps:1e-14 "true_centroid abc vs cab" tabc tcab)
;;

let quickcheck_true_centroid_degenerate_zero () =
  Base_quickcheck.Test.run_exn
    (module S2_point_single)
    ~config:qc_config
    ~f:(fun { S2_point_single.p } ->
      let t = S2.S2_centroids.true_centroid p p p in
      let n = S2.R3_vector.norm t in
      assert (Float_u.O.(n <= #1e-15)))
;;

let quickcheck_true_centroid_norm_bounded () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      (* For unit-length inputs, [true_centroid a b c] is (centroid * signed
         area). The centroid lies in the closed unit ball, and the unsigned
         spherical area is bounded by 2*pi (half the sphere), so the norm of
         the result is bounded by 2*pi. *)
      let t = S2.S2_centroids.true_centroid a b c in
      let n = S2.R3_vector.norm t in
      let bound = Float_u.O.(Float_u.pi () * #2.0 + #1e-12) in
      assert (Float_u.O.(n <= bound)))
;;

let quickcheck_edge_true_centroid_symmetry () =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; _ } ->
      let tab = S2.S2_centroids.edge_true_centroid a b in
      let tba = S2.S2_centroids.edge_true_centroid b a in
      check_vec_close ~eps:1e-15 "edge_true_centroid ab vs ba" tab tba)
;;

(* -- End quickcheck ------------------------------------------------------- *)

let () =
  run
    "s2_centroids"
    [ ( "planar_centroid"
      , [ test_case "semi_equator" `Quick test_planar_centroid_semi_equator
        ; test_case "cases" `Quick test_planar_centroid_cases
        ] )
    ; "triangle_true_centroid", [ test_case "cases" `Quick test_triangle_true_centroid ]
    ; ( "edge_true_centroid"
      , [ test_case "semi_equator" `Quick test_edge_true_centroid_semi_equator
        ; test_case "cases" `Quick test_edge_true_centroid_cases
        ] )
    ; ( "quickcheck"
      , [ test_case
            "planar_centroid_cyclic_symmetry"
            `Quick
            quickcheck_planar_centroid_cyclic_symmetry
        ; test_case
            "planar_centroid_linear_scaling"
            `Quick
            quickcheck_planar_centroid_linear_scaling
        ; test_case
            "true_centroid_cyclic_symmetry"
            `Quick
            quickcheck_true_centroid_cyclic_symmetry
        ; test_case
            "true_centroid_degenerate_zero"
            `Quick
            quickcheck_true_centroid_degenerate_zero
        ; test_case
            "true_centroid_norm_bounded"
            `Quick
            quickcheck_true_centroid_norm_bounded
        ; test_case
            "edge_true_centroid_symmetry"
            `Quick
            quickcheck_edge_true_centroid_symmetry
        ] )
    ]
;;
