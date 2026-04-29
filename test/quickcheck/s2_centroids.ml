(* Quickcheck property tests for S2_centroids. *)
open Core

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
  then Alcotest.failf "%s: diff=%.17g (eps=%.17g)" msg (Float_u.to_float m) eps
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "planar_centroid_cyclic_symmetry" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let pabc = S2.S2_centroids.planar_centroid a b c in
      let pbca = S2.S2_centroids.planar_centroid b c a in
      let pcab = S2.S2_centroids.planar_centroid c a b in
      check_vec_close ~eps:1e-15 "planar_centroid abc vs bca" pabc pbca;
      check_vec_close ~eps:1e-15 "planar_centroid abc vs cab" pabc pcab)
;;

let%test_unit "planar_centroid_linear_scaling" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let open Float_u.O in
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

let%test_unit "true_centroid_cyclic_symmetry" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let tabc = S2.S2_centroids.true_centroid a b c in
      let tbca = S2.S2_centroids.true_centroid b c a in
      let tcab = S2.S2_centroids.true_centroid c a b in
      check_vec_close ~eps:1e-14 "true_centroid abc vs bca" tabc tbca;
      check_vec_close ~eps:1e-14 "true_centroid abc vs cab" tabc tcab)
;;

let%test_unit "true_centroid_degenerate_zero" =
  Base_quickcheck.Test.run_exn
    (module S2_point_single)
    ~config:qc_config
    ~f:(fun { S2_point_single.p } ->
      let t = S2.S2_centroids.true_centroid p p p in
      let n = S2.R3_vector.norm t in
      assert (Float_u.O.(n <= #1e-15)))
;;

let%test_unit "true_centroid_norm_bounded" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; c } ->
      let t = S2.S2_centroids.true_centroid a b c in
      let n = S2.R3_vector.norm t in
      let bound = Float_u.O.((Float_u.pi * #2.0) + #1e-12) in
      assert (Float_u.O.(n <= bound)))
;;

let%test_unit "edge_true_centroid_symmetry" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; _ } ->
      let tab = S2.S2_centroids.edge_true_centroid a b in
      let tba = S2.S2_centroids.edge_true_centroid b a in
      check_vec_close ~eps:1e-15 "edge_true_centroid ab vs ba" tab tba)
;;

let%test_unit "planar_centroid_degenerate_equals_point" =
  (* Degenerate triangles (all vertices identical) have planar centroid equal to that
     point. *)
  Base_quickcheck.Test.run_exn
    (module S2_point_single)
    ~config:qc_config
    ~f:(fun { S2_point_single.p } ->
      let c = S2.S2_centroids.planar_centroid p p p in
      check_vec_close ~eps:1e-15 "planar_centroid degenerate" c p)
;;

let%test_unit "edge_true_centroid_norm_bounded" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; _ } ->
      let t = S2.S2_centroids.edge_true_centroid a b in
      let n = S2.R3_vector.norm t in
      (* The edge centroid integrated over the great-circle arc has norm bounded by the
         arc length (<= pi). *)
      let bound = Float_u.O.(Float_u.pi + #1e-12) in
      assert (Float_u.O.(n <= bound)))
;;
