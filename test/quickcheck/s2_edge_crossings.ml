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
