(* Quickcheck property tests for S2_edge_crosser. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

module S2_point_quad = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    ; c : S2.S2_point.t
    ; d : S2.S2_point.t
    }

  let sexp_of_t { a; b; c; d } =
    Sexp.List
      [ Sexp.Atom (S2.R3_vector.to_string a)
      ; Sexp.Atom (S2.R3_vector.to_string b)
      ; Sexp.Atom (S2.R3_vector.to_string c)
      ; Sexp.Atom (S2.R3_vector.to_string d)
      ]
  ;;

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
        S2.R3_vector.normalize
          (S2.S2_point.of_coords
             ~x:(Float_u.of_float x)
             ~y:(Float_u.of_float y)
             ~z:(Float_u.of_float z))
    in
    loop ()
  ;;

  let quickcheck_generator =
    Base_quickcheck.Generator.create (fun ~size:_ ~random:rnd ->
      let a = gen_unit_point rnd in
      let b = gen_unit_point rnd in
      let c = gen_unit_point rnd in
      let d = gen_unit_point rnd in
      { a; b; c; d })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "crossing_sign_matches_edge_crossings" =
  (* The stateful crosser should produce the same sign as the stateless
     crossing_sign in S2_edge_crossings. *)
  Base_quickcheck.Test.run_exn
    (module S2_point_quad)
    ~config:qc_config
    ~f:(fun { S2_point_quad.a; b; c; d } ->
      let crosser = S2.S2_edge_crosser.create ~a ~b in
      let #{ S2.S2_edge_crosser.state = _; sign = got } =
        S2.S2_edge_crosser.crossing_sign crosser c d
      in
      let expected = S2.S2_edge_crossings.crossing_sign a b c d in
      assert (Int.( = ) got expected))
;;

let%test_unit "chain_crossing_matches_stateless" =
  (* Feeding a chain c -> d through chain_crossing_sign after restart_at c
     should equal crossing_sign for edge cd. *)
  Base_quickcheck.Test.run_exn
    (module S2_point_quad)
    ~config:qc_config
    ~f:(fun { S2_point_quad.a; b; c; d } ->
      let crosser = S2.S2_edge_crosser.create ~a ~b in
      let crosser = S2.S2_edge_crosser.restart_at crosser c in
      let #{ S2.S2_edge_crosser.state = _; sign = got } =
        S2.S2_edge_crosser.chain_crossing_sign crosser d
      in
      let expected = S2.S2_edge_crossings.crossing_sign a b c d in
      assert (Int.( = ) got expected))
;;

let%test_unit "crossing_sign_symmetric_in_edges" =
  (* Swapping the fixed edge with the query edge gives the same sign. *)
  Base_quickcheck.Test.run_exn
    (module S2_point_quad)
    ~config:qc_config
    ~f:(fun { S2_point_quad.a; b; c; d } ->
      let c1 = S2.S2_edge_crosser.create ~a ~b in
      let #{ S2.S2_edge_crosser.state = _; sign = s1 } =
        S2.S2_edge_crosser.crossing_sign c1 c d
      in
      let c2 = S2.S2_edge_crosser.create ~a:c ~b:d in
      let #{ S2.S2_edge_crosser.state = _; sign = s2 } =
        S2.S2_edge_crosser.crossing_sign c2 a b
      in
      assert (Int.( = ) s1 s2))
;;

let%test_unit "create_with_chain_equivalent_to_restart_at" =
  Base_quickcheck.Test.run_exn
    (module S2_point_quad)
    ~config:qc_config
    ~f:(fun { S2_point_quad.a; b; c; d } ->
      let c1 = S2.S2_edge_crosser.create_with_chain ~a ~b ~c in
      let c2 = S2.S2_edge_crosser.restart_at (S2.S2_edge_crosser.create ~a ~b) c in
      let #{ S2.S2_edge_crosser.state = _; sign = s1 } =
        S2.S2_edge_crosser.chain_crossing_sign c1 d
      in
      let #{ S2.S2_edge_crosser.state = _; sign = s2 } =
        S2.S2_edge_crosser.chain_crossing_sign c2 d
      in
      assert (Int.( = ) s1 s2))
;;

let%test_unit "crossing_sign_result_in_set" =
  Base_quickcheck.Test.run_exn
    (module S2_point_quad)
    ~config:qc_config
    ~f:(fun { S2_point_quad.a; b; c; d } ->
      let crosser = S2.S2_edge_crosser.create ~a ~b in
      let #{ S2.S2_edge_crosser.state = _; sign } =
        S2.S2_edge_crosser.crossing_sign crosser c d
      in
      assert (Int.( = ) sign (-1) || Int.( = ) sign 0 || Int.( = ) sign 1))
;;
