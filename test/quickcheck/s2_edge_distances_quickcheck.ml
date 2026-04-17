(* Quickcheck property tests for S2_edge_distances. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

module S2_point_triple = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
    ; x : S2.S2_point.t
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
      let x = gen_unit_point rnd in
      { a; b; x })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "distance_to_degenerate_edge" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; x; _ } ->
      let d_edge =
        Float_u.to_float (S2.S1_angle.radians (S2.S2_edge_distances.get_distance x a a))
      in
      let d_point = Float_u.to_float (S2.S1_angle.radians (S2.S2_point.distance x a)) in
      let diff = Float.abs (d_edge -. d_point) in
      if Float.( > ) diff 1e-13
      then Alcotest.failf "get_distance x a a vs dist(x,a): %g vs %g" d_edge d_point)
;;

let%test_unit "interpolate_endpoints" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; _ } ->
      let at_a = S2.S2_edge_distances.interpolate a b #0.0 in
      let at_b = S2.S2_edge_distances.interpolate a b #1.0 in
      let d_a = Float_u.to_float (S2.S1_angle.radians (S2.S2_point.distance at_a a)) in
      let d_b = Float_u.to_float (S2.S1_angle.radians (S2.S2_point.distance at_b b)) in
      if Float.( > ) d_a 3e-15 then Alcotest.failf "interpolate a b 0 angle to a = %g" d_a;
      if Float.( > ) d_b 3e-15 then Alcotest.failf "interpolate a b 1 angle to b = %g" d_b)
;;

let%test_unit "project_is_on_edge" =
  Base_quickcheck.Test.run_exn
    (module S2_point_triple)
    ~config:qc_config
    ~f:(fun { S2_point_triple.a; b; x } ->
      if not (S2.S2_point.equal a b)
      then (
        let proj = S2.S2_edge_distances.project x a b in
        let d =
          Float_u.to_float
            (S2.S1_angle.radians (S2.S2_edge_distances.get_distance proj a b))
        in
        if Float.( > ) d 1e-13 then Alcotest.failf "project then get_distance = %g" d))
;;
