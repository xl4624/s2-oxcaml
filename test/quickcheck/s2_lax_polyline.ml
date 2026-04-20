(* Quickcheck property tests for S2_lax_polyline. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

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

module Polyline_gen = struct
  type t = { vertices : S2.S2_point.t array }

  let sexp_of_t { vertices } =
    let acc = ref [] in
    for i = Array.length vertices - 1 downto 0 do
      acc := Sexp.Atom (S2.R3_vector.to_string vertices.(i)) :: !acc
    done;
    Sexp.List !acc
  ;;

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    bind (int_uniform_inclusive 2 6) ~f:(fun n ->
      create (fun ~size:_ ~random:rnd ->
        let vertices = Array.create ~len:n (gen_unit_point rnd) in
        for i = 1 to n - 1 do
          vertices.(i) <- gen_unit_point rnd
        done;
        { vertices }))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 100; shrink_count = 25 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "num_vertices_matches_input" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let pl = S2.S2_lax_polyline.of_vertices vertices in
      assert (S2.S2_lax_polyline.num_vertices pl = Array.length vertices))
;;

let%test_unit "num_edges_is_n_minus_one" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let pl = S2.S2_lax_polyline.of_vertices vertices in
      assert (S2.S2_lax_polyline.num_edges pl = S2.S2_lax_polyline.num_vertices pl - 1))
;;

let%test_unit "vertices_preserved" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let pl = S2.S2_lax_polyline.of_vertices vertices in
      for i = 0 to Array.length vertices - 1 do
        assert (S2.S2_point.equal (S2.S2_lax_polyline.vertex pl i) vertices.(i))
      done)
;;

let%test_unit "num_chains_is_one" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let pl = S2.S2_lax_polyline.of_vertices vertices in
      assert (S2.S2_lax_polyline.num_chains pl = 1))
;;

let%test_unit "dimension_is_one" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let pl = S2.S2_lax_polyline.of_vertices vertices in
      assert (S2.S2_lax_polyline.dimension pl = 1))
;;
