(* Quickcheck property tests for S2_polyline. *)
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

(* Draw a point that is not too close to [prev] or its antipode, to avoid
   degenerate consecutive edges. *)
let gen_distinct_point rnd ~prev =
  let rec loop () =
    let p = gen_unit_point rnd in
    let open Float_u.O in
    let d2 = S2.R3_vector.norm2 (S2.R3_vector.sub p prev) in
    let d2_anti = S2.R3_vector.norm2 (S2.R3_vector.add p prev) in
    if d2 > #1e-6 && d2_anti > #1e-6 then p else loop ()
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
          vertices.(i) <- gen_distinct_point rnd ~prev:vertices.(i - 1)
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
      let pl = S2.S2_polyline.of_vertices vertices in
      assert (S2.S2_polyline.num_vertices pl = Array.length vertices))
;;

let%test_unit "vertices_preserved" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let pl = S2.S2_polyline.of_vertices vertices in
      for i = 0 to Array.length vertices - 1 do
        assert (S2.S2_point.equal (S2.S2_polyline.vertex pl i) vertices.(i))
      done)
;;

let%test_unit "reverse_involution" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let pl = S2.S2_polyline.of_vertices vertices in
      let rr = S2.S2_polyline.reverse (S2.S2_polyline.reverse pl) in
      assert (S2.S2_polyline.equal rr pl))
;;

let%test_unit "length_reverse_invariant" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let open Float_u.O in
      let pl = S2.S2_polyline.of_vertices vertices in
      let r = S2.S2_polyline.reverse pl in
      let lp = S2.S1_angle.radians (S2.S2_polyline.length pl) in
      let lr = S2.S1_angle.radians (S2.S2_polyline.length r) in
      let scale = Float_u.max #1.0 lp in
      assert (Float_u.abs (lp - lr) <= #1e-12 * scale))
;;

let%test_unit "length_nonneg" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let pl = S2.S2_polyline.of_vertices vertices in
      assert (Float_u.O.(S2.S1_angle.radians (S2.S2_polyline.length pl) >= #0.0)))
;;

let%test_unit "interpolate_zero_is_first" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let pl = S2.S2_polyline.of_vertices vertices in
      let p = S2.S2_polyline.interpolate pl #0.0 in
      assert (
        S2.S2_point.approx_equal
          ~max_error:(Packed_float_option.Unboxed.some #1e-14)
          p
          vertices.(0)))
;;

let%test_unit "interpolate_one_is_last" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let pl = S2.S2_polyline.of_vertices vertices in
      let p = S2.S2_polyline.interpolate pl #1.0 in
      let last = vertices.(Array.length vertices - 1) in
      assert (
        S2.S2_point.approx_equal
          ~max_error:(Packed_float_option.Unboxed.some #1e-14)
          p
          last))
;;

let%test_unit "num_edges_is_num_vertices_minus_one" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let pl = S2.S2_polyline.of_vertices vertices in
      assert (S2.S2_polyline.num_edges pl = S2.S2_polyline.num_vertices pl - 1))
;;

let%test_unit "cap_bound_expanded_contains_vertices" =
  Base_quickcheck.Test.run_exn
    (module Polyline_gen)
    ~config:qc_config
    ~f:(fun { Polyline_gen.vertices } ->
      let pl = S2.S2_polyline.of_vertices vertices in
      let cap = S2.S2_polyline.cap_bound pl in
      (* cap_bound is a conservative bound but floating-point roundoff in
         [contains_point] can reject vertices that lie on the boundary; a
         tiny expansion closes that gap. *)
      let cap = S2.S2_cap.expanded_exn cap (S2.S1_angle.of_radians #1e-14) in
      for i = 0 to Array.length vertices - 1 do
        assert (S2.S2_cap.contains_point cap vertices.(i))
      done)
;;
