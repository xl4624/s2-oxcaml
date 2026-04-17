(* Quickcheck property tests for S2_polyline_measures. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

module Polyline_input = struct
  type t = { vertices : S2.S2_point.t array }

  let sexp_of_t { vertices } =
    let acc = ref [] in
    for i = Array.length vertices - 1 downto 0 do
      acc := Sexp.Atom (S2.R3_vector.to_string vertices.(i)) :: !acc
    done;
    Sexp.List !acc
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
        S2.S2_point.of_coords
          ~x:(Float_u.of_float x)
          ~y:(Float_u.of_float y)
          ~z:(Float_u.of_float z)
    in
    loop ()
  ;;

  let quickcheck_generator =
    Base_quickcheck.Generator.create (fun ~size:_ ~random:rnd ->
      let open Base_quickcheck.Generator in
      let n = generate (int_inclusive 0 8) ~size:30 ~random:rnd in
      if n = 0
      then { vertices = [||] }
      else (
        let first = gen_unit_point rnd in
        let vertices = Array.create ~len:n first in
        for i = 1 to n - 1 do
          vertices.(i) <- gen_unit_point rnd
        done;
        { vertices }))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

let reverse_array (a : S2.S2_point.t array) : S2.S2_point.t array =
  let n = Array.length a in
  if n = 0
  then [||]
  else (
    let out = Array.create ~len:n a.(0) in
    for i = 0 to n - 1 do
      out.(i) <- a.(n - 1 - i)
    done;
    out)
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
  then Alcotest.failf "%s: diff=%.17g (eps=%.17g)" msg (Float_u.to_float m) eps
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "length_reversal_invariant" =
  Base_quickcheck.Test.run_exn
    (module Polyline_input)
    ~config:qc_config
    ~f:(fun { Polyline_input.vertices } ->
      let l1 = S2.S1_angle.radians (S2.S2_polyline_measures.length vertices) in
      let reversed = reverse_array vertices in
      let l2 = S2.S1_angle.radians (S2.S2_polyline_measures.length reversed) in
      let open Float_u.O in
      let diff = Float_u.abs (l1 - l2) in
      if diff > #1e-14
      then
        Alcotest.failf
          "length reversal mismatch: %.17g vs %.17g"
          (Float_u.to_float l1)
          (Float_u.to_float l2))
;;

let%test_unit "centroid_reversal_invariant" =
  Base_quickcheck.Test.run_exn
    (module Polyline_input)
    ~config:qc_config
    ~f:(fun { Polyline_input.vertices } ->
      let c1 = S2.S2_polyline_measures.centroid vertices in
      let reversed = reverse_array vertices in
      let c2 = S2.S2_polyline_measures.centroid reversed in
      (* [edge_true_centroid a b] is symmetric in its arguments, so reversing
         the polyline does not change the centroid (up to floating-point
         roundoff in the accumulation order). *)
      check_vec_close ~eps:1e-14 "centroid reversal invariant" c1 c2)
;;

let%test_unit "length_sum_of_edges" =
  Base_quickcheck.Test.run_exn
    (module Polyline_input)
    ~config:qc_config
    ~f:(fun { Polyline_input.vertices } ->
      let expected =
        let mutable acc = #0.0 in
        for i = 1 to Array.length vertices - 1 do
          let prev = vertices.(i - 1) in
          let cur = vertices.(i) in
          acc <- Float_u.O.(acc + S2.S1_angle.radians (S2.S2_point.distance prev cur))
        done;
        acc
      in
      let actual = S2.S1_angle.radians (S2.S2_polyline_measures.length vertices) in
      let open Float_u.O in
      let diff = Float_u.abs (expected - actual) in
      if diff > #1e-14
      then
        Alcotest.failf
          "length vs edge-sum mismatch: %.17g vs %.17g"
          (Float_u.to_float expected)
          (Float_u.to_float actual))
;;
