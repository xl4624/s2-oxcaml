(* Quickcheck property tests for S2_loop_measures. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

(* We build random four-vertex loops from an S2 cell; these are always valid, have known
   small area, and give us something to check curvature/area identities against. *)
module Cell_vertices = struct
  type t = { id : Int64.t }

  let sexp_of_t { id } = Sexp.Atom (Int64.to_string id)

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let rec descend depth id =
      let cell = cid_of_int64 id in
      if depth = 0 || S2.S2_cell_id.is_leaf cell
      then return id
      else
        bind (int_uniform_inclusive 0 3) ~f:(fun k ->
          descend (depth - 1) (int64_of_cid (S2.S2_cell_id.child_exn cell k)))
    in
    bind (int_uniform_inclusive 0 5) ~f:(fun f ->
      bind (int_uniform_inclusive 2 12) ~f:(fun depth ->
        map
          (descend depth (int64_of_cid (S2.S2_cell_id.from_face_exn f)))
          ~f:(fun id -> { id })))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

  let to_vertices t =
    let cell = S2.S2_cell.of_cell_id (cid_of_int64 t.id) in
    let n = 4 in
    let arr = Array.create ~len:n (S2.S2_cell.vertex cell 0) in
    for i = 1 to n - 1 do
      arr.(i) <- S2.S2_cell.vertex cell i
    done;
    arr
  ;;
end

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

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 100; shrink_count = 25 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "area_nonneg" =
  Base_quickcheck.Test.run_exn (module Cell_vertices) ~config:qc_config ~f:(fun t ->
    let vs = Cell_vertices.to_vertices t in
    let a = S2.S2_loop_measures.area vs in
    assert (Float_u.O.(a >= #0.0)))
;;

let%test_unit "area_plus_curvature_is_2pi" =
  Base_quickcheck.Test.run_exn (module Cell_vertices) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let vs = Cell_vertices.to_vertices t in
    let a = S2.S2_loop_measures.area vs in
    let c = S2.S2_loop_measures.curvature vs in
    let expected = #2.0 * Float_u.pi in
    assert (Float_u.abs (a + c - expected) <= #1e-10))
;;

let%test_unit "signed_area_abs_matches_area" =
  Base_quickcheck.Test.run_exn (module Cell_vertices) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let vs = Cell_vertices.to_vertices t in
    let a = S2.S2_loop_measures.area vs in
    let sa = S2.S2_loop_measures.signed_area vs in
    assert (Float_u.abs (Float_u.abs sa - a) <= #1e-10))
;;

let%test_unit "signed_area_reverse_negates" =
  Base_quickcheck.Test.run_exn (module Cell_vertices) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let vs = Cell_vertices.to_vertices t in
    let rs = reverse_array vs in
    let sa = S2.S2_loop_measures.signed_area vs in
    let sar = S2.S2_loop_measures.signed_area rs in
    assert (Float_u.abs (sa + sar) <= #1e-10))
;;

let%test_unit "perimeter_reverse_invariant" =
  Base_quickcheck.Test.run_exn (module Cell_vertices) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let vs = Cell_vertices.to_vertices t in
    let rs = reverse_array vs in
    let p = S2.S1_angle.radians (S2.S2_loop_measures.perimeter vs) in
    let pr = S2.S1_angle.radians (S2.S2_loop_measures.perimeter rs) in
    let scale = Float_u.max #1.0 p in
    assert (Float_u.abs (p - pr) <= #1e-12 * scale))
;;

let%test_unit "perimeter_nonneg" =
  Base_quickcheck.Test.run_exn (module Cell_vertices) ~config:qc_config ~f:(fun t ->
    let vs = Cell_vertices.to_vertices t in
    let p = S2.S1_angle.radians (S2.S2_loop_measures.perimeter vs) in
    assert (Float_u.O.(p >= #0.0)))
;;

let%test_unit "approx_area_close_to_area" =
  Base_quickcheck.Test.run_exn (module Cell_vertices) ~config:qc_config ~f:(fun t ->
    let open Float_u.O in
    let vs = Cell_vertices.to_vertices t in
    let a = S2.S2_loop_measures.area vs in
    let ap = S2.S2_loop_measures.approx_area vs in
    let scale = Float_u.max #1.0 a in
    (* approx_area trades some accuracy for speed but should match area closely for small,
       near-planar quads like cell corners. *)
    assert (Float_u.abs (a - ap) <= #1e-8 * scale))
;;
