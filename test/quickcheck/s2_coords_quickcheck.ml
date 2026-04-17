(* Quickcheck property tests for S2_coords. *)
open Core
open Test_helpers

(* --- Generators ----------------------------------------------------------- *)

module S_point = struct
  type t = { s : float } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    create (fun ~size:_ ~random:rnd ->
      { s = generate (float_inclusive 0.0 1.0) ~size:30 ~random:rnd })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Face_uv = struct
  type t =
    { face : int
    ; u : float
    ; v : float
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    create (fun ~size:_ ~random:rnd ->
      { face = generate (int_inclusive 0 5) ~size:30 ~random:rnd
      ; u = generate (float_inclusive (-0.999) 0.999) ~size:30 ~random:rnd
      ; v = generate (float_inclusive (-0.999) 0.999) ~size:30 ~random:rnd
      })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "st_uv_roundtrip" =
  Base_quickcheck.Test.run_exn (module S_point) ~config:qc_config ~f:(fun { S_point.s } ->
    let su = Float_u.of_float s in
    let u = S2.S2_coords.st_to_uv su in
    let back = S2.S2_coords.uv_to_st u in
    let diff = Float.abs (Float_u.to_float back -. s) in
    if Float.( > ) diff 1e-15 then Alcotest.failf "st->uv->st diff %g for s=%g" diff s)
;;

let%test_unit "si_ti_st_roundtrip" =
  Base_quickcheck.Test.run_exn (module S_point) ~config:qc_config ~f:(fun { S_point.s } ->
    let su = Float_u.of_float s in
    let si = S2.S2_coords.st_to_si_ti su in
    let back = S2.S2_coords.si_ti_to_st si in
    let max_diff = 1.0 /. Float.of_int S2.S2_coords.max_si_ti in
    let diff = Float.abs (Float_u.to_float back -. s) in
    if Float.( > ) diff max_diff
    then Alcotest.failf "si_ti roundtrip diff %g > %g for s=%g" diff max_diff s)
;;

let%test_unit "face_uv_xyz_roundtrip" =
  Base_quickcheck.Test.run_exn
    (module Face_uv)
    ~config:qc_config
    ~f:(fun { Face_uv.face; u; v } ->
      let uf = Float_u.of_float u in
      let vf = Float_u.of_float v in
      let p = S2.S2_coords.face_uv_to_xyz face uf vf in
      let actual_face = S2.S2_coords.get_face p in
      if Int.( <> ) actual_face face
      then Alcotest.failf "get_face: expected %d, got %d" face actual_face;
      let uv_back = S2.S2_coords.valid_face_xyz_to_uv face p in
      let u_back = Float_u.to_float (S2.R2_point.x uv_back) in
      let v_back = Float_u.to_float (S2.R2_point.y uv_back) in
      let du = Float.abs (u_back -. u) in
      let dv = Float.abs (v_back -. v) in
      if Float.( > ) du 1e-14 || Float.( > ) dv 1e-14
      then Alcotest.failf "uv roundtrip face=%d u=%g->%g v=%g->%g" face u u_back v v_back)
;;
