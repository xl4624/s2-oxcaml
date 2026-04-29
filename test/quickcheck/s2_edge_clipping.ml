(* Quickcheck property tests for S2_edge_clipping. *)
open Core
module S2_edge_clipping = S2.S2_edge_clipping
module R2_point = S2.R2_point
module R2_rect = S2.R2_rect

(* --- Generators ----------------------------------------------------------- *)

module S2_point_pair = struct
  type t =
    { a : S2.S2_point.t
    ; b : S2.S2_point.t
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
      { a; b })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module R2_segment_in_unit_square = struct
  type t =
    { a : R2_point.t
    ; b : R2_point.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    create (fun ~size:_ ~random:rnd ->
      let ax = generate coord ~size:30 ~random:rnd in
      let ay = generate coord ~size:30 ~random:rnd in
      let bx = generate coord ~size:30 ~random:rnd in
      let by = generate coord ~size:30 ~random:rnd in
      { a = R2_point.create ~x:(Float_u.of_float ax) ~y:(Float_u.of_float ay)
      ; b = R2_point.create ~x:(Float_u.of_float bx) ~y:(Float_u.of_float by)
      })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

let unit_uv_rect =
  R2_rect.create_intervals_exn
    ~x:(S2.R1_interval.create ~lo:(-#1.0) ~hi:#1.0)
    ~y:(S2.R1_interval.create ~lo:(-#1.0) ~hi:#1.0)
;;

let in_unit_uv p =
  let x = Float_u.to_float (R2_point.x p) in
  let y = Float_u.to_float (R2_point.y p) in
  Float.( >= ) x (-1.0 -. 1e-14)
  && Float.( <= ) x (1.0 +. 1e-14)
  && Float.( >= ) y (-1.0 -. 1e-14)
  && Float.( <= ) y (1.0 +. 1e-14)
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "face_segments_in_uv_rect" =
  Base_quickcheck.Test.run_exn
    (module S2_point_pair)
    ~config:qc_config
    ~f:(fun { S2_point_pair.a; b } ->
      let segs = S2_edge_clipping.get_face_segments a b in
      List.iter segs ~f:(fun seg ->
        if not (in_unit_uv seg.S2_edge_clipping.a && in_unit_uv seg.S2_edge_clipping.b)
        then
          Alcotest.failf
            "face segment outside [-1,1]^2 on face %d"
            seg.S2_edge_clipping.face))
;;

let%test_unit "clip_to_padded_face_in_uv_rect" =
  Base_quickcheck.Test.run_exn
    (module S2_point_pair)
    ~config:qc_config
    ~f:(fun { S2_point_pair.a; b } ->
      for face = 0 to 5 do
        match S2_edge_clipping.clip_to_padded_face a b face ~padding:#0.0 with
        | None -> ()
        | Some { a = a_uv; b = b_uv } ->
          if not (in_unit_uv a_uv && in_unit_uv b_uv)
          then Alcotest.failf "clip_to_padded_face face %d outside uv rect" face
      done)
;;

let%test_unit "clip_edge_inside_unit_rect" =
  Base_quickcheck.Test.run_exn
    (module R2_segment_in_unit_square)
    ~config:qc_config
    ~f:(fun { R2_segment_in_unit_square.a; b } ->
      match S2_edge_clipping.clip_edge a b unit_uv_rect with
      | None -> Alcotest.fail "clip_edge returned None for segment inside rect"
      | Some { a = a_clip; b = b_clip } ->
        let close p q =
          Float_u.O.(
            Float_u.abs (R2_point.x p - R2_point.x q) <= #1e-14
            && Float_u.abs (R2_point.y p - R2_point.y q) <= #1e-14)
        in
        if not (close a a_clip && close b b_clip)
        then Alcotest.fail "clip_edge altered inside-segment")
;;
