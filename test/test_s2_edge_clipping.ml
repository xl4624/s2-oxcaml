(* C++ test parity: s2geometry/src/s2/s2edge_clipping_test.cc Golden data from
   test/gen/s2edge_clipping.cc.

   Covered:
   - TEST(S2, FaceClipping) - hand-picked deterministic GetFaceSegments and
     ClipToPaddedFace cases
   - TEST(S2, EdgeClipping) - hand-picked deterministic ClipEdge, GetClippedEdgeBound, and
     IntersectsRect cases
   - TEST(S2, InterpolateDouble) - hand-picked deterministic cases

   Deliberately omitted:
   - Randomized sweeps that rely on absl BitGen seeding. *)

open Core
open Test_helpers
module S2_edge_clipping = S2.S2_edge_clipping
module R2_point = S2.R2_point
module R2_rect = S2.R2_rect
module R3_vector = S2.R3_vector
module S2_point = S2.S2_point

let fixture = lazy (load_fixture "s2edge_clipping.json")

let s2_point_of_json j =
  match to_list j with
  | [ x; y; z ] ->
    R3_vector.create
      ~x:(float_u_of_json_exn x)
      ~y:(float_u_of_json_exn y)
      ~z:(float_u_of_json_exn z)
  | _ ->
    (match failwith "expected [x, y, z]" with
     | (_ : Nothing.t) -> .)
;;

(* ---------- error constants ------------------------------------------------ *)

let test_constants () =
  let data = Lazy.force fixture in
  let c = member "constants" data in
  check_float_u
    "face_clip_error_radians"
    ~expected:(float_u_of_json_exn (member "face_clip_error_radians" c))
    ~actual:S2_edge_clipping.face_clip_error_radians;
  check_float_u
    "face_clip_error_uv_dist"
    ~expected:(float_u_of_json_exn (member "face_clip_error_uv_dist" c))
    ~actual:S2_edge_clipping.face_clip_error_uv_dist;
  check_float_u
    "face_clip_error_uv_coord"
    ~expected:(float_u_of_json_exn (member "face_clip_error_uv_coord" c))
    ~actual:S2_edge_clipping.face_clip_error_uv_coord;
  check_float_u
    "intersects_rect_error_uv_dist"
    ~expected:(float_u_of_json_exn (member "intersects_rect_error_uv_dist" c))
    ~actual:S2_edge_clipping.intersects_rect_error_uv_dist;
  check_float_u
    "edge_clip_error_uv_coord"
    ~expected:(float_u_of_json_exn (member "edge_clip_error_uv_coord" c))
    ~actual:S2_edge_clipping.edge_clip_error_uv_coord;
  check_float_u
    "edge_clip_error_uv_dist"
    ~expected:(float_u_of_json_exn (member "edge_clip_error_uv_dist" c))
    ~actual:S2_edge_clipping.edge_clip_error_uv_dist
;;

(* ---------- face segments -------------------------------------------------- *)

let test_face_segments () =
  let data = Lazy.force fixture in
  let cases = to_list (member "face_segments" data) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = s2_point_of_json (member "a" c) in
    let b = s2_point_of_json (member "b" c) in
    let expected_segs = to_list (member "segments" c) in
    let actual_segs = S2_edge_clipping.get_face_segments a b in
    Alcotest.(check int)
      (name ^ " num segments")
      (List.length expected_segs)
      (List.length actual_segs);
    List.iter2_exn expected_segs actual_segs ~f:(fun exp act ->
      let exp_face = int_of_json_exn (member "face" exp) in
      let exp_a = r2_point_of_json (member "a" exp) in
      let exp_b = r2_point_of_json (member "b" exp) in
      Alcotest.(check int) (name ^ " face") exp_face act.S2_edge_clipping.face;
      check_r2_point
        ~eps:1e-14
        (name ^ " seg a")
        ~expected:exp_a
        ~actual:act.S2_edge_clipping.a;
      check_r2_point
        ~eps:1e-14
        (name ^ " seg b")
        ~expected:exp_b
        ~actual:act.S2_edge_clipping.b))
;;

(* ---------- clip_to_padded_face ------------------------------------------- *)

let test_clip_to_padded_face () =
  let data = Lazy.force fixture in
  let cases = to_list (member "clip_to_padded_face" data) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = s2_point_of_json (member "a" c) in
    let b = s2_point_of_json (member "b" c) in
    let padding = float_u_of_json_exn (member "padding" c) in
    let faces = to_list (member "faces" c) in
    List.iter faces ~f:(fun f ->
      let face = int_of_json_exn (member "face" f) in
      let expected_ok = bool_of_json_exn (member "intersects" f) in
      let actual = S2_edge_clipping.clip_to_padded_face a b face ~padding in
      let label = sprintf "%s face %d" name face in
      match%optional_u.S2_edge_clipping.Clipped_uv.Option actual with
      | None -> if expected_ok then Alcotest.fail (label ^ ": expected intersection")
      | Some clipped ->
        if not expected_ok
        then Alcotest.fail (label ^ ": expected no intersection")
        else (
          let a_uv = S2_edge_clipping.Clipped_uv.a clipped in
          let b_uv = S2_edge_clipping.Clipped_uv.b clipped in
          let exp_a = r2_point_of_json (member "a_uv" f) in
          let exp_b = r2_point_of_json (member "b_uv" f) in
          check_r2_point ~eps:1e-14 (label ^ " a_uv") ~expected:exp_a ~actual:a_uv;
          check_r2_point ~eps:1e-14 (label ^ " b_uv") ~expected:exp_b ~actual:b_uv)))
;;

(* ---------- clip_edge / get_clipped_edge_bound / intersects_rect ---------- *)

let test_clip_edge () =
  let data = Lazy.force fixture in
  let cases = to_list (member "clip_edge" data) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = r2_point_of_json (member "a" c) in
    let b = r2_point_of_json (member "b" c) in
    let clip = r2_rect_of_json (member "clip" c) in
    let expected_ok = bool_of_json_exn (member "intersects" c) in
    let expected_ir = bool_of_json_exn (member "intersects_rect" c) in
    let bound_is_empty = bool_of_json_exn (member "bound_is_empty" c) in
    let actual = S2_edge_clipping.clip_edge a b clip in
    (match%optional_u.S2_edge_clipping.Clipped_uv.Option actual with
     | None -> if expected_ok then Alcotest.fail (name ^ ": expected clip_edge = Some")
     | Some clipped ->
       if not expected_ok
       then Alcotest.fail (name ^ ": expected clip_edge = None")
       else (
         let a_clip = S2_edge_clipping.Clipped_uv.a clipped in
         let b_clip = S2_edge_clipping.Clipped_uv.b clipped in
         let exp_a = r2_point_of_json (member "a_clipped" c) in
         let exp_b = r2_point_of_json (member "b_clipped" c) in
         check_r2_point ~eps:1e-14 (name ^ " clip a") ~expected:exp_a ~actual:a_clip;
         check_r2_point ~eps:1e-14 (name ^ " clip b") ~expected:exp_b ~actual:b_clip));
    let actual_bound = S2_edge_clipping.get_clipped_edge_bound a b clip in
    check_bool
      (name ^ " bound empty")
      ~expected:bound_is_empty
      ~actual:(R2_rect.is_empty actual_bound);
    if not bound_is_empty
    then (
      let exp_bound = r2_rect_of_json (member "bound" c) in
      check_float_u
        ~eps:1e-14
        (name ^ " bound x.lo")
        ~expected:(S2.R1_interval.lo (R2_rect.x exp_bound))
        ~actual:(S2.R1_interval.lo (R2_rect.x actual_bound));
      check_float_u
        ~eps:1e-14
        (name ^ " bound x.hi")
        ~expected:(S2.R1_interval.hi (R2_rect.x exp_bound))
        ~actual:(S2.R1_interval.hi (R2_rect.x actual_bound));
      check_float_u
        ~eps:1e-14
        (name ^ " bound y.lo")
        ~expected:(S2.R1_interval.lo (R2_rect.y exp_bound))
        ~actual:(S2.R1_interval.lo (R2_rect.y actual_bound));
      check_float_u
        ~eps:1e-14
        (name ^ " bound y.hi")
        ~expected:(S2.R1_interval.hi (R2_rect.y exp_bound))
        ~actual:(S2.R1_interval.hi (R2_rect.y actual_bound)));
    let actual_ir = S2_edge_clipping.intersects_rect a b clip in
    check_bool (name ^ " intersects_rect") ~expected:expected_ir ~actual:actual_ir)
;;

(* ---------- intersects_rect (dedicated fixtures) --------------------------- *)

let test_intersects_rect () =
  let data = Lazy.force fixture in
  let cases = to_list (member "intersects_rect" data) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let a = r2_point_of_json (member "a" c) in
    let b = r2_point_of_json (member "b" c) in
    let clip = r2_rect_of_json (member "clip" c) in
    let expected = bool_of_json_exn (member "intersects_rect" c) in
    let actual = S2_edge_clipping.intersects_rect a b clip in
    check_bool name ~expected ~actual)
;;

(* ---------- interpolate_double --------------------------------------------- *)

let test_interpolate_double () =
  let data = Lazy.force fixture in
  let cases = to_list (member "interpolate_double" data) in
  List.iter cases ~f:(fun c ->
    let name = string_of_json_exn (member "name" c) in
    let x = float_u_of_json_exn (member "x" c) in
    let a = float_u_of_json_exn (member "a" c) in
    let b = float_u_of_json_exn (member "b" c) in
    let a1 = float_u_of_json_exn (member "a1" c) in
    let b1 = float_u_of_json_exn (member "b1" c) in
    let expected = float_u_of_json_exn (member "y" c) in
    let actual = S2_edge_clipping.interpolate_double x a b a1 b1 in
    check_float_u_exact name ~expected ~actual)
;;

let () =
  Alcotest.run
    "s2_edge_clipping"
    [ "constants", [ Alcotest.test_case "constants" `Quick test_constants ]
    ; "face_segments", [ Alcotest.test_case "face_segments" `Quick test_face_segments ]
    ; ( "clip_to_padded_face"
      , [ Alcotest.test_case "clip_to_padded_face" `Quick test_clip_to_padded_face ] )
    ; "clip_edge", [ Alcotest.test_case "clip_edge" `Quick test_clip_edge ]
    ; ( "intersects_rect"
      , [ Alcotest.test_case "intersects_rect" `Quick test_intersects_rect ] )
    ; ( "interpolate_double"
      , [ Alcotest.test_case "interpolate_double" `Quick test_interpolate_double ] )
    ]
;;
