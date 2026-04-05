(* C++ test parity: s2geometry/src/s2/s2coords_test.cc
   - TEST(S2, ST_UV_Conversions) - st_uv_conversions
   - TEST(S2, STtoIJBoundaries) - st_to_ij (boundaries)
   - TEST(S2, STtoIJHalfway) - st_to_ij (halfway values)
   - TEST(S2, FaceUVtoXYZ) - face_uv_to_xyz
   - TEST(S2, FaceXYZtoUVW) - face_xyz_to_uvw
   - TEST(S2, UVNorms) - uv_norms
   - TEST(S2, UVWAxis) - uvw_axis
   - TEST(S2, UVWFace) - uvw_face

   Extra coverage beyond upstream:
   - constants: max_cell_level, limit_ij, max_si_ti, max_xyz_to_uv_error
   - si_ti_to_st / st_to_si_ti round-trips
   - ij_to_st_min
   - face_xyz_to_uv (with validity check)
   - get_face
   - xyz_to_face_uv

   Deliberately omitted:
   - TEST(S2, TraversalOrder) - internal Hilbert tables (not exposed)
   - TEST(S2, IJtoSTtoIJRoundtripRandom) - random testing (covered by deterministic cases)
   - TEST(S2, XYZToFaceSiTi) - requires S2CellId (not yet available)
   - TEST(S2, NaNInput) - platform-specific *)

open Core
open Test_helpers
open Alcotest

let test_constants fixture () =
  let c = member "constants" fixture in
  (check int)
    "max_cell_level"
    (int_of_json_exn (member "max_cell_level" c))
    S2.S2_coords.max_cell_level;
  (check int) "limit_ij" (int_of_json_exn (member "limit_ij" c)) S2.S2_coords.limit_ij;
  (check int) "max_si_ti" (int_of_json_exn (member "max_si_ti" c)) S2.S2_coords.max_si_ti;
  check_float_exact
    "max_xyz_to_uv_error"
    ~expected:(float_of_json_exn (member "max_xyz_to_uv_error" c))
    ~actual:S2.S2_coords.max_xyz_to_uv_error
;;

let test_st_uv_conversions fixture () =
  let cases = to_list (member "st_uv_conversions" fixture) in
  List.iter cases ~f:(fun c ->
    let op = string_of_json_exn (member "op" c) in
    let input = float_of_json_exn (member "input" c) in
    let result = float_of_json_exn (member "result" c) in
    match op with
    | "st_to_uv" ->
      let actual = S2.S2_coords.st_to_uv input in
      let expected_linear = float_of_json_exn (member "expected_linear" c) in
      check_float_exact (sprintf "st_to_uv(%g)" input) ~expected:result ~actual;
      check_float_exact
        (sprintf "st_to_uv(%g) linear" input)
        ~expected:expected_linear
        ~actual
    | "uv_to_st" ->
      let actual = S2.S2_coords.uv_to_st input in
      let expected_linear = float_of_json_exn (member "expected_linear" c) in
      check_float_exact (sprintf "uv_to_st(%g)" input) ~expected:result ~actual;
      check_float_exact
        (sprintf "uv_to_st(%g) linear" input)
        ~expected:expected_linear
        ~actual
    | "st_roundtrip" ->
      let actual = S2.S2_coords.uv_to_st (S2.S2_coords.st_to_uv input) in
      check_float (sprintf "st_roundtrip(%g)" input) ~expected:result ~actual
    | "uv_roundtrip" ->
      let actual = S2.S2_coords.st_to_uv (S2.S2_coords.uv_to_st input) in
      check_float (sprintf "uv_roundtrip(%g)" input) ~expected:result ~actual
    | _ -> failwith (sprintf "unknown op: %s" op))
;;

let test_st_to_ij fixture () =
  let cases = to_list (member "st_to_ij" fixture) in
  List.iter cases ~f:(fun c ->
    let input = float_of_json_exn (member "input" c) in
    let expected = int_of_json_exn (member "expected" c) in
    let actual = S2.S2_coords.st_to_ij input in
    (check int) (sprintf "st_to_ij(%g)" input) expected actual)
;;

let test_si_ti_to_st fixture () =
  let cases = to_list (member "si_ti_to_st" fixture) in
  List.iter cases ~f:(fun c ->
    let si = int_of_json_exn (member "si" c) in
    let expected = float_of_json_exn (member "st" c) in
    let actual = S2.S2_coords.si_ti_to_st si in
    check_float_exact (sprintf "si_ti_to_st(%d)" si) ~expected ~actual)
;;

let test_st_to_si_ti fixture () =
  let cases = to_list (member "st_to_si_ti" fixture) in
  List.iter cases ~f:(fun c ->
    let input = float_of_json_exn (member "input_st" c) in
    let expected_si = int_of_json_exn (member "si_ti" c) in
    let actual_si = S2.S2_coords.st_to_si_ti input in
    (check int) (sprintf "st_to_si_ti(%g)" input) expected_si actual_si;
    let expected_rt = float_of_json_exn (member "roundtrip_st" c) in
    let actual_rt = S2.S2_coords.si_ti_to_st actual_si in
    check_float_exact
      (sprintf "si_ti roundtrip(%g)" input)
      ~expected:expected_rt
      ~actual:actual_rt)
;;

let test_ij_to_st_min fixture () =
  let cases = to_list (member "ij_to_st_min" fixture) in
  List.iter cases ~f:(fun c ->
    let i = int_of_json_exn (member "i" c) in
    let expected = float_of_json_exn (member "st_min" c) in
    let actual = S2.S2_coords.ij_to_st_min i in
    check_float_exact (sprintf "ij_to_st_min(%d)" i) ~expected ~actual)
;;

let test_face_uv_to_xyz fixture () =
  let cases = to_list (member "face_uv_to_xyz" fixture) in
  List.iter cases ~f:(fun c ->
    let face = int_of_json_exn (member "face" c) in
    let u = float_of_json_exn (member "u" c) in
    let v = float_of_json_exn (member "v" c) in
    let expected = r3_vector_of_json (member "result" c) in
    match member "cross_dot" c with
    | `Null ->
      (match member "next_start" c with
       | `Null ->
         let actual = S2.S2_coords.face_uv_to_xyz face u v in
         check_r3_vector_exact
           (sprintf "face_uv_to_xyz(%d,%g,%g)" face u v)
           ~expected
           ~actual;
         (match member "norm" c with
          | `Null -> ()
          | norm_j ->
            let expected_norm = r3_vector_of_json norm_j in
            let actual_norm = S2.S2_coords.get_norm face in
            check_r3_vector_exact
              (sprintf "get_norm(%d)" face)
              ~expected:expected_norm
              ~actual:actual_norm)
       | next_j ->
         let actual = S2.S2_coords.face_uv_to_xyz face u v in
         check_r3_vector_exact (sprintf "hilbert end(%d)" face) ~expected ~actual;
         let next_start = r3_vector_of_json next_j in
         let actual_next = S2.S2_coords.face_uv_to_xyz ((face + 1) mod 6) (-1.0) (-1.0) in
         check_r3_vector_exact
           (sprintf "hilbert start(%d)" face)
           ~expected:next_start
           ~actual:actual_next)
    | cross_j ->
      let expected_cross_dot = float_of_json_exn cross_j in
      let u_axis = S2.S2_coords.get_u_axis face in
      let v_axis = S2.S2_coords.get_v_axis face in
      let center = S2.S2_coords.face_uv_to_xyz face 0.0 0.0 in
      let cross_dot = S2.R3_vector.dot (S2.R3_vector.cross u_axis v_axis) center in
      check_float_exact
        (sprintf "right-handed(%d)" face)
        ~expected:expected_cross_dot
        ~actual:(Float_u.to_float cross_dot))
;;

let test_face_xyz_to_uvw fixture () =
  let cases = to_list (member "face_xyz_to_uvw" fixture) in
  List.iter cases ~f:(fun c ->
    let face = int_of_json_exn (member "face" c) in
    let input = r3_vector_of_json (member "input" c) in
    let expected = r3_vector_of_json (member "expected" c) in
    let result = r3_vector_of_json (member "result" c) in
    check_r3_vector_exact
      (sprintf "face_xyz_to_uvw(%d) expected" face)
      ~expected
      ~actual:result;
    let actual = S2.S2_coords.face_xyz_to_uvw face input in
    check_r3_vector_exact (sprintf "face_xyz_to_uvw(%d) actual" face) ~expected ~actual)
;;

let test_uv_norms fixture () =
  let cases = to_list (member "uv_norms" fixture) in
  List.iter cases ~f:(fun c ->
    let face = int_of_json_exn (member "face" c) in
    let x = float_of_json_exn (member "x" c) in
    let expected_u_norm = r3_vector_of_json (member "u_norm" c) in
    let expected_v_norm = r3_vector_of_json (member "v_norm" c) in
    let expected_u_angle = float_of_json_exn (member "u_angle" c) in
    let expected_v_angle = float_of_json_exn (member "v_angle" c) in
    let actual_u_norm = S2.S2_coords.get_u_norm face x in
    let actual_v_norm = S2.S2_coords.get_v_norm face x in
    check_r3_vector_exact
      (sprintf "u_norm(%d,%g)" face x)
      ~expected:expected_u_norm
      ~actual:actual_u_norm;
    check_r3_vector_exact
      (sprintf "v_norm(%d,%g)" face x)
      ~expected:expected_v_norm
      ~actual:actual_v_norm;
    let p1 = S2.S2_coords.face_uv_to_xyz face x (-1.0) in
    let p2 = S2.S2_coords.face_uv_to_xyz face x 1.0 in
    let q1 = S2.S2_coords.face_uv_to_xyz face (-1.0) x in
    let q2 = S2.S2_coords.face_uv_to_xyz face 1.0 x in
    let actual_u_angle =
      Float_u.to_float
        (S2.S1_angle.radians
           (S2.R3_vector.angle (S2.R3_vector.cross p1 p2) actual_u_norm))
    in
    let actual_v_angle =
      Float_u.to_float
        (S2.S1_angle.radians
           (S2.R3_vector.angle (S2.R3_vector.cross q1 q2) actual_v_norm))
    in
    check_float_exact
      (sprintf "u_angle(%d,%g)" face x)
      ~expected:expected_u_angle
      ~actual:actual_u_angle;
    check_float_exact
      (sprintf "v_angle(%d,%g)" face x)
      ~expected:expected_v_angle
      ~actual:actual_v_angle)
;;

let test_uvw_axis fixture () =
  let cases = to_list (member "uvw_axis" fixture) in
  List.iter cases ~f:(fun c ->
    let face = int_of_json_exn (member "face" c) in
    let expected_u = r3_vector_of_json (member "u_axis" c) in
    let expected_v = r3_vector_of_json (member "v_axis" c) in
    let expected_norm = r3_vector_of_json (member "norm" c) in
    let expected_center = r3_vector_of_json (member "center" c) in
    let expected_u_from_face = r3_vector_of_json (member "u_from_face" c) in
    let expected_v_from_face = r3_vector_of_json (member "v_from_face" c) in
    let expected_cross_dot = float_of_json_exn (member "cross_dot" c) in
    let actual_u = S2.S2_coords.get_u_axis face in
    let actual_v = S2.S2_coords.get_v_axis face in
    let actual_norm = S2.S2_coords.get_norm face in
    let actual_center = S2.S2_coords.face_uv_to_xyz face 0.0 0.0 in
    let actual_u_from_face =
      S2.R3_vector.sub (S2.S2_coords.face_uv_to_xyz face 1.0 0.0) actual_center
    in
    let actual_v_from_face =
      S2.R3_vector.sub (S2.S2_coords.face_uv_to_xyz face 0.0 1.0) actual_center
    in
    check_r3_vector_exact
      (sprintf "u_axis(%d)" face)
      ~expected:expected_u
      ~actual:actual_u;
    check_r3_vector_exact
      (sprintf "v_axis(%d)" face)
      ~expected:expected_v
      ~actual:actual_v;
    check_r3_vector_exact
      (sprintf "norm(%d)" face)
      ~expected:expected_norm
      ~actual:actual_norm;
    check_r3_vector_exact
      (sprintf "center(%d)" face)
      ~expected:expected_center
      ~actual:actual_center;
    check_r3_vector_exact
      (sprintf "u_from_face(%d)" face)
      ~expected:expected_u_from_face
      ~actual:actual_u_from_face;
    check_r3_vector_exact
      (sprintf "v_from_face(%d)" face)
      ~expected:expected_v_from_face
      ~actual:actual_v_from_face;
    let actual_cross_dot =
      S2.R3_vector.dot (S2.R3_vector.cross actual_u actual_v) actual_norm
    in
    check_float_exact
      (sprintf "cross_dot(%d)" face)
      ~expected:expected_cross_dot
      ~actual:(Float_u.to_float actual_cross_dot);
    let uvw0 = S2.S2_coords.get_uvw_axis face 0 in
    let uvw1 = S2.S2_coords.get_uvw_axis face 1 in
    let uvw2 = S2.S2_coords.get_uvw_axis face 2 in
    let expected_uvw0 = r3_vector_of_json (member "uvw_axis_0" c) in
    let expected_uvw1 = r3_vector_of_json (member "uvw_axis_1" c) in
    let expected_uvw2 = r3_vector_of_json (member "uvw_axis_2" c) in
    check_r3_vector_exact
      (sprintf "uvw_axis(%d,0)" face)
      ~expected:expected_uvw0
      ~actual:uvw0;
    check_r3_vector_exact
      (sprintf "uvw_axis(%d,1)" face)
      ~expected:expected_uvw1
      ~actual:uvw1;
    check_r3_vector_exact
      (sprintf "uvw_axis(%d,2)" face)
      ~expected:expected_uvw2
      ~actual:uvw2)
;;

let test_uvw_face fixture () =
  let cases = to_list (member "uvw_face" fixture) in
  List.iter cases ~f:(fun c ->
    let face = int_of_json_exn (member "face" c) in
    let axis = int_of_json_exn (member "axis" c) in
    let expected_neg = int_of_json_exn (member "uvw_face_neg" c) in
    let expected_pos = int_of_json_exn (member "uvw_face_pos" c) in
    let expected_neg_from_get = int_of_json_exn (member "neg_face" c) in
    let expected_pos_from_get = int_of_json_exn (member "pos_face" c) in
    (check int)
      (sprintf "uvw_face_neg(%d,%d) consistency" face axis)
      expected_neg_from_get
      expected_neg;
    (check int)
      (sprintf "uvw_face_pos(%d,%d) consistency" face axis)
      expected_pos_from_get
      expected_pos;
    let actual_neg = S2.S2_coords.get_uvw_face face axis 0 in
    let actual_pos = S2.S2_coords.get_uvw_face face axis 1 in
    (check int) (sprintf "uvw_face(%d,%d,0)" face axis) expected_neg actual_neg;
    (check int) (sprintf "uvw_face(%d,%d,1)" face axis) expected_pos actual_pos)
;;

let test_face_xyz_to_uv fixture () =
  let cases = to_list (member "face_xyz_to_uv" fixture) in
  List.iter cases ~f:(fun c ->
    let face = int_of_json_exn (member "face" c) in
    let point = r3_vector_of_json (member "point" c) in
    let expected_ok = bool_of_json_exn (member "ok" c) in
    let actual = S2.S2_coords.face_xyz_to_uv face point in
    if expected_ok
    then
      if S2.R2_point.Option.is_none actual
      then Alcotest.fail (sprintf "face_xyz_to_uv(%d): expected Some, got None" face)
      else (
        let expected =
          S2.R2_point.create
            ~x:(Float_u.of_float (float_of_json_exn (member "u" c)))
            ~y:(Float_u.of_float (float_of_json_exn (member "v" c)))
        in
        check_r2_point_exact
          (sprintf "face_xyz_to_uv(%d)" face)
          ~expected
          ~actual:(S2.R2_point.Option.unchecked_value actual))
    else if S2.R2_point.Option.is_some actual
    then Alcotest.fail (sprintf "face_xyz_to_uv(%d): expected None, got Some" face))
;;

let test_get_face fixture () =
  let cases = to_list (member "get_face" fixture) in
  List.iter cases ~f:(fun c ->
    let point = r3_vector_of_json (member "point" c) in
    let expected = int_of_json_exn (member "face" c) in
    let actual = S2.S2_coords.get_face point in
    (check int)
      (sprintf
         "get_face(%g,%g,%g)"
         (Float_u.to_float (S2.R3_vector.x point))
         (Float_u.to_float (S2.R3_vector.y point))
         (Float_u.to_float (S2.R3_vector.z point)))
      expected
      actual)
;;

let test_xyz_to_face_uv fixture () =
  let cases = to_list (member "xyz_to_face_uv" fixture) in
  List.iter cases ~f:(fun c ->
    let point = r3_vector_of_json (member "point" c) in
    let expected_face = int_of_json_exn (member "face" c) in
    let expected_u = float_of_json_exn (member "u" c) in
    let expected_v = float_of_json_exn (member "v" c) in
    let actual_face, actual_u, actual_v = S2.S2_coords.xyz_to_face_uv point in
    (check int) "xyz_to_face_uv face" expected_face actual_face;
    check_float_exact "xyz_to_face_uv u" ~expected:expected_u ~actual:actual_u;
    check_float_exact "xyz_to_face_uv v" ~expected:expected_v ~actual:actual_v)
;;

let () =
  let fixture = load_fixture "s2coords.json" in
  Alcotest.run
    "S2_coords"
    [ "constants", [ test_case "Constants" `Quick (test_constants fixture) ]
    ; ( "st_uv_conversions"
      , [ test_case "ST_UV_Conversions" `Quick (test_st_uv_conversions fixture) ] )
    ; "st_to_ij", [ test_case "STtoIJ" `Quick (test_st_to_ij fixture) ]
    ; "si_ti_to_st", [ test_case "SiTitoST" `Quick (test_si_ti_to_st fixture) ]
    ; "st_to_si_ti", [ test_case "STtoSiTi" `Quick (test_st_to_si_ti fixture) ]
    ; "ij_to_st_min", [ test_case "IJtoSTMin" `Quick (test_ij_to_st_min fixture) ]
    ; "face_uv_to_xyz", [ test_case "FaceUVtoXYZ" `Quick (test_face_uv_to_xyz fixture) ]
    ; ( "face_xyz_to_uvw"
      , [ test_case "FaceXYZtoUVW" `Quick (test_face_xyz_to_uvw fixture) ] )
    ; "uv_norms", [ test_case "UVNorms" `Quick (test_uv_norms fixture) ]
    ; "uvw_axis", [ test_case "UVWAxis" `Quick (test_uvw_axis fixture) ]
    ; "uvw_face", [ test_case "UVWFace" `Quick (test_uvw_face fixture) ]
    ; "face_xyz_to_uv", [ test_case "FaceXYZtoUV" `Quick (test_face_xyz_to_uv fixture) ]
    ; "get_face", [ test_case "GetFace" `Quick (test_get_face fixture) ]
    ; "xyz_to_face_uv", [ test_case "XYZtoFaceUV" `Quick (test_xyz_to_face_uv fixture) ]
    ]
;;
