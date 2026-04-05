open Core

let%expect_test "constants" =
  printf
    "max_cell_level=%d limit_ij=%d max_si_ti=%d\n"
    S2.S2_coords.max_cell_level
    S2.S2_coords.limit_ij
    S2.S2_coords.max_si_ti;
  [%expect {| max_cell_level=30 limit_ij=1073741824 max_si_ti=2147483648 |}]
;;

let%expect_test "st_uv_roundtrip" =
  List.iter [ 0.0; 0.25; 0.5; 0.75; 1.0 ] ~f:(fun s ->
    let u = S2.S2_coords.st_to_uv s in
    let back = S2.S2_coords.uv_to_st u in
    printf "s=%.17g -> u=%.17g -> back=%.17g\n" s u back);
  [%expect
    {|
    s=0 -> u=-1 -> back=0
    s=0.25 -> u=-0.41666666666666663 -> back=0.25
    s=0.5 -> u=0 -> back=0.5
    s=0.75 -> u=0.41666666666666663 -> back=0.75
    s=1 -> u=1 -> back=1
    |}]
;;

let%expect_test "face_uv_to_xyz_face0" =
  let p = S2.S2_coords.face_uv_to_xyz 0 0.0 0.0 in
  printf
    "(%g, %g, %g)\n"
    (Float_u.to_float (S2.R3_vector.x p))
    (Float_u.to_float (S2.R3_vector.y p))
    (Float_u.to_float (S2.R3_vector.z p));
  [%expect {| (1, 0, 0) |}]
;;

let%expect_test "get_face" =
  List.iter
    [ 1.0, 0.0, 0.0, 0
    ; 0.0, 1.0, 0.0, 1
    ; 0.0, 0.0, 1.0, 2
    ; -1.0, 0.0, 0.0, 3
    ; 0.0, -1.0, 0.0, 4
    ; 0.0, 0.0, -1.0, 5
    ]
    ~f:(fun (x, y, z, expected_face) ->
      let p =
        S2.R3_vector.create
          ~x:(Float_u.of_float x)
          ~y:(Float_u.of_float y)
          ~z:(Float_u.of_float z)
      in
      printf "(%g,%g,%g) -> face %d\n" x y z (S2.S2_coords.get_face p);
      assert (S2.S2_coords.get_face p = expected_face));
  [%expect
    {|
    (1,0,0) -> face 0
    (0,1,0) -> face 1
    (0,0,1) -> face 2
    (-1,0,0) -> face 3
    (0,-1,0) -> face 4
    (0,0,-1) -> face 5 |}]
;;

let%expect_test "face_xyz_to_uv_exn_face0" =
  let p = S2.R3_vector.create ~x:#1.0 ~y:#0.5 ~z:#0.25 in
  let uv = S2.S2_coords.face_xyz_to_uv_exn 0 p in
  printf
    "u=%.17g v=%.17g\n"
    (Float_u.to_float (S2.R2_point.x uv))
    (Float_u.to_float (S2.R2_point.y uv));
  [%expect {| u=0.5 v=0.25 |}]
;;

let%expect_test "face_xyz_to_uv_exn_wrong_face_raises" =
  let p = S2.R3_vector.create ~x:(-#1.0) ~y:#0.0 ~z:#0.0 in
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (S2.S2_coords.face_xyz_to_uv_exn 0 p : S2.R2_point.t));
  [%expect
    {| (raised ("S2Coords.face_xyz_to_uv_exn: invalid face for point" (face 0))) |}]
;;

let%expect_test "face_xyz_to_uv_match_optional_u" =
  let p = S2.R3_vector.create ~x:#1.0 ~y:#0.5 ~z:#0.25 in
  let result =
    match%optional_u.S2.R2_point.Option S2.S2_coords.face_xyz_to_uv 0 p with
    | None -> "none"
    | Some uv ->
      sprintf
        "u=%.17g v=%.17g"
        (Float_u.to_float (S2.R2_point.x uv))
        (Float_u.to_float (S2.R2_point.y uv))
  in
  printf "%s\n" result;
  [%expect {| u=0.5 v=0.25 |}]
;;

let%expect_test "face_xyz_to_uv_wrong_face_none" =
  let p = S2.R3_vector.create ~x:(-#1.0) ~y:#0.0 ~z:#0.0 in
  let result =
    match%optional_u.S2.R2_point.Option S2.S2_coords.face_xyz_to_uv 0 p with
    | None -> "none"
    | Some _ -> "some"
  in
  printf "%s\n" result;
  [%expect {| none |}]
;;

let%expect_test "axes_right_handed" =
  for face = 0 to 5 do
    let u = S2.S2_coords.get_u_axis face in
    let v = S2.S2_coords.get_v_axis face in
    let n = S2.S2_coords.get_norm face in
    let cross_dot = S2.R3_vector.dot (S2.R3_vector.cross u v) n in
    printf "face %d: u×v·n = %.17g\n" face (Float_u.to_float cross_dot)
  done;
  [%expect
    {|
    face 0: u×v·n = 1
    face 1: u×v·n = 1
    face 2: u×v·n = 1
    face 3: u×v·n = 1
    face 4: u×v·n = 1
    face 5: u×v·n = 1 |}]
;;
