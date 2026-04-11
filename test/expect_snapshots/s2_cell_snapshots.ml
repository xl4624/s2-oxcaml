open Core

(* No error-path snapshot: [S2_cell] exposes no [_exn] functions in
   [lib/s2_cell.mli]. Operations such as [S2_cell.from_face] and
   [S2_cell.child] do raise on invalid input, but those code paths are
   covered by the corresponding [S2_cell_id.from_face_exn] and
   [S2_cell_id.child_exn] snapshots in [s2_cell_id_snapshots.ml]. *)

let%expect_test "face0_sexp" =
  Stdlib.print_endline
    (Sexp.to_string ([%sexp_of: S2.S2_cell.t] (S2.S2_cell.from_face 0)));
  [%expect
    {|
    ((id 1152921504606846976)(face 0)(level 0)(orientation 0)(uv((x((lo -1)(hi 1)))(y((lo -1)(hi 1))))))
    |}]
;;

let%expect_test "leaf_sexp" =
  Stdlib.print_endline
    (Sexp.to_string
       ([%sexp_of: S2.S2_cell.t] (S2.S2_cell.of_cell_id (S2.S2_cell_id.from_token "0f"))));
  [%expect
    {|
    ((id 1080863910568919040)(face 0)(level 2)(orientation 3)(uv((x((lo -0.41666666666666663)(hi 0)))(y((lo 0)(hi 0.41666666666666663))))))
    |}]
;;

let%expect_test "all_six_faces" =
  for face = 0 to 5 do
    let cell = S2.S2_cell.from_face face in
    printf "face %d: level=%d\n" face (S2.S2_cell.level cell)
  done;
  [%expect
    {|
    face 0: level=0
    face 1: level=0
    face 2: level=0
    face 3: level=0
    face 4: level=0
    face 5: level=0 |}]
;;

let%expect_test "contains_point_center" =
  let cell = S2.S2_cell.from_face 0 in
  let center = S2.S2_cell.center cell in
  printf "%b\n" (S2.S2_cell.contains_point cell center);
  [%expect {| true |}]
;;

let%expect_test "distance_to_own_center" =
  let cell = S2.S2_cell.from_face 0 in
  let center = S2.S2_cell.center cell in
  printf
    "%.17g\n"
    (Float_u.to_float
       (S2.S1_chord_angle.length2 (S2.S2_cell.distance_to_point cell center)));
  [%expect {| 0 |}]
;;
