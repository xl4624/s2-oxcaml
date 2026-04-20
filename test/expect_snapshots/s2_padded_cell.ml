open Core

(* No error-path snapshot: [S2_padded_cell] exposes no [_exn] functions. *)

let%expect_test "face0_no_padding_sexp" =
  let face0 = S2.S2_cell_id.from_face_exn 0 in
  let pc = S2.S2_padded_cell.create face0 ~padding:#0.0 in
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_padded_cell.t] pc));
  [%expect
    {| ((id 1152921504606846976)(padding 0)(bound((x((lo -1)(hi 1)))(y((lo -1)(hi 1)))))(middle((x((lo -0)(hi 0)))(y((lo -0)(hi 0)))))(i_lo 0)(j_lo 0)(orientation 0)(level 0)) |}]
;;

let%expect_test "face0_with_padding_accessors" =
  let face0 = S2.S2_cell_id.from_face_exn 0 in
  let pc = S2.S2_padded_cell.create face0 ~padding:#0.25 in
  printf
    "padding=%s level=%d orientation=%d\n"
    (Float_u.to_string (S2.S2_padded_cell.padding pc))
    (S2.S2_padded_cell.level pc)
    (S2.S2_padded_cell.orientation pc);
  printf "bound: %s\n" (S2.R2_rect.to_string (S2.S2_padded_cell.bound pc));
  [%expect
    {|
    padding=0.25 level=0 orientation=0
    bound: {x=[-1.25, 1.25], y=[-1.25, 1.25]}
    |}]
;;

let%expect_test "child_ij_quadrants" =
  let face0 = S2.S2_cell_id.from_face_exn 0 in
  let parent = S2.S2_padded_cell.create face0 ~padding:#0.0 in
  List.iter
    [ 0, 0; 0, 1; 1, 0; 1, 1 ]
    ~f:(fun (i, j) ->
      let c = S2.S2_padded_cell.child_ij parent ~i ~j in
      printf
        "(i=%d,j=%d): level=%d orientation=%d\n"
        i
        j
        (S2.S2_padded_cell.level c)
        (S2.S2_padded_cell.orientation c));
  [%expect
    {|
    (i=0,j=0): level=1 orientation=1
    (i=0,j=1): level=1 orientation=0
    (i=1,j=0): level=1 orientation=3
    (i=1,j=1): level=1 orientation=0
    |}]
;;

let%expect_test "child_ij_of_pos_roundtrip" =
  let face0 = S2.S2_cell_id.from_face_exn 0 in
  let parent = S2.S2_padded_cell.create face0 ~padding:#0.0 in
  for pos = 0 to 3 do
    let #(i, j) = S2.S2_padded_cell.child_ij_of_pos parent ~pos in
    printf "pos=%d -> (i=%d,j=%d)\n" pos i j
  done;
  [%expect
    {|
    pos=0 -> (i=0,j=0)
    pos=1 -> (i=0,j=1)
    pos=2 -> (i=1,j=1)
    pos=3 -> (i=1,j=0)
    |}]
;;
