open Core
module C = S2.S2_cell_id

let%expect_test "token_roundtrip" =
  let tokens = [ "1"; "3"; "5"; "89c25c"; "89c25c4"; "89c25c4722e39291" ] in
  List.iter tokens ~f:(fun tok ->
    let cell = C.from_token tok in
    printf "%s -> %s\n" tok (C.to_token cell));
  [%expect
    {|
    1 -> 1
    3 -> 3
    5 -> 5
    89c25c -> 89c25c
    89c25c4 -> 89c25c4
    89c25c4722e39291 -> 89c25c4722e39291
    |}]
;;

let%expect_test "face_level_pos_decomposition" =
  let show name c =
    printf
      "%s: face=%d level=%d pos=%Ld is_leaf=%b is_face=%b\n"
      name
      (C.face c)
      (C.level c)
      (C.pos c)
      (C.is_leaf c)
      (C.is_face c)
  in
  show "face0" (C.from_face_exn 0);
  show "face3" (C.from_face_exn 3);
  show "face5" (C.from_face_exn 5);
  show "token_89c25c" (C.from_token "89c25c");
  show "token_89c25c4722e39291" (C.from_token "89c25c4722e39291");
  [%expect
    {|
    face0: face=0 level=0 pos=1152921504606846976 is_leaf=false is_face=true
    face3: face=3 level=0 pos=1152921504606846976 is_leaf=false is_face=true
    face5: face=5 level=0 pos=1152921504606846976 is_leaf=false is_face=true
    token_89c25c: face=4 level=9 pos=703225646892974080 is_leaf=false is_face=false
    token_89c25c4722e39291: face=4 level=30 pos=703225952420991633 is_leaf=true is_face=false
    |}]
;;

let%expect_test "to_string_and_sexp" =
  let show name c =
    printf
      "%s: to_string=%s sexp=%s\n"
      name
      (C.to_string c)
      (Sexp.to_string [%sexp (c : C.t)])
  in
  show "face0" (C.from_face_exn 0);
  show "face2" (C.from_face_exn 2);
  show "token_89c25c" (C.from_token "89c25c");
  show "none" C.none;
  [%expect
    {|
    face0: to_string=0/ sexp=1152921504606846976
    face2: to_string=2/ sexp=5764607523034234880
    token_89c25c: to_string=4/103201023 sexp=-8520146389961801728
    none: to_string=Invalid: 0000000000000000 sexp=0
    |}]
;;

let%expect_test "parent_child_tree" =
  let face0 = C.from_face_exn 0 in
  printf "face0 to_string=%s\n" (C.to_string face0);
  let child0 = C.child_exn face0 0 in
  let child1 = C.child_exn face0 1 in
  let child2 = C.child_exn face0 2 in
  let child3 = C.child_exn face0 3 in
  let show name c =
    printf
      "%s: level=%d to_string=%s child_pos=%d\n"
      name
      (C.level c)
      (C.to_string c)
      (C.child_position c)
  in
  show "child0" child0;
  show "child1" child1;
  show "child2" child2;
  show "child3" child3;
  printf "parent_exn(child2) == face0: %b\n" (C.equal (C.parent_exn child2) face0);
  printf "contains(face0, child0) = %b\n" (C.contains face0 child0);
  [%expect
    {|
    face0 to_string=0/
    child0: level=1 to_string=0/0 child_pos=0
    child1: level=1 to_string=0/1 child_pos=1
    child2: level=1 to_string=0/2 child_pos=2
    child3: level=1 to_string=0/3 child_pos=3
    parent_exn(child2) == face0: true
    contains(face0, child0) = true
    |}]
;;

let%expect_test "child_begin_end_range" =
  let face0 = C.from_face_exn 0 in
  let cb = C.child_begin face0 in
  let ce = C.child_end face0 in
  let rmin = C.range_min face0 in
  let rmax = C.range_max face0 in
  printf "child_begin=%s\n" (C.to_token cb);
  printf "child_end=%s\n" (C.to_token ce);
  printf "range_min=%s (leaf=%b)\n" (C.to_token rmin) (C.is_leaf rmin);
  printf "range_max=%s (leaf=%b)\n" (C.to_token rmax) (C.is_leaf rmax);
  printf "contains(face0, range_min) = %b\n" (C.contains face0 rmin);
  printf "contains(face0, range_max) = %b\n" (C.contains face0 rmax);
  [%expect
    {|
    child_begin=04
    child_end=24
    range_min=0000000000000001 (leaf=true)
    range_max=1fffffffffffffff (leaf=true)
    contains(face0, range_min) = true
    contains(face0, range_max) = true
    |}]
;;

let%expect_test "from_face_exn_out_of_range_raises" =
  Expect_test_helpers_core.show_raise (fun () -> ignore (C.from_face_exn 6 : C.t));
  [%expect {| (raised ("S2CellId.from_face_exn: face out of range" (face 6))) |}]
;;

let%expect_test "parent_exn_on_face_cell_raises" =
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (C.parent_exn (C.from_face_exn 0) : C.t));
  [%expect
    {| (raised ("S2CellId.parent_exn: already a face cell" (id 1152921504606846976))) |}]
;;

let%expect_test "child_exn_leaf_or_bad_index_raises" =
  let face0 = C.from_face_exn 0 in
  Expect_test_helpers_core.show_raise (fun () -> ignore (C.child_exn face0 4 : C.t));
  [%expect
    {|
    (raised (
      "S2CellId.child_exn: invalid leaf cell or child index"
      (id  1152921504606846976)
      (pos 4)))
    |}]
;;

let%expect_test "from_token_invalid_returns_none" =
  let cases = [ ""; "X"; "zzz" ] in
  List.iter cases ~f:(fun tok ->
    let c = C.from_token tok in
    printf "%S -> equal none: %b\n" tok (C.equal c C.none));
  [%expect
    {|
    "" -> equal none: true
    "X" -> equal none: true
    "zzz" -> equal none: true
    |}]
;;
