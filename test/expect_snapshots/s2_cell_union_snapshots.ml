open Core

(* Hand-crafted set operation examples for S2_cell_union.

   The building blocks below are:
   - [face n]         : the nth face cell as a raw Int64 id
   - [face0_child k]  : the kth level-1 child of face 0 as a raw Int64 id

   These give us concrete normalized and non-normalized inputs to exercise
   normalize, union, intersection, difference, contains_union, and
   intersects_union. *)

let face n = S2.S2_cell_id.id (S2.S2_cell_id.from_face_exn n)

let face0_child k =
  S2.S2_cell_id.id (S2.S2_cell_id.child_exn (S2.S2_cell_id.from_face_exn 0) k)
;;

let make ids = S2.S2_cell_union.create ids
let verbatim ids = S2.S2_cell_union.from_verbatim ids

let print_cu label cu =
  printf "%s: %s\n" label (Sexp.to_string ([%sexp_of: S2.S2_cell_union.t] cu))
;;

let%expect_test "empty_sexp" =
  print_cu "empty" (S2.S2_cell_union.empty ());
  [%expect {| empty: () |}]
;;

let%expect_test "whole_sphere_sexp" =
  print_cu "whole_sphere" (S2.S2_cell_union.whole_sphere ());
  [%expect {| whole_sphere: (1 3 5 7 9 b) |}]
;;

let%expect_test "small_union_sexp" =
  let cu = verbatim [| face0_child 0; face0_child 1; face 1 |] in
  print_cu "three-cell" cu;
  [%expect {| three-cell: (04 0c 3) |}]
;;

let%expect_test "normalize_collapses_four_siblings" =
  (* The four level-1 children of face 0 should collapse back to face 0. *)
  let input = verbatim [| face0_child 0; face0_child 1; face0_child 2; face0_child 3 |] in
  print_cu "input" input;
  let normalized = S2.S2_cell_union.normalize input in
  print_cu "normalized" normalized;
  let renormalized = S2.S2_cell_union.normalize normalized in
  print_cu "renormalized" renormalized;
  printf
    "equal normalized/renormalized: %b\n"
    (S2.S2_cell_union.equal normalized renormalized);
  printf "is_normalized input: %b\n" (S2.S2_cell_union.is_normalized input);
  printf "is_normalized normalized: %b\n" (S2.S2_cell_union.is_normalized normalized);
  [%expect
    {|
    input: (04 0c 14 1c)
    normalized: (1)
    renormalized: (1)
    equal normalized/renormalized: true
    is_normalized input: false
    is_normalized normalized: true
    |}]
;;

let%expect_test "normalize_is_idempotent_on_mixed_input" =
  (* Duplicate face0 plus two children of face0; should collapse to face0. *)
  let input = verbatim [| face 0; face0_child 0; face0_child 1; face 0 |] in
  printf "is_valid input: %b\n" (S2.S2_cell_union.is_valid input);
  let n1 = S2.S2_cell_union.normalize input in
  let n2 = S2.S2_cell_union.normalize n1 in
  print_cu "normalize once" n1;
  print_cu "normalize twice" n2;
  printf "idempotent: %b\n" (S2.S2_cell_union.equal n1 n2);
  [%expect
    {|
    is_valid input: false
    normalize once: (1)
    normalize twice: (1)
    idempotent: true
    |}]
;;

let%expect_test "union_of_disjoint_faces" =
  let a = make [| face 0 |] in
  let b = make [| face 2 |] in
  let u = S2.S2_cell_union.union a b in
  print_cu "face0 U face2" u;
  printf "num_cells: %d\n" (S2.S2_cell_union.num_cells u);
  [%expect {|
    face0 U face2: (1 5)
    num_cells: 2
    |}]
;;

let%expect_test "union_of_children_collapses_to_parent" =
  let a = make [| face0_child 0; face0_child 1 |] in
  let b = make [| face0_child 2; face0_child 3 |] in
  let u = S2.S2_cell_union.union a b in
  print_cu "union" u;
  [%expect {| union: (1) |}]
;;

let%expect_test "intersection_nested" =
  (* face0 contains any of its children. *)
  let parent = make [| face 0 |] in
  let children = make [| face0_child 1; face0_child 2 |] in
  let i = S2.S2_cell_union.intersection parent children in
  print_cu "parent ^ children" i;
  let i_self = S2.S2_cell_union.intersection parent parent in
  print_cu "parent ^ parent" i_self;
  [%expect {|
    parent ^ children: (0c 14)
    parent ^ parent: (1)
    |}]
;;

let%expect_test "intersection_disjoint_is_empty" =
  let a = make [| face 0 |] in
  let b = make [| face 3 |] in
  let i = S2.S2_cell_union.intersection a b in
  print_cu "face0 ^ face3" i;
  printf "is_empty: %b\n" (S2.S2_cell_union.is_empty i);
  [%expect {|
    face0 ^ face3: ()
    is_empty: true
    |}]
;;

let%expect_test "difference_removes_child" =
  (* face0 - child0 leaves the other three children of face0. *)
  let a = make [| face 0 |] in
  let b = make [| face0_child 0 |] in
  let d = S2.S2_cell_union.difference a b in
  print_cu "face0 - child0" d;
  [%expect {| face0 - child0: (0c 14 1c) |}]
;;

let%expect_test "difference_of_self_is_empty" =
  let a = make [| face 0; face 2; face 4 |] in
  let d = S2.S2_cell_union.difference a a in
  print_cu "a - a" d;
  printf "is_empty: %b\n" (S2.S2_cell_union.is_empty d);
  [%expect {|
    a - a: ()
    is_empty: true
    |}]
;;

let%expect_test "contains_union_nested" =
  let parent = make [| face 0 |] in
  let child = make [| face0_child 2 |] in
  printf "face0 contains child: %b\n" (S2.S2_cell_union.contains_union parent child);
  printf "child contains face0: %b\n" (S2.S2_cell_union.contains_union child parent);
  [%expect {|
    face0 contains child: true
    child contains face0: false
    |}]
;;

let%expect_test "contains_union_disjoint_and_empty" =
  let a = make [| face 0 |] in
  let b = make [| face 3 |] in
  let empty = S2.S2_cell_union.empty () in
  printf "face0 contains face3: %b\n" (S2.S2_cell_union.contains_union a b);
  printf "face0 contains empty: %b\n" (S2.S2_cell_union.contains_union a empty);
  printf "empty contains face0: %b\n" (S2.S2_cell_union.contains_union empty a);
  printf "empty contains empty: %b\n" (S2.S2_cell_union.contains_union empty empty);
  [%expect
    {|
    face0 contains face3: false
    face0 contains empty: true
    empty contains face0: false
    empty contains empty: true
    |}]
;;

let%expect_test "intersects_union_nested_and_disjoint" =
  let parent = make [| face 0 |] in
  let child = make [| face0_child 1 |] in
  let other = make [| face 3 |] in
  let empty = S2.S2_cell_union.empty () in
  printf "face0 intersects child: %b\n" (S2.S2_cell_union.intersects_union parent child);
  printf "face0 intersects face3: %b\n" (S2.S2_cell_union.intersects_union parent other);
  printf "face0 intersects empty: %b\n" (S2.S2_cell_union.intersects_union parent empty);
  [%expect
    {|
    face0 intersects child: true
    face0 intersects face3: false
    face0 intersects empty: false
    |}]
;;
