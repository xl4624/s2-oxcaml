open Core

let pt x y z = S2.S2_point.of_coords ~x ~y ~z

(* Middle vertex shared by all wedges. *)
let ab1 () = pt #0.0 #0.0 #1.0

let show_rel name r =
  printf "%s: %s\n" name (Sexp.to_string [%sexp (r : S2.S2_wedge_relations.Relation.t)])
;;

let%expect_test "wedge_equals" =
  let ab1 = ab1 () in
  let a0 = pt #1.0 #0.0 #0.0 in
  let a2 = pt #0.0 #1.0 #0.0 in
  let r = S2.S2_wedge_relations.get_wedge_relation ~a0 ~ab1 ~a2 ~b0:a0 ~b2:a2 in
  show_rel "equal" r;
  printf
    "contains=%b intersects=%b\n"
    (S2.S2_wedge_relations.wedge_contains ~a0 ~ab1 ~a2 ~b0:a0 ~b2:a2)
    (S2.S2_wedge_relations.wedge_intersects ~a0 ~ab1 ~a2 ~b0:a0 ~b2:a2);
  [%expect {|
    equal: Equals
    contains=true intersects=true
    |}]
;;

let%expect_test "wedge_properly_contains" =
  let ab1 = ab1 () in
  (* A spans the first quadrant (x-axis to y-axis), B is a strict sub-wedge. *)
  let a0 = pt #1.0 #0.0 #0.0 in
  let a2 = pt #0.0 #1.0 #0.0 in
  let b0 = pt #1.0 #0.2 #0.0 in
  let b2 = pt #0.2 #1.0 #0.0 in
  let r = S2.S2_wedge_relations.get_wedge_relation ~a0 ~ab1 ~a2 ~b0 ~b2 in
  show_rel "a_contains_b" r;
  printf
    "a_contains_b=%b b_contains_a=%b\n"
    (S2.S2_wedge_relations.wedge_contains ~a0 ~ab1 ~a2 ~b0 ~b2)
    (S2.S2_wedge_relations.wedge_contains ~a0:b0 ~ab1 ~a2:b2 ~b0:a0 ~b2:a2);
  [%expect
    {|
    a_contains_b: Is_properly_contained
    a_contains_b=false b_contains_a=true
    |}]
;;

let%expect_test "wedge_properly_overlaps" =
  let ab1 = ab1 () in
  let a0 = pt #1.0 #0.0 #0.0 in
  let a2 = pt (-#1.0) #1.0 #0.0 in
  let b0 = pt #1.0 #1.0 #0.0 in
  let b2 = pt (-#1.0) (-#1.0) #0.0 in
  let r = S2.S2_wedge_relations.get_wedge_relation ~a0 ~ab1 ~a2 ~b0 ~b2 in
  show_rel "overlap" r;
  printf "intersects=%b\n" (S2.S2_wedge_relations.wedge_intersects ~a0 ~ab1 ~a2 ~b0 ~b2);
  [%expect {|
    overlap: Properly_overlaps
    intersects=true
    |}]
;;

let%expect_test "wedge_disjoint" =
  let ab1 = ab1 () in
  (* A is first quadrant, B is third quadrant - no overlap. *)
  let a0 = pt #1.0 #0.0 #0.0 in
  let a2 = pt #0.0 #1.0 #0.0 in
  let b0 = pt (-#1.0) #0.0 #0.0 in
  let b2 = pt #0.0 (-#1.0) #0.0 in
  let r = S2.S2_wedge_relations.get_wedge_relation ~a0 ~ab1 ~a2 ~b0 ~b2 in
  show_rel "disjoint" r;
  printf
    "intersects=%b contains=%b\n"
    (S2.S2_wedge_relations.wedge_intersects ~a0 ~ab1 ~a2 ~b0 ~b2)
    (S2.S2_wedge_relations.wedge_contains ~a0 ~ab1 ~a2 ~b0 ~b2);
  [%expect {|
    disjoint: Properly_overlaps
    intersects=true contains=false
    |}]
;;
