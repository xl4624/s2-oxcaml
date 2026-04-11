open Core

let pt x y z = S2.S2_point.of_coords ~x ~y ~z

let%expect_test "sign_ccw_and_cw" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let c = pt #0.0 #0.0 #1.0 in
  printf
    "abc=%b acb=%b\n"
    (S2.S2_predicates.sign a b c)
    (S2.S2_predicates.sign a c b);
  [%expect {| abc=true acb=false |}]
;;

let%expect_test "robust_sign_direction" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let c = pt #0.0 #0.0 #1.0 in
  let show name d =
    printf "%s: %s (int=%d)\n" name
      (match d with
       | S2.S2_predicates.Direction.Clockwise -> "CW"
       | Counter_clockwise -> "CCW"
       | Indeterminate -> "Indeterminate")
      (S2.S2_predicates.Direction.to_int d)
  in
  show "abc" (S2.S2_predicates.robust_sign a b c);
  show "acb" (S2.S2_predicates.robust_sign a c b);
  show "aab" (S2.S2_predicates.robust_sign a a b);
  [%expect {|
    abc: CCW (int=1)
    acb: CW (int=-1)
    aab: Indeterminate (int=0)
    |}]
;;

let%expect_test "robust_sign_identity" =
  let a = pt #1.0 #0.2 (-#0.1) in
  let b = pt #0.3 #1.0 #0.5 in
  let c = pt (-#0.2) #0.4 #1.0 in
  let d_abc = S2.S2_predicates.robust_sign a b c in
  let d_bca = S2.S2_predicates.robust_sign b c a in
  let d_cba = S2.S2_predicates.robust_sign c b a in
  printf
    "abc=%d bca=%d cba=%d\n"
    (S2.S2_predicates.Direction.to_int d_abc)
    (S2.S2_predicates.Direction.to_int d_bca)
    (S2.S2_predicates.Direction.to_int d_cba);
  [%expect {| abc=1 bca=1 cba=-1 |}]
;;

let%expect_test "ordered_ccw" =
  let o = pt #0.0 #0.0 #1.0 in
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let c = pt (-#1.0) #0.0 #0.0 in
  printf
    "abc_around_o=%b acb_around_o=%b\n"
    (S2.S2_predicates.ordered_ccw a b c o)
    (S2.S2_predicates.ordered_ccw a c b o);
  [%expect {| abc_around_o=true acb_around_o=false |}]
;;

let%expect_test "compare_distance_and_distances" =
  let x = pt #1.0 #0.0 #0.0 in
  let a = pt #1.0 #0.1 #0.0 in
  let b = pt #1.0 #0.2 #0.0 in
  printf "compare_distances x a b = %d\n" (S2.S2_predicates.compare_distances x a b);
  let r_small = S2.S1_chord_angle.of_radians #0.01 in
  let r_large = S2.S1_chord_angle.of_radians #1.0 in
  printf
    "compare_distance x a small=%d large=%d\n"
    (S2.S2_predicates.compare_distance x a r_small)
    (S2.S2_predicates.compare_distance x a r_large);
  [%expect {|
    compare_distances x a b = -1
    compare_distance x a small=1 large=-1
    |}]
;;

let%expect_test "sign_dot_prod" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let c = pt (-#1.0) #0.0 #0.0 in
  printf
    "ab=%d aa=%d ac=%d\n"
    (S2.S2_predicates.sign_dot_prod a b)
    (S2.S2_predicates.sign_dot_prod a a)
    (S2.S2_predicates.sign_dot_prod a c);
  [%expect {| ab=0 aa=1 ac=-1 |}]
;;
