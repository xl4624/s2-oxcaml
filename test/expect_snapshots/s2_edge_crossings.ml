open Core

let pt x y z = S2.S2_point.of_coords ~x ~y ~z

let%expect_test "crossing_sign_clear_crossing" =
  (* AB is the quarter equator arc from (1,0,0) to (0,1,0). CD goes from (0.5, 0.5, 0.5)
     to (0.5, 0.5, -0.5) and crosses the equator near the midpoint of AB, so the two
     great-circle edges intersect at a point interior to both. *)
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let c = pt #0.5 #0.5 #0.5 in
  let d = pt #0.5 #0.5 (-#0.5) in
  printf "%d\n" (S2.S2_edge_crossings.crossing_sign a b c d);
  [%expect {| 1 |}]
;;

let%expect_test "crossing_sign_non_crossing" =
  (* Two disjoint arcs: equator quarter vs antimeridian quarter. *)
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let c = pt #0.0 #0.0 #1.0 in
  let d = pt (-#1.0) #0.0 #0.0 in
  printf "%d\n" (S2.S2_edge_crossings.crossing_sign a b c d);
  [%expect {| -1 |}]
;;

let%expect_test "vertex_crossing_shared_endpoint_a_eq_c" =
  (* Two edges sharing vertex A == C. *)
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let c = a in
  let d = pt #0.0 #0.0 #1.0 in
  printf
    "cs=%d vc=%b\n"
    (S2.S2_edge_crossings.crossing_sign a b c d)
    (S2.S2_edge_crossings.vertex_crossing a b c d);
  [%expect {| cs=0 vc=false |}]
;;

let%expect_test "vertex_crossing_shared_endpoint_b_eq_d" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #0.0 #1.0 in
  let c = pt #0.0 #1.0 #0.0 in
  let d = b in
  printf
    "cs=%d vc=%b\n"
    (S2.S2_edge_crossings.crossing_sign a b c d)
    (S2.S2_edge_crossings.vertex_crossing a b c d);
  [%expect {| cs=0 vc=false |}]
;;

let%expect_test "edge_or_vertex_crossing_clean" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let c = pt #0.5 #0.5 #0.5 in
  let d = pt #0.5 #0.5 (-#0.5) in
  printf "%b\n" (S2.S2_edge_crossings.edge_or_vertex_crossing a b c d);
  [%expect {| true |}]
;;

let%expect_test "edge_or_vertex_crossing_vertex_touching" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #0.0 #1.0 in
  let c = pt #0.0 #1.0 #0.0 in
  let d = b in
  printf "%b\n" (S2.S2_edge_crossings.edge_or_vertex_crossing a b c d);
  [%expect {| false |}]
;;

let%expect_test "angle_contains_vertex_yes_no" =
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let c = pt #0.0 #0.0 #1.0 in
  printf
    "abc=%b cba=%b\n"
    (S2.S2_edge_crossings.angle_contains_vertex a b c)
    (S2.S2_edge_crossings.angle_contains_vertex c b a);
  [%expect {| abc=false cba=true |}]
;;
