open Core

(* No error-path snapshot: [S2_lax_polygon] accepts any collection of loops including
   empty loops (full), single-vertex loops (degenerate edge), and two-vertex loops
   (sibling edge pairs). *)

let p x y z = S2.S2_point.of_coords ~x ~y ~z

let%expect_test "empty_sexp" =
  Stdlib.print_endline
    (Sexp.to_string ([%sexp_of: S2.S2_lax_polygon.t] (S2.S2_lax_polygon.of_loops [||])));
  [%expect {| (S2_lax_polygon()) |}]
;;

let%expect_test "single_triangle_loop_sexp" =
  let t =
    S2.S2_lax_polygon.of_loops
      [| [| p #1.0 #0.0 #0.0; p #0.0 #1.0 #0.0; p #0.0 #0.0 #1.0 |] |]
  in
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_lax_polygon.t] t));
  printf
    "num_loops=%d num_vertices=%d num_edges=%d num_chains=%d\n"
    (S2.S2_lax_polygon.num_loops t)
    (S2.S2_lax_polygon.num_vertices t)
    (S2.S2_lax_polygon.num_edges t)
    (S2.S2_lax_polygon.num_chains t);
  [%expect
    {|
    (S2_lax_polygon((((x 1)(y 0)(z 0))((x 0)(y 1)(z 0))((x 0)(y 0)(z 1)))))
    num_loops=1 num_vertices=3 num_edges=3 num_chains=1
    |}]
;;

let%expect_test "two_loops_sexp" =
  let outer = [| p #1.0 #0.0 #0.0; p #0.0 #1.0 #0.0; p #0.0 #0.0 #1.0 |] in
  let inner = [| p #1.0 #0.0 #0.0; p #0.0 #0.0 #1.0 |] in
  let t = S2.S2_lax_polygon.of_loops [| outer; inner |] in
  printf
    "num_loops=%d num_vertices=%d num_loop_vertices[0]=%d num_loop_vertices[1]=%d\n"
    (S2.S2_lax_polygon.num_loops t)
    (S2.S2_lax_polygon.num_vertices t)
    (S2.S2_lax_polygon.num_loop_vertices t 0)
    (S2.S2_lax_polygon.num_loop_vertices t 1);
  [%expect
    {|
    num_loops=2 num_vertices=5 num_loop_vertices[0]=3 num_loop_vertices[1]=2
    |}]
;;

let%expect_test "full_loop_sexp" =
  (* A single empty loop represents the full polygon covering the sphere. *)
  let t = S2.S2_lax_polygon.of_loops [| [||] |] in
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_lax_polygon.t] t));
  printf
    "num_loops=%d num_vertices=%d num_edges=%d\n"
    (S2.S2_lax_polygon.num_loops t)
    (S2.S2_lax_polygon.num_vertices t)
    (S2.S2_lax_polygon.num_edges t);
  [%expect {|
    (S2_lax_polygon(()))
    num_loops=1 num_vertices=0 num_edges=0
    |}]
;;

let%expect_test "type_tag_is_five" =
  printf "%d\n" S2.S2_lax_polygon.type_tag;
  [%expect {| 5 |}]
;;
