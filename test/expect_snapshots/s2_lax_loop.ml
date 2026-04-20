open Core

(* No error-path snapshot: [S2_lax_loop] accepts any vertex sequence, including
   empty, single-vertex, and antipodal-adjacent loops. *)

let p x y z = S2.S2_point.of_coords ~x ~y ~z

let%expect_test "empty_sexp" =
  Stdlib.print_endline
    (Sexp.to_string ([%sexp_of: S2.S2_lax_loop.t] (S2.S2_lax_loop.of_vertices [||])));
  [%expect {| (S2_lax_loop()) |}]
;;

let%expect_test "single_vertex_sexp" =
  let t = S2.S2_lax_loop.of_vertices [| p #1.0 #0.0 #0.0 |] in
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_lax_loop.t] t));
  printf
    "n=%d dim=%d num_chains=%d\n"
    (S2.S2_lax_loop.num_vertices t)
    (S2.S2_lax_loop.dimension t)
    (S2.S2_lax_loop.num_chains t);
  [%expect {|
    (S2_lax_loop(((x 1)(y 0)(z 0))))
    n=1 dim=2 num_chains=1
    |}]
;;

let%expect_test "triangle_sexp" =
  let t =
    S2.S2_lax_loop.of_vertices [| p #1.0 #0.0 #0.0; p #0.0 #1.0 #0.0; p #0.0 #0.0 #1.0 |]
  in
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_lax_loop.t] t));
  printf
    "n=%d num_edges=%d dim=%d\n"
    (S2.S2_lax_loop.num_vertices t)
    (S2.S2_lax_loop.num_edges t)
    (S2.S2_lax_loop.dimension t);
  [%expect
    {|
    (S2_lax_loop(((x 1)(y 0)(z 0))((x 0)(y 1)(z 0))((x 0)(y 0)(z 1))))
    n=3 num_edges=3 dim=2
    |}]
;;

let%expect_test "empty_num_chains_zero" =
  let t = S2.S2_lax_loop.of_vertices [||] in
  printf
    "n=%d num_edges=%d num_chains=%d\n"
    (S2.S2_lax_loop.num_vertices t)
    (S2.S2_lax_loop.num_edges t)
    (S2.S2_lax_loop.num_chains t);
  [%expect {| n=0 num_edges=0 num_chains=0 |}]
;;

let%expect_test "type_tag_is_none" =
  printf "%d\n" S2.S2_lax_loop.type_tag;
  [%expect {| 0 |}]
;;
