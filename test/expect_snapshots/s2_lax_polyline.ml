open Core

(* No error-path snapshot: [S2_lax_polyline] accepts any vertex sequence
   including empty, single-vertex, and repeated/antipodal-adjacent vertices. *)

let p x y z = S2.S2_point.of_coords ~x ~y ~z

let%expect_test "empty_sexp" =
  Stdlib.print_endline
    (Sexp.to_string
       ([%sexp_of: S2.S2_lax_polyline.t] (S2.S2_lax_polyline.of_vertices [||])));
  [%expect {| (S2_lax_polyline()) |}]
;;

let%expect_test "single_vertex_sexp" =
  let t = S2.S2_lax_polyline.of_vertices [| p #1.0 #0.0 #0.0 |] in
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_lax_polyline.t] t));
  printf
    "n=%d num_edges=%d num_chains=%d\n"
    (S2.S2_lax_polyline.num_vertices t)
    (S2.S2_lax_polyline.num_edges t)
    (S2.S2_lax_polyline.num_chains t);
  [%expect
    {|
    (S2_lax_polyline(((x 1)(y 0)(z 0))))
    n=1 num_edges=0 num_chains=0
    |}]
;;

let%expect_test "two_vertex_sexp" =
  let t = S2.S2_lax_polyline.of_vertices [| p #1.0 #0.0 #0.0; p #0.0 #1.0 #0.0 |] in
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_lax_polyline.t] t));
  printf
    "n=%d num_edges=%d dim=%d num_chains=%d\n"
    (S2.S2_lax_polyline.num_vertices t)
    (S2.S2_lax_polyline.num_edges t)
    (S2.S2_lax_polyline.dimension t)
    (S2.S2_lax_polyline.num_chains t);
  [%expect
    {|
    (S2_lax_polyline(((x 1)(y 0)(z 0))((x 0)(y 1)(z 0))))
    n=2 num_edges=1 dim=1 num_chains=1
    |}]
;;

let%expect_test "degenerate_repeated_vertex_allowed" =
  (* A polyline that repeats the same vertex twice is the canonical single
     degenerate edge. *)
  let v = p #1.0 #0.0 #0.0 in
  let t = S2.S2_lax_polyline.of_vertices [| v; v |] in
  printf
    "n=%d num_edges=%d\n"
    (S2.S2_lax_polyline.num_vertices t)
    (S2.S2_lax_polyline.num_edges t);
  [%expect {| n=2 num_edges=1 |}]
;;

let%expect_test "type_tag_is_four" =
  printf "%d\n" S2.S2_lax_polyline.type_tag;
  [%expect {| 4 |}]
;;
