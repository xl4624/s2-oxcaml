open Core

let p x y z = S2.S2_point.of_coords ~x ~y ~z

let%expect_test "empty_sexp" =
  Stdlib.print_endline
    (Sexp.to_string ([%sexp_of: S2.S2_polyline.t] (S2.S2_polyline.of_vertices [||])));
  [%expect {| (S2_polyline()) |}]
;;

let%expect_test "two_vertex_sexp" =
  let t = S2.S2_polyline.of_vertices [| p #1.0 #0.0 #0.0; p #0.0 #1.0 #0.0 |] in
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_polyline.t] t));
  printf
    "n=%d num_edges=%d dim=%d num_chains=%d\n"
    (S2.S2_polyline.num_vertices t)
    (S2.S2_polyline.num_edges t)
    (S2.S2_polyline.dimension t)
    (S2.S2_polyline.num_chains t);
  [%expect
    {|
    (S2_polyline(((x 1)(y 0)(z 0))((x 0)(y 1)(z 0))))
    n=2 num_edges=1 dim=1 num_chains=1
    |}]
;;

let%expect_test "invalid_vertex_raises" =
  (* Adjacent identical vertices are invalid for [S2_polyline]; use [S2_lax_polyline] for
     that case. *)
  let v = p #1.0 #0.0 #0.0 in
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (S2.S2_polyline.of_vertices [| v; v |] : S2.S2_polyline.t));
  [%expect {| (raised "S2_polyline.of_vertices: invalid polyline") |}]
;;

let%expect_test "antipodal_adjacent_raises" =
  let a = p #1.0 #0.0 #0.0 in
  let b = p (-#1.0) #0.0 #0.0 in
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (S2.S2_polyline.of_vertices [| a; b |] : S2.S2_polyline.t));
  [%expect {| (raised "S2_polyline.of_vertices: invalid polyline") |}]
;;

let%expect_test "get_suffix_empty_raises" =
  let empty = S2.S2_polyline.of_vertices [||] in
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (S2.S2_polyline.get_suffix empty #0.5 : S2.S2_polyline.suffix));
  [%expect {| (raised "S2_polyline.get_suffix: empty polyline") |}]
;;

let%expect_test "un_interpolate_empty_raises" =
  let empty = S2.S2_polyline.of_vertices [||] in
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (S2.S2_polyline.un_interpolate empty (p #1.0 #0.0 #0.0) 1 : float#));
  [%expect {| (raised "S2_polyline.un_interpolate: empty polyline") |}]
;;

let%expect_test "project_empty_raises" =
  let empty = S2.S2_polyline.of_vertices [||] in
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (S2.S2_polyline.project empty (p #1.0 #0.0 #0.0) : S2.S2_polyline.projection));
  [%expect {| (raised "S2_polyline.project: empty polyline") |}]
;;

let%expect_test "is_on_right_single_vertex_raises" =
  let t = S2.S2_polyline.of_vertices [| p #1.0 #0.0 #0.0 |] in
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (S2.S2_polyline.is_on_right t (p #0.0 #1.0 #0.0) : bool));
  [%expect {| (raised "S2_polyline.is_on_right: requires at least two vertices") |}]
;;

let%expect_test "type_tag_is_two" =
  printf "%d\n" S2.S2_polyline.type_tag;
  [%expect {| 2 |}]
;;
