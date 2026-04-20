open Core

let p x y z = S2.S2_point.of_coords ~x ~y ~z

let%expect_test "empty_loop_sexp" =
  (* The canonical empty loop has a single vertex at the north pole. *)
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_loop.t] (S2.S2_loop.empty ())));
  [%expect {| (S2_loop(((x 0)(y 0)(z 1)))) |}]
;;

let%expect_test "full_loop_sexp" =
  (* The canonical full loop has a single vertex at the south pole. *)
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_loop.t] (S2.S2_loop.full ())));
  [%expect {| (S2_loop(((x 0)(y 0)(z -1)))) |}]
;;

let%expect_test "empty_full_predicates" =
  let e = S2.S2_loop.empty () in
  let f = S2.S2_loop.full () in
  printf
    "empty: is_empty=%b is_full=%b is_empty_or_full=%b num_edges=%d\n"
    (S2.S2_loop.is_empty e)
    (S2.S2_loop.is_full e)
    (S2.S2_loop.is_empty_or_full e)
    (S2.S2_loop.num_edges e);
  printf
    "full: is_empty=%b is_full=%b is_empty_or_full=%b num_edges=%d\n"
    (S2.S2_loop.is_empty f)
    (S2.S2_loop.is_full f)
    (S2.S2_loop.is_empty_or_full f)
    (S2.S2_loop.num_edges f);
  [%expect
    {|
    empty: is_empty=true is_full=false is_empty_or_full=true num_edges=0
    full: is_empty=false is_full=true is_empty_or_full=true num_edges=0
    |}]
;;

let%expect_test "invert_swaps_empty_and_full" =
  let e = S2.S2_loop.empty () in
  let inv_e = S2.S2_loop.invert e in
  let f = S2.S2_loop.full () in
  let inv_f = S2.S2_loop.invert f in
  printf
    "invert(empty): is_full=%b\ninvert(full): is_empty=%b\n"
    (S2.S2_loop.is_full inv_e)
    (S2.S2_loop.is_empty inv_f);
  [%expect {|
    invert(empty): is_full=true
    invert(full): is_empty=true
    |}]
;;

let%expect_test "too_few_vertices_raises" =
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (S2.S2_loop.of_vertices [||] : S2.S2_loop.t));
  [%expect {| (raised "S2_loop.of_vertices: empty vertex array") |}]
;;

let%expect_test "invalid_two_vertex_loop_raises" =
  (* A two-vertex loop is not allowed; loops need the two special
     single-vertex sentinels or at least three real vertices. *)
  let a = p #1.0 #0.0 #0.0 in
  let b = p #0.0 #1.0 #0.0 in
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (S2.S2_loop.of_vertices [| a; b |] : S2.S2_loop.t));
  [%expect
    {|
    (raised (
      "S2_loop.of_vertices: invalid loop" (
        msg "Non-empty, non-full loops must have at least 3 vertices")))
    |}]
;;

let%expect_test "find_validation_error_two_vertex_loop" =
  let a = p #1.0 #0.0 #0.0 in
  let b = p #0.0 #1.0 #0.0 in
  let l = S2.S2_loop.of_vertices ~validate:false [| a; b |] in
  Stdlib.print_endline
    (Sexp.to_string ([%sexp_of: string option] (S2.S2_loop.find_validation_error l)));
  [%expect {| ("Non-empty, non-full loops must have at least 3 vertices") |}]
;;

let%expect_test "empty_loop_area_and_curvature" =
  let e = S2.S2_loop.empty () in
  printf
    "area=%.17g curvature=%.17g\n"
    (Float_u.to_float (S2.S2_loop.area e))
    (Float_u.to_float (S2.S2_loop.curvature e));
  [%expect {| area=0 curvature=6.2831853071795862 |}]
;;

let%expect_test "full_loop_area_and_curvature" =
  let f = S2.S2_loop.full () in
  printf
    "area=%.17g curvature=%.17g\n"
    (Float_u.to_float (S2.S2_loop.area f))
    (Float_u.to_float (S2.S2_loop.curvature f));
  [%expect {| area=12.566370614359172 curvature=-6.2831853071795862 |}]
;;

let%expect_test "type_tag_is_none" =
  printf "%d\n" S2.S2_loop.type_tag;
  [%expect {| 0 |}]
;;
