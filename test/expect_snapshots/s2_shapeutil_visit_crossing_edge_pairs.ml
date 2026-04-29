open Core
module V = S2.S2_shapeutil_visit_crossing_edge_pairs

let polygon_shape_of_loops loops =
  S2.S2_lax_polygon.to_shape (S2.S2_lax_polygon.of_loops loops)
;;

let make_index loops =
  let index = S2.S2_shape_index.create () in
  let _id : int = S2.S2_shape_index.add index (polygon_shape_of_loops loops) in
  S2.S2_shape_index.build index;
  index
;;

let from_degrees lat lng =
  S2.S2_latlng.to_point
    (S2.S2_latlng.of_degrees ~lat:(Float_u.of_float lat) ~lng:(Float_u.of_float lng))
;;

let%expect_test "find_self_intersection_clean" =
  let loops =
    [| [| from_degrees 0.0 0.0
        ; from_degrees 0.0 1.0
        ; from_degrees 0.0 2.0
        ; from_degrees 1.0 2.0
        ; from_degrees 1.0 1.0
        ; from_degrees 1.0 0.0
       |]
    |]
  in
  print_endline
    (Sexp.to_string
       ([%sexp_of: string option] (V.find_self_intersection (make_index loops))));
  [%expect {| () |}]
;;

let%expect_test "find_self_intersection_duplicate_vertex" =
  (* "0:0, 0:1, 0:2, 1:2, 0:1, 1:0" - two edges land on the (0,1) vertex. *)
  let loops =
    [| [| from_degrees 0.0 0.0
        ; from_degrees 0.0 1.0
        ; from_degrees 0.0 2.0
        ; from_degrees 1.0 2.0
        ; from_degrees 0.0 1.0
        ; from_degrees 1.0 0.0
       |]
    |]
  in
  print_endline
    (Option.value (V.find_self_intersection (make_index loops)) ~default:"<none>");
  [%expect {| Edge 0 has duplicate vertex with edge 3 |}]
;;

let%expect_test "find_self_intersection_edge_crossing" =
  (* "0:0, 0:1, 1:0, 1:1" - edges (0,1)->(1,0) and (1,1)->(0,0) cross. *)
  let loops =
    [| [| from_degrees 0.0 0.0
        ; from_degrees 0.0 1.0
        ; from_degrees 1.0 0.0
        ; from_degrees 1.0 1.0
       |]
    |]
  in
  print_endline
    (Option.value (V.find_self_intersection (make_index loops)) ~default:"<none>");
  [%expect {| Edge 1 crosses edge 3 |}]
;;

let%expect_test "find_self_intersection_loops_cross" =
  (* Two loops with a proper interior crossing. *)
  let loops =
    [| [| from_degrees 0.0 0.0; from_degrees 0.0 2.0; from_degrees 2.0 1.0 |]
     ; [| from_degrees 1.0 0.5; from_degrees 1.0 1.5; from_degrees 3.0 1.0 |]
    |]
  in
  print_endline
    (Option.value (V.find_self_intersection (make_index loops)) ~default:"<none>");
  [%expect {| Loop 0 edge 1 crosses loop 1 edge 1 |}]
;;

let%expect_test "find_self_intersection_empty_index" =
  let index = S2.S2_shape_index.create () in
  S2.S2_shape_index.build index;
  print_endline
    (Sexp.to_string ([%sexp_of: string option] (V.find_self_intersection index)));
  [%expect {| () |}]
;;

let%expect_test "find_self_intersection_raises_on_multi_shape" =
  let index = S2.S2_shape_index.create () in
  let single =
    polygon_shape_of_loops
      [| [| from_degrees 0.0 0.0; from_degrees 0.0 1.0; from_degrees 1.0 0.0 |] |]
  in
  let _id1 : int = S2.S2_shape_index.add index single in
  let _id2 : int = S2.S2_shape_index.add index single in
  S2.S2_shape_index.build index;
  Expect_test_helpers_core.show_raise (fun () ->
    ignore (V.find_self_intersection index : string option));
  [%expect
    {|
    (raised
     "S2_shapeutil_visit_crossing_edge_pairs.find_self_intersection: index must contain at most one shape")
    |}]
;;
