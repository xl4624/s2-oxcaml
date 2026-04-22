(* Expect tests for [S2_contains_point_query] - focuses on the [Vertex_model.Open]
   and [Vertex_model.Closed] paths for 2D shapes (polygons). *)

open Core
module Q = S2.S2_contains_point_query

let make_square_polygon () =
  (* Unit square near origin with lat-lng vertices (0,0), (0,1), (1,1), (1,0).
     Interior is the small enclosed region. *)
  let v0 = S2.S2_latlng.to_point (S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#0.0) in
  let v1 = S2.S2_latlng.to_point (S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#1.0) in
  let v2 = S2.S2_latlng.to_point (S2.S2_latlng.of_degrees ~lat:#1.0 ~lng:#1.0) in
  let v3 = S2.S2_latlng.to_point (S2.S2_latlng.of_degrees ~lat:#1.0 ~lng:#0.0) in
  S2.S2_polygon.of_loops [| S2.S2_loop.of_vertices ~validate:false [| v0; v1; v2; v3 |] |]
;;

let probe ~vertex_model ~point_lat ~point_lng =
  let poly = make_square_polygon () in
  let index = S2.S2_shape_index.create () in
  let sid = S2.S2_shape_index.add index (S2.S2_polygon.to_shape poly) in
  let q = Q.create index ~vertex_model () in
  let p = S2.S2_latlng.to_point (S2.S2_latlng.of_degrees ~lat:point_lat ~lng:point_lng) in
  Q.shape_contains q ~shape_id:sid p
;;

let%expect_test "vertex models on polygon boundary vertex" =
  let at_v0 lat lng =
    probe ~vertex_model:lat ~point_lat:#0.0 ~point_lng:#0.0 |> printf "%s: %b\n" lng
  in
  at_v0 Q.Vertex_model.Open "open";
  at_v0 Q.Vertex_model.Semi_open "semi_open";
  at_v0 Q.Vertex_model.Closed "closed";
  [%expect {|
    open: false
    semi_open: false
    closed: true
    |}]
;;

let%expect_test "vertex models at interior point" =
  let at_interior lat lng =
    probe ~vertex_model:lat ~point_lat:#0.5 ~point_lng:#0.5 |> printf "%s: %b\n" lng
  in
  at_interior Q.Vertex_model.Open "open";
  at_interior Q.Vertex_model.Semi_open "semi_open";
  at_interior Q.Vertex_model.Closed "closed";
  [%expect {|
    open: true
    semi_open: true
    closed: true
    |}]
;;

let%expect_test "vertex models at clearly-exterior point" =
  let at_far lat lng =
    probe ~vertex_model:lat ~point_lat:#10.0 ~point_lng:#10.0 |> printf "%s: %b\n" lng
  in
  at_far Q.Vertex_model.Open "open";
  at_far Q.Vertex_model.Semi_open "semi_open";
  at_far Q.Vertex_model.Closed "closed";
  [%expect {|
    open: false
    semi_open: false
    closed: false
    |}]
;;
