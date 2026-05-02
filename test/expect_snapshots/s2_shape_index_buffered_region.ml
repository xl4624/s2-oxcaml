open Core
open Expect_test_helpers_core

let buffered_region_of_points points radius_deg =
  let index = S2.S2_shape_index.create () in
  let shape =
    S2.S2_point_vector_shape.to_shape (S2.S2_point_vector_shape.of_points points)
  in
  let (_ : int) = S2.S2_shape_index.add index shape in
  S2.S2_shape_index.build index;
  let radius = S2.S1_chord_angle.of_angle (S2.S1_angle.of_degrees radius_deg) in
  S2.S2_shape_index_buffered_region.create index radius
;;

let%expect_test "create_angle_negative_radius_raises" =
  let index = S2.S2_shape_index.create () in
  S2.S2_shape_index.build index;
  show_raise (fun () ->
    S2.S2_shape_index_buffered_region.create_angle
      index
      (S2.S1_angle.of_degrees (Float_u.neg #1.0)));
  [%expect
    {|
    (raised (
      "S2_shape_index_buffered_region.create_angle: negative radius"
      (radians -0.017453292519943295)))
    |}]
;;

let%expect_test "empty_index_covering_is_empty" =
  let index = S2.S2_shape_index.create () in
  S2.S2_shape_index.build index;
  let region =
    S2.S2_shape_index_buffered_region.create
      index
      (S2.S1_chord_angle.of_angle (S2.S1_angle.of_degrees #2.0))
  in
  let cu = S2.S2_shape_index_buffered_region.cell_union_bound region in
  printf "n=%d\n" (Array.length cu);
  [%expect {| n=0 |}]
;;

let%expect_test "radius_round_trip" =
  let region =
    buffered_region_of_points [| S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 |] #2.0
  in
  let radius = S2.S2_shape_index_buffered_region.radius region in
  printf
    "%.6f\n"
    (Float_u.to_float (S2.S1_angle.radians (S2.S1_chord_angle.to_angle radius)));
  [%expect {| 0.034907 |}]
;;

let%expect_test "single_point_zero_radius_contains_only_self" =
  let p = S2.S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let other = S2.S2_point.of_coords ~x:#0.0 ~y:#1.0 ~z:#0.0 in
  let region = buffered_region_of_points [| p |] #0.0 in
  printf
    "self: %b other: %b\n"
    (S2.S2_shape_index_buffered_region.contains_point region p)
    (S2.S2_shape_index_buffered_region.contains_point region other);
  [%expect {| self: true other: false |}]
;;
