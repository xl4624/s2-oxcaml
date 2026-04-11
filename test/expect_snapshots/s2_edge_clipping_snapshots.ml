open Core

let pt x y z = S2.S2_point.of_coords ~x ~y ~z

let%expect_test "face_clip_error_constants" =
  printf
    "face_rad=%.17g face_uv=%.17g face_coord=%.17g rect_uv=%.17g edge_coord=%.17g edge_uv=%.17g\n"
    (Float_u.to_float S2.S2_edge_clipping.face_clip_error_radians)
    (Float_u.to_float S2.S2_edge_clipping.face_clip_error_uv_dist)
    (Float_u.to_float S2.S2_edge_clipping.face_clip_error_uv_coord)
    (Float_u.to_float S2.S2_edge_clipping.intersects_rect_error_uv_dist)
    (Float_u.to_float S2.S2_edge_clipping.edge_clip_error_uv_coord)
    (Float_u.to_float S2.S2_edge_clipping.edge_clip_error_uv_dist);
  [%expect {| face_rad=6.6613381477509392e-16 face_uv=1.9984014443252818e-15 face_coord=1.4130832128153975e-15 rect_uv=9.4205547521026515e-16 edge_coord=4.9960036108132044e-16 edge_uv=4.9960036108132044e-16 |}]
;;

let%expect_test "get_face_segments_single_face" =
  (* Short edge inside face 0 (which is centered on +x). *)
  let a = pt #1.0 #0.1 #0.1 in
  let b = pt #1.0 #0.2 #0.0 in
  let segs = S2.S2_edge_clipping.get_face_segments a b in
  List.iter segs ~f:(fun s ->
    printf "%s\n" (Sexp.to_string [%sexp (s : S2.S2_edge_clipping.face_segment)]));
  printf "n=%d\n" (List.length segs);
  [%expect {|
    ((face 0)(a((x 0.1)(y 0.1)))(b((x 0.2)(y 0))))
    n=1
    |}]
;;

let%expect_test "get_face_segments_crossing_faces" =
  (* Edge from +x axis to +y axis crosses the boundary between face 0 and face 1. *)
  let a = pt #1.0 #0.0 #0.0 in
  let b = pt #0.0 #1.0 #0.0 in
  let segs = S2.S2_edge_clipping.get_face_segments a b in
  List.iter segs ~f:(fun s ->
    printf "face=%d\n" s.face);
  printf "n=%d\n" (List.length segs);
  [%expect {|
    face=0
    face=1
    n=2
    |}]
;;

let%expect_test "clip_to_face_intersect_and_miss" =
  let a = pt #1.0 #0.1 #0.1 in
  let b = pt #1.0 #0.2 #0.0 in
  (match S2.S2_edge_clipping.clip_to_face a b 0 with
   | None -> printf "face0=none\n"
   | Some s ->
     printf "face0=%s\n" (Sexp.to_string [%sexp (s : S2.S2_edge_clipping.clipped_uv)]));
  (match S2.S2_edge_clipping.clip_to_face a b 3 with
   | None -> printf "face3=none\n"
   | Some _ -> printf "face3=some\n");
  [%expect {|
    face0=((a((x 0.1)(y 0.1)))(b((x 0.2)(y 0))))
    face3=none
    |}]
;;

let%expect_test "intersects_rect_and_clip_edge" =
  let lo = S2.R2_point.create ~x:#0.0 ~y:#0.0 in
  let hi = S2.R2_point.create ~x:#1.0 ~y:#1.0 in
  let rect = S2.R2_rect.create_exn ~lo ~hi in
  let a = S2.R2_point.create ~x:(-#0.5) ~y:#0.5 in
  let b = S2.R2_point.create ~x:#0.5 ~y:#0.5 in
  printf "intersects=%b\n" (S2.S2_edge_clipping.intersects_rect a b rect);
  (match S2.S2_edge_clipping.clip_edge a b rect with
   | None -> printf "clip=none\n"
   | Some c ->
     printf "clip=%s\n" (Sexp.to_string [%sexp (c : S2.S2_edge_clipping.clipped_uv)]));
  [%expect {|
    intersects=true
    clip=((a((x 0)(y 0.5)))(b((x 0.5)(y 0.5))))
    |}]
;;

let%expect_test "interpolate_double" =
  let show x =
    printf
      "x=%.2f -> %.17g\n"
      (Float_u.to_float x)
      (Float_u.to_float
         (S2.S2_edge_clipping.interpolate_double x #0.0 #1.0 #10.0 #20.0))
  in
  show #0.0;
  show #0.25;
  show #0.5;
  show #0.75;
  show #1.0;
  (* Degenerate: a = b, should return a1. *)
  printf
    "degen=%.17g\n"
    (Float_u.to_float
       (S2.S2_edge_clipping.interpolate_double #0.5 #1.0 #1.0 #7.0 #42.0));
  [%expect {|
    x=0.00 -> 10
    x=0.25 -> 12.5
    x=0.50 -> 15
    x=0.75 -> 17.5
    x=1.00 -> 20
    degen=7
    |}]
;;
