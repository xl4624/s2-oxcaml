open Core

let point ~x ~y ~z = S2.S2_point.of_coords ~x ~y ~z

let print_vec label v =
  printf
    "%s: x=%.17g y=%.17g z=%.17g\n"
    label
    (Float_u.to_float (S2.R3_vector.x v))
    (Float_u.to_float (S2.R3_vector.y v))
    (Float_u.to_float (S2.R3_vector.z v))
;;

(* The "octant triangle" with vertices on the positive x, y, z axes. This is an eighth of
   the sphere: signed area = pi/2, and by symmetry the true centroid direction is (1, 1,
   1). *)
let octant_a () = point ~x:#1.0 ~y:#0.0 ~z:#0.0
let octant_b () = point ~x:#0.0 ~y:#1.0 ~z:#0.0
let octant_c () = point ~x:#0.0 ~y:#0.0 ~z:#1.0

let%expect_test "planar_centroid_octant" =
  let a = octant_a () in
  let b = octant_b () in
  let c = octant_c () in
  let pc = S2.S2_centroids.planar_centroid a b c in
  print_vec "planar" pc;
  printf "norm=%.17g\n" (Float_u.to_float (S2.R3_vector.norm pc));
  [%expect
    {|
    planar: x=0.33333333333333331 y=0.33333333333333331 z=0.33333333333333331
    norm=0.57735026918962573
    |}]
;;

let%expect_test "true_centroid_octant" =
  let a = octant_a () in
  let b = octant_b () in
  let c = octant_c () in
  let tc = S2.S2_centroids.true_centroid a b c in
  print_vec "true" tc;
  printf "norm=%.17g\n" (Float_u.to_float (S2.R3_vector.norm tc));
  (* By symmetry, each component equals pi/4 exactly. *)
  [%expect
    {|
    true: x=0.78539816339744828 y=0.78539816339744828 z=0.78539816339744828
    norm=1.3603495231756633
    |}]
;;

let%expect_test "true_centroid_degenerate_same_point" =
  let a = point ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let tc = S2.S2_centroids.true_centroid a a a in
  print_vec "degenerate" tc;
  printf "norm=%.17g\n" (Float_u.to_float (S2.R3_vector.norm tc));
  [%expect {|
    degenerate: x=0 y=0 z=0
    norm=0
    |}]
;;

let%expect_test "planar_centroid_degenerate_same_point" =
  let a = point ~x:#0.0 ~y:#1.0 ~z:#0.0 in
  let pc = S2.S2_centroids.planar_centroid a a a in
  print_vec "degenerate" pc;
  [%expect {| degenerate: x=0 y=1 z=0 |}]
;;

(* A second, asymmetric but well-behaved triangle: three unit vectors with no symmetry
   between them. *)
let asym_a () = point ~x:#1.0 ~y:#0.1 ~z:#0.0
let asym_b () = point ~x:#0.2 ~y:#1.0 ~z:#0.1
let asym_c () = point ~x:#0.0 ~y:#0.3 ~z:#1.0

let%expect_test "planar_centroid_asym" =
  let a = asym_a () in
  let b = asym_b () in
  let c = asym_c () in
  let pc = S2.S2_centroids.planar_centroid a b c in
  print_vec "planar" pc;
  [%expect {| planar: x=0.3967390682665653 y=0.45425055917862578 z=0.35180543083866822 |}]
;;

let%expect_test "true_centroid_asym" =
  let a = asym_a () in
  let b = asym_b () in
  let c = asym_c () in
  let tc = S2.S2_centroids.true_centroid a b c in
  print_vec "true" tc;
  printf "norm=%.17g\n" (Float_u.to_float (S2.R3_vector.norm tc));
  [%expect
    {|
    true: x=0.5134477138186363 y=0.55080356242773154 z=0.44961258817030414
    norm=0.87702029546063742
    |}]
;;

let%expect_test "edge_true_centroid_quarter" =
  let a = point ~x:#0.0 ~y:(-#1.0) ~z:#0.0 in
  let b = point ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let tc = S2.S2_centroids.edge_true_centroid a b in
  print_vec "edge" tc;
  [%expect {| edge: x=1 y=-1 z=0 |}]
;;

let%expect_test "edge_true_centroid_degenerate" =
  let a = point ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let tc = S2.S2_centroids.edge_true_centroid a a in
  print_vec "degenerate" tc;
  [%expect {| degenerate: x=0 y=0 z=0 |}]
;;
