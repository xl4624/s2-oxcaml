open Core

(* No error-path snapshot: [S2_edge_tessellator] exposes no [_exn] functions; tolerances
   below the minimum are silently clamped. *)

let%expect_test "min_tolerance_constant" =
  printf
    "min_tolerance_rad=%.17g\n"
    (Float_u.to_float (S2.S1_angle.radians S2.S2_edge_tessellator.min_tolerance));
  [%expect {| min_tolerance_rad=1e-13 |}]
;;

let%expect_test "create_sexp" =
  let proj = S2.S2_projections.plate_carree ~x_scale:#180.0 in
  let t =
    S2.S2_edge_tessellator.create
      ~projection:proj
      ~tolerance:(S2.S1_angle.of_degrees #1.0)
  in
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_edge_tessellator.t] t));
  [%expect
    {| ((projection((kind Plate_carree)(x_wrap 360)(to_radians 0.017453292519943295)(from_radians 57.295779513082323)))(scaled_tolerance 0.014631093822669371)) |}]
;;

let%expect_test "create_clamps_small_tolerance" =
  let proj = S2.S2_projections.plate_carree ~x_scale:#180.0 in
  let tiny = S2.S1_angle.of_radians #1e-20 in
  let t = S2.S2_edge_tessellator.create ~projection:proj ~tolerance:tiny in
  (* Even though we passed 1e-20 rad, the scaled_tolerance reflects the clamped value, not
     the input. *)
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.S2_edge_tessellator.t] t));
  [%expect
    {| ((projection((kind Plate_carree)(x_wrap 360)(to_radians 0.017453292519943295)(from_radians 57.295779513082323)))(scaled_tolerance 8.3829992569888513E-14)) |}]
;;

let%expect_test "project_single_vertex" =
  let proj = S2.S2_projections.plate_carree ~x_scale:#180.0 in
  let t =
    S2.S2_edge_tessellator.create
      ~projection:proj
      ~tolerance:(S2.S1_angle.of_degrees #1.0)
  in
  let p = S2.S2_latlng.to_point (S2.S2_latlng.of_degrees ~lat:#30.0 ~lng:#60.0) in
  let projected = S2.S2_edge_tessellator.project t [| p |] in
  printf "len=%d\n" (Array.length projected);
  printf "%s\n" (S2.R2_point.to_string projected.(0));
  [%expect {|
    len=1
    (59.999999999999993, 29.999999999999996)
    |}]
;;
