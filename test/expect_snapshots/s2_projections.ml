open Core

(* No error-path snapshot: [S2_projections] exposes no [_exn] functions and
   does not raise on any input. *)

let%expect_test "plate_carree_sexp" =
  Stdlib.print_endline
    (Sexp.to_string
       ([%sexp_of: S2.S2_projections.t] (S2.S2_projections.plate_carree ~x_scale:#180.0)));
  [%expect
    {| ((kind Plate_carree)(x_wrap 360)(to_radians 0.017453292519943295)(from_radians 57.295779513082323)) |}]
;;

let%expect_test "mercator_sexp" =
  Stdlib.print_endline
    (Sexp.to_string
       ([%sexp_of: S2.S2_projections.t] (S2.S2_projections.mercator ~max_x:#180.0)));
  [%expect
    {| ((kind Mercator)(x_wrap 360)(to_radians 0.017453292519943295)(from_radians 57.295779513082323)) |}]
;;

let%expect_test "kind_dispatch" =
  let show t =
    match S2.S2_projections.kind t with
    | S2.S2_projections.Plate_carree -> "Plate_carree"
    | S2.S2_projections.Mercator -> "Mercator"
  in
  let pc = S2.S2_projections.plate_carree ~x_scale:#180.0 in
  let mc = S2.S2_projections.mercator ~max_x:#180.0 in
  printf "%s\n" (show pc);
  printf "%s\n" (show mc);
  [%expect {|
    Plate_carree
    Mercator
    |}]
;;

let%expect_test "wrap_distance" =
  let pc = S2.S2_projections.plate_carree ~x_scale:#180.0 in
  printf "%s\n" (S2.R2_point.to_string (S2.S2_projections.wrap_distance pc));
  [%expect {| (360., 0.) |}]
;;

let%expect_test "plate_carree_from_latlng_degrees" =
  let pc = S2.S2_projections.plate_carree ~x_scale:#180.0 in
  let ll = S2.S2_latlng.of_degrees ~lat:#45.0 ~lng:#90.0 in
  printf "%s\n" (S2.R2_point.to_string (S2.S2_projections.from_latlng pc ll));
  [%expect {| (90., 45.) |}]
;;
