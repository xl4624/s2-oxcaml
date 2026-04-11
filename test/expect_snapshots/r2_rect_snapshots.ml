open Core

let%expect_test "empty_sexp" =
  Stdlib.print_endline (Sexp.to_string ([%sexp_of: S2.R2_rect.t] S2.R2_rect.empty));
  [%expect {| ((x((lo 1)(hi 0)))(y((lo 1)(hi 0)))) |}]
;;

let%expect_test "empty_is_empty" =
  printf "%b\n" (S2.R2_rect.is_empty S2.R2_rect.empty);
  [%expect {| true |}]
;;

let%expect_test "from_center_size" =
  let r =
    S2.R2_rect.from_center_size
      ~center:(S2.R2_point.create ~x:#1.0 ~y:#2.0)
      ~size:(S2.R2_point.create ~x:#4.0 ~y:#6.0)
  in
  printf
    "lo_x=%.17g hi_x=%.17g lo_y=%.17g hi_y=%.17g\n"
    (Float_u.to_float (S2.R1_interval.lo (S2.R2_rect.x r)))
    (Float_u.to_float (S2.R1_interval.hi (S2.R2_rect.x r)))
    (Float_u.to_float (S2.R1_interval.lo (S2.R2_rect.y r)))
    (Float_u.to_float (S2.R1_interval.hi (S2.R2_rect.y r)));
  [%expect {| lo_x=-1 hi_x=3 lo_y=-1 hi_y=5 |}]
;;

let%expect_test "exn_path" =
  (* [create_intervals_exn] with one empty and one non-empty interval is not
     a valid rectangle. *)
  Expect_test_helpers_core.show_raise (fun () ->
    ignore
      (S2.R2_rect.create_intervals_exn
         ~x:S2.R1_interval.empty
         ~y:(S2.R1_interval.create ~lo:#0.0 ~hi:#1.0)
       : S2.R2_rect.t));
  [%expect {| (raised "R2Rect: both intervals must be empty or non-empty") |}];
  (* [project_exn] on the empty rectangle must raise. *)
  Expect_test_helpers_core.show_raise (fun () ->
    ignore
      (S2.R2_rect.project_exn
         S2.R2_rect.empty
         (S2.R2_point.create ~x:#0.0 ~y:#0.0)
       : S2.R2_point.t));
  [%expect {| (raised "R2_rect.project: rectangle is empty") |}]
;;

let%expect_test "contains_point" =
  let r =
    S2.R2_rect.from_point_pair
      (S2.R2_point.create ~x:#0.0 ~y:#0.0)
      (S2.R2_point.create ~x:#2.0 ~y:#3.0)
  in
  printf
    "contains (1,1): %b\n"
    (S2.R2_rect.contains_point r (S2.R2_point.create ~x:#1.0 ~y:#1.0));
  printf
    "contains (5,5): %b\n"
    (S2.R2_rect.contains_point r (S2.R2_point.create ~x:#5.0 ~y:#5.0));
  [%expect {|
    contains (1,1): true
    contains (5,5): false |}]
;;
