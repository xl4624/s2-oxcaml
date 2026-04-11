(* C++ test parity: s2geometry/src/s2/s2metrics_test.cc and the Metrics block
   in s2cell_test.cc.

   Covered:
   -  TEST(S2, Metrics)         - constant deriv + values-at-every-level
                                  for all 15 metrics, plus the boundary
                                  width/area loop and the negative/infinity
                                  boundary cases.
   -  GetLevelForMaxValue / GetLevelForMinValue / GetClosestLevel coverage
      via fixture-driven cases.

   Deliberately omitted:
   -  Randomized consistency checks (no upstream randomized test corresponds). *)

open Core
open Test_helpers
module M = S2.S2_metrics

let fixture = lazy (load_fixture "s2metrics.json")

let metric_by_name = function
  | "min_angle_span" -> M.min_angle_span
  | "max_angle_span" -> M.max_angle_span
  | "avg_angle_span" -> M.avg_angle_span
  | "min_width" -> M.min_width
  | "max_width" -> M.max_width
  | "avg_width" -> M.avg_width
  | "min_edge" -> M.min_edge
  | "max_edge" -> M.max_edge
  | "avg_edge" -> M.avg_edge
  | "min_diag" -> M.min_diag
  | "max_diag" -> M.max_diag
  | "avg_diag" -> M.avg_diag
  | "min_area" -> M.min_area
  | "max_area" -> M.max_area
  | "avg_area" -> M.avg_area
  | name ->
    (match failwith (sprintf "unknown metric: %s" name) with
     | (_ : Nothing.t) -> .)
;;

let all_metric_names =
  [ "min_angle_span"
  ; "max_angle_span"
  ; "avg_angle_span"
  ; "min_width"
  ; "max_width"
  ; "avg_width"
  ; "min_edge"
  ; "max_edge"
  ; "avg_edge"
  ; "min_diag"
  ; "max_diag"
  ; "avg_diag"
  ; "min_area"
  ; "max_area"
  ; "avg_area"
  ]
;;

let test_constants () =
  let fx = Lazy.force fixture in
  let metrics = member "metrics" fx in
  List.iter all_metric_names ~f:(fun name ->
    let entry = member name metrics in
    let metric = metric_by_name name in
    let expected_dim = int_of_json_exn (member "dim" entry) in
    let expected_deriv = float_u_of_json_exn (member "deriv" entry) in
    Alcotest.(check int) (name ^ " dim") expected_dim (M.dim metric);
    check_float_u_exact
      (name ^ " deriv")
      ~expected:expected_deriv
      ~actual:(M.deriv metric);
    let values = to_list (member "values" entry) in
    List.iteri values ~f:(fun level expected_json ->
      let expected = float_u_of_json_exn expected_json in
      let actual = M.get_value metric level in
      check_float_u_exact (sprintf "%s.get_value(%d)" name level) ~expected ~actual))
;;

let test_scalar_constants () =
  let fx = Lazy.force fixture in
  Alcotest.(check int)
    "max_cell_level"
    (int_of_json_exn (member "max_cell_level" fx))
    M.max_cell_level;
  check_float_u_exact
    "max_edge_aspect"
    ~expected:(float_u_of_json_exn (member "max_edge_aspect" fx))
    ~actual:M.max_edge_aspect;
  check_float_u_exact
    "max_diag_aspect"
    ~expected:(float_u_of_json_exn (member "max_diag_aspect" fx))
    ~actual:M.max_diag_aspect
;;

let test_boundary_level_cases () =
  let cases = to_list (member "boundary_level_cases" (Lazy.force fixture)) in
  List.iter cases ~f:(fun c ->
    let metric_name = string_of_json_exn (member "metric" c) in
    let op = string_of_json_exn (member "op" c) in
    let input =
      (* JSON has no infinity literal; the generator emits it as null. *)
      match member "input" c with
      | `Null -> Float_u.infinity ()
      | j -> float_u_of_json_exn j
    in
    let expected = int_of_json_exn (member "expected_level" c) in
    let metric = metric_by_name metric_name in
    let actual =
      match op with
      | "level_for_max_value" -> M.get_level_for_max_value metric input
      | "level_for_min_value" -> M.get_level_for_min_value metric input
      | s ->
        (match failwith (sprintf "unknown op: %s" s) with
         | (_ : Nothing.t) -> .)
    in
    Alcotest.(check int) (sprintf "%s %s" metric_name op) expected actual)
;;

let test_closest_level_cases () =
  let cases = to_list (member "closest_level_cases" (Lazy.force fixture)) in
  List.iter cases ~f:(fun c ->
    let metric_name = string_of_json_exn (member "metric" c) in
    let value = float_u_of_json_exn (member "value" c) in
    let expected = int_of_json_exn (member "expected_level" c) in
    let metric = metric_by_name metric_name in
    let actual = M.get_closest_level metric value in
    Alcotest.(check int) (sprintf "closest %s" metric_name) expected actual)
;;

let test_width_loop () =
  let cases = to_list (member "width_loop" (Lazy.force fixture)) in
  List.iter cases ~f:(fun c ->
    let level = int_of_json_exn (member "level" c) in
    let expected = int_of_json_exn (member "expected_level" c) in
    let width = float_u_of_json_exn (member "width" c) in
    let width_1_2 = float_u_of_json_exn (member "width_1_2" c) in
    let width_0_8 = float_u_of_json_exn (member "width_0_8" c) in
    let label s = sprintf "width_loop level=%d %s" level s in
    Alcotest.(check int)
      (label "min_width.level_for_max_value(1.2*w)")
      expected
      (M.get_level_for_max_value M.min_width width_1_2);
    Alcotest.(check int)
      (label "min_width.level_for_min_value(0.8*w)")
      expected
      (M.get_level_for_min_value M.min_width width_0_8);
    Alcotest.(check int)
      (label "min_width.closest(w)")
      expected
      (M.get_closest_level M.min_width width))
;;

let test_area_loop () =
  let cases = to_list (member "area_loop" (Lazy.force fixture)) in
  List.iter cases ~f:(fun c ->
    let level = int_of_json_exn (member "level" c) in
    let expected = int_of_json_exn (member "expected_level" c) in
    let area = float_u_of_json_exn (member "area" c) in
    let area_1_2 = float_u_of_json_exn (member "area_1_2" c) in
    let area_0_8 = float_u_of_json_exn (member "area_0_8" c) in
    let label s = sprintf "area_loop level=%d %s" level s in
    Alcotest.(check int)
      (label "min_area.level_for_max_value(1.2*a)")
      expected
      (M.get_level_for_max_value M.min_area area_1_2);
    Alcotest.(check int)
      (label "min_area.level_for_min_value(0.8*a)")
      expected
      (M.get_level_for_min_value M.min_area area_0_8);
    Alcotest.(check int)
      (label "min_area.closest(a)")
      expected
      (M.get_closest_level M.min_area area))
;;

(* -- Quickcheck: structural laws on metric values ------------------------ *)

module Level = struct
  type t = { level : int } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    map (int_inclusive 0 M.max_cell_level) ~f:(fun level -> { level })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Level_pos = struct
  (* A level in [0, max_cell_level - 1] so that [level + 1] is also valid. *)
  type t = { level : int } [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    map (int_inclusive 0 (M.max_cell_level - 1)) ~f:(fun level -> { level })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

(* At every level, min <= avg <= max for width, edge, diag, and area metrics. *)
let quickcheck_monotone_at_level () =
  Base_quickcheck.Test.run_exn (module Level) ~config:qc_config ~f:(fun { Level.level } ->
    let check family =
      let lo = metric_by_name ("min_" ^ family) in
      let avg = metric_by_name ("avg_" ^ family) in
      let hi = metric_by_name ("max_" ^ family) in
      let open Float_u.O in
      let lo_v = M.get_value lo level in
      let avg_v = M.get_value avg level in
      let hi_v = M.get_value hi level in
      if not (lo_v <= avg_v && avg_v <= hi_v)
      then
        raise_s
          [%message
            "metric ordering violated"
              (family : string)
              (level : int)
              (Float_u.to_float lo_v : float)
              (Float_u.to_float avg_v : float)
              (Float_u.to_float hi_v : float)]
    in
    List.iter [ "width"; "edge"; "diag"; "area"; "angle_span" ] ~f:check)
;;

(* Scaling law: get_value(l+1) = get_value(l) * 2^(-dim). Matches the C++
   definition [ldexp(deriv, -dim * level)]. *)
let quickcheck_level_scaling () =
  Base_quickcheck.Test.run_exn
    (module Level_pos)
    ~config:qc_config
    ~f:(fun { Level_pos.level } ->
      let next_level = level + 1 in
      let check metric =
        let factor = if M.dim metric = 1 then #0.5 else #0.25 in
        let open Float_u.O in
        let v = M.get_value metric level in
        let v_next = M.get_value metric next_level in
        let expected = v * factor in
        if not (v_next = expected)
        then
          raise_s
            [%message
              "scaling law violated"
                (level : int)
                (M.dim metric : int)
                (Float_u.to_float v : float)
                (Float_u.to_float v_next : float)
                (Float_u.to_float expected : float)]
      in
      List.iter all_metric_names ~f:(fun name -> check (metric_by_name name)))
;;

(* For any metric m and level l, feeding [m.get_value l] back into
   [get_level_for_min_value] or [get_level_for_max_value] recovers l. This is
   the precise postcondition guaranteed by the C++ DCHECKs. *)
let quickcheck_level_roundtrip () =
  Base_quickcheck.Test.run_exn (module Level) ~config:qc_config ~f:(fun { Level.level } ->
    let check_roundtrip metric =
      let v = M.get_value metric level in
      let max_level = M.get_level_for_max_value metric v in
      if max_level <> level
      then
        raise_s
          [%message
            "get_level_for_max_value roundtrip failed"
              (level : int)
              (max_level : int)
              (Float_u.to_float v : float)];
      let min_level = M.get_level_for_min_value metric v in
      if min_level <> level
      then
        raise_s
          [%message
            "get_level_for_min_value roundtrip failed"
              (level : int)
              (min_level : int)
              (Float_u.to_float v : float)]
    in
    List.iter
      [ "min_width"
      ; "max_width"
      ; "min_edge"
      ; "max_edge"
      ; "min_diag"
      ; "max_diag"
      ; "min_area"
      ; "max_area"
      ]
      ~f:(fun name -> check_roundtrip (metric_by_name name)))
;;

(* get_closest_level on min_width.get_value(l) returns l, matching the width
   and area loop assertions in s2metrics_test.cc. *)
let quickcheck_closest_level_roundtrip () =
  Base_quickcheck.Test.run_exn (module Level) ~config:qc_config ~f:(fun { Level.level } ->
    let check metric =
      let v = M.get_value metric level in
      let got = M.get_closest_level metric v in
      if got <> level
      then
        raise_s
          [%message
            "get_closest_level roundtrip failed"
              (level : int)
              (got : int)
              (Float_u.to_float v : float)]
    in
    List.iter [ "min_width"; "min_edge"; "min_diag"; "min_area" ] ~f:(fun name ->
      check (metric_by_name name)))
;;

let () =
  Alcotest.run
    "S2_metrics"
    [ "constants", [ Alcotest.test_case "all metrics" `Quick test_constants ]
    ; "scalars", [ Alcotest.test_case "scalar constants" `Quick test_scalar_constants ]
    ; ( "levels"
      , [ Alcotest.test_case "boundary cases" `Quick test_boundary_level_cases
        ; Alcotest.test_case "closest level" `Quick test_closest_level_cases
        ; Alcotest.test_case "width loop" `Quick test_width_loop
        ; Alcotest.test_case "area loop" `Quick test_area_loop
        ] )
    ; ( "quickcheck"
      , [ Alcotest.test_case "monotone at level" `Quick quickcheck_monotone_at_level
        ; Alcotest.test_case "level scaling" `Quick quickcheck_level_scaling
        ; Alcotest.test_case "level roundtrip" `Quick quickcheck_level_roundtrip
        ; Alcotest.test_case
            "closest level roundtrip"
            `Quick
            quickcheck_closest_level_roundtrip
        ] )
    ]
;;
