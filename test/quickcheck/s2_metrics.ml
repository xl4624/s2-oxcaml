(* Quickcheck property tests for S2_metrics. *)
open Core
module M = S2.S2_metrics

(* --- Generators ----------------------------------------------------------- *)

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

(* --- Properties ----------------------------------------------------------- *)

(* At every level, min <= avg <= max for width, edge, diag, and area metrics. *)
let%test_unit "monotone_at_level" =
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
let%test_unit "level_scaling" =
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
let%test_unit "level_roundtrip" =
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
let%test_unit "closest_level_roundtrip" =
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
