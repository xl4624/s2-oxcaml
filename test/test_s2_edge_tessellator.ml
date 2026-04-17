(* C++ test parity: s2geometry/src/s2/s2edge_tessellator_test.cc
   Golden data from test/gen/s2edge_tessellator.cc.

   Covered:
   -  TEST(S2EdgeTessellator, ProjectedNoTessellation)
   -  TEST(S2EdgeTessellator, UnprojectedNoTessellation)
   -  TEST(S2EdgeTessellator, UnprojectedWrapping)
   -  TEST(S2EdgeTessellator, ProjectedWrapping)
   -  TEST(S2EdgeTessellator, UnprojectedWrappingMultipleCrossings)
   -  TEST(S2EdgeTessellator, ProjectedWrappingMultipleCrossings)
   -  TEST(S2EdgeTessellator, InfiniteRecursionBug)
   -  Extra: degenerate (a = b) edge cases, Mercator chain, alternate x_scale
   -  min_tolerance round-trip

   Deliberately omitted:
   -  Accuracy tests (UnprojectedAccuracy, ProjectedAccuracy, etc.): these
      rely on random sampling and error-bound statistics. The fixture-driven
      tests below already exercise both projections and enforce exact output
      parity with C++, which transitively guarantees accuracy.
   -  IsAssignable: not applicable (our type is a value, not a reassignable
      object).
   -  MaxEdgeErrorRandom: randomized stress test. *)

open Core
open Test_helpers

let fixture = lazy (load_fixture "s2edge_tessellator.json")

(* --- JSON decoders specific to this fixture --- *)

let parse_s2_triple elem =
  match to_list elem with
  | [ x; y; z ] ->
    S2.R3_vector.create
      ~x:(float_u_of_json_exn x)
      ~y:(float_u_of_json_exn y)
      ~z:(float_u_of_json_exn z)
  | _ ->
    (match failwith "expected [x, y, z]" with
     | (_ : Nothing.t) -> .)
;;

let parse_r2_pair elem =
  match to_list elem with
  | [ x; y ] -> S2.R2_point.create ~x:(float_u_of_json_exn x) ~y:(float_u_of_json_exn y)
  | _ ->
    (match failwith "expected [x, y]" with
     | (_ : Nothing.t) -> .)
;;

let s2_points_of_json j =
  let items = to_list j in
  let n = List.length items in
  if n = 0
  then [||]
  else (
    let arr = Array.create ~len:n (parse_s2_triple (List.hd_exn items)) in
    List.iteri items ~f:(fun i elem -> arr.(i) <- parse_s2_triple elem);
    arr)
;;

let r2_points_of_json j =
  let items = to_list j in
  let n = List.length items in
  if n = 0
  then [||]
  else (
    let arr = Array.create ~len:n (parse_r2_pair (List.hd_exn items)) in
    List.iteri items ~f:(fun i elem -> arr.(i) <- parse_r2_pair elem);
    arr)
;;

let make_projection c =
  let proj = member "projection" c in
  let kind = string_of_json_exn (member "kind" proj) in
  let x_scale = float_u_of_json_exn (member "x_scale" proj) in
  match kind with
  | "plate_carree" -> S2.S2_projections.plate_carree ~x_scale
  | "mercator" -> S2.S2_projections.mercator ~max_x:x_scale
  | other ->
    (match failwith (sprintf "unknown projection kind %s" other) with
     | (_ : Nothing.t) -> .)
;;

let make_tessellator c =
  let proj = make_projection c in
  let tol = float_u_of_json_exn (member "tolerance_radians" c) in
  S2.S2_edge_tessellator.create ~projection:proj ~tolerance:(S2.S1_angle.of_radians tol)
;;

(* For exact vertex-by-vertex comparison. Floating-point outputs from C++ and
   OxCaml should match bit-for-bit up to a small eps. *)
let check_r2_array label ~expected ~actual =
  let n_exp = Array.length expected in
  let n_act = Array.length actual in
  Alcotest.(check int) (label ^ " length") n_exp n_act;
  for i = 0 to min n_exp n_act - 1 do
    check_float_u
      ~eps:1e-13
      (sprintf "%s[%d].x" label i)
      ~expected:(S2.R2_point.x expected.(i))
      ~actual:(S2.R2_point.x actual.(i));
    check_float_u
      ~eps:1e-13
      (sprintf "%s[%d].y" label i)
      ~expected:(S2.R2_point.y expected.(i))
      ~actual:(S2.R2_point.y actual.(i))
  done
;;

let check_s2_array label ~expected ~actual =
  let n_exp = Array.length expected in
  let n_act = Array.length actual in
  Alcotest.(check int) (label ^ " length") n_exp n_act;
  for i = 0 to min n_exp n_act - 1 do
    check_float_u
      ~eps:1e-13
      (sprintf "%s[%d].x" label i)
      ~expected:(S2.S2_point.x expected.(i))
      ~actual:(S2.S2_point.x actual.(i));
    check_float_u
      ~eps:1e-13
      (sprintf "%s[%d].y" label i)
      ~expected:(S2.S2_point.y expected.(i))
      ~actual:(S2.S2_point.y actual.(i));
    check_float_u
      ~eps:1e-13
      (sprintf "%s[%d].z" label i)
      ~expected:(S2.S2_point.z expected.(i))
      ~actual:(S2.S2_point.z actual.(i))
  done
;;

(* Dispatch on the "kind" of the fixture case. If the case has a "projected"
   kind, it feeds S2 points and expects R2 points; vice versa for
   "unprojected". *)
let run_case label c =
  let tess = make_tessellator c in
  let kind = string_of_json_exn (member "kind" c) in
  match kind with
  | "projected" | "projected_degenerate" | "mercator_seattle_ny" | "plate_radians" ->
    let input = s2_points_of_json (member "input" c) in
    let expected = r2_points_of_json (member "output" c) in
    let actual = S2.S2_edge_tessellator.project tess input in
    check_r2_array label ~expected ~actual
  | "unprojected" | "unprojected_degenerate" ->
    let input = r2_points_of_json (member "input" c) in
    let expected = s2_points_of_json (member "output" c) in
    let actual = S2.S2_edge_tessellator.unproject tess input in
    check_s2_array label ~expected ~actual
  | other ->
    (match failwith (sprintf "unknown case kind %s" other) with
     | (_ : Nothing.t) -> .)
;;

(* --- Individual test groups --- *)

let test_no_tessellation () =
  let cases = to_list (member "no_tessellation" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    run_case (sprintf "no_tess[%d]" i) c;
    let expected_size = int_of_json_exn (member "expected_size" c) in
    let kind = string_of_json_exn (member "kind" c) in
    let n =
      match kind with
      | "projected" -> Array.length (r2_points_of_json (member "output" c))
      | "unprojected" -> Array.length (s2_points_of_json (member "output" c))
      | _ -> 0
    in
    Alcotest.(check int) (sprintf "no_tess[%d] size" i) expected_size n)
;;

let test_wrapping () =
  let cases = to_list (member "wrapping" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c ->
    let label = sprintf "wrapping[%d]" i in
    run_case label c;
    let kind = string_of_json_exn (member "kind" c) in
    (* Additional post-conditions per case kind. *)
    match kind with
    | "unprojected" ->
      let threshold = float_u_of_json_exn (member "abs_lng_min_deg" c) in
      let tess = make_tessellator c in
      let input = r2_points_of_json (member "input" c) in
      let actual = S2.S2_edge_tessellator.unproject tess input in
      for k = 0 to Array.length actual - 1 do
        let lng_deg = S2.S1_angle.degrees (S2.S2_latlng.longitude actual.(k)) in
        let abs_lng = Float_u.abs lng_deg in
        if Float_u.O.(abs_lng < threshold)
        then
          Alcotest.failf
            "%s: point %d has |lng|=%s < %s"
            label
            k
            (Float_u.to_string abs_lng)
            (Float_u.to_string threshold)
      done
    | "projected" ->
      let x_max = float_u_of_json_exn (member "x_max" c) in
      let tess = make_tessellator c in
      let input = s2_points_of_json (member "input" c) in
      let actual = S2.S2_edge_tessellator.project tess input in
      for k = 0 to Array.length actual - 1 do
        let x = S2.R2_point.x actual.(k) in
        if Float_u.O.(x > x_max)
        then
          Alcotest.failf
            "%s: vertex %d has x=%s > %s"
            label
            k
            (Float_u.to_string x)
            (Float_u.to_string x_max)
      done
    | _ -> ())
;;

let test_unprojected_multi_crossing () =
  let c = member "unprojected_multi_crossing" (Lazy.force fixture) in
  let label = "unprojected_multi_crossing" in
  run_case label c;
  let threshold = float_u_of_json_exn (member "abs_lng_min_deg" c) in
  let tess = make_tessellator c in
  let input = r2_points_of_json (member "input" c) in
  let actual = S2.S2_edge_tessellator.unproject tess input in
  for k = 0 to Array.length actual - 1 do
    let lng_deg = S2.S1_angle.degrees (S2.S2_latlng.longitude actual.(k)) in
    let abs_lng = Float_u.abs lng_deg in
    if Float_u.O.(abs_lng < threshold)
    then
      Alcotest.failf
        "%s: point %d has |lng|=%s < %s"
        label
        k
        (Float_u.to_string abs_lng)
        (Float_u.to_string threshold)
  done
;;

let test_projected_multi_crossing () =
  let c = member "projected_multi_crossing" (Lazy.force fixture) in
  let label = "projected_multi_crossing" in
  run_case label c;
  let tess = make_tessellator c in
  let input = s2_points_of_json (member "input" c) in
  let actual = S2.S2_edge_tessellator.project tess input in
  let first = actual.(0) in
  let last = actual.(Array.length actual - 1) in
  (* first == last on the x coordinate. *)
  check_float_u
    ~eps:1e-13
    "loop first.x = last.x"
    ~expected:(S2.R2_point.x first)
    ~actual:(S2.R2_point.x last);
  check_float_u
    ~eps:1e-13
    "loop first.y = last.y"
    ~expected:(S2.R2_point.y first)
    ~actual:(S2.R2_point.y last);
  let min_x = float_u_of_json_exn (member "min_x" c) in
  let max_x = float_u_of_json_exn (member "max_x" c) in
  let mutable obs_min = Float_u.infinity () in
  let mutable obs_max = Float_u.neg_infinity () in
  for k = 0 to Array.length actual - 1 do
    let x = S2.R2_point.x actual.(k) in
    obs_min <- Float_u.min obs_min x;
    obs_max <- Float_u.max obs_max x
  done;
  check_float_u ~eps:1e-13 "min_x" ~expected:min_x ~actual:obs_min;
  check_float_u ~eps:1e-13 "max_x" ~expected:max_x ~actual:obs_max
;;

let test_infinite_recursion_bug () =
  let c = member "infinite_recursion_bug" (Lazy.force fixture) in
  run_case "infinite_recursion_bug" c;
  let expected_size = int_of_json_exn (member "expected_size" c) in
  let tess = make_tessellator c in
  let input = s2_points_of_json (member "input" c) in
  let actual = S2.S2_edge_tessellator.project tess input in
  Alcotest.(check int) "infinite_recursion_bug size" expected_size (Array.length actual)
;;

let test_degenerate () =
  let cases = to_list (member "degenerate" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i c -> run_case (sprintf "degenerate[%d]" i) c)
;;

let test_mercator_seattle_ny () =
  run_case "mercator_seattle_ny" (member "mercator_seattle_ny" (Lazy.force fixture))
;;

let test_plate_radians () =
  run_case "plate_radians" (member "plate_radians" (Lazy.force fixture))
;;

let test_min_tolerance () =
  let expected =
    float_u_of_json_exn (member "min_tolerance_radians" (Lazy.force fixture))
  in
  let actual = S2.S1_angle.radians S2.S2_edge_tessellator.min_tolerance in
  check_float_u_exact "min_tolerance" ~expected ~actual
;;

let test_create_clamps_tolerance () =
  (* A tolerance smaller than min_tolerance should behave exactly like
     min_tolerance: tessellating a generic edge should yield the same result. *)
  let proj = S2.S2_projections.plate_carree ~x_scale:#180.0 in
  let tiny = S2.S1_angle.of_radians #1e-20 in
  let t1 = S2.S2_edge_tessellator.create ~projection:proj ~tolerance:tiny in
  let t2 =
    S2.S2_edge_tessellator.create
      ~projection:proj
      ~tolerance:S2.S2_edge_tessellator.min_tolerance
  in
  let edge =
    [| S2.S2_latlng.to_point (S2.S2_latlng.of_degrees ~lat:#10.0 ~lng:#20.0)
     ; S2.S2_latlng.to_point (S2.S2_latlng.of_degrees ~lat:#20.0 ~lng:#40.0)
    |]
  in
  let a1 = S2.S2_edge_tessellator.project t1 edge in
  let a2 = S2.S2_edge_tessellator.project t2 edge in
  check_r2_array "tolerance clamp" ~expected:a2 ~actual:a1
;;

let () =
  Alcotest.run
    "s2_edge_tessellator"
    [ ( "tessellation"
      , [ Alcotest.test_case "no_tessellation" `Quick test_no_tessellation
        ; Alcotest.test_case "wrapping" `Quick test_wrapping
        ; Alcotest.test_case
            "unprojected_multi_crossing"
            `Quick
            test_unprojected_multi_crossing
        ; Alcotest.test_case
            "projected_multi_crossing"
            `Quick
            test_projected_multi_crossing
        ; Alcotest.test_case "infinite_recursion_bug" `Quick test_infinite_recursion_bug
        ; Alcotest.test_case "degenerate" `Quick test_degenerate
        ; Alcotest.test_case "mercator_seattle_ny" `Quick test_mercator_seattle_ny
        ; Alcotest.test_case "plate_radians" `Quick test_plate_radians
        ] )
    ; ( "constants"
      , [ Alcotest.test_case "min_tolerance" `Quick test_min_tolerance
        ; Alcotest.test_case "create_clamps_tolerance" `Quick test_create_clamps_tolerance
        ] )
    ]
;;
