(* C++ test parity: s2geometry/src/s2/s2latlng_rect_bounder_test.cc
   Golden data from test/gen/s2latlng_rect_bounder.cc.

   Covered:
   -  TEST(RectBounder, MaxLatitudeSimple) - vertex cube, interior CW/CCW, poles
   -  TEST(RectBounder, NearlyIdenticalOrAntipodalPoints) - identical and
      exact antipodal cases
   -  TEST(RectBounder, ExpandForSubregions)
   -  TEST(RectBounder, AccuracyBug)
   -  Multi-step accumulation via successive add_point / get_bound
   -  AddPoint-vs-AddLatLng equivalence for normalized inputs
   -  MaxErrorForTests values

   Deliberately omitted:
   -  TEST(RectBounder, MaxLatitudeRandom) randomized
   -  TEST(RectBounder, NearlyIdenticalOrAntipodalPoints) full 10,000-iteration
      random sweep; spot-checked exactly via fixture
   -  Benchmarks (BM_AddPoints, BM_AddLatLngAsPoints, BM_AddLatLngAsLatLng) *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2latlng_rect_bounder.json")
let get key = member key (Lazy.force fixture)

let point_of_json j =
  match to_list j with
  | [ x; y; z ] ->
    S2.R3_vector.create
      ~x:(float_u_of_json_exn x)
      ~y:(float_u_of_json_exn y)
      ~z:(float_u_of_json_exn z)
  | _ ->
    (match failwith "expected [x, y, z]" with
     | (_ : Nothing.t) -> .)
;;

let latlng_rect_of_json j =
  let lat = r1_interval_of_json (member "lat" j) in
  let lng = s1_interval_of_json (member "lng" j) in
  S2.S2_latlng_rect.create ~lat ~lng
;;

let check_rect_exact msg ~expected ~actual =
  check_float_u_exact
    (msg ^ " lat.lo")
    ~expected:(S2.R1_interval.lo (S2.S2_latlng_rect.lat expected))
    ~actual:(S2.R1_interval.lo (S2.S2_latlng_rect.lat actual));
  check_float_u_exact
    (msg ^ " lat.hi")
    ~expected:(S2.R1_interval.hi (S2.S2_latlng_rect.lat expected))
    ~actual:(S2.R1_interval.hi (S2.S2_latlng_rect.lat actual));
  check_float_u_exact
    (msg ^ " lng.lo")
    ~expected:(S2.S1_interval.lo (S2.S2_latlng_rect.lng expected))
    ~actual:(S2.S1_interval.lo (S2.S2_latlng_rect.lng actual));
  check_float_u_exact
    (msg ^ " lng.hi")
    ~expected:(S2.S1_interval.hi (S2.S2_latlng_rect.lng expected))
    ~actual:(S2.S1_interval.hi (S2.S2_latlng_rect.lng actual))
;;

let get_edge_bound a b =
  let bounder = S2.S2_latlng_rect_bounder.create () in
  let bounder = S2.S2_latlng_rect_bounder.add_point bounder a in
  let bounder = S2.S2_latlng_rect_bounder.add_point bounder b in
  S2.S2_latlng_rect_bounder.get_bound bounder
;;

(* -Tests ---------------------------------------------------------------- *)

let test_max_error_for_tests () =
  let d = get "max_error_for_tests" in
  let expected_lat = float_u_of_json_exn (member "lat_rad" d) in
  let expected_lng = float_u_of_json_exn (member "lng_rad" d) in
  let err = S2.S2_latlng_rect_bounder.max_error_for_tests () in
  check_float_u_exact
    "max_error lat"
    ~expected:expected_lat
    ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat err));
  check_float_u_exact
    "max_error lng"
    ~expected:expected_lng
    ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng err))
;;

let test_max_latitude_vertex_cases () =
  let cases = to_list (get "max_latitude_vertex_cases") in
  List.iter cases ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let a = point_of_json (member "a" case) in
    let b = point_of_json (member "b" case) in
    let expected = latlng_rect_of_json (member "bound" case) in
    let actual = get_edge_bound a b in
    check_rect_exact ("vertex_cube " ^ label) ~expected ~actual)
;;

let test_max_latitude_interior_cases () =
  let cases = to_list (get "max_latitude_interior_cases") in
  List.iter cases ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let a = point_of_json (member "a" case) in
    let b = point_of_json (member "b" case) in
    let expected = latlng_rect_of_json (member "bound" case) in
    let actual = get_edge_bound a b in
    check_rect_exact ("interior " ^ label) ~expected ~actual)
;;

let test_polar_edges () =
  let d = get "polar_edges" in
  let north_expected = latlng_rect_of_json (member "north_bound" d) in
  let north =
    get_edge_bound
      (S2.R3_vector.create ~x:#0.3 ~y:#0.4 ~z:#1.0 |> S2.R3_vector.normalize)
      (S2.R3_vector.create ~x:(-#0.3) ~y:(-#0.4) ~z:#1.0 |> S2.R3_vector.normalize)
  in
  check_rect_exact "north_polar" ~expected:north_expected ~actual:north;
  (* The lat.hi should exactly equal pi/2 (90 degrees). *)
  let expected_hi = float_u_of_json_exn (member "north_lat_hi" d) in
  check_float_u_exact
    "north lat_hi = pi/2"
    ~expected:expected_hi
    ~actual:(S2.R1_interval.hi (S2.S2_latlng_rect.lat north));
  let south_expected = latlng_rect_of_json (member "south_bound" d) in
  let south =
    get_edge_bound
      (S2.R3_vector.create ~x:#0.3 ~y:#0.4 ~z:(-#1.0) |> S2.R3_vector.normalize)
      (S2.R3_vector.create ~x:(-#0.3) ~y:(-#0.4) ~z:(-#1.0) |> S2.R3_vector.normalize)
  in
  check_rect_exact "south_polar" ~expected:south_expected ~actual:south;
  let expected_lo = float_u_of_json_exn (member "south_lat_lo" d) in
  check_float_u_exact
    "south lat_lo = -pi/2"
    ~expected:expected_lo
    ~actual:(S2.R1_interval.lo (S2.S2_latlng_rect.lat south))
;;

module Cell = struct
  type t = { mutable v : S2.S2_latlng_rect_bounder.t }
end

let test_chain_accumulation () =
  let d = get "chain_accumulation" in
  let pts_json = to_list (member "points" d) in
  let steps = to_list (member "steps" d) in
  let cell = { Cell.v = S2.S2_latlng_rect_bounder.create () } in
  List.iter2_exn pts_json steps ~f:(fun pt_j step ->
    let pt = point_of_json pt_j in
    cell.v <- S2.S2_latlng_rect_bounder.add_point cell.v pt;
    let expected = latlng_rect_of_json (member "bound" step) in
    let after = int_of_json_exn (member "after" step) in
    let actual = S2.S2_latlng_rect_bounder.get_bound cell.v in
    check_rect_exact (sprintf "step after=%d" after) ~expected ~actual);
  let expected_final = latlng_rect_of_json (member "final" d) in
  let actual_final = S2.S2_latlng_rect_bounder.get_bound cell.v in
  check_rect_exact "final" ~expected:expected_final ~actual:actual_final
;;

let test_single_point () =
  let d = get "single_point" in
  let bounder = S2.S2_latlng_rect_bounder.create () in
  let before_expected = latlng_rect_of_json (member "bound_before" d) in
  let before_actual = S2.S2_latlng_rect_bounder.get_bound bounder in
  (* "empty" bound expanded and polar-closed is still empty. *)
  check_bool
    "before is empty"
    ~expected:(S2.S2_latlng_rect.is_empty before_expected)
    ~actual:(S2.S2_latlng_rect.is_empty before_actual);
  let p = point_of_json (member "point" d) in
  let bounder = S2.S2_latlng_rect_bounder.add_point bounder p in
  let after_expected = latlng_rect_of_json (member "bound_after" d) in
  let after_actual = S2.S2_latlng_rect_bounder.get_bound bounder in
  check_rect_exact "after_single_point" ~expected:after_expected ~actual:after_actual
;;

let test_add_latlng_vs_point () =
  let d = get "add_latlng_vs_point" in
  let a = S2.S2_latlng.of_degrees ~lat:#10.0 ~lng:#20.0 in
  let b = S2.S2_latlng.of_degrees ~lat:#30.0 ~lng:#40.0 in
  let via_point_bounder = S2.S2_latlng_rect_bounder.create () in
  let via_point_bounder =
    S2.S2_latlng_rect_bounder.add_point via_point_bounder (S2.S2_latlng.to_point a)
  in
  let via_point_bounder =
    S2.S2_latlng_rect_bounder.add_point via_point_bounder (S2.S2_latlng.to_point b)
  in
  let via_latlng_bounder = S2.S2_latlng_rect_bounder.create () in
  let via_latlng_bounder = S2.S2_latlng_rect_bounder.add_latlng via_latlng_bounder a in
  let via_latlng_bounder = S2.S2_latlng_rect_bounder.add_latlng via_latlng_bounder b in
  let expected_via_point = latlng_rect_of_json (member "via_point" d) in
  let expected_via_latlng = latlng_rect_of_json (member "via_latlng" d) in
  check_rect_exact
    "via_point"
    ~expected:expected_via_point
    ~actual:(S2.S2_latlng_rect_bounder.get_bound via_point_bounder);
  check_rect_exact
    "via_latlng"
    ~expected:expected_via_latlng
    ~actual:(S2.S2_latlng_rect_bounder.get_bound via_latlng_bounder)
;;

let test_identical_or_antipodal () =
  let d = get "identical_or_antipodal" in
  let a = point_of_json (member "a" d) in
  let expected = latlng_rect_of_json (member "bound" d) in
  let actual = get_edge_bound a a in
  check_rect_exact "identical" ~expected ~actual;
  let expected_antipodal = latlng_rect_of_json (member "antipodal_bound" d) in
  let actual_antipodal =
    get_edge_bound
      a
      (S2.R3_vector.create
         ~x:(Float_u.neg (S2.R3_vector.x a))
         ~y:(Float_u.neg (S2.R3_vector.y a))
         ~z:(Float_u.neg (S2.R3_vector.z a)))
  in
  check_rect_exact "antipodal" ~expected:expected_antipodal ~actual:actual_antipodal;
  check_bool
    "antipodal_is_full"
    ~expected:(bool_of_json_exn (member "antipodal_is_full" d))
    ~actual:(S2.S2_latlng_rect.is_full actual_antipodal)
;;

let test_expand_for_subregions () =
  let cases = to_list (get "expand_for_subregions") in
  List.iter cases ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let input = latlng_rect_of_json (member "in" case) in
    let expected = latlng_rect_of_json (member "out" case) in
    let actual = S2.S2_latlng_rect_bounder.expand_for_subregions input in
    check_rect_exact ("expand " ^ label) ~expected ~actual;
    check_bool
      ("expand " ^ label ^ " is_full")
      ~expected:(bool_of_json_exn (member "is_full" case))
      ~actual:(S2.S2_latlng_rect.is_full actual))
;;

let test_expand_full_empty () =
  let d = get "expand_full_empty" in
  check_bool
    "full_in_full_out"
    ~expected:(bool_of_json_exn (member "full_in_full_out" d))
    ~actual:
      (S2.S2_latlng_rect.is_full
         (S2.S2_latlng_rect_bounder.expand_for_subregions S2.S2_latlng_rect.full));
  check_bool
    "empty_in_empty_out"
    ~expected:(bool_of_json_exn (member "empty_in_empty_out" d))
    ~actual:
      (S2.S2_latlng_rect.is_empty
         (S2.S2_latlng_rect_bounder.expand_for_subregions S2.S2_latlng_rect.empty))
;;

let test_accuracy_bug () =
  let d = get "accuracy_bug" in
  let a = point_of_json (member "a" d) in
  let b = point_of_json (member "b" d) in
  let c = point_of_json (member "c" d) in
  let ab = get_edge_bound a b in
  let ac = get_edge_bound a c in
  let bc = get_edge_bound b c in
  let ac_expanded = S2.S2_latlng_rect_bounder.expand_for_subregions ac in
  check_rect_exact "ab" ~expected:(latlng_rect_of_json (member "ab" d)) ~actual:ab;
  check_rect_exact "ac" ~expected:(latlng_rect_of_json (member "ac" d)) ~actual:ac;
  check_rect_exact "bc" ~expected:(latlng_rect_of_json (member "bc" d)) ~actual:bc;
  check_rect_exact
    "ac_expanded"
    ~expected:(latlng_rect_of_json (member "ac_expanded" d))
    ~actual:ac_expanded;
  check_bool
    "ac_expanded.lat_hi >= ab.lat_hi"
    ~expected:(bool_of_json_exn (member "ac_expanded_ge_ab_hi" d))
    ~actual:
      Float_u.O.(
        S2.R1_interval.hi (S2.S2_latlng_rect.lat ac_expanded)
        >= S2.R1_interval.hi (S2.S2_latlng_rect.lat ab));
  check_bool
    "ac_expanded.lat_hi >= ac.lat_hi"
    ~expected:(bool_of_json_exn (member "ac_expanded_ge_ac_hi" d))
    ~actual:
      Float_u.O.(
        S2.R1_interval.hi (S2.S2_latlng_rect.lat ac_expanded)
        >= S2.R1_interval.hi (S2.S2_latlng_rect.lat ac))
;;

let () =
  run
    "S2LatLngRectBounder"
    [ ( "basic"
      , [ test_case "max_error_for_tests" `Quick test_max_error_for_tests
        ; test_case "single_point" `Quick test_single_point
        ; test_case "add_latlng_vs_point" `Quick test_add_latlng_vs_point
        ; test_case "chain_accumulation" `Quick test_chain_accumulation
        ] )
    ; ( "max_latitude"
      , [ test_case "vertex_cases" `Quick test_max_latitude_vertex_cases
        ; test_case "interior_cases" `Quick test_max_latitude_interior_cases
        ; test_case "polar_edges" `Quick test_polar_edges
        ] )
    ; ( "degenerate"
      , [ test_case "identical_or_antipodal" `Quick test_identical_or_antipodal
        ; test_case "accuracy_bug" `Quick test_accuracy_bug
        ] )
    ; ( "expand_for_subregions"
      , [ test_case "cases" `Quick test_expand_for_subregions
        ; test_case "full_empty" `Quick test_expand_full_empty
        ] )
    ]
;;
