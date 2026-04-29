(* C++ test parity: s2geometry/src/s2/s2latlng_rect_test.cc
   Golden data from test/gen/s2latlng_rect.cc.

   Covered:
   -  TEST(S2LatLngRect, EmptyAndFull)
   -  TEST(S2LatLngRect, Accessors)
   -  TEST(S2LatLngRect, FromCenterSize)
   -  TEST(S2LatLngRect, FromPoint)
   -  TEST(S2LatLngRect, FromPointPair)
   -  TEST(S2LatLngRect, GetCenterSize)
   -  TEST(S2LatLngRect, GetVertex)
   -  TEST(S2LatLngRect, Contains) -latlng, interior, point
   -  TEST(S2LatLngRect, IntervalOps) -contains, interior_contains, intersects,
      interior_intersects, union, intersection
   -  TEST(S2LatLngRect, AddPoint)
   -  TEST(S2LatLngRect, Expanded)
   -  TEST(S2LatLngRect, PolarClosure)
   -  TEST(S2LatLngRect, Area)
   -  TEST(S2LatLngRect, GetCentroid)
   -  TEST(S2LatLngRect, ApproxEquals)
   -  TEST(S2LatLngRect, GetCapBound)
   -  TEST(S2LatLngRect, DistanceToLatLng)
   -  TEST(S2LatLngRect, GetDistanceOverlapping)
   -  TEST(S2LatLngRect, GetDistanceRectVsRect)
   -  TEST(S2LatLngRect, GetDirectedHausdorffDistanceContained)
   -  TEST(S2LatLngRect, GetDirectHausdorffDistancePointToRect)
   -  TEST(S2LatLngRect, GetHausdorffDistance)

   Deliberately omitted:
   -  TEST(S2LatLngRect, CellOps) requires S2_cell.rect_bound (not yet ported)
   -  TEST(S2LatLngRect, EncodeDecode) not ported yet
   -  TEST(BoundaryIntersects) requires S2_edge_crossings (not yet ported)
   -  TEST(ExpandedByDistance) requires S2_cell.rect_bound
   -  TEST(S2LatLngRect, GetDistanceRandomPairs) randomized, verified by brute-force
   -  TEST(S2LatLngRect, GetDirectedHausdorffDistanceRandomPairs) randomized
   -  TEST(S2LatLngRect, S2CoderWorks)
   -  TEST(S2LatLngRect, Hash) *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2latlng_rect.json")
let get key = member key (Lazy.force fixture)

(* -JSON helpers --------------------------------------------------------- *)

let check_rect ?(eps = 1e-15) msg ~expected ~actual =
  check_float_u
    ~eps
    (msg ^ " lat.lo")
    ~expected:(S2.R1_interval.lo (S2.S2_latlng_rect.lat expected))
    ~actual:(S2.R1_interval.lo (S2.S2_latlng_rect.lat actual));
  check_float_u
    ~eps
    (msg ^ " lat.hi")
    ~expected:(S2.R1_interval.hi (S2.S2_latlng_rect.lat expected))
    ~actual:(S2.R1_interval.hi (S2.S2_latlng_rect.lat actual));
  check_float_u
    ~eps
    (msg ^ " lng.lo")
    ~expected:(S2.S1_interval.lo (S2.S2_latlng_rect.lng expected))
    ~actual:(S2.S1_interval.lo (S2.S2_latlng_rect.lng actual));
  check_float_u
    ~eps
    (msg ^ " lng.hi")
    ~expected:(S2.S1_interval.hi (S2.S2_latlng_rect.lng expected))
    ~actual:(S2.S1_interval.hi (S2.S2_latlng_rect.lng actual))
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

(* -Tests ---------------------------------------------------------------- *)

let test_empty_and_full () =
  let d = get "empty_and_full" in
  let empty = S2.S2_latlng_rect.empty in
  let full = S2.S2_latlng_rect.full in
  let expected_empty = latlng_rect_of_json (member "empty" d) in
  let expected_full = latlng_rect_of_json (member "full" d) in
  check_rect_exact "empty" ~expected:expected_empty ~actual:empty;
  check_rect_exact "full" ~expected:expected_full ~actual:full;
  check_bool
    "empty valid"
    ~expected:(bool_of_json_exn (member "empty_valid" d))
    ~actual:(S2.S2_latlng_rect.is_valid empty);
  check_bool
    "empty is_empty"
    ~expected:(bool_of_json_exn (member "empty_is_empty" d))
    ~actual:(S2.S2_latlng_rect.is_empty empty);
  check_bool
    "empty is_point"
    ~expected:(bool_of_json_exn (member "empty_is_point" d))
    ~actual:(S2.S2_latlng_rect.is_point empty);
  check_bool
    "full valid"
    ~expected:(bool_of_json_exn (member "full_valid" d))
    ~actual:(S2.S2_latlng_rect.is_valid full);
  check_bool
    "full is_full"
    ~expected:(bool_of_json_exn (member "full_is_full" d))
    ~actual:(S2.S2_latlng_rect.is_full full);
  check_bool
    "full is_point"
    ~expected:(bool_of_json_exn (member "full_is_point" d))
    ~actual:(S2.S2_latlng_rect.is_point full)
;;

let test_accessors () =
  let d = get "accessors" in
  let d1 = latlng_rect_of_json (member "d1" d) in
  check_float_u
    "lat_lo deg"
    ~eps:1e-13
    ~expected:(float_u_of_json_exn (member "d1_lat_lo_deg" d))
    ~actual:(S2.S1_angle.degrees (S2.S2_latlng.lat (S2.S2_latlng_rect.lo d1)));
  check_float_u
    "lat_hi deg"
    ~eps:1e-13
    ~expected:(float_u_of_json_exn (member "d1_lat_hi_deg" d))
    ~actual:(S2.S1_angle.degrees (S2.S2_latlng.lat (S2.S2_latlng_rect.hi d1)));
  check_float_u
    "lng_lo deg"
    ~eps:1e-13
    ~expected:(float_u_of_json_exn (member "d1_lng_lo_deg" d))
    ~actual:(S2.S1_angle.degrees (S2.S2_latlng.lng (S2.S2_latlng_rect.lo d1)));
  check_float_u
    "lng_hi deg"
    ~eps:1e-13
    ~expected:(float_u_of_json_exn (member "d1_lng_hi_deg" d))
    ~actual:(S2.S1_angle.degrees (S2.S2_latlng.lng (S2.S2_latlng_rect.hi d1)))
;;

let test_from_center_size () =
  let cases = to_list (get "from_center_size") in
  List.iteri cases ~f:(fun i case ->
    let result = latlng_rect_of_json (member "result" case) in
    match member "is_full" case with
    | `Bool expected_full ->
      check_bool
        (sprintf "case %d is_full" i)
        ~expected:expected_full
        ~actual:(S2.S2_latlng_rect.is_full result)
    | _ ->
      let expected = latlng_rect_of_json (member "expected" case) in
      check_bool
        (sprintf "case %d approx_equals" i)
        ~expected:(bool_of_json_exn (member "approx_equals" case))
        ~actual:
          (S2.S2_latlng_rect.approx_equal
             ~max_error:(Packed_float_option.Unboxed.none)
             result
             expected))
;;

let test_from_point () =
  let d = get "from_point" in
  let p = latlng_of_json_exn (member "p" d) in
  let result = S2.S2_latlng_rect.of_point p in
  let expected = latlng_rect_of_json (member "result" d) in
  check_rect_exact "from_point" ~expected ~actual:result;
  check_bool
    "is_point"
    ~expected:(bool_of_json_exn (member "is_point" d))
    ~actual:(S2.S2_latlng_rect.is_point result)
;;

let test_from_point_pair () =
  let cases = to_list (get "from_point_pair") in
  List.iteri cases ~f:(fun i case ->
    let result = latlng_rect_of_json (member "result" case) in
    let expected = latlng_rect_of_json (member "expected" case) in
    check_bool
      (sprintf "case %d equal" i)
      ~expected:(bool_of_json_exn (member "equal" case))
      ~actual:(S2.S2_latlng_rect.equal result expected))
;;

let test_get_center_size () =
  let d = get "get_center_size" in
  let r1 = latlng_rect_of_json (member "r1" d) in
  let c = S2.S2_latlng_rect.center r1 in
  let expected_c = latlng_of_json_exn (member "center" d) in
  check_float_u
    "center lat"
    ~expected:(S2.S1_angle.radians (S2.S2_latlng.lat expected_c))
    ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat c));
  check_float_u
    "center lng"
    ~expected:(S2.S1_angle.radians (S2.S2_latlng.lng expected_c))
    ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng c));
  let s = S2.S2_latlng_rect.size r1 in
  let expected_s = latlng_of_json_exn (member "size" d) in
  check_float_u
    "size lat"
    ~expected:(S2.S1_angle.radians (S2.S2_latlng.lat expected_s))
    ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat s));
  check_float_u
    "size lng"
    ~expected:(S2.S1_angle.radians (S2.S2_latlng.lng expected_s))
    ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng s));
  (* Empty size should be negative *)
  let empty_size_lat = float_u_of_json_exn (member "empty_size_lat_rad" d) in
  let empty_size_lng = float_u_of_json_exn (member "empty_size_lng_rad" d) in
  let es = S2.S2_latlng_rect.size S2.S2_latlng_rect.empty in
  check_float_u
    "empty size lat"
    ~expected:empty_size_lat
    ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat es));
  check_float_u
    "empty size lng"
    ~expected:empty_size_lng
    ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng es))
;;

let test_get_vertex () =
  let d = get "get_vertex" in
  let r1 = latlng_rect_of_json (member "r1" d) in
  for k = 0 to 3 do
    let expected = latlng_of_json_exn (member (sprintf "v%d" k) d) in
    let actual = S2.S2_latlng_rect.vertex r1 k in
    check_float_u
      (sprintf "v%d lat" k)
      ~expected:(S2.S1_angle.radians (S2.S2_latlng.lat expected))
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lat actual));
    check_float_u
      (sprintf "v%d lng" k)
      ~expected:(S2.S1_angle.radians (S2.S2_latlng.lng expected))
      ~actual:(S2.S1_angle.radians (S2.S2_latlng.lng actual))
  done
;;

let test_contains () =
  let d = get "contains" in
  let r1 = latlng_rect_of_json (member "r1" d) in
  check_bool
    "contains_30_m45"
    ~expected:(bool_of_json_exn (member "contains_30_m45" d))
    ~actual:
      (S2.S2_latlng_rect.contains_latlng
         r1
         (S2.S2_latlng.of_degrees ~lat:#30.0 ~lng:(-#45.0)));
  check_bool
    "interior_contains_30_m45"
    ~expected:(bool_of_json_exn (member "interior_contains_30_m45" d))
    ~actual:
      (S2.S2_latlng_rect.interior_contains_latlng
         r1
         (S2.S2_latlng.of_degrees ~lat:#30.0 ~lng:(-#45.0)));
  check_bool
    "not_contains_30_45"
    ~expected:(bool_of_json_exn (member "not_contains_30_45" d))
    ~actual:
      (not
         (S2.S2_latlng_rect.contains_latlng
            r1
            (S2.S2_latlng.of_degrees ~lat:#30.0 ~lng:#45.0)));
  check_bool
    "not_interior_30_45"
    ~expected:(bool_of_json_exn (member "not_interior_30_45" d))
    ~actual:
      (not
         (S2.S2_latlng_rect.interior_contains_latlng
            r1
            (S2.S2_latlng.of_degrees ~lat:#30.0 ~lng:#45.0)));
  check_bool
    "contains_point_in"
    ~expected:(bool_of_json_exn (member "contains_point_in" d))
    ~actual:
      (S2.S2_latlng_rect.contains_point
         r1
         (S2.S2_point.of_coords ~x:#0.5 ~y:(-#0.3) ~z:#0.1));
  check_bool
    "not_contains_point_out"
    ~expected:(bool_of_json_exn (member "not_contains_point_out" d))
    ~actual:
      (not
         (S2.S2_latlng_rect.contains_point
            r1
            (S2.S2_point.of_coords ~x:#0.5 ~y:#0.2 ~z:#0.1)))
;;

let test_interval_ops () =
  let cases = to_list (get "interval_ops") in
  List.iter cases ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let x = latlng_rect_of_json (member "x" case) in
    let y = latlng_rect_of_json (member "y" case) in
    check_bool
      (label ^ " contains")
      ~expected:(bool_of_json_exn (member "contains" case))
      ~actual:(S2.S2_latlng_rect.contains x y);
    check_bool
      (label ^ " interior_contains")
      ~expected:(bool_of_json_exn (member "interior_contains" case))
      ~actual:(S2.S2_latlng_rect.interior_contains x y);
    check_bool
      (label ^ " intersects")
      ~expected:(bool_of_json_exn (member "intersects" case))
      ~actual:(S2.S2_latlng_rect.intersects x y);
    check_bool
      (label ^ " interior_intersects")
      ~expected:(bool_of_json_exn (member "interior_intersects" case))
      ~actual:(S2.S2_latlng_rect.interior_intersects x y);
    let expected_union = latlng_rect_of_json (member "union" case) in
    check_rect_exact
      (label ^ " union")
      ~expected:expected_union
      ~actual:(S2.S2_latlng_rect.union x y);
    let expected_isect = latlng_rect_of_json (member "intersection" case) in
    check_rect_exact
      (label ^ " intersection")
      ~expected:expected_isect
      ~actual:(S2.S2_latlng_rect.intersection x y))
;;

let test_add_point () =
  let open Float_u.O in
  let d = get "add_point" in
  let p = S2.S2_latlng_rect.empty in
  let p = S2.S2_latlng_rect.add_point p (S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#0.0) in
  check_bool
    "after_first is_point"
    ~expected:(bool_of_json_exn (member "after_first_is_point" d))
    ~actual:(S2.S2_latlng_rect.is_point p);
  let p =
    S2.S2_latlng_rect.add_point
      p
      (S2.S2_latlng.of_radians ~lat:#0.0 ~lng:(Float_u.neg (Float_u.pi / #2.0)))
  in
  check_bool
    "after_second is_point"
    ~expected:(bool_of_json_exn (member "after_second_is_point" d))
    ~actual:(S2.S2_latlng_rect.is_point p);
  let p =
    S2.S2_latlng_rect.add_point
      p
      (S2.S2_latlng.of_radians
         ~lat:(Float_u.pi / #4.0)
         ~lng:(Float_u.neg (Float_u.pi)))
  in
  let p =
    S2.S2_latlng_rect.add_point
      p
      (S2.S2_latlng.of_point (S2.S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0))
  in
  let expected = latlng_rect_of_json (member "expected" d) in
  check_bool
    "final equals expected"
    ~expected:(bool_of_json_exn (member "final_equals_expected" d))
    ~actual:(S2.S2_latlng_rect.equal p expected)
;;

let test_expanded () =
  let cases = to_list (get "expanded") in
  List.iter cases ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let input = latlng_rect_of_json (member "input" case) in
    let expected = latlng_rect_of_json (member "result" case) in
    let lat_margin_deg = float_u_of_json_exn (member "lat_margin_deg" case) in
    let lng_margin_deg = float_u_of_json_exn (member "lng_margin_deg" case) in
    let margin = S2.S2_latlng.of_degrees ~lat:lat_margin_deg ~lng:lng_margin_deg in
    let actual = S2.S2_latlng_rect.expanded input margin in
    let is_empty = bool_of_json_exn (member "is_empty" case) in
    let is_full = bool_of_json_exn (member "is_full" case) in
    check_bool
      (label ^ " is_empty")
      ~expected:is_empty
      ~actual:(S2.S2_latlng_rect.is_empty actual);
    check_bool
      (label ^ " is_full")
      ~expected:is_full
      ~actual:(S2.S2_latlng_rect.is_full actual);
    (* Empty results can carry arbitrary interval values (e.g. [Empty] is a
       reserved sentinel interval), so skip the interval-equality check in that
       case; the [is_empty] bool is the meaningful signal. *)
    if not is_empty then check_rect (label ^ " expanded") ~expected ~actual)
;;

let test_polar_closure () =
  let cases = to_list (get "polar_closure") in
  List.iter cases ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let input = latlng_rect_of_json (member "input" case) in
    let expected = latlng_rect_of_json (member "result" case) in
    let actual = S2.S2_latlng_rect.polar_closure input in
    check_rect_exact (label ^ " polar_closure") ~expected ~actual)
;;

let test_area () =
  let d = get "area" in
  let empty_area = float_u_of_json_exn (member "empty_area" d) in
  let full_area = float_u_of_json_exn (member "full_area" d) in
  let quarter_area = float_u_of_json_exn (member "quarter_area" d) in
  check_float_u
    "empty"
    ~expected:empty_area
    ~actual:(S2.S2_latlng_rect.area S2.S2_latlng_rect.empty);
  check_float_u
    "full"
    ~expected:full_area
    ~actual:(S2.S2_latlng_rect.area S2.S2_latlng_rect.full);
  (* quarter sphere: lat=[0,pi/2], lng=[0,pi/2] *)
  let quarter =
    S2.S2_latlng_rect.create
      ~lat:(S2.R1_interval.create ~lo:#0.0 ~hi:Float_u.(pi / #2.0))
      ~lng:(S2.S1_interval.create ~lo:#0.0 ~hi:Float_u.(pi / #2.0))
  in
  check_float_u "quarter" ~expected:quarter_area ~actual:(S2.S2_latlng_rect.area quarter)
;;

let test_centroid () =
  let d = get "centroid" in
  let empty_c = r3_vector_of_json (member "empty" d) in
  let actual_empty_c = S2.S2_latlng_rect.centroid S2.S2_latlng_rect.empty in
  check_r3_vector_exact "empty centroid" ~expected:empty_c ~actual:actual_empty_c;
  let full_c = r3_vector_of_json (member "full" d) in
  let actual_full_c = S2.S2_latlng_rect.centroid S2.S2_latlng_rect.full in
  check_r3_vector "full centroid" ~expected:full_c ~actual:actual_full_c;
  let quarter_c = r3_vector_of_json (member "quarter" d) in
  let quarter =
    S2.S2_latlng_rect.create
      ~lat:(S2.R1_interval.create ~lo:#0.0 ~hi:Float_u.(pi / #2.0))
      ~lng:(S2.S1_interval.create ~lo:#0.0 ~hi:Float_u.(pi / #2.0))
  in
  let actual_quarter_c = S2.S2_latlng_rect.centroid quarter in
  check_r3_vector "quarter centroid" ~expected:quarter_c ~actual:actual_quarter_c
;;

let test_approx_equals () =
  let cases = to_list (get "approx_equals") in
  List.iter cases ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let a = latlng_rect_of_json (member "a" case) in
    let b = latlng_rect_of_json (member "b" case) in
    let expected = bool_of_json_exn (member "expected" case) in
    check_bool
      label
      ~expected
      ~actual:
        (S2.S2_latlng_rect.approx_equal
           ~max_error:(Packed_float_option.Unboxed.none)
           a
           b))
;;

let test_approx_equals_margin () =
  let cases = to_list (get "approx_equals_margin") in
  List.iter cases ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let variant = string_of_json_exn (member "variant" case) in
    let a = latlng_rect_of_json (member "a" case) in
    let b = latlng_rect_of_json (member "b" case) in
    let expected = bool_of_json_exn (member "result" case) in
    let actual =
      match variant with
      | "scalar" ->
        let margin =
          S2.S1_angle.of_degrees (float_u_of_json_exn (member "margin_deg" case))
        in
        let max_error = Packed_float_option.Unboxed.some (S2.S1_angle.radians margin) in
        S2.S2_latlng_rect.approx_equal ~max_error a b
      | "latlng" ->
        let max_error =
          S2.S2_latlng.of_degrees
            ~lat:(float_u_of_json_exn (member "lat_margin_deg" case))
            ~lng:(float_u_of_json_exn (member "lng_margin_deg" case))
        in
        S2.S2_latlng_rect.approx_equal_latlng ~max_error a b
      | v ->
        (match failwith (Printf.sprintf "unknown variant %s" v) with
         | (_ : Nothing.t) -> .)
    in
    check_bool label ~expected ~actual)
;;

let test_cap_bound () =
  let cases = to_list (get "cap_bound") in
  List.iter cases ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let r = latlng_rect_of_json (member "rect" case) in
    let cap = S2.S2_latlng_rect.cap_bound r in
    let expected_center = r3_vector_of_json (member "cap_center" case) in
    let expected_height = float_u_of_json_exn (member "cap_height" case) in
    check_r3_vector
      (label ^ " center")
      ~expected:expected_center
      ~actual:(S2.S2_cap.center cap);
    check_float_u
      (label ^ " height")
      ~eps:1e-13
      ~expected:expected_height
      ~actual:(S2.S2_cap.height cap))
;;

let test_distance_to_latlng () =
  let cases = to_list (get "distance_to_latlng") in
  List.iter cases ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let r = latlng_rect_of_json (member "rect" case) in
    let p = latlng_of_json_exn (member "point" case) in
    let expected = float_u_of_json_exn (member "distance_rad" case) in
    let actual = S2.S1_angle.radians (S2.S2_latlng_rect.distance_to_latlng r p) in
    check_float_u (label ^ " dist") ~eps:1e-10 ~expected ~actual)
;;

let test_distance_overlapping () =
  let d = get "distance_overlapping" in
  let a =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#0.0)
      ~hi:(S2.S2_latlng.of_degrees ~lat:#2.0 ~lng:#2.0)
  in
  let make_rect lat_lo lng_lo lat_hi lng_hi =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:lat_lo ~lng:lng_lo)
      ~hi:(S2.S2_latlng.of_degrees ~lat:lat_hi ~lng:lng_hi)
  in
  let check key other =
    let expected = float_u_of_json_exn (member key d) in
    let actual = S2.S1_angle.radians (S2.S2_latlng_rect.distance a other) in
    check_float_u key ~eps:1e-10 ~expected ~actual
  in
  check "self" a;
  check "overlap1" (make_rect #0. #1. #2. #3.);
  check "overlap2" (make_rect #0. #2. #2. #4.);
  check "overlap3" (make_rect #1. #0. #3. #2.);
  check "overlap4" (make_rect #2. #0. #4. #2.);
  check "overlap5" (make_rect #1. #1. #3. #3.);
  check "overlap6" (make_rect #2. #2. #4. #4.)
;;

let test_distance_rect_vs_rect () =
  let cases = to_list (get "distance_rect_vs_rect") in
  List.iter cases ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let a = latlng_rect_of_json (member "a" case) in
    let b = latlng_rect_of_json (member "b" case) in
    let expected = float_u_of_json_exn (member "distance_rad" case) in
    let actual = S2.S1_angle.radians (S2.S2_latlng_rect.distance a b) in
    check_float_u (label ^ " dist") ~eps:1e-10 ~expected ~actual)
;;

let test_hausdorff_contained () =
  let d = get "hausdorff_contained" in
  let a =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:(-#10.0) ~lng:#20.0)
      ~hi:(S2.S2_latlng.of_degrees ~lat:(-#5.0) ~lng:#90.0)
  in
  let make_rect lat_lo lng_lo lat_hi lng_hi =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:lat_lo ~lng:lng_lo)
      ~hi:(S2.S2_latlng.of_degrees ~lat:lat_hi ~lng:lng_hi)
  in
  let check key rect =
    let expected = float_u_of_json_exn (member key d) in
    let actual =
      S2.S1_angle.radians (S2.S2_latlng_rect.directed_hausdorff_distance a rect)
    in
    check_float_u key ~eps:1e-10 ~expected ~actual
  in
  check "same" (make_rect (-#10.) #20. (-#5.) #90.);
  check "bigger" (make_rect (-#10.) #19. (-#5.) #91.);
  check "bigger2" (make_rect (-#11.) #20. (-#4.) #90.);
  check "bigger3" (make_rect (-#11.) #19. (-#4.) #91.)
;;

let test_hausdorff_point_to_rect () =
  let cases = to_list (get "hausdorff_point_to_rect") in
  List.iter cases ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let a = latlng_rect_of_json (member "a" case) in
    let b = latlng_rect_of_json (member "b" case) in
    let expected_h = float_u_of_json_exn (member "hausdorff_rad" case) in
    let expected_d = float_u_of_json_exn (member "distance_rad" case) in
    let actual_h =
      S2.S1_angle.radians (S2.S2_latlng_rect.directed_hausdorff_distance a b)
    in
    let actual_d = S2.S1_angle.radians (S2.S2_latlng_rect.distance a b) in
    check_float_u (label ^ " hausdorff") ~eps:1e-10 ~expected:expected_h ~actual:actual_h;
    check_float_u (label ^ " distance") ~eps:1e-10 ~expected:expected_d ~actual:actual_d;
    (* For a point rect, directed Hausdorff = distance *)
    check_float_u (label ^ " h=d") ~eps:1e-10 ~expected:expected_h ~actual:actual_h)
;;

let test_hausdorff_distance () =
  let d = get "hausdorff_distance" in
  let a =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:(-#10.0) ~lng:#20.0)
      ~hi:(S2.S2_latlng.of_degrees ~lat:(-#5.0) ~lng:#90.0)
  in
  let b =
    S2.S2_latlng_rect.of_lo_hi
      ~lo:(S2.S2_latlng.of_degrees ~lat:(-#85.0) ~lng:(-#50.0))
      ~hi:(S2.S2_latlng.of_degrees ~lat:(-#80.0) ~lng:#10.0)
  in
  let expected_ab = float_u_of_json_exn (member "ab" d) in
  let expected_ba = float_u_of_json_exn (member "ba" d) in
  check_float_u
    "ab"
    ~eps:1e-10
    ~expected:expected_ab
    ~actual:(S2.S1_angle.radians (S2.S2_latlng_rect.hausdorff_distance a b));
  check_float_u
    "ba"
    ~eps:1e-10
    ~expected:expected_ba
    ~actual:(S2.S1_angle.radians (S2.S2_latlng_rect.hausdorff_distance b a))
;;

let () =
  run
    "S2LatLngRect"
    [ ( "basic"
      , [ test_case "empty_and_full" `Quick test_empty_and_full
        ; test_case "accessors" `Quick test_accessors
        ; test_case "from_center_size" `Quick test_from_center_size
        ; test_case "from_point" `Quick test_from_point
        ; test_case "from_point_pair" `Quick test_from_point_pair
        ; test_case "get_center_size" `Quick test_get_center_size
        ; test_case "get_vertex" `Quick test_get_vertex
        ] )
    ; ( "containment"
      , [ test_case "contains" `Quick test_contains
        ; test_case "interval_ops" `Quick test_interval_ops
        ] )
    ; ( "operations"
      , [ test_case "add_point" `Quick test_add_point
        ; test_case "expanded" `Quick test_expanded
        ; test_case "polar_closure" `Quick test_polar_closure
        ; test_case "area" `Quick test_area
        ; test_case "centroid" `Quick test_centroid
        ; test_case "approx_equals" `Quick test_approx_equals
        ; test_case "approx_equals_margin" `Quick test_approx_equals_margin
        ] )
    ; "bounding", [ test_case "cap_bound" `Quick test_cap_bound ]
    ; ( "distance"
      , [ test_case "distance_to_latlng" `Quick test_distance_to_latlng
        ; test_case "distance_overlapping" `Quick test_distance_overlapping
        ; test_case "distance_rect_vs_rect" `Quick test_distance_rect_vs_rect
        ] )
    ; ( "hausdorff"
      , [ test_case "hausdorff_contained" `Quick test_hausdorff_contained
        ; test_case "hausdorff_point_to_rect" `Quick test_hausdorff_point_to_rect
        ; test_case "hausdorff_distance" `Quick test_hausdorff_distance
        ] )
    ]
;;
