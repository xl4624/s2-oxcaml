(* Golden data produced by test/gen/s2shape_index_region.cc.

   Upstream C++ tests covered:
   - TEST(S2ShapeIndexRegion, GetCapBound) - via "smaller_polygon_at_cell"
   - TEST(S2ShapeIndexRegion, GetRectBound) - via "smaller_polygon_at_cell"
   - TEST(S2ShapeIndexRegion, GetCellUnionBoundMultipleFaces) - via
     "cell_union_multiple_faces"
   - TEST(S2ShapeIndexRegion, GetCellUnionBoundOneFace) - via "cell_union_one_face"
   - TEST(S2ShapeIndexRegion, ContainsCellMultipleShapes) - via
     "contains_cell_multiple_shapes" (true branch) and "contains_cell_smaller_polygon"
     (false branch)
   - TEST(S2ShapeIndexRegion, IntersectsShrunkenCell) - via "intersects_shrunken_cell"
   - TEST(S2ShapeIndexRegion, IntersectsExactCell) - via "intersects_exact_cell"

   Extra coverage:
   - "contains_point_loop_and_polyline" exercises [contains_point] under the [Semi_open]
     vertex model with a polygon-and-polyline mix.

   Deliberately omitted:
   - The TEST(VisitIntersectingShapes, ...) randomized property tests are not ported
     because the C++ harness depends on [S2Fractal] (not yet available) and on a generic
     visitor API that is intentionally not part of the [t] surface yet. *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2shape_index_region.json")
let get key = member key (Lazy.force fixture)
let s2_point_of_json j = r3_vector_of_json j

let points_of_json j =
  let xs = to_list j in
  let n = List.length xs in
  if n = 0
  then [||]
  else (
    let first = s2_point_of_json (List.hd_exn xs) in
    let arr = Array.create ~len:n first in
    List.iteri xs ~f:(fun i p -> arr.(i) <- s2_point_of_json p);
    arr)
;;

let shape_of_json j =
  let kind = string_of_json_exn (member "kind" j) in
  match kind with
  | "lax_loop" ->
    let vertices = points_of_json (member "vertices" j) in
    S2.S2_lax_loop.to_shape (S2.S2_lax_loop.of_vertices vertices)
  | "points" ->
    let points = points_of_json (member "points" j) in
    S2.S2_point_vector_shape.to_shape (S2.S2_point_vector_shape.of_points points)
  | "polyline" ->
    let vertices = points_of_json (member "vertices" j) in
    S2.S2_lax_polyline.to_shape (S2.S2_lax_polyline.of_vertices vertices)
  | other ->
    (match failwithf "unknown shape kind: %s" other () with
     | (_ : Nothing.t) -> .)
;;

let index_of_json j =
  let index = S2.S2_shape_index.create () in
  List.iter (to_list j) ~f:(fun shape_json ->
    let shape = shape_of_json shape_json in
    let (_ : int) = S2.S2_shape_index.add index shape in
    ());
  S2.S2_shape_index.build index;
  index
;;

let check_cap_contains msg ~expected ~actual =
  if not (S2.S2_cap.contains_cap actual expected)
  then
    Alcotest.failf
      "%s: actual cap does not contain expected cap (expected %s, actual %s)"
      msg
      (Sexp.to_string (S2.S2_cap.sexp_of_t expected))
      (Sexp.to_string (S2.S2_cap.sexp_of_t actual))
;;

let check_radius_close msg ~expected_radians ~actual_radians =
  let bound = Float_u.O.(expected_radians * #1.00001) in
  if Float_u.O.(actual_radians > bound)
  then
    Alcotest.failf
      "%s: actual radius %g exceeds 1.00001 * expected radius %g"
      msg
      (Float_u.to_float actual_radians)
      (Float_u.to_float expected_radians)
;;

(* The C++ assertion is just [EXPECT_EQ(index_bound, cell_bound)] for the single-cell
   case, but the OCaml fixture stores the actual region rect computed by C++. Compare the
   four corners with a tight tolerance. *)
let check_rect_close msg ~expected ~actual =
  let exp_lat = S2.S2_latlng_rect.lat expected in
  let act_lat = S2.S2_latlng_rect.lat actual in
  let exp_lng = S2.S2_latlng_rect.lng expected in
  let act_lng = S2.S2_latlng_rect.lng actual in
  check_float_u
    (msg ^ " lat_lo")
    ~expected:(S2.R1_interval.lo exp_lat)
    ~actual:(S2.R1_interval.lo act_lat);
  check_float_u
    (msg ^ " lat_hi")
    ~expected:(S2.R1_interval.hi exp_lat)
    ~actual:(S2.R1_interval.hi act_lat);
  check_float_u
    (msg ^ " lng_lo")
    ~expected:(S2.S1_interval.lo exp_lng)
    ~actual:(S2.S1_interval.lo act_lng);
  check_float_u
    (msg ^ " lng_hi")
    ~expected:(S2.S1_interval.hi exp_lng)
    ~actual:(S2.S1_interval.hi act_lng)
;;

let cap_of_json j =
  let center = s2_point_of_json (member "center" j) in
  let height = float_u_of_json_exn (member "height" j) in
  S2.S2_cap.of_center_height center height
;;

let rect_of_json j =
  let lat_lo = float_u_of_json_exn (member "lat_lo_radians" j) in
  let lat_hi = float_u_of_json_exn (member "lat_hi_radians" j) in
  let lng_lo = float_u_of_json_exn (member "lng_lo_radians" j) in
  let lng_hi = float_u_of_json_exn (member "lng_hi_radians" j) in
  S2.S2_latlng_rect.create
    ~lat:(S2.R1_interval.create ~lo:lat_lo ~hi:lat_hi)
    ~lng:(S2.S1_interval.create ~lo:lng_lo ~hi:lng_hi)
;;

let cell_ids_of_json j =
  let xs = to_list j in
  let n = List.length xs in
  let arr = Array.create ~len:n S2.S2_cell_id.none in
  List.iteri xs ~f:(fun i s -> arr.(i) <- s2_cell_id_of_json s);
  arr
;;

let check_cell_ids msg ~expected ~actual =
  let exp_n = Array.length expected in
  let act_n = Array.length actual in
  if exp_n <> act_n
  then Alcotest.failf "%s: expected %d cell ids, got %d" msg exp_n act_n
  else
    for i = 0 to exp_n - 1 do
      check_cell_id (sprintf "%s [%d]" msg i) expected.(i) actual.(i)
    done
;;

let test_cases () =
  List.iter
    (to_list (get "cases"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let index = index_of_json (member "shapes" case) in
      let region = S2.S2_shape_index_region.create index in
      (* Bounds. *)
      let bounds = member "bounds" case in
      let exp_cap = cap_of_json (member "cap" bounds) in
      let act_cap = S2.S2_shape_index_region.cap_bound region in
      check_cap_contains (name ^ " cap_bound") ~expected:exp_cap ~actual:act_cap;
      let exp_radius =
        float_u_of_json_exn (member "radius_radians" (member "cap" bounds))
      in
      let act_radius = S2.S1_angle.radians (S2.S2_cap.radius_angle act_cap) in
      check_radius_close
        (name ^ " cap radius")
        ~expected_radians:exp_radius
        ~actual_radians:act_radius;
      let exp_rect = rect_of_json (member "rect" bounds) in
      let act_rect = S2.S2_shape_index_region.rect_bound region in
      check_rect_close (name ^ " rect_bound") ~expected:exp_rect ~actual:act_rect;
      let exp_cu = cell_ids_of_json (member "cell_union" bounds) in
      let act_cu = S2.S2_shape_index_region.cell_union_bound region in
      check_cell_ids (name ^ " cell_union_bound") ~expected:exp_cu ~actual:act_cu;
      (* contains_cell. *)
      List.iter
        (to_list (member "contains_cell" case))
        ~f:(fun entry ->
          let cid = s2_cell_id_of_json (member "cell_id" entry) in
          let expected = bool_of_json_exn (member "expected" entry) in
          let cell = S2.S2_cell.of_cell_id cid in
          let actual = S2.S2_shape_index_region.contains_cell region cell in
          (Alcotest.check Alcotest.bool)
            (sprintf "%s contains_cell %s" name (S2.S2_cell_id.to_token cid))
            expected
            actual);
      (* may_intersect_cell. *)
      List.iter
        (to_list (member "may_intersect_cell" case))
        ~f:(fun entry ->
          let cid = s2_cell_id_of_json (member "cell_id" entry) in
          let expected = bool_of_json_exn (member "expected" entry) in
          let cell = S2.S2_cell.of_cell_id cid in
          let actual = S2.S2_shape_index_region.may_intersect_cell region cell in
          (Alcotest.check Alcotest.bool)
            (sprintf "%s may_intersect_cell %s" name (S2.S2_cell_id.to_token cid))
            expected
            actual);
      (* contains_point. *)
      List.iter
        (to_list (member "contains_point" case))
        ~f:(fun entry ->
          let p = s2_point_of_json (member "point" entry) in
          let expected = bool_of_json_exn (member "expected" entry) in
          let actual = S2.S2_shape_index_region.contains_point region p in
          (Alcotest.check Alcotest.bool) (name ^ " contains_point") expected actual))
;;

let () =
  Alcotest.run "S2_shape_index_region" [ "cases", [ test_case "all" `Quick test_cases ] ]
;;
