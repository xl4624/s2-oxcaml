(* Golden data produced by test/gen/s2polygon.cc.

   Upstream C++ tests mirrored (s2polygon_test.cc):
   - TEST_F(S2Polygon, BasicFromLoopsProperties) -> polygons fixture
   - TEST_F(S2Polygon, Contains and Intersects) -> polygon_relations fixture
   - TEST(S2Polygon, ContainsPoint) -> point_containment fixture
   - TEST(S2Polygon, ContainsCell / MayIntersect) -> cell_containment fixture
   - TEST(S2Polygon, Invert) -> invert fixture
   - TEST(S2Polygon, Shape) -> shape fixture

   Deliberately omitted:
   - EncodeDecode / CompressedEncoding: encoding is not ported.
   - InitToIntersection/Union/Difference and snap/simplify variants: these
     require S2_builder, which is a later tier.
   - Polyline interop (IntersectWithPolyline, SubtractFromPolyline, ...):
     not in scope until S2_builder is ported.
   - Distance / projection queries.

   TODO: the polygon_relations fixture currently restricts itself to
   single-loop polygons. Once S2_builder and S2BooleanOperation land, the
   generator can restore the multi-loop cases so we match C++ exactly for
   shared-boundary inputs (see the note on S2_polygon.contains). *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2polygon.json")
let get key = member key (Lazy.force fixture)
let point_of_json j = r3_vector_of_json j

let vertices_of_json j =
  let xs = to_list j in
  let n = List.length xs in
  if n = 0
  then [||]
  else (
    let first = point_of_json (List.hd_exn xs) in
    let arr = Array.create ~len:n first in
    List.iteri xs ~f:(fun i p -> arr.(i) <- point_of_json p);
    arr)
;;

let loop_of_json j = S2.S2_loop.of_vertices ~validate:false (vertices_of_json j)

let loops_of_json j =
  let xs = to_list j in
  let n = List.length xs in
  if n = 0
  then [||]
  else (
    let first = loop_of_json (List.hd_exn xs) in
    let arr = Array.create ~len:n first in
    List.iteri xs ~f:(fun i l -> arr.(i) <- loop_of_json l);
    arr)
;;

let catalog =
  lazy
    (let entries = to_list (get "catalog") in
     List.map entries ~f:(fun e ->
       let name = string_of_json_exn (member "name" e) in
       let loops = loops_of_json (member "loops" e) in
       let p = S2.S2_polygon.of_loops loops in
       name, p))
;;

let polygon_by_name name =
  let found = List.find (Lazy.force catalog) ~f:(fun (n, _) -> String.equal n name) in
  match found with
  | Some (_, p) -> p
  | None ->
    (match failwith (sprintf "polygon %s not in catalog" name) with
     | (_ : Nothing.t) -> .)
;;

let check_rect_bound name ~expected ~actual =
  let el = S2.S2_latlng_rect.lat expected in
  let al = S2.S2_latlng_rect.lat actual in
  let eng = S2.S2_latlng_rect.lng expected in
  let ang = S2.S2_latlng_rect.lng actual in
  check_float_u
    ~eps:1e-12
    (name ^ " lat.lo")
    ~expected:(S2.R1_interval.lo el)
    ~actual:(S2.R1_interval.lo al);
  check_float_u
    ~eps:1e-12
    (name ^ " lat.hi")
    ~expected:(S2.R1_interval.hi el)
    ~actual:(S2.R1_interval.hi al);
  check_float_u
    ~eps:1e-12
    (name ^ " lng.lo")
    ~expected:(S2.S1_interval.lo eng)
    ~actual:(S2.S1_interval.lo ang);
  check_float_u
    ~eps:1e-12
    (name ^ " lng.hi")
    ~expected:(S2.S1_interval.hi eng)
    ~actual:(S2.S1_interval.hi ang)
;;

let test_polygons () =
  List.iter
    (to_list (get "polygons"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let p = polygon_by_name name in
      (check int)
        (name ^ " num_loops")
        (int_of_json_exn (member "num_loops" case))
        (S2.S2_polygon.num_loops p);
      (check int)
        (name ^ " num_vertices")
        (int_of_json_exn (member "num_vertices" case))
        (S2.S2_polygon.num_vertices p);
      (check bool)
        (name ^ " is_empty")
        (bool_of_json_exn (member "is_empty" case))
        (S2.S2_polygon.is_empty p);
      (check bool)
        (name ^ " is_full")
        (bool_of_json_exn (member "is_full" case))
        (S2.S2_polygon.is_full p);
      (check bool)
        (name ^ " has_holes")
        (bool_of_json_exn (member "has_holes" case))
        (S2.S2_polygon.has_holes p);
      (check int)
        (name ^ " num_edges")
        (int_of_json_exn (member "num_edges" case))
        (S2.S2_polygon.num_edges p);
      (check int)
        (name ^ " num_chains")
        (int_of_json_exn (member "num_chains" case))
        (S2.S2_polygon.num_chains p);
      (check int) (name ^ " dimension") 2 (S2.S2_polygon.dimension p);
      check_float_u
        ~eps:1e-12
        (name ^ " area")
        ~expected:(float_u_of_json_exn (member "area" case))
        ~actual:(S2.S2_polygon.area p);
      check_r3_vector
        (name ^ " centroid")
        ~expected:(r3_vector_of_json (member "centroid" case))
        ~actual:(S2.S2_polygon.centroid p);
      check_rect_bound
        (name ^ " rect_bound")
        ~expected:(latlng_rect_of_json (member "rect_bound" case))
        ~actual:(S2.S2_polygon.rect_bound p);
      let loops_json = to_list (member "loops" case) in
      (check int)
        (name ^ " loops len")
        (List.length loops_json)
        (S2.S2_polygon.num_loops p);
      List.iteri loops_json ~f:(fun i lj ->
        let l = S2.S2_polygon.loop p i in
        (check int)
          (sprintf "%s loop[%d] depth" name i)
          (int_of_json_exn (member "depth" lj))
          (S2.S2_loop.depth l);
        (check bool)
          (sprintf "%s loop[%d] is_hole" name i)
          (bool_of_json_exn (member "is_hole" lj))
          (S2.S2_loop.is_hole l)))
;;

let test_point_containment () =
  List.iter
    (to_list (get "point_containment"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let p = polygon_by_name name in
      List.iter
        (to_list (member "cases" case))
        ~f:(fun c ->
          let pt = point_of_json (member "point" c) in
          let expected = bool_of_json_exn (member "contains" c) in
          (check bool)
            (sprintf "%s contains_point" name)
            expected
            (S2.S2_polygon.contains_point p pt)))
;;

let test_cell_containment () =
  List.iter
    (to_list (get "cell_containment"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let p = polygon_by_name name in
      List.iter
        (to_list (member "cases" case))
        ~f:(fun c ->
          let cell =
            let token = string_of_json_exn (member "cell_id_token" c) in
            S2.S2_cell.of_cell_id (S2.S2_cell_id.from_token token)
          in
          (check bool)
            (sprintf
               "%s contains_cell %s"
               name
               (string_of_json_exn (member "cell_id_token" c)))
            (bool_of_json_exn (member "contains_cell" c))
            (S2.S2_polygon.contains_cell p cell);
          (check bool)
            (sprintf
               "%s may_intersect %s"
               name
               (string_of_json_exn (member "cell_id_token" c)))
            (bool_of_json_exn (member "may_intersect" c))
            (S2.S2_polygon.may_intersect_cell p cell)))
;;

let test_polygon_relations () =
  List.iter
    (to_list (get "polygon_relations"))
    ~f:(fun case ->
      let a_name = string_of_json_exn (member "a" case) in
      let b_name = string_of_json_exn (member "b" case) in
      let a = polygon_by_name a_name in
      let b = polygon_by_name b_name in
      (check bool)
        (sprintf "%s contains %s" a_name b_name)
        (bool_of_json_exn (member "contains" case))
        (S2.S2_polygon.contains a b);
      (check bool)
        (sprintf "%s intersects %s" a_name b_name)
        (bool_of_json_exn (member "intersects" case))
        (S2.S2_polygon.intersects a b))
;;

let test_invert () =
  List.iter
    (to_list (get "invert"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let p = polygon_by_name name in
      (check int)
        (name ^ " num_loops_before")
        (int_of_json_exn (member "num_loops_before" case))
        (S2.S2_polygon.num_loops p);
      check_float_u
        ~eps:1e-12
        (name ^ " area_before")
        ~expected:(float_u_of_json_exn (member "area_before" case))
        ~actual:(S2.S2_polygon.area p);
      let inv = S2.S2_polygon.invert p in
      (check int)
        (name ^ " num_loops_after")
        (int_of_json_exn (member "num_loops_after" case))
        (S2.S2_polygon.num_loops inv);
      check_float_u
        ~eps:1e-12
        (name ^ " area_after")
        ~expected:(float_u_of_json_exn (member "area_after" case))
        ~actual:(S2.S2_polygon.area inv))
;;

let test_shape () =
  List.iter
    (to_list (get "shape"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let p = polygon_by_name name in
      let edges_json = to_list (member "edges" case) in
      let actual_edges = S2.S2_polygon.num_edges p in
      (check int) (name ^ " num_edges") (List.length edges_json) actual_edges;
      List.iteri edges_json ~f:(fun i ej ->
        let expected_v0 = point_of_json (member "v0" ej) in
        let expected_v1 = point_of_json (member "v1" ej) in
        let e = S2.S2_polygon.edge p i in
        check_r3_vector_exact
          (sprintf "%s edge[%d].v0" name i)
          ~expected:expected_v0
          ~actual:e.#v0;
        check_r3_vector_exact
          (sprintf "%s edge[%d].v1" name i)
          ~expected:expected_v1
          ~actual:e.#v1);
      let chains_json = to_list (member "chains" case) in
      (check int)
        (name ^ " num_chains")
        (List.length chains_json)
        (S2.S2_polygon.num_chains p);
      List.iteri chains_json ~f:(fun i cj ->
        let c = S2.S2_polygon.chain p i in
        (check int)
          (sprintf "%s chain[%d] start" name i)
          (int_of_json_exn (member "start" cj))
          c.#start;
        (check int)
          (sprintf "%s chain[%d] length" name i)
          (int_of_json_exn (member "length" cj))
          c.#length);
      let rp = S2.S2_polygon.reference_point p in
      let rp_json = member "reference_point" case in
      check_r3_vector_exact
        (name ^ " reference_point point")
        ~expected:(point_of_json (member "point" rp_json))
        ~actual:rp.#point;
      (check bool)
        (name ^ " reference_point contained")
        (bool_of_json_exn (member "contained" rp_json))
        rp.#contained)
;;

let () =
  run
    "s2_polygon"
    [ "polygons", [ test_case "basic properties" `Quick test_polygons ]
    ; "point", [ test_case "contains_point" `Quick test_point_containment ]
    ; "cell", [ test_case "cell relations" `Quick test_cell_containment ]
    ; "relations", [ test_case "polygon relations" `Quick test_polygon_relations ]
    ; "invert", [ test_case "invert" `Quick test_invert ]
    ; "shape", [ test_case "shape interface" `Quick test_shape ]
    ]
;;
