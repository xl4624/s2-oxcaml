(* Golden data produced by test/gen/s2loop.cc.

   Upstream C++ tests covered (s2loop_test.cc):
   - TEST_F(S2LoopTestBase, GetRectBound) -> loops fixture (rect_bound)
   - TEST_F(S2LoopTestBase, GetCapBound) -> loops fixture (cap_bound)
   - TEST_F(S2LoopTestBase, AreaConsistentWithCurvature) -> loops fixture (area, curvature)
   - TEST_F(S2LoopTestBase, GetCurvature) -> loops fixture
   - TEST_F(S2LoopTestBase, GetCentroid) -> loops fixture
   - TEST_F(S2LoopTestBase, IsValid_NoIndex) -> loops fixture (is_valid)
   - TEST_F(S2LoopTestBase, IsNormalized) -> loops fixture (is_normalized)
   - TEST(S2Loop, EmptyAndFullLoops) -> loops fixture (is_empty / is_full)
   - TEST(S2Loop, LoopFromCell) -> from_cell fixture
   - TEST(S2Loop, Vertex) -> vertex_wrap fixture
   - TEST(S2Loop, Invert) -> invert fixture
   - TEST_F(S2LoopTestBase, Normalize) -> normalize fixture
   - TEST_F(S2LoopTestBase, Contains and ContainsPoint) -> contains_point fixture
   - TEST_F(S2LoopTestBase, ContainsCell, MayIntersect) -> cell_relations fixture
   - TEST(S2Loop, EqualsAndBoundaryEquals) -> equality fixture
   - TEST(S2Loop, BoundaryApproxEquals) -> approx_equality_perturbed fixture
   - TEST(S2LoopShape, basic and reference_point) -> shape fixture
   - TEST(S2Loop, FindValidationError) -> validation fixture

   Deliberately omitted:
   - Tests requiring an S2ShapeIndex (Contains(loop), Intersects(loop),
     CompareBoundary, BoundaryNear, Project, ProjectToBoundary, GetDistance,
     GetDistanceToBoundary): the OCaml port defers index-driven operations until
     S2_shape_index is available.
   - EncodeDecode / EncodeCompressed: encoding is not ported.
   - MakeRegularLoop / RegularLoopForFrame: helper not exposed in the OCaml
     port surface. *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2loop.json")
let get key = member key (Lazy.force fixture)
let s2_point_of_json j = r3_vector_of_json j

let check_edge msg ~(expected : S2.S2_shape.Edge.t) ~(actual : S2.S2_shape.Edge.t) =
  check_r3_vector_exact (msg ^ " v0") ~expected:expected.#v0 ~actual:actual.#v0;
  check_r3_vector_exact (msg ^ " v1") ~expected:expected.#v1 ~actual:actual.#v1
;;

let edge_of_json j =
  let v0 = s2_point_of_json (member "v0" j) in
  let v1 = s2_point_of_json (member "v1" j) in
  S2.S2_shape.Edge.create ~v0 ~v1
;;

let vertices_of_json j =
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

let int_pair_of_json j =
  match to_list j with
  | [ a; b ] -> int_of_json_exn a, int_of_json_exn b
  | _ ->
    (match failwith "expected [a, b]" with
     | (_ : Nothing.t) -> .)
;;

let make_loop vertices = S2.S2_loop.of_vertices ~validate:false vertices

(* Per-loop scalar properties. Geometry comparisons use the standard
   1e-15 epsilon for centroid/area/curvature, while the rect-bound
   tolerance is the same one S2LatLngRectBounder advertises. *)
let test_loops () =
  let rect_eps =
    S2.S1_angle.radians
      (S2.S2_latlng.lat (S2.S2_latlng_rect_bounder.max_error_for_tests ()))
  in
  let rect_eps = Float_u.to_float rect_eps in
  List.iter
    (to_list (get "loops"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let vertices = vertices_of_json (member "vertices" case) in
      let loop = make_loop vertices in
      (check int)
        (name ^ " num_vertices")
        (int_of_json_exn (member "num_vertices" case))
        (S2.S2_loop.num_vertices loop);
      (check int)
        (name ^ " num_edges")
        (int_of_json_exn (member "num_edges" case))
        (S2.S2_loop.num_edges loop);
      (check int)
        (name ^ " num_chains")
        (int_of_json_exn (member "num_chains" case))
        (S2.S2_loop.num_chains loop);
      (check int)
        (name ^ " dimension")
        (int_of_json_exn (member "dimension" case))
        (S2.S2_loop.dimension loop);
      check_bool
        (name ^ " is_empty")
        ~expected:(bool_of_json_exn (member "is_empty" case))
        ~actual:(S2.S2_loop.is_empty loop);
      check_bool
        (name ^ " is_full")
        ~expected:(bool_of_json_exn (member "is_full" case))
        ~actual:(S2.S2_loop.is_full loop);
      check_bool
        (name ^ " is_empty_or_full")
        ~expected:(bool_of_json_exn (member "is_empty_or_full" case))
        ~actual:(S2.S2_loop.is_empty_or_full loop);
      check_bool
        (name ^ " is_valid")
        ~expected:(bool_of_json_exn (member "is_valid" case))
        ~actual:(S2.S2_loop.is_valid loop);
      check_bool
        (name ^ " is_normalized")
        ~expected:(bool_of_json_exn (member "is_normalized" case))
        ~actual:(S2.S2_loop.is_normalized loop);
      check_bool
        (name ^ " contains_origin")
        ~expected:(bool_of_json_exn (member "contains_origin" case))
        ~actual:(S2.S2_loop.contains_origin loop);
      check_float_u
        (name ^ " area")
        ~expected:(float_u_of_json_exn (member "area" case))
        ~actual:(S2.S2_loop.area loop);
      check_float_u
        ~eps:1e-13
        (name ^ " curvature")
        ~expected:(float_u_of_json_exn (member "curvature" case))
        ~actual:(S2.S2_loop.curvature loop);
      check_r3_vector
        (name ^ " centroid")
        ~expected:(s2_point_of_json (member "centroid" case))
        ~actual:(S2.S2_loop.centroid loop);
      let rect_json = member "rect_bound" case in
      let expected_rect = latlng_rect_of_json rect_json in
      let actual_rect = S2.S2_loop.rect_bound loop in
      check_bool
        (name ^ " rect_bound is_empty")
        ~expected:(bool_of_json_exn (member "is_empty" rect_json))
        ~actual:(S2.S2_latlng_rect.is_empty actual_rect);
      check_bool
        (name ^ " rect_bound is_full")
        ~expected:(bool_of_json_exn (member "is_full" rect_json))
        ~actual:(S2.S2_latlng_rect.is_full actual_rect);
      if (not (S2.S2_latlng_rect.is_empty expected_rect))
         && not (S2.S2_latlng_rect.is_empty actual_rect)
      then (
        check_float_u
          ~eps:rect_eps
          (name ^ " rect_bound lat lo")
          ~expected:(S2.R1_interval.lo (S2.S2_latlng_rect.lat expected_rect))
          ~actual:(S2.R1_interval.lo (S2.S2_latlng_rect.lat actual_rect));
        check_float_u
          ~eps:rect_eps
          (name ^ " rect_bound lat hi")
          ~expected:(S2.R1_interval.hi (S2.S2_latlng_rect.lat expected_rect))
          ~actual:(S2.R1_interval.hi (S2.S2_latlng_rect.lat actual_rect)));
      let cap_json = member "cap_bound" case in
      let expected_cap_center = s2_point_of_json (member "center" cap_json) in
      let expected_cap_l2 = float_u_of_json_exn (member "length2" cap_json) in
      let cap = S2.S2_loop.cap_bound loop in
      check_r3_vector
        (name ^ " cap_bound center")
        ~expected:expected_cap_center
        ~actual:(S2.S2_cap.center cap);
      check_float_u
        ~eps:1e-13
        (name ^ " cap_bound length2")
        ~expected:expected_cap_l2
        ~actual:(S2.S1_chord_angle.length2 (S2.S2_cap.radius_chord cap));
      let rp_json = member "reference_point" case in
      let rp = S2.S2_loop.reference_point loop in
      check_bool
        (name ^ " reference_point contained")
        ~expected:(bool_of_json_exn (member "contained" rp_json))
        ~actual:rp.#contained)
;;

let test_vertex_wrap () =
  List.iter
    (to_list (get "vertex_wrap"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let vertices = vertices_of_json (member "vertices" case) in
      let loop = make_loop vertices in
      let wrapped = to_list (member "wrapped" case) in
      List.iteri wrapped ~f:(fun i p_json ->
        let expected = s2_point_of_json p_json in
        check_r3_vector_exact
          (sprintf "%s vertex(%d)" name i)
          ~expected
          ~actual:(S2.S2_loop.vertex loop i)))
;;

let test_from_cell () =
  List.iter
    (to_list (get "from_cell"))
    ~f:(fun case ->
      let token = string_of_json_exn (member "cell_id_token" case) in
      let id = S2.S2_cell_id.from_token token in
      let cell = S2.S2_cell.of_cell_id id in
      let loop = S2.S2_loop.of_cell cell in
      let expected_vertices = vertices_of_json (member "vertices" case) in
      let n = Array.length expected_vertices in
      (check int) (token ^ " num_vertices") n (S2.S2_loop.num_vertices loop);
      for i = 0 to n - 1 do
        check_r3_vector_exact
          (sprintf "%s vertex[%d]" token i)
          ~expected:expected_vertices.(i)
          ~actual:(S2.S2_loop.vertex loop i)
      done;
      check_bool
        (token ^ " is_valid")
        ~expected:(bool_of_json_exn (member "is_valid" case))
        ~actual:(S2.S2_loop.is_valid loop);
      check_float_u
        (token ^ " area")
        ~expected:(float_u_of_json_exn (member "area" case))
        ~actual:(S2.S2_loop.area loop))
;;

let test_invert () =
  List.iter
    (to_list (get "invert"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let before = vertices_of_json (member "before" case) in
      let after = vertices_of_json (member "after" case) in
      let loop = make_loop before in
      let inverted = S2.S2_loop.invert loop in
      let n = Array.length after in
      (check int) (name ^ " num_vertices") n (S2.S2_loop.num_vertices inverted);
      for i = 0 to n - 1 do
        check_r3_vector_exact
          (sprintf "%s vertex[%d]" name i)
          ~expected:after.(i)
          ~actual:(S2.S2_loop.vertex inverted i)
      done;
      check_bool
        (name ^ " is_full_after")
        ~expected:(bool_of_json_exn (member "is_full_after" case))
        ~actual:(S2.S2_loop.is_full inverted);
      check_bool
        (name ^ " is_empty_after")
        ~expected:(bool_of_json_exn (member "is_empty_after" case))
        ~actual:(S2.S2_loop.is_empty inverted))
;;

let test_normalize () =
  List.iter
    (to_list (get "normalize"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let before = vertices_of_json (member "before" case) in
      let after = vertices_of_json (member "after" case) in
      let loop = make_loop before in
      let normalized = S2.S2_loop.normalize loop in
      let n = Array.length after in
      (check int) (name ^ " num_vertices") n (S2.S2_loop.num_vertices normalized);
      for i = 0 to n - 1 do
        check_r3_vector_exact
          (sprintf "%s vertex[%d]" name i)
          ~expected:after.(i)
          ~actual:(S2.S2_loop.vertex normalized i)
      done;
      check_float_u
        (name ^ " area_after")
        ~expected:(float_u_of_json_exn (member "area_after" case))
        ~actual:(S2.S2_loop.area normalized))
;;

let test_contains_point () =
  List.iter
    (to_list (get "contains_point"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let vertices = vertices_of_json (member "vertices" case) in
      let loop = make_loop vertices in
      List.iter
        (to_list (member "cases" case))
        ~f:(fun pcase ->
          let p = s2_point_of_json (member "point" pcase) in
          let expected = bool_of_json_exn (member "contains" pcase) in
          check_bool
            (sprintf
               "%s contains (%g,%g,%g)"
               name
               (Float_u.to_float (S2.R3_vector.x p))
               (Float_u.to_float (S2.R3_vector.y p))
               (Float_u.to_float (S2.R3_vector.z p)))
            ~expected
            ~actual:(S2.S2_loop.contains_point loop p)))
;;

let test_cell_relations () =
  List.iter
    (to_list (get "cell_relations"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let vertices = vertices_of_json (member "vertices" case) in
      let loop = make_loop vertices in
      List.iter
        (to_list (member "cases" case))
        ~f:(fun ccase ->
          let token = string_of_json_exn (member "cell_id_token" ccase) in
          let cell = S2.S2_cell.of_cell_id (S2.S2_cell_id.from_token token) in
          check_bool
            (sprintf "%s contains_cell %s" name token)
            ~expected:(bool_of_json_exn (member "contains_cell" ccase))
            ~actual:(S2.S2_loop.contains_cell loop cell);
          check_bool
            (sprintf "%s may_intersect %s" name token)
            ~expected:(bool_of_json_exn (member "may_intersect" ccase))
            ~actual:(S2.S2_loop.may_intersect_cell loop cell)))
;;

let test_equality () =
  List.iter
    (to_list (get "equality"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let a = vertices_of_json (member "a" case) in
      let b = vertices_of_json (member "b" case) in
      let la = make_loop a in
      let lb = make_loop b in
      check_bool
        (name ^ " equal")
        ~expected:(bool_of_json_exn (member "equal" case))
        ~actual:(S2.S2_loop.equal la lb);
      check_bool
        (name ^ " boundary_equals")
        ~expected:(bool_of_json_exn (member "boundary_equals" case))
        ~actual:(S2.S2_loop.boundary_equals la lb);
      let max_error = Packed_float_option.Unboxed.some #1e-15 in
      check_bool
        (name ^ " boundary_approx_equals_default")
        ~expected:(bool_of_json_exn (member "boundary_approx_equals_default" case))
        ~actual:(S2.S2_loop.boundary_approx_equals ~max_error la lb))
;;

let test_approx_equality_perturbed () =
  let case = get "approx_equality_perturbed" in
  let a = vertices_of_json (member "a" case) in
  let b = vertices_of_json (member "b" case) in
  let la = make_loop a in
  let lb = make_loop b in
  let loose = Packed_float_option.Unboxed.some #1e-5 in
  let tight = Packed_float_option.Unboxed.some #1e-15 in
  check_bool
    "approx_equal_loose"
    ~expected:(bool_of_json_exn (member "approx_equal_loose" case))
    ~actual:(S2.S2_loop.boundary_approx_equals ~max_error:loose la lb);
  check_bool
    "approx_equal_tight"
    ~expected:(bool_of_json_exn (member "approx_equal_tight" case))
    ~actual:(S2.S2_loop.boundary_approx_equals ~max_error:tight la lb);
  check_bool
    "approx_equal_default"
    ~expected:(bool_of_json_exn (member "approx_equal_tight" case))
    ~actual:
      (S2.S2_loop.boundary_approx_equals
         ~max_error:(Packed_float_option.Unboxed.none)
         la
         lb)
;;

(* [depth] / [set_depth] / [is_hole] / [sign] / [oriented_vertex] are
   OCaml-only mutations not exercised by the C++ fixture. A hole-depth loop
   must iterate its vertices in reverse so that the interior stays on the
   left side of each edge. *)
let test_depth_and_oriented_vertex () =
  let vertices =
    vertices_of_json
      (member
         "vertices"
         (List.find_exn
            (to_list (get "loops"))
            ~f:(fun case ->
              String.equal (string_of_json_exn (member "name" case)) "square")))
  in
  let loop = make_loop vertices in
  let n = S2.S2_loop.num_vertices loop in
  (check int) "default depth" 0 (S2.S2_loop.depth loop);
  check_bool "default is_hole" ~expected:false ~actual:(S2.S2_loop.is_hole loop);
  (check int) "default sign" 1 (S2.S2_loop.sign loop);
  for i = 0 to n - 1 do
    check_r3_vector_exact
      (sprintf "depth=0 oriented_vertex[%d]" i)
      ~expected:(S2.S2_loop.vertex loop i)
      ~actual:(S2.S2_loop.oriented_vertex loop i)
  done;
  S2.S2_loop.set_depth loop 1;
  (check int) "odd depth" 1 (S2.S2_loop.depth loop);
  check_bool "odd is_hole" ~expected:true ~actual:(S2.S2_loop.is_hole loop);
  (check int) "odd sign" (-1) (S2.S2_loop.sign loop);
  for i = 0 to n - 1 do
    check_r3_vector_exact
      (sprintf "depth=1 oriented_vertex[%d]" i)
      ~expected:(S2.S2_loop.vertex loop (n - 1 - i))
      ~actual:(S2.S2_loop.oriented_vertex loop i)
  done;
  S2.S2_loop.set_depth loop 2;
  check_bool "even is_hole" ~expected:false ~actual:(S2.S2_loop.is_hole loop);
  (check int) "even sign" 1 (S2.S2_loop.sign loop)
;;

let test_shape () =
  List.iter
    (to_list (get "shape"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let vertices = vertices_of_json (member "vertices" case) in
      let loop = make_loop vertices in
      let expected_num_edges = int_of_json_exn (member "num_edges" case) in
      let expected_num_chains = int_of_json_exn (member "num_chains" case) in
      (check int) (name ^ " num_edges") expected_num_edges (S2.S2_loop.num_edges loop);
      (check int) (name ^ " num_chains") expected_num_chains (S2.S2_loop.num_chains loop);
      let generic = S2.S2_loop.to_shape loop in
      (check int) (name ^ " shape num_edges") expected_num_edges generic.#num_edges;
      (check int) (name ^ " shape num_chains") expected_num_chains generic.#num_chains;
      (check int) (name ^ " shape dimension") 2 generic.#dimension;
      let edges_json = to_list (member "edges" case) in
      List.iteri edges_json ~f:(fun i e_json ->
        let expected = edge_of_json e_json in
        check_edge
          (sprintf "%s edge[%d]" name i)
          ~expected
          ~actual:(S2.S2_loop.edge loop i);
        check_edge
          (sprintf "%s shape edge[%d]" name i)
          ~expected
          ~actual:(generic.#edge i));
      let chains_json = to_list (member "chains" case) in
      List.iteri chains_json ~f:(fun i c_json ->
        let s, l = int_pair_of_json c_json in
        let expected = S2.S2_shape.Chain.create ~start:s ~length:l in
        let actual = S2.S2_loop.chain loop i in
        check_bool
          (sprintf "%s chain[%d] equal" name i)
          ~expected:true
          ~actual:(S2.S2_shape.Chain.equal expected actual));
      let cp_json = to_list (member "chain_positions" case) in
      List.iteri cp_json ~f:(fun e cp ->
        let cid, off = int_pair_of_json cp in
        let expected = S2.S2_shape.Chain_position.create ~chain_id:cid ~offset:off in
        let actual = S2.S2_loop.chain_position loop e in
        check_bool
          (sprintf "%s chain_position[%d] equal" name e)
          ~expected:true
          ~actual:(S2.S2_shape.Chain_position.equal expected actual)))
;;

let test_validation () =
  List.iter
    (to_list (get "validation"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let vertices = vertices_of_json (member "vertices" case) in
      let loop = make_loop vertices in
      check_bool
        (name ^ " is_valid")
        ~expected:(bool_of_json_exn (member "is_valid" case))
        ~actual:(S2.S2_loop.is_valid loop))
;;

let () =
  Alcotest.run
    "S2_loop"
    [ "loops", [ test_case "all" `Quick test_loops ]
    ; "vertex_wrap", [ test_case "all" `Quick test_vertex_wrap ]
    ; "from_cell", [ test_case "all" `Quick test_from_cell ]
    ; "invert", [ test_case "all" `Quick test_invert ]
    ; "normalize", [ test_case "all" `Quick test_normalize ]
    ; "contains_point", [ test_case "all" `Quick test_contains_point ]
    ; "cell_relations", [ test_case "all" `Quick test_cell_relations ]
    ; "equality", [ test_case "all" `Quick test_equality ]
    ; "approx_equality", [ test_case "perturbed" `Quick test_approx_equality_perturbed ]
    ; "depth", [ test_case "oriented_vertex" `Quick test_depth_and_oriented_vertex ]
    ; "shape", [ test_case "all" `Quick test_shape ]
    ; "validation", [ test_case "all" `Quick test_validation ]
    ]
;;
