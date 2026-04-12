(* C++ test parity: s2geometry/src/s2/s2cell_test.cc
   -  TEST(S2Cell, TestFaces)                           - face constructors, accessors,
      bound_uv, center, center_raw, vertices, edges
   -  TEST(S2Cell, TestSubdivide)                       - child ids, face/level/orientation,
      bound_uv
   -  TEST(S2Cell, GetDistanceToPoint)                  - distance_to_point,
      boundary_distance_to_point, max_distance_to_point, contains_point
   -  TEST(S2Cell, GetUVCoordOfEdge)                    - full parity (edge_coords.uv_coords)
   -  TEST(S2Cell, GetSizeIJAgreesWithCellId)           - full parity (edge_coords.size_ij)
   -  TEST(S2Cell, GetIJCoordOfEdge)                    - full parity (edge_coords.ij_coords)
   -  TEST(S2Cell, ConsistentWithS2CellIdFromPointExample1) - full parity
      (contains_examples[0])
   -  TEST(S2CellId, AmbiguousContainsPoint)            - full parity
      (contains_examples[1])

   Extra coverage:
   - Areas                                              - exact_area, approx_area,
      average_area against C++ golden data

   -  TEST(S2Cell, ConsistentWithS2CellIdFromPoint)     - quickcheck: vertex ->
      of_point -> contains_point roundtrip

   Deliberately omitted:
   - GetRectBound / GetCapBound / CellUnionBound / EncodeDecode
   - distance to edge / distance to cell / max distance to edge / max distance to cell *)

open Core
open Test_helpers
open Alcotest

module Cell_id_int = struct
  type t = Int64.t [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let rec descend depth id =
      let cell = cid_of_int64 id in
      if depth = 0 || S2.S2_cell_id.is_leaf cell
      then return id
      else
        bind (int_uniform_inclusive 0 3) ~f:(fun k ->
          descend (depth - 1) (int64_of_cid (S2.S2_cell_id.child_exn cell k)))
    in
    bind (int_uniform_inclusive 0 5) ~f:(fun f ->
      bind (int_uniform_inclusive 0 24) ~f:(fun depth ->
        descend depth (int64_of_cid (S2.S2_cell_id.from_face_exn f))))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

let check_point_exact msg expected actual =
  check_float_u_exact
    (msg ^ ".x")
    ~expected:(S2.R3_vector.x expected)
    ~actual:(S2.R3_vector.x actual);
  check_float_u_exact
    (msg ^ ".y")
    ~expected:(S2.R3_vector.y expected)
    ~actual:(S2.R3_vector.y actual);
  check_float_u_exact
    (msg ^ ".z")
    ~expected:(S2.R3_vector.z expected)
    ~actual:(S2.R3_vector.z actual)
;;

let check_rect_exact msg (expected : S2.R2_rect.t) actual =
  let ex = S2.R2_rect.x expected in
  let ey = S2.R2_rect.y expected in
  let ax = S2.R2_rect.x actual in
  let ay = S2.R2_rect.y actual in
  check_float_u_exact
    (msg ^ " x.lo")
    ~expected:(S2.R1_interval.lo ex)
    ~actual:(S2.R1_interval.lo ax);
  check_float_u_exact
    (msg ^ " x.hi")
    ~expected:(S2.R1_interval.hi ex)
    ~actual:(S2.R1_interval.hi ax);
  check_float_u_exact
    (msg ^ " y.lo")
    ~expected:(S2.R1_interval.lo ey)
    ~actual:(S2.R1_interval.lo ay);
  check_float_u_exact
    (msg ^ " y.hi")
    ~expected:(S2.R1_interval.hi ey)
    ~actual:(S2.R1_interval.hi ay)
;;

let cell_of_token_json j =
  S2.S2_cell.of_cell_id (S2.S2_cell_id.from_token (string_of_json_exn j))
;;

let test_faces fixture () =
  let cases = to_list (member "faces" fixture) in
  List.iteri cases ~f:(fun i c ->
    let label = sprintf "face %d" i in
    let cell = cell_of_token_json (member "id" c) in
    (check int)
      (label ^ " face")
      (int_of_json_exn (member "face" c))
      (S2.S2_cell.face cell);
    (check int)
      (label ^ " level")
      (int_of_json_exn (member "level" c))
      (S2.S2_cell.level cell);
    (check int)
      (label ^ " orientation")
      (int_of_json_exn (member "orientation" c))
      (S2.S2_cell.orientation cell);
    check_rect_exact
      (label ^ " uv")
      (r2_rect_of_json (member "uv" c))
      (S2.S2_cell.bound_uv cell);
    check_r3_vector
      (label ^ " center")
      ~expected:(r3_vector_of_json (member "center" c))
      ~actual:(S2.S2_cell.center cell);
    check_r3_vector_exact
      (label ^ " center_raw")
      ~expected:(r3_vector_of_json (member "center_raw" c))
      ~actual:(S2.S2_cell.center_raw cell);
    List.iteri
      (to_list (member "vertices" c))
      ~f:(fun k v ->
        check_point_exact
          (sprintf "%s vertex %d" label k)
          (r3_vector_of_json v)
          (S2.S2_cell.vertex cell k));
    List.iteri
      (to_list (member "vertices_raw" c))
      ~f:(fun k v ->
        check_point_exact
          (sprintf "%s vertex_raw %d" label k)
          (r3_vector_of_json v)
          (S2.S2_cell.vertex_raw cell k));
    List.iteri
      (to_list (member "edges" c))
      ~f:(fun k e ->
        check_point_exact
          (sprintf "%s edge %d" label k)
          (r3_vector_of_json e)
          (S2.S2_cell.edge cell k));
    List.iteri
      (to_list (member "edges_raw" c))
      ~f:(fun k e ->
        check_point_exact
          (sprintf "%s edge_raw %d" label k)
          (r3_vector_of_json e)
          (S2.S2_cell.edge_raw cell k)))
;;

let test_subdivide fixture () =
  let cases = to_list (member "subdivide" fixture) in
  let parent_id =
    S2.S2_cell_id.child_exn
      (S2.S2_cell_id.child_exn (S2.S2_cell_id.from_face_pos_level 0 #0L 0) 0)
      3
  in
  let cell = S2.S2_cell.of_cell_id parent_id in
  for i = 0 to 3 do
    let child = S2.S2_cell.child cell ~pos:i in
    let c = List.nth_exn cases i in
    let label = sprintf "child %d" i in
    (check string)
      (label ^ " id")
      (string_of_json_exn (member "id" c))
      (S2.S2_cell_id.to_token (S2.S2_cell.id child));
    (check int)
      (label ^ " face")
      (int_of_json_exn (member "face" c))
      (S2.S2_cell.face child);
    (check int)
      (label ^ " level")
      (int_of_json_exn (member "level" c))
      (S2.S2_cell.level child);
    (check int)
      (label ^ " orientation")
      (int_of_json_exn (member "orientation" c))
      (S2.S2_cell.orientation child);
    check_rect_exact
      (label ^ " uv")
      (r2_rect_of_json (member "uv" c))
      (S2.S2_cell.bound_uv child)
  done
;;

let test_areas fixture () =
  let cases = to_list (member "areas" fixture) in
  List.iter cases ~f:(fun c ->
    let cell = cell_of_token_json (member "id" c) in
    check_float_u
      ~eps:2e-15
      "exact_area"
      ~expected:(float_u_of_json_exn (member "exact_area" c))
      ~actual:(S2.S2_cell.exact_area cell);
    check_float_u
      ~eps:2e-15
      "approx_area"
      ~expected:(float_u_of_json_exn (member "approx_area" c))
      ~actual:(S2.S2_cell.approx_area cell);
    check_float_u
      ~eps:2e-15
      "avg_area"
      ~expected:(float_u_of_json_exn (member "avg_area" c))
      ~actual:(S2.S2_cell.average_area (S2.S2_cell.level cell)))
;;

let test_distance_to_point fixture () =
  let cases = to_list (member "distance_point" fixture) in
  List.iteri cases ~f:(fun i c ->
    let label = sprintf "distance case %d" i in
    let cell = cell_of_token_json (member "cell_id" c) in
    let target = r3_vector_of_json (member "target" c) in
    (check bool)
      (label ^ " contains")
      (bool_of_json_exn (member "contains" c))
      (S2.S2_cell.contains_point cell target);
    check_float_u
      ~eps:1e-13
      (label ^ " distance")
      ~expected:(float_u_of_json_exn (member "distance" c))
      ~actual:(S2.S1_chord_angle.length2 (S2.S2_cell.distance_to_point cell target));
    check_float_u
      ~eps:1e-13
      (label ^ " boundary_distance")
      ~expected:(float_u_of_json_exn (member "boundary_distance" c))
      ~actual:
        (S2.S1_chord_angle.length2 (S2.S2_cell.boundary_distance_to_point cell target));
    check_float_u
      ~eps:1e-13
      (label ^ " max_distance")
      ~expected:(float_u_of_json_exn (member "max_distance" c))
      ~actual:(S2.S1_chord_angle.length2 (S2.S2_cell.max_distance_to_point cell target)))
;;

let test_edge_coords fixture () =
  let cases = to_list (member "edge_coords" fixture) in
  List.iteri cases ~f:(fun i c ->
    let label = sprintf "edge_coords %d" i in
    let cell = cell_of_token_json (member "id" c) in
    (check int)
      (label ^ " size_ij")
      (int_of_json_exn (member "size_ij" c))
      (S2.S2_cell_id.size_ij (S2.S2_cell.level cell));
    List.iteri
      (to_list (member "uv_coords" c))
      ~f:(fun k uv ->
        check_float_u_exact
          (sprintf "%s uv_coord_%d" label k)
          ~expected:(float_u_of_json_exn uv)
          ~actual:(S2.S2_cell.uv_coord_of_edge cell k));
    List.iteri
      (to_list (member "ij_coords" c))
      ~f:(fun k ij ->
        (check int)
          (sprintf "%s ij_coord_%d" label k)
          (int_of_json_exn ij)
          (S2.S2_cell.ij_coord_of_edge cell k)))
;;

let test_contains_examples fixture () =
  let cases = to_list (member "contains_examples" fixture) in
  List.iteri cases ~f:(fun i c ->
    let label = sprintf "contains example %d" i in
    let cell = cell_of_token_json (member "cell_id" c) in
    let target = r3_vector_of_json (member "target" c) in
    (check bool)
      label
      (bool_of_json_exn (member "contains" c))
      (S2.S2_cell.contains_point cell target))
;;

let quickcheck_subdivide_matches_cell_id_hierarchy () =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cell_id = cid_of_int64 id in
    let cell = S2.S2_cell.of_cell_id cell_id in
    if S2.S2_cell.is_leaf cell
    then ()
    else
      for k = 0 to 3 do
        let child = S2.S2_cell.child cell ~pos:k in
        assert (
          S2.S2_cell_id.equal (S2.S2_cell.id child) (S2.S2_cell_id.child_exn cell_id k));
        assert (S2.S2_cell.contains_cell cell child);
        assert (S2.S2_cell.intersects_cell cell child)
      done)
;;

(* TEST(S2Cell, ConsistentWithS2CellIdFromPoint) - randomized version.
   For random cells, verify that S2Cell(S2CellId(vertex)).Contains(vertex)
   holds for all vertices. This tests containment of points that lie exactly
   on cell boundaries. *)
let quickcheck_consistent_with_cell_id_from_point () =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cell = S2.S2_cell.of_cell_id (cid_of_int64 id) in
    for k = 0 to 3 do
      let v = S2.S2_cell.vertex cell k in
      let cell_from_v = S2.S2_cell.of_point v in
      assert (S2.S2_cell.contains_point cell_from_v v)
    done)
;;

let quickcheck_center_and_vertices_contained () =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cell = S2.S2_cell.of_cell_id (cid_of_int64 id) in
    assert (S2.S2_cell.contains_point cell (S2.S2_cell.center_raw cell));
    for k = 0 to 3 do
      assert (S2.S2_cell.contains_point cell (S2.S2_cell.vertex_raw cell k))
    done)
;;

let () =
  let fixture = load_fixture "s2cell.json" in
  Alcotest.run
    "S2_cell"
    [ "faces", [ test_case "TestFaces" `Quick (test_faces fixture) ]
    ; "subdivide", [ test_case "TestSubdivide" `Quick (test_subdivide fixture) ]
    ; "areas", [ test_case "Areas" `Quick (test_areas fixture) ]
    ; ( "distance_to_point"
      , [ test_case "GetDistanceToPoint" `Quick (test_distance_to_point fixture) ] )
    ; ( "edge_coords"
      , [ test_case "GetUVCoordOfEdge" `Quick (test_edge_coords fixture)
        ; test_case "GetSizeIJAgreesWithCellId" `Quick (test_edge_coords fixture)
        ; test_case "GetIJCoordOfEdge" `Quick (test_edge_coords fixture)
        ] )
    ; ( "contains_examples"
      , [ test_case
            "ConsistentWithS2CellIdFromPointExample1"
            `Quick
            (test_contains_examples fixture)
        ; test_case "AmbiguousContainsPoint" `Quick (test_contains_examples fixture)
        ] )
    ; ( "quickcheck"
      , [ test_case
            "subdivide_matches_cell_id_hierarchy"
            `Quick
            quickcheck_subdivide_matches_cell_id_hierarchy
        ; test_case
            "consistent_with_cell_id_from_point"
            `Quick
            quickcheck_consistent_with_cell_id_from_point
        ; test_case
            "center_and_vertices_contained"
            `Quick
            quickcheck_center_and_vertices_contained
        ] )
    ]
;;
