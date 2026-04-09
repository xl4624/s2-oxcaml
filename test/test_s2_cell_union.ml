(* C++ test parity: s2geometry/src/s2/s2cell_union_test.cc
   - TEST(S2CellUnion, WholeSphere)                       - whole_sphere
   - TEST(S2CellUnion, IsNormalized)                      - is_normalized
   - TEST(S2CellUnion, Normalize)                         - normalize (via create)
   - TEST(S2CellUnion, ContainsCellID/IntersectsCellID)   - contains_intersects
   - TEST(S2CellUnion, Contains/Intersects S2CellUnion)   - contains_intersects_union
   - TEST(S2CellUnion, Union/Intersection/Difference)     - boolean_ops
   - TEST(S2CellUnion, IntersectionWithCellId)            - intersection_with_cell_id
   - TEST(S2CellUnion, FromMinMax)                        - from_min_max
   - TEST(S2CellUnion, FromBeginEnd)                      - from_begin_end
   - TEST(S2CellUnion, Denormalize)                       - denormalize
   - TEST(S2CellUnion, LeafCellsCovered)                  - leaf_cells_covered
   - TEST(S2CellUnion, Area)                              - area (average_based, approx, exact)
   - TEST(S2CellUnion, EmptyAndNonEmptyBooleanOps)        - empty_ops

   Extra coverage:
   - equal

   Deliberately omitted:
   - Expand (requires AllNeighbors not yet ported)
   - EncodeDecode (serialization)
   - CapBound / RectBound (requires S2LatLngRect not yet ported)
   - Iterator
   - ContainsExpectedCells / AddCells random tests (complex PRNG-seeded tests)
   - ContainsInputCells random tests
   - ContainsIntersectsBruteForce random tests *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2cellunion.json")

let cell_id_of_json j =
  let s = string_of_json_exn j in
  S2.S2_cell_id.of_int64 (Int64.of_string ("0u" ^ s))
;;

let raw_id_of_json j =
  let s = string_of_json_exn j in
  Int64.of_string ("0u" ^ s)
;;

let cell_ids_of_json j = Array.of_list (List.map (to_list j) ~f:raw_id_of_json)
let make_union j = S2.S2_cell_union.create (cell_ids_of_json j)
let make_union_verbatim j = S2.S2_cell_union.from_verbatim (cell_ids_of_json j)

let check_cell_ids msg ~expected ~actual =
  let n_exp = Array.length expected in
  let n_act = Array.length actual in
  if n_exp <> n_act
  then Alcotest.fail (sprintf "%s: length mismatch: expected %d, got %d" msg n_exp n_act)
  else
    Array.iteri expected ~f:(fun i exp ->
      let act = actual.(i) in
      if not (Int64.equal exp act)
      then Alcotest.fail (sprintf "%s[%d]: expected %Ld, got %Ld" msg i exp act))
;;

(* {1 Tests} *)

let test_whole_sphere () =
  let data = member "whole_sphere" (Lazy.force fixture) in
  let ws = S2.S2_cell_union.whole_sphere () in
  check
    int
    "num_cells"
    (int_of_json_exn (member "num_cells" data))
    (S2.S2_cell_union.num_cells ws);
  let expected_ids = cell_ids_of_json (member "cell_ids" data) in
  check_cell_ids
    "cell_ids"
    ~expected:expected_ids
    ~actual:(S2.S2_cell_union.cell_ids_raw ws);
  let expected_leaves = int64_of_json_exn (member "leaf_cells_covered" data) in
  check
    bool
    "leaf_cells"
    true
    (Int64.equal expected_leaves (S2.S2_cell_union.leaf_cells_covered ws))
;;

let test_is_normalized () =
  let data = to_list (member "is_normalized" (Lazy.force fixture)) in
  List.iteri data ~f:(fun idx case ->
    let ids = cell_ids_of_json (member "cell_ids" case) in
    let cu = S2.S2_cell_union.from_verbatim ids in
    let expected_valid = bool_of_json_exn (member "is_valid" case) in
    let expected_normalized = bool_of_json_exn (member "is_normalized" case) in
    check
      bool
      (sprintf "case%d is_valid" idx)
      expected_valid
      (S2.S2_cell_union.is_valid cu);
    check
      bool
      (sprintf "case%d is_normalized" idx)
      expected_normalized
      (S2.S2_cell_union.is_normalized cu))
;;

let test_normalize () =
  let data = to_list (member "normalize" (Lazy.force fixture)) in
  List.iter data ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let input = cell_ids_of_json (member "input" case) in
    let expected = cell_ids_of_json (member "normalized" case) in
    let cu = S2.S2_cell_union.create input in
    check_cell_ids label ~expected ~actual:(S2.S2_cell_union.cell_ids_raw cu))
;;

let test_contains_intersects () =
  let data = to_list (member "contains_intersects" (Lazy.force fixture)) in
  List.iter data ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let union_ids = cell_ids_of_json (member "union_ids" case) in
    let cu = S2.S2_cell_union.from_verbatim union_ids in
    let test_id = cell_id_of_json (member "test_id" case) in
    let expected_contains = bool_of_json_exn (member "contains" case) in
    let expected_intersects = bool_of_json_exn (member "intersects" case) in
    check
      bool
      (label ^ " contains")
      expected_contains
      (S2.S2_cell_union.contains_cell_id cu test_id);
    check
      bool
      (label ^ " intersects")
      expected_intersects
      (S2.S2_cell_union.intersects_cell_id cu test_id))
;;

let test_contains_intersects_union () =
  let data = to_list (member "contains_intersects_union" (Lazy.force fixture)) in
  List.iter data ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let x = make_union_verbatim (member "x" case) in
    let y = make_union_verbatim (member "y" case) in
    let expected_contains = bool_of_json_exn (member "contains" case) in
    let expected_intersects = bool_of_json_exn (member "intersects" case) in
    check
      bool
      (label ^ " contains")
      expected_contains
      (S2.S2_cell_union.contains_union x y);
    check
      bool
      (label ^ " intersects")
      expected_intersects
      (S2.S2_cell_union.intersects_union x y))
;;

let test_boolean_ops () =
  let data = to_list (member "boolean_ops" (Lazy.force fixture)) in
  List.iter data ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let x = make_union (member "x" case) in
    let y = make_union (member "y" case) in
    let expected_union = cell_ids_of_json (member "union" case) in
    let expected_intersection = cell_ids_of_json (member "intersection" case) in
    let expected_difference = cell_ids_of_json (member "difference" case) in
    check_cell_ids
      (label ^ " union")
      ~expected:expected_union
      ~actual:(S2.S2_cell_union.cell_ids_raw (S2.S2_cell_union.union x y));
    check_cell_ids
      (label ^ " intersection")
      ~expected:expected_intersection
      ~actual:(S2.S2_cell_union.cell_ids_raw (S2.S2_cell_union.intersection x y));
    check_cell_ids
      (label ^ " difference")
      ~expected:expected_difference
      ~actual:(S2.S2_cell_union.cell_ids_raw (S2.S2_cell_union.difference x y)))
;;

let test_intersection_with_cell_id () =
  let data = to_list (member "intersection_with_cell_id" (Lazy.force fixture)) in
  List.iter data ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let cu = make_union_verbatim (member "union_ids" case) in
    let cid = cell_id_of_json (member "cell_id" case) in
    let expected = cell_ids_of_json (member "result" case) in
    let result = S2.S2_cell_union.intersection_with_cell_id cu cid in
    check_cell_ids label ~expected ~actual:(S2.S2_cell_union.cell_ids_raw result))
;;

let test_from_min_max () =
  let data = to_list (member "from_min_max" (Lazy.force fixture)) in
  List.iter data ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let min_id = cell_id_of_json (member "min_id" case) in
    let max_id = cell_id_of_json (member "max_id" case) in
    let expected = cell_ids_of_json (member "result" case) in
    let cu = S2.S2_cell_union.from_min_max min_id max_id in
    check_cell_ids label ~expected ~actual:(S2.S2_cell_union.cell_ids_raw cu);
    let expected_normalized = bool_of_json_exn (member "is_normalized" case) in
    check
      bool
      (label ^ " is_normalized")
      expected_normalized
      (S2.S2_cell_union.is_normalized cu);
    (* Check range endpoints *)
    let expected_first_min = raw_id_of_json (member "first_range_min" case) in
    let expected_last_max = raw_id_of_json (member "last_range_max" case) in
    let ids = S2.S2_cell_union.cell_ids_raw cu in
    let n = Array.length ids in
    check
      bool
      (label ^ " first_range_min")
      true
      (Int64.equal
         expected_first_min
         (S2.S2_cell_id.id (S2.S2_cell_id.range_min (S2.S2_cell_id.of_int64 ids.(0)))));
    check
      bool
      (label ^ " last_range_max")
      true
      (Int64.equal
         expected_last_max
         (S2.S2_cell_id.id (S2.S2_cell_id.range_max (S2.S2_cell_id.of_int64 ids.(n - 1))))))
;;

let test_from_begin_end () =
  let data = to_list (member "from_begin_end" (Lazy.force fixture)) in
  List.iter data ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let begin_id = cell_id_of_json (member "begin" case) in
    let end_id = cell_id_of_json (member "end" case) in
    let expected_num = int_of_json_exn (member "num_cells" case) in
    let cu = S2.S2_cell_union.from_begin_end begin_id end_id in
    check int (label ^ " num_cells") expected_num (S2.S2_cell_union.num_cells cu);
    let expected_ids = cell_ids_of_json (member "result" case) in
    check_cell_ids label ~expected:expected_ids ~actual:(S2.S2_cell_union.cell_ids_raw cu))
;;

let test_denormalize () =
  let data = to_list (member "denormalize" (Lazy.force fixture)) in
  List.iter data ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let cu = make_union_verbatim (member "input" case) in
    let min_level = int_of_json_exn (member "min_level" case) in
    let level_mod = int_of_json_exn (member "level_mod" case) in
    let expected = cell_ids_of_json (member "result" case) in
    let result = S2.S2_cell_union.denormalize cu ~min_level ~level_mod in
    check_cell_ids label ~expected ~actual:result)
;;

let test_leaf_cells_covered () =
  let data = to_list (member "leaf_cells_covered" (Lazy.force fixture)) in
  List.iter data ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let cu = make_union_verbatim (member "cell_ids" case) in
    let expected = int64_of_json_exn (member "leaf_cells_covered" case) in
    let actual = S2.S2_cell_union.leaf_cells_covered cu in
    check bool label true (Int64.equal expected actual))
;;

let test_area () =
  let data = to_list (member "area" (Lazy.force fixture)) in
  List.iter data ~f:(fun case ->
    let label = string_of_json_exn (member "label" case) in
    let cu = make_union_verbatim (member "cell_ids" case) in
    let expected_avg = float_u_of_json_exn (member "average_based_area" case) in
    let expected_approx = float_u_of_json_exn (member "approx_area" case) in
    let expected_exact = float_u_of_json_exn (member "exact_area" case) in
    (* Area computations can differ by up to ~2e-15 due to rounding in spherical_area. *)
    check_float_u
      ~eps:1e-14
      (label ^ " average_based")
      ~expected:expected_avg
      ~actual:(S2.S2_cell_union.average_based_area cu);
    check_float_u
      ~eps:1e-14
      (label ^ " approx")
      ~expected:expected_approx
      ~actual:(S2.S2_cell_union.approx_area cu);
    check_float_u
      ~eps:1e-14
      (label ^ " exact")
      ~expected:expected_exact
      ~actual:(S2.S2_cell_union.exact_area cu))
;;

let test_empty_ops () =
  let data = member "empty_ops" (Lazy.force fixture) in
  let face1_id = cell_id_of_json (member "face1_id" data) in
  let empty_cu = S2.S2_cell_union.empty () in
  let face1_cu = S2.S2_cell_union.create [| S2.S2_cell_id.id face1_id |] in
  check
    bool
    "empty contains face1 id"
    (bool_of_json_exn (member "empty_contains_face1_id" data))
    (S2.S2_cell_union.contains_cell_id empty_cu face1_id);
  check
    bool
    "face1 contains face1 id"
    (bool_of_json_exn (member "face1_contains_face1_id" data))
    (S2.S2_cell_union.contains_cell_id face1_cu face1_id);
  check
    bool
    "empty intersects face1 id"
    (bool_of_json_exn (member "empty_intersects_face1_id" data))
    (S2.S2_cell_union.intersects_cell_id empty_cu face1_id);
  check
    bool
    "face1 intersects face1 id"
    (bool_of_json_exn (member "face1_intersects_face1_id" data))
    (S2.S2_cell_union.intersects_cell_id face1_cu face1_id);
  (* union *)
  check_cell_ids
    "empty union empty"
    ~expected:(cell_ids_of_json (member "empty_union_empty" data))
    ~actual:(S2.S2_cell_union.cell_ids_raw (S2.S2_cell_union.union empty_cu empty_cu));
  check_cell_ids
    "face1 union empty"
    ~expected:(cell_ids_of_json (member "face1_union_empty" data))
    ~actual:(S2.S2_cell_union.cell_ids_raw (S2.S2_cell_union.union face1_cu empty_cu));
  check_cell_ids
    "empty union face1"
    ~expected:(cell_ids_of_json (member "empty_union_face1" data))
    ~actual:(S2.S2_cell_union.cell_ids_raw (S2.S2_cell_union.union empty_cu face1_cu));
  (* intersection *)
  check_cell_ids
    "face1 intersection empty"
    ~expected:(cell_ids_of_json (member "face1_intersection_empty" data))
    ~actual:
      (S2.S2_cell_union.cell_ids_raw (S2.S2_cell_union.intersection face1_cu empty_cu));
  check_cell_ids
    "empty intersection face1"
    ~expected:(cell_ids_of_json (member "empty_intersection_face1" data))
    ~actual:
      (S2.S2_cell_union.cell_ids_raw (S2.S2_cell_union.intersection empty_cu face1_cu));
  (* difference *)
  check_cell_ids
    "face1 difference empty"
    ~expected:(cell_ids_of_json (member "face1_difference_empty" data))
    ~actual:
      (S2.S2_cell_union.cell_ids_raw (S2.S2_cell_union.difference face1_cu empty_cu));
  check_cell_ids
    "empty difference face1"
    ~expected:(cell_ids_of_json (member "empty_difference_face1" data))
    ~actual:
      (S2.S2_cell_union.cell_ids_raw (S2.S2_cell_union.difference empty_cu face1_cu));
  check_cell_ids
    "face1 difference face1"
    ~expected:(cell_ids_of_json (member "face1_difference_face1" data))
    ~actual:
      (S2.S2_cell_union.cell_ids_raw (S2.S2_cell_union.difference face1_cu face1_cu))
;;

let test_equal () =
  let a = S2.S2_cell_union.whole_sphere () in
  let b = S2.S2_cell_union.whole_sphere () in
  check bool "equal whole spheres" true (S2.S2_cell_union.equal a b);
  check
    bool
    "not equal whole vs empty"
    false
    (S2.S2_cell_union.equal a (S2.S2_cell_union.empty ()))
;;

let () =
  run
    "S2CellUnion"
    [ ( "constructors"
      , [ test_case "whole_sphere" `Quick test_whole_sphere
        ; test_case "from_min_max" `Quick test_from_min_max
        ; test_case "from_begin_end" `Quick test_from_begin_end
        ] )
    ; "validation", [ test_case "is_normalized" `Quick test_is_normalized ]
    ; "normalize", [ test_case "normalize" `Quick test_normalize ]
    ; ( "containment"
      , [ test_case "contains_intersects" `Quick test_contains_intersects
        ; test_case "contains_intersects_union" `Quick test_contains_intersects_union
        ] )
    ; ( "set_ops"
      , [ test_case "boolean_ops" `Quick test_boolean_ops
        ; test_case "intersection_with_cell_id" `Quick test_intersection_with_cell_id
        ] )
    ; "denormalize", [ test_case "denormalize" `Quick test_denormalize ]
    ; ( "measures"
      , [ test_case "leaf_cells_covered" `Quick test_leaf_cells_covered
        ; test_case "area" `Quick test_area
        ] )
    ; "empty_ops", [ test_case "empty_ops" `Quick test_empty_ops ]
    ; "comparison", [ test_case "equal" `Quick test_equal ]
    ]
;;
