(* Tests for S2_region. There is no upstream C++ S2RegionTest with directly
   matching cases, so this file exercises the Region interface methods on each
   concrete S2 type (S2Cap, S2LatLngRect, S2Cell, S2CellUnion) against golden data
   produced by test/gen/s2region.cc.

   Coverage:
   - cap_bound, rect_bound, contains_point, contains_cell, intersects_cell,
     and cell_union_bound on S2_region.of_cap, of_rect, of_cell, of_cell_union

   Deliberately omitted:
   - rect_bound for the [empty] case is compared via R1/S1 interval equality
     using the canonical empty representation. *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2region.json")
let get key = member key (Lazy.force fixture)
let cell_id_of_token_json j = S2.S2_cell_id.from_token (string_of_json_exn j)
let probe_cell_tokens () = to_list (get "probe_cells")
let probe_point_jsons () = to_list (get "probe_points")

let check_cap msg ~expected ~actual =
  check_r3_vector
    (msg ^ " center")
    ~expected:(S2.S2_cap.center expected)
    ~actual:(S2.S2_cap.center actual);
  check_float_u
    (msg ^ " length2")
    ~expected:(S2.S1_chord_angle.length2 (S2.S2_cap.radius_chord expected))
    ~actual:(S2.S1_chord_angle.length2 (S2.S2_cap.radius_chord actual))
;;

let check_rect msg ~expected ~actual =
  check_float_u
    (msg ^ " lat lo")
    ~expected:(S2.R1_interval.lo (S2.S2_latlng_rect.lat expected))
    ~actual:(S2.R1_interval.lo (S2.S2_latlng_rect.lat actual));
  check_float_u
    (msg ^ " lat hi")
    ~expected:(S2.R1_interval.hi (S2.S2_latlng_rect.lat expected))
    ~actual:(S2.R1_interval.hi (S2.S2_latlng_rect.lat actual));
  check_float_u
    (msg ^ " lng lo")
    ~expected:(S2.S1_interval.lo (S2.S2_latlng_rect.lng expected))
    ~actual:(S2.S1_interval.lo (S2.S2_latlng_rect.lng actual));
  check_float_u
    (msg ^ " lng hi")
    ~expected:(S2.S1_interval.hi (S2.S2_latlng_rect.lng expected))
    ~actual:(S2.S1_interval.hi (S2.S2_latlng_rect.lng actual))
;;

let cap_of_json j =
  let center = r3_vector_of_json (member "center" j) in
  let length2 = float_u_of_json_exn (member "length2" j) in
  S2.S2_cap.of_center_chord_angle center (S2.S1_chord_angle.of_length2 length2)
;;

let check_region label region region_j =
  let r = region in
  let expected_cap = cap_of_json (member "cap_bound" region_j) in
  let expected_rect = latlng_rect_of_json (member "rect_bound" region_j) in
  let actual_cap = S2.S2_region.cap_bound r in
  let actual_rect = S2.S2_region.rect_bound r in
  check_cap (label ^ " cap_bound") ~expected:expected_cap ~actual:actual_cap;
  check_rect (label ^ " rect_bound") ~expected:expected_rect ~actual:actual_rect;
  let cell_tokens = probe_cell_tokens () in
  let point_jsons = probe_point_jsons () in
  let expected_contains_cell = to_list (member "contains_cell" region_j) in
  List.iteri
    (List.zip_exn cell_tokens expected_contains_cell)
    ~f:(fun i (cell_j, exp_j) ->
      let cell = S2.S2_cell.of_cell_id (cell_id_of_token_json cell_j) in
      check_bool
        (sprintf "%s contains_cell[%d]" label i)
        ~expected:(bool_of_json_exn exp_j)
        ~actual:(S2.S2_region.contains_cell r cell));
  let expected_intersects_cell = to_list (member "intersects_cell" region_j) in
  List.iteri
    (List.zip_exn cell_tokens expected_intersects_cell)
    ~f:(fun i (cell_j, exp_j) ->
      let cell = S2.S2_cell.of_cell_id (cell_id_of_token_json cell_j) in
      check_bool
        (sprintf "%s intersects_cell[%d]" label i)
        ~expected:(bool_of_json_exn exp_j)
        ~actual:(S2.S2_region.intersects_cell r cell));
  let expected_contains_point = to_list (member "contains_point" region_j) in
  List.iteri
    (List.zip_exn point_jsons expected_contains_point)
    ~f:(fun i (point_j, exp_j) ->
      let p = r3_vector_of_json point_j in
      check_bool
        (sprintf "%s contains_point[%d]" label i)
        ~expected:(bool_of_json_exn exp_j)
        ~actual:(S2.S2_region.contains_point r p));
  let expected_cu_bound = to_list (member "cell_union_bound" region_j) in
  let actual_cu_bound = S2.S2_region.cell_union_bound r in
  (check int)
    (label ^ " cell_union_bound length")
    (List.length expected_cu_bound)
    (List.length actual_cu_bound);
  List.iteri
    (List.zip_exn expected_cu_bound actual_cu_bound)
    ~f:(fun i (expected_j, actual_id) ->
      let expected_id = cell_id_of_token_json expected_j in
      check_bool
        (sprintf "%s cell_union_bound[%d]" label i)
        ~expected:true
        ~actual:(S2.S2_cell_id.equal expected_id (cid_of_int64 actual_id)))
;;

let test_caps () =
  List.iter
    (to_list (get "cap"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let cap = cap_of_json (member "cap" case) in
      let region = S2.S2_region.of_cap cap in
      check_region ("cap:" ^ name) region (member "region" case))
;;

let test_rects () =
  List.iter
    (to_list (get "rect"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let rect = latlng_rect_of_json (member "rect" case) in
      let region = S2.S2_region.of_rect rect in
      check_region ("rect:" ^ name) region (member "region" case))
;;

let test_cells () =
  List.iter
    (to_list (get "cell"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let cell = S2.S2_cell.of_cell_id (cell_id_of_token_json (member "cell_id" case)) in
      let region = S2.S2_region.of_cell cell in
      check_region ("cell:" ^ name) region (member "region" case))
;;

let test_cell_unions () =
  List.iter
    (to_list (get "cell_union"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let tokens = to_list (member "cell_ids" case) |> Array.of_list in
      let ids = Array.create ~len:(Array.length tokens) S2.S2_cell_id.none in
      for i = 0 to Array.length tokens - 1 do
        ids.(i) <- cell_id_of_token_json tokens.(i)
      done;
      let union = S2.S2_cell_union.create ids in
      let region = S2.S2_region.of_cell_union union in
      check_region ("cell_union:" ^ name) region (member "region" case))
;;

let () =
  Alcotest.run
    "S2_region"
    [ "cap", [ test_case "of_cap" `Quick test_caps ]
    ; "rect", [ test_case "of_rect" `Quick test_rects ]
    ; "cell", [ test_case "of_cell" `Quick test_cells ]
    ; "cell_union", [ test_case "of_cell_union" `Quick test_cell_unions ]
    ]
;;
