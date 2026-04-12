(* C++ test parity: s2geometry/src/s2/s2padded_cell_test.cc
   -  TEST(S2PaddedCell, S2CellMethods)          - s2cell_methods, child_construction,
      GetChildIJ, GetCenter
   -  TEST(S2PaddedCell, GetEntryExitVertices)   - entry_exit_vertices (padding
      independence, next_wrap coincidence, child entry/exit coincidence)
   -  TEST(S2PaddedCell, ShrinkToFit)            - shrink_to_fit (deterministic
      hand-picked cases mirroring the randomized upstream test)

   Extra coverage (quickcheck):
   -  padded cell bound parity vs S2Cell bound_uv expanded by padding
   -  child_ij_of_pos is the inverse of kIJtoPos via round-trip child construction
   -  exit vertex of a cell equals entry vertex of next_wrap at the same level *)

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
      bind (int_uniform_inclusive 0 20) ~f:(fun depth ->
        descend depth (int64_of_cid (S2.S2_cell_id.from_face_exn f))))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

let cell_id_of_json j = S2.S2_cell_id.from_token (string_of_json_exn j)
let check_point_exact msg ~expected ~actual = check_r3_vector_exact msg ~expected ~actual

let check_padded_cell_against_json msg j pc =
  (check string)
    (msg ^ " id")
    (string_of_json_exn (member "id" j))
    (S2.S2_cell_id.to_token (S2.S2_padded_cell.id pc));
  (check int)
    (msg ^ " level")
    (int_of_json_exn (member "level" j))
    (S2.S2_padded_cell.level pc);
  check_float_u_exact
    (msg ^ " padding")
    ~expected:(float_u_of_json_exn (member "padding" j))
    ~actual:(S2.S2_padded_cell.padding pc);
  let bound_json = r2_rect_of_json (member "bound" j) in
  let bound = S2.S2_padded_cell.bound pc in
  check_float_u_exact
    (msg ^ " bound x.lo")
    ~expected:(S2.R1_interval.lo (S2.R2_rect.x bound_json))
    ~actual:(S2.R1_interval.lo (S2.R2_rect.x bound));
  check_float_u_exact
    (msg ^ " bound x.hi")
    ~expected:(S2.R1_interval.hi (S2.R2_rect.x bound_json))
    ~actual:(S2.R1_interval.hi (S2.R2_rect.x bound));
  check_float_u_exact
    (msg ^ " bound y.lo")
    ~expected:(S2.R1_interval.lo (S2.R2_rect.y bound_json))
    ~actual:(S2.R1_interval.lo (S2.R2_rect.y bound));
  check_float_u_exact
    (msg ^ " bound y.hi")
    ~expected:(S2.R1_interval.hi (S2.R2_rect.y bound_json))
    ~actual:(S2.R1_interval.hi (S2.R2_rect.y bound));
  let middle_json = r2_rect_of_json (member "middle" j) in
  let middle = S2.S2_padded_cell.middle pc in
  check_float_u_exact
    (msg ^ " middle x.lo")
    ~expected:(S2.R1_interval.lo (S2.R2_rect.x middle_json))
    ~actual:(S2.R1_interval.lo (S2.R2_rect.x middle));
  check_float_u_exact
    (msg ^ " middle x.hi")
    ~expected:(S2.R1_interval.hi (S2.R2_rect.x middle_json))
    ~actual:(S2.R1_interval.hi (S2.R2_rect.x middle));
  check_float_u_exact
    (msg ^ " middle y.lo")
    ~expected:(S2.R1_interval.lo (S2.R2_rect.y middle_json))
    ~actual:(S2.R1_interval.lo (S2.R2_rect.y middle));
  check_float_u_exact
    (msg ^ " middle y.hi")
    ~expected:(S2.R1_interval.hi (S2.R2_rect.y middle_json))
    ~actual:(S2.R1_interval.hi (S2.R2_rect.y middle));
  check_point_exact
    (msg ^ " center")
    ~expected:(r3_vector_of_json (member "center" j))
    ~actual:(S2.S2_padded_cell.center pc);
  check_point_exact
    (msg ^ " entry_vertex")
    ~expected:(r3_vector_of_json (member "entry_vertex" j))
    ~actual:(S2.S2_padded_cell.entry_vertex pc);
  check_point_exact
    (msg ^ " exit_vertex")
    ~expected:(r3_vector_of_json (member "exit_vertex" j))
    ~actual:(S2.S2_padded_cell.exit_vertex pc);
  List.iteri
    (to_list (member "child_ij" j))
    ~f:(fun pos ij_j ->
      let expected_i = int_of_json_exn (List.nth_exn (to_list ij_j) 0) in
      let expected_j = int_of_json_exn (List.nth_exn (to_list ij_j) 1) in
      let #(got_i, got_j) = S2.S2_padded_cell.child_ij_of_pos pc ~pos in
      (check int) (sprintf "%s child_ij[%d].i" msg pos) expected_i got_i;
      (check int) (sprintf "%s child_ij[%d].j" msg pos) expected_j got_j)
;;

let test_s2cell_methods fixture () =
  let cases = to_list (member "s2cell_methods" fixture) in
  List.iteri cases ~f:(fun idx j ->
    let id = cell_id_of_json (member "id" j) in
    let padding = float_u_of_json_exn (member "padding" j) in
    let pc = S2.S2_padded_cell.create id ~padding in
    check_padded_cell_against_json (sprintf "s2cell_methods[%d]" idx) j pc)
;;

let test_child_construction fixture () =
  let cases = to_list (member "child_construction" fixture) in
  List.iteri cases ~f:(fun idx j ->
    let parent_id = cell_id_of_json (member "parent_id" j) in
    let padding = float_u_of_json_exn (member "padding" j) in
    let parent = S2.S2_padded_cell.create parent_id ~padding in
    List.iter
      (to_list (member "children" j))
      ~f:(fun cj ->
        let i = int_of_json_exn (member "i" cj) in
        let j = int_of_json_exn (member "j" cj) in
        let child = S2.S2_padded_cell.child_ij parent ~i ~j in
        check_padded_cell_against_json
          (sprintf "child_construction[%d](%d,%d)" idx i j)
          cj
          child))
;;

let test_entry_exit_vertices fixture () =
  let cases = to_list (member "entry_exit_vertices" fixture) in
  List.iteri cases ~f:(fun idx j ->
    let id = cell_id_of_json (member "id" j) in
    let label = sprintf "entry_exit[%d]" idx in
    let pc0 = S2.S2_padded_cell.create id ~padding:#0.0 in
    let pc05 = S2.S2_padded_cell.create id ~padding:#0.5 in
    check_point_exact
      (label ^ " entry_pad0")
      ~expected:(r3_vector_of_json (member "entry_pad0" j))
      ~actual:(S2.S2_padded_cell.entry_vertex pc0);
    check_point_exact
      (label ^ " entry_pad05")
      ~expected:(r3_vector_of_json (member "entry_pad05" j))
      ~actual:(S2.S2_padded_cell.entry_vertex pc05);
    check_point_exact
      (label ^ " exit_pad0")
      ~expected:(r3_vector_of_json (member "exit_pad0" j))
      ~actual:(S2.S2_padded_cell.exit_vertex pc0);
    check_point_exact
      (label ^ " exit_pad05")
      ~expected:(r3_vector_of_json (member "exit_pad05" j))
      ~actual:(S2.S2_padded_cell.exit_vertex pc05);
    let next_id = cell_id_of_json (member "next_id" j) in
    let pc_next = S2.S2_padded_cell.create next_id ~padding:#0.0 in
    check_point_exact
      (label ^ " next_entry")
      ~expected:(r3_vector_of_json (member "next_entry" j))
      ~actual:(S2.S2_padded_cell.entry_vertex pc_next);
    if not (S2.S2_cell_id.is_leaf id)
    then (
      let child0 =
        S2.S2_padded_cell.create (S2.S2_cell_id.child_exn id 0) ~padding:#0.0
      in
      let child3 =
        S2.S2_padded_cell.create (S2.S2_cell_id.child_exn id 3) ~padding:#0.0
      in
      check_point_exact
        (label ^ " child0_entry")
        ~expected:(r3_vector_of_json (member "child0_entry" j))
        ~actual:(S2.S2_padded_cell.entry_vertex child0);
      check_point_exact
        (label ^ " child3_exit")
        ~expected:(r3_vector_of_json (member "child3_exit" j))
        ~actual:(S2.S2_padded_cell.exit_vertex child3)))
;;

let test_shrink_to_fit fixture () =
  let cases = to_list (member "shrink_to_fit" fixture) in
  List.iteri cases ~f:(fun idx j ->
    let initial_id = cell_id_of_json (member "initial_id" j) in
    let padding = float_u_of_json_exn (member "padding" j) in
    let rect = r2_rect_of_json (member "rect" j) in
    let expected = cell_id_of_json (member "expected" j) in
    let pc = S2.S2_padded_cell.create initial_id ~padding in
    let got = S2.S2_padded_cell.shrink_to_fit pc rect in
    (check string)
      (sprintf "shrink_to_fit[%d]" idx)
      (S2.S2_cell_id.to_token expected)
      (S2.S2_cell_id.to_token got))
;;

(* Quickcheck: the bound of a padded cell with padding 0 equals the (u, v)-bound of the
   corresponding S2Cell, and padding > 0 expands it symmetrically by the padding. *)
let quickcheck_bound_matches_s2cell () =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cid = cid_of_int64 id in
    let cell = S2.S2_cell.of_cell_id cid in
    let expected = S2.S2_cell.bound_uv cell in
    let pc = S2.S2_padded_cell.create cid ~padding:#0.0 in
    let got = S2.S2_padded_cell.bound pc in
    assert (
      S2.R2_rect.approx_equal
        ~max_error:(Packed_float_option.Unboxed.none ())
        expected
        got);
    let padding = #0.01 in
    let expanded = S2.R2_rect.expanded_scalar expected padding in
    let pc2 = S2.S2_padded_cell.create cid ~padding in
    let got2 = S2.S2_padded_cell.bound pc2 in
    assert (
      S2.R2_rect.approx_equal
        ~max_error:(Packed_float_option.Unboxed.none ())
        expanded
        got2))
;;

(* Quickcheck: the padded cell center equals the S2Cell center. *)
let quickcheck_center_matches_s2cell () =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cid = cid_of_int64 id in
    let cell = S2.S2_cell.of_cell_id cid in
    let pc = S2.S2_padded_cell.create cid ~padding:#0.0 in
    let expected = S2.S2_cell.center cell in
    let actual = S2.S2_padded_cell.center pc in
    check_point_exact "center_match" ~expected ~actual)
;;

(* Quickcheck: exit_vertex of one cell equals entry_vertex of its Hilbert successor. *)
let quickcheck_exit_equals_next_entry () =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cid = cid_of_int64 id in
    let pc = S2.S2_padded_cell.create cid ~padding:#0.0 in
    let pc_next = S2.S2_padded_cell.create (S2.S2_cell_id.next_wrap cid) ~padding:#0.0 in
    check_point_exact
      "exit->next_entry"
      ~expected:(S2.S2_padded_cell.exit_vertex pc)
      ~actual:(S2.S2_padded_cell.entry_vertex pc_next))
;;

(* Quickcheck: child_ij_of_pos inverts child_ij (up to the orientation-dependent
   kPosToIJ / kIJtoPos tables). *)
let quickcheck_child_ij_roundtrip () =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let cid = cid_of_int64 id in
    if not (S2.S2_cell_id.is_leaf cid)
    then (
      let parent = S2.S2_padded_cell.create cid ~padding:#0.0 in
      for pos = 0 to 3 do
        let #(i, j) = S2.S2_padded_cell.child_ij_of_pos parent ~pos in
        let child = S2.S2_padded_cell.child_ij parent ~i ~j in
        let expected_id = S2.S2_cell_id.child_exn cid pos in
        assert (S2.S2_cell_id.equal (S2.S2_padded_cell.id child) expected_id)
      done))
;;

let () =
  let fixture = load_fixture "s2padded_cell.json" in
  Alcotest.run
    "S2_padded_cell"
    [ "s2cell_methods", [ test_case "S2CellMethods" `Quick (test_s2cell_methods fixture) ]
    ; ( "child_construction"
      , [ test_case "ChildConstruction" `Quick (test_child_construction fixture) ] )
    ; ( "entry_exit_vertices"
      , [ test_case "GetEntryExitVertices" `Quick (test_entry_exit_vertices fixture) ] )
    ; "shrink_to_fit", [ test_case "ShrinkToFit" `Quick (test_shrink_to_fit fixture) ]
    ; ( "quickcheck"
      , [ test_case "bound_matches_s2cell" `Quick quickcheck_bound_matches_s2cell
        ; test_case "center_matches_s2cell" `Quick quickcheck_center_matches_s2cell
        ; test_case "exit_equals_next_entry" `Quick quickcheck_exit_equals_next_entry
        ; test_case "child_ij_roundtrip" `Quick quickcheck_child_ij_roundtrip
        ] )
    ]
;;
