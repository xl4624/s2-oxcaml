(* C++ test parity: s2geometry/src/s2/s2padded_cell_test.cc
   - TEST(S2PaddedCell, S2CellMethods) - s2cell_methods, child_construction, GetChildIJ,
     GetCenter
   - TEST(S2PaddedCell, GetEntryExitVertices) - entry_exit_vertices (padding independence,
     next_wrap coincidence, child entry/exit coincidence)
   - TEST(S2PaddedCell, ShrinkToFit) - shrink_to_fit (deterministic hand-picked cases
     mirroring the randomized upstream test) *)

open Core
open Test_helpers
open Alcotest

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
    ]
;;
