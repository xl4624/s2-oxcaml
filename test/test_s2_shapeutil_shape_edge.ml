(* Golden data from test/gen/s2shapeutil_shape_edge.cc.
   No upstream s2shapeutil_shape_edge_test.cc exists; the generator emits a
   handful of (shape_id, edge_id, v0, v1) cases so we can verify the OCaml
   constructor and accessors round-trip the inputs as the C++ struct does.

   Covered:
   -  [create] stores [shape_id] and [edge_id] in the [id] field.
   -  [v0] / [v1] return the edge endpoints unchanged, including the
      degenerate case [v0 = v1] and negative-coordinate inputs. *)

open Core
open Alcotest
open Test_helpers
module Shape_edge = S2.S2_shapeutil_shape_edge

let fixture = lazy (load_fixture "s2shapeutil_shape_edge.json")
let point_of_json = r3_vector_of_json

let check_point msg ~expected ~actual =
  let exp = S2.S2_point.to_r3 expected in
  let act = S2.S2_point.to_r3 actual in
  check_float_u_exact
    (msg ^ " x")
    ~expected:(S2.R3_vector.x exp)
    ~actual:(S2.R3_vector.x act);
  check_float_u_exact
    (msg ^ " y")
    ~expected:(S2.R3_vector.y exp)
    ~actual:(S2.R3_vector.y act);
  check_float_u_exact
    (msg ^ " z")
    ~expected:(S2.R3_vector.z exp)
    ~actual:(S2.R3_vector.z act)
;;

let test_cases () =
  let cases = to_list (member "cases" (Lazy.force fixture)) in
  List.iteri cases ~f:(fun i case ->
    let shape_id = int_of_json_exn (member "shape_id" case) in
    let edge_id = int_of_json_exn (member "edge_id" case) in
    let v0 = point_of_json (member "v0_in" case) in
    let v1 = point_of_json (member "v1_in" case) in
    let edge = S2.S2_shape.Edge.create ~v0 ~v1 in
    let se = Shape_edge.create ~shape_id ~edge_id ~edge in
    check
      int
      (sprintf "case[%d] id.shape_id" i)
      (int_of_json_exn (member "id_shape_id" case))
      se.#id.#shape_id;
    check
      int
      (sprintf "case[%d] id.edge_id" i)
      (int_of_json_exn (member "id_edge_id" case))
      se.#id.#edge_id;
    check_point
      (sprintf "case[%d] v0" i)
      ~expected:(point_of_json (member "v0_out" case))
      ~actual:(Shape_edge.v0 se);
    check_point
      (sprintf "case[%d] v1" i)
      ~expected:(point_of_json (member "v1_out" case))
      ~actual:(Shape_edge.v1 se))
;;

let () =
  Alcotest.run
    "S2_shapeutil_shape_edge"
    [ "fixture", [ test_case "constructor and accessors" `Quick test_cases ] ]
;;
