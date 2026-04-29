(* Golden data produced by test/gen/s2convex_hull_query.cc.

   Upstream C++ tests mirrored (s2convex_hull_query_test.cc):
   - TEST(S2ConvexHullQuery, NoPoints)
   - TEST(S2ConvexHullQuery, OnePoint)
   - TEST(S2ConvexHullQuery, TwoPoints)
   - TEST(S2ConvexHullQuery, TwoAntipodalPoints)
   - TEST(S2ConvexHullQuery, EmptyLoop / FullLoop / EmptyPolygon)
   - TEST(S2ConvexHullQuery, NonConvexPoints)
   - TEST(S2ConvexHullQuery, SimplePolyline)
   - TEST(S2ConvexHullQuery, CapBoundExpandedToHemisphere)
   - TEST(S2ConvexHullQuery, LoopsAroundNorthPole) (a small sample of radii)

   Deliberately omitted:
   - PointsInsideHull: randomized test; not needed once the direct cases above are green. *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2convex_hull_query.json")
let point_of_json = r3_vector_of_json

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

let apply_input q input =
  let kind = string_of_json_exn (member "kind" input) in
  let vertices = vertices_of_json (member "vertices" input) in
  match kind with
  | "point" ->
    for i = 0 to Array.length vertices - 1 do
      S2.S2_convex_hull_query.add_point q vertices.(i)
    done
  | "polyline" ->
    let pl = S2.S2_polyline.of_vertices ~validate:false vertices in
    S2.S2_convex_hull_query.add_polyline q pl
  | "loop" ->
    let l = S2.S2_loop.of_vertices ~validate:false vertices in
    S2.S2_convex_hull_query.add_loop q l
  | "empty_loop" -> S2.S2_convex_hull_query.add_loop q (S2.S2_loop.empty ())
  | "full_loop" -> S2.S2_convex_hull_query.add_loop q (S2.S2_loop.full ())
  | "empty_polygon" -> S2.S2_convex_hull_query.add_polygon q (S2.S2_polygon.empty ())
  | other ->
    (match failwith (sprintf "unknown input kind: %s" other) with
     | (_ : Nothing.t) -> .)
;;

let run_case case =
  let name = string_of_json_exn (member "name" case) in
  let q = S2.S2_convex_hull_query.create () in
  List.iter (to_list (member "inputs" case)) ~f:(apply_input q);
  let hull = S2.S2_convex_hull_query.convex_hull q in
  (check bool)
    (name ^ " is_empty")
    (bool_of_json_exn (member "is_empty" case))
    (S2.S2_loop.is_empty hull);
  (check bool)
    (name ^ " is_full")
    (bool_of_json_exn (member "is_full" case))
    (S2.S2_loop.is_full hull);
  (check int)
    (name ^ " num_vertices")
    (int_of_json_exn (member "num_vertices" case))
    (S2.S2_loop.num_vertices hull);
  (check bool)
    (name ^ " is_normalized")
    (bool_of_json_exn (member "is_normalized" case))
    (S2.S2_loop.is_normalized hull);
  (* Every point in [must_contain] must either be a hull vertex or be contained by the
     hull. *)
  let expected_vertices = vertices_of_json (member "hull_vertices" case) in
  let must_contain = vertices_of_json (member "must_contain" case) in
  for k = 0 to Array.length must_contain - 1 do
    let p = must_contain.(k) in
    let mutable is_vertex = false in
    for j = 0 to Array.length expected_vertices - 1 do
      if S2.S2_point.equal expected_vertices.(j) p then is_vertex <- true
    done;
    let in_hull = S2.S2_loop.contains_point hull p in
    let mutable hull_is_a_vertex = false in
    let n = S2.S2_loop.num_vertices hull in
    for i = 0 to n - 1 do
      if S2.S2_point.equal (S2.S2_loop.vertex hull i) p then hull_is_a_vertex <- true
    done;
    (check bool)
      (sprintf "%s contains or is-vertex %s" name (S2.S2_point.to_string p))
      true
      (is_vertex || in_hull || hull_is_a_vertex)
  done
;;

let test_all () =
  let cases = to_list (member "cases" (Lazy.force fixture)) in
  List.iter cases ~f:run_case
;;

let () = run "s2_convex_hull_query" [ "cases", [ test_case "all" `Quick test_all ] ]
