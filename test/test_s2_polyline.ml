(* Golden data produced by test/gen/s2polyline.cc.

   Upstream C++ tests covered (s2polyline_test.cc):
   - TEST(S2Polyline, Basic) -> polylines fixture
   - TEST(S2Polyline, NoData) -> polylines empty case
   - TEST(S2Polyline, GetLengthAndCentroid) -> polylines fixture
   - TEST(S2Polyline, Interpolate) -> interpolate fixture
   - TEST(S2Polyline, UnInterpolate) -> uninterpolate fixture
   - TEST(S2Polyline, Project) -> project fixture
   - TEST(S2Polyline, IsOnRight) -> is_on_right fixture
   - TEST(S2Polyline, IntersectsEmptyPolyline)
   - TEST(S2Polyline, IntersectsOnePointPolyline)
   - TEST(S2Polyline, Intersects)
   - TEST(S2Polyline, IntersectsAtVertex)
   - TEST(S2Polyline, IntersectsVertexOnEdge) -> intersects fixture
   - TEST(S2Polyline, MayIntersect) -> may_intersect fixture
   - TEST(S2Polyline, SubsampleVerticesTrivialInputs)
   - TEST(S2Polyline, SubsampleVerticesSimpleExample)
   - TEST(S2Polyline, SubsampleVerticesGuarantees) -> subsample fixture
   - TEST(S2Polyline, ApproxEquals) -> approx_equal fixture
   - TEST(S2Polyline, Equals) -> equals fixture
   - TEST(S2Polyline, FindValidationError) -> validation fixture
   - TEST(S2PolylineShape, Basic) -> shapes fixture
   - TEST(S2PolylineShape, EmptyPolyline) -> shapes fixture (empty)

   Deliberately omitted:
   - TEST(S2Polyline, NearlyCovers): NearlyCovers is not part of the OCaml
     port surface.
   - EncodeDecode / S2CoderWorks / RoundtripEncoding: encoding is not ported.
   - TEST(S2Polyline, InitToSnapped) / InitToSimplified: snapping/simplifying
     initialisation is not ported.
   - TEST(S2Polyline, GetSnapLevel): snap-level helpers are not ported.
   - Copy / move semantics: OCaml values are immutable at this level.
   - OwningShape: not exposed as a separate constructor. *)

open Core
open Test_helpers
open Alcotest

let fixture = lazy (load_fixture "s2polyline.json")
let get key = member key (Lazy.force fixture)
let s2_point_of_json j = r3_vector_of_json j
let check_point msg ~expected ~actual = check_r3_vector_exact msg ~expected ~actual

let check_edge msg ~(expected : S2.S2_shape.Edge.t) ~(actual : S2.S2_shape.Edge.t) =
  check_point (msg ^ " v0") ~expected:expected.#v0 ~actual:actual.#v0;
  check_point (msg ^ " v1") ~expected:expected.#v1 ~actual:actual.#v1
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

(* polylines: per-case scalar accessors plus length / centroid / cap_bound /
   rect_bound. *)
let test_polylines () =
  List.iter
    (to_list (get "polylines"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let vertices = vertices_of_json (member "vertices" case) in
      let pl = S2.S2_polyline.of_vertices ~validate:false vertices in
      let expected_n = List.length (to_list (member "vertices" case)) in
      (check int) (name ^ " num_vertices") expected_n (S2.S2_polyline.num_vertices pl);
      check_bool
        (name ^ " is_valid")
        ~expected:(bool_of_json_exn (member "is_valid" case))
        ~actual:(S2.S2_polyline.is_valid pl);
      let expected_length = float_u_of_json_exn (member "length_radians" case) in
      check_float_u
        (name ^ " length")
        ~expected:expected_length
        ~actual:(S2.S1_angle.radians (S2.S2_polyline.length pl));
      let expected_centroid = s2_point_of_json (member "centroid" case) in
      check_r3_vector
        (name ^ " centroid")
        ~expected:expected_centroid
        ~actual:(S2.S2_polyline.centroid pl);
      (* vertex accessor *)
      for i = 0 to expected_n - 1 do
        check_point
          (sprintf "%s vertex[%d]" name i)
          ~expected:vertices.(i)
          ~actual:(S2.S2_polyline.vertex pl i)
      done)
;;

let test_reverse () =
  List.iter
    (to_list (get "reverse"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let original = vertices_of_json (member "original" case) in
      let expected = vertices_of_json (member "reversed" case) in
      let pl = S2.S2_polyline.of_vertices ~validate:false original in
      let r = S2.S2_polyline.reverse pl in
      (check int)
        (name ^ " num_vertices")
        (Array.length expected)
        (S2.S2_polyline.num_vertices r);
      for i = 0 to Array.length expected - 1 do
        check_point
          (sprintf "%s reversed[%d]" name i)
          ~expected:expected.(i)
          ~actual:(S2.S2_polyline.vertex r i)
      done)
;;

let test_interpolate () =
  List.iter
    (to_list (get "interpolate"))
    ~f:(fun group ->
      let vertices = vertices_of_json (member "vertices" group) in
      let pl = S2.S2_polyline.of_vertices ~validate:false vertices in
      List.iter
        (to_list (member "cases" group))
        ~f:(fun case ->
          let fraction = float_u_of_json_exn (member "fraction" case) in
          let expected_point = s2_point_of_json (member "point" case) in
          let expected_next = int_of_json_exn (member "next_vertex" case) in
          let (#{ point; next_vertex } : S2.S2_polyline.suffix) =
            S2.S2_polyline.get_suffix pl fraction
          in
          check_r3_vector
            (sprintf "interpolate frac=%g point" (Float_u.to_float fraction))
            ~expected:expected_point
            ~actual:point;
          (check int)
            (sprintf "interpolate frac=%g next_vertex" (Float_u.to_float fraction))
            expected_next
            next_vertex;
          let p2 = S2.S2_polyline.interpolate pl fraction in
          check_r3_vector
            (sprintf "interpolate (alias) frac=%g" (Float_u.to_float fraction))
            ~expected:expected_point
            ~actual:p2))
;;

let test_uninterpolate () =
  List.iter
    (to_list (get "uninterpolate"))
    ~f:(fun case ->
      let vertices = vertices_of_json (member "vertices" case) in
      let point = s2_point_of_json (member "point" case) in
      let next_vertex = int_of_json_exn (member "next_vertex" case) in
      let expected = float_u_of_json_exn (member "expected" case) in
      let pl = S2.S2_polyline.of_vertices ~validate:false vertices in
      let actual = S2.S2_polyline.un_interpolate pl point next_vertex in
      check_float_u
        (sprintf "uninterpolate (n=%d, next=%d)" (Array.length vertices) next_vertex)
        ~expected
        ~actual)
;;

let test_project () =
  List.iter
    (to_list (get "project"))
    ~f:(fun case ->
      let vertices = vertices_of_json (member "vertices" case) in
      let query = s2_point_of_json (member "query" case) in
      let expected_point = s2_point_of_json (member "point" case) in
      let expected_next = int_of_json_exn (member "next_vertex" case) in
      let pl = S2.S2_polyline.of_vertices ~validate:false vertices in
      let (#{ point; next_vertex } : S2.S2_polyline.projection) =
        S2.S2_polyline.project pl query
      in
      check_r3_vector
        (sprintf "project (n=%d) point" (Array.length vertices))
        ~expected:expected_point
        ~actual:point;
      (check int)
        (sprintf "project (n=%d) next_vertex" (Array.length vertices))
        expected_next
        next_vertex)
;;

let test_is_on_right () =
  List.iter
    (to_list (get "is_on_right"))
    ~f:(fun case ->
      let vertices = vertices_of_json (member "vertices" case) in
      let query = s2_point_of_json (member "query" case) in
      let expected = bool_of_json_exn (member "on_right" case) in
      let pl = S2.S2_polyline.of_vertices ~validate:false vertices in
      let actual = S2.S2_polyline.is_on_right pl query in
      check_bool (sprintf "is_on_right (n=%d)" (Array.length vertices)) ~expected ~actual)
;;

let test_intersects () =
  List.iter
    (to_list (get "intersects"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let a = vertices_of_json (member "a" case) in
      let b = vertices_of_json (member "b" case) in
      let expected = bool_of_json_exn (member "intersects" case) in
      let pa = S2.S2_polyline.of_vertices ~validate:false a in
      let pb = S2.S2_polyline.of_vertices ~validate:false b in
      let actual = S2.S2_polyline.intersects pa pb in
      check_bool (name ^ " intersects") ~expected ~actual)
;;

let test_may_intersect () =
  let group = get "may_intersect" in
  let vertices = vertices_of_json (member "vertices" group) in
  let pl = S2.S2_polyline.of_vertices ~validate:false vertices in
  List.iter
    (to_list (member "faces" group))
    ~f:(fun case ->
      let face = int_of_json_exn (member "face" case) in
      let expected = bool_of_json_exn (member "may_intersect" case) in
      let cell = S2.S2_cell.from_face face in
      let actual = S2.S2_polyline.may_intersect_cell pl cell in
      check_bool (sprintf "face %d" face) ~expected ~actual)
;;

let test_subsample () =
  List.iter
    (to_list (get "subsample"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let vertices = vertices_of_json (member "vertices" case) in
      let tolerance =
        S2.S1_angle.of_degrees (float_u_of_json_exn (member "tolerance_degrees" case))
      in
      let expected =
        List.map (to_list (member "indices" case)) ~f:int_of_json_exn |> Array.of_list
      in
      let pl = S2.S2_polyline.of_vertices ~validate:false vertices in
      let actual = S2.S2_polyline.subsample_vertices pl tolerance in
      (check int)
        (name ^ " subsample length")
        (Array.length expected)
        (Array.length actual);
      Array.iteri expected ~f:(fun i v ->
        (check int) (sprintf "%s subsample[%d]" name i) v actual.(i)))
;;

let test_approx_equal () =
  List.iter
    (to_list (get "approx_equal"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let a = vertices_of_json (member "a" case) in
      let b = vertices_of_json (member "b" case) in
      let expected = bool_of_json_exn (member "approx_equal" case) in
      let max_err =
        S2.S1_angle.of_degrees (float_u_of_json_exn (member "max_error_degrees" case))
      in
      let max_error = Packed_float_option.Unboxed.some (S2.S1_angle.radians max_err) in
      let pa = S2.S2_polyline.of_vertices ~validate:false a in
      let pb = S2.S2_polyline.of_vertices ~validate:false b in
      let actual = S2.S2_polyline.approx_equal ~max_error pa pb in
      check_bool (name ^ " approx_equal") ~expected ~actual)
;;

let test_equals () =
  List.iter
    (to_list (get "equals"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let a = vertices_of_json (member "a" case) in
      let b = vertices_of_json (member "b" case) in
      let expected = bool_of_json_exn (member "equal" case) in
      let pa = S2.S2_polyline.of_vertices ~validate:false a in
      let pb = S2.S2_polyline.of_vertices ~validate:false b in
      let actual = S2.S2_polyline.equal pa pb in
      check_bool (name ^ " equal") ~expected ~actual)
;;

let test_validation () =
  List.iter
    (to_list (get "validation"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let vertices = vertices_of_json (member "vertices" case) in
      let expected = bool_of_json_exn (member "is_valid" case) in
      let pl = S2.S2_polyline.of_vertices ~validate:false vertices in
      let actual = S2.S2_polyline.is_valid pl in
      check_bool (name ^ " is_valid") ~expected ~actual)
;;

let test_shapes () =
  List.iter
    (to_list (get "shapes"))
    ~f:(fun case ->
      let name = string_of_json_exn (member "name" case) in
      let vertices = vertices_of_json (member "vertices" case) in
      let pl = S2.S2_polyline.of_vertices ~validate:false vertices in
      let expected_num_edges = int_of_json_exn (member "num_edges" case) in
      let expected_num_chains = int_of_json_exn (member "num_chains" case) in
      let expected_dim = int_of_json_exn (member "dimension" case) in
      let expected_is_empty = bool_of_json_exn (member "is_empty" case) in
      let expected_is_full = bool_of_json_exn (member "is_full" case) in
      let expected_type_tag = int_of_json_exn (member "type_tag" case) in
      (check int) (name ^ " num_edges") expected_num_edges (S2.S2_polyline.num_edges pl);
      (check int)
        (name ^ " num_chains")
        expected_num_chains
        (S2.S2_polyline.num_chains pl);
      (check int) (name ^ " dimension") expected_dim (S2.S2_polyline.dimension pl);
      (check int) (name ^ " type_tag") expected_type_tag S2.S2_polyline.type_tag;
      let generic = S2.S2_polyline.to_shape pl in
      (check int) (name ^ " shape num_edges") expected_num_edges generic.#num_edges;
      (check int) (name ^ " shape num_chains") expected_num_chains generic.#num_chains;
      (check int) (name ^ " shape dimension") expected_dim generic.#dimension;
      (check int) (name ^ " shape type_tag") expected_type_tag generic.#type_tag;
      let is_empty = S2.S2_shape.is_empty generic in
      let is_full = S2.S2_shape.is_full generic in
      check_bool (name ^ " is_empty") ~expected:expected_is_empty ~actual:is_empty;
      check_bool (name ^ " is_full") ~expected:expected_is_full ~actual:is_full;
      let rp_json = member "reference_point" case in
      let rp = S2.S2_polyline.reference_point pl in
      check_bool
        (name ^ " reference_point contained")
        ~expected:(bool_of_json_exn (member "contained" rp_json))
        ~actual:rp.#contained;
      let edges_json = to_list (member "edges" case) in
      List.iteri edges_json ~f:(fun i e_json ->
        let expected = edge_of_json e_json in
        check_edge
          (sprintf "%s edge[%d]" name i)
          ~expected
          ~actual:(S2.S2_polyline.edge pl i);
        check_edge
          (sprintf "%s shape edge[%d]" name i)
          ~expected
          ~actual:(generic.#edge i));
      let chains_json = to_list (member "chains" case) in
      List.iteri chains_json ~f:(fun i c_json ->
        let s, l = int_pair_of_json c_json in
        let expected = S2.S2_shape.Chain.create ~start:s ~length:l in
        let actual = S2.S2_polyline.chain pl i in
        check_bool
          (sprintf "%s chain[%d] equal" name i)
          ~expected:true
          ~actual:(S2.S2_shape.Chain.equal expected actual));
      let cp_json = to_list (member "chain_positions" case) in
      List.iteri cp_json ~f:(fun e cp ->
        let cid, off = int_pair_of_json cp in
        let expected = S2.S2_shape.Chain_position.create ~chain_id:cid ~offset:off in
        let actual = S2.S2_polyline.chain_position pl e in
        check_bool
          (sprintf "%s chain_position[%d] equal" name e)
          ~expected:true
          ~actual:(S2.S2_shape.Chain_position.equal expected actual));
      let ce_json = to_list (member "chain_edges" case) in
      let ce_idx = ref 0 in
      for i = 0 to expected_num_chains - 1 do
        let c = S2.S2_polyline.chain pl i in
        for j = 0 to c.#length - 1 do
          let expected = edge_of_json (List.nth_exn ce_json !ce_idx) in
          check_edge
            (sprintf "%s chain_edge[%d,%d]" name i j)
            ~expected
            ~actual:(S2.S2_polyline.chain_edge pl i j);
          incr ce_idx
        done
      done)
;;

let test_type_tag () =
  (check int) "type_tag" (int_of_json_exn (get "type_tag")) S2.S2_polyline.type_tag
;;

let () =
  Alcotest.run
    "S2_polyline"
    [ "polylines", [ test_case "all" `Quick test_polylines ]
    ; "reverse", [ test_case "all" `Quick test_reverse ]
    ; "interpolate", [ test_case "all" `Quick test_interpolate ]
    ; "uninterpolate", [ test_case "all" `Quick test_uninterpolate ]
    ; "project", [ test_case "all" `Quick test_project ]
    ; "is_on_right", [ test_case "all" `Quick test_is_on_right ]
    ; "intersects", [ test_case "all" `Quick test_intersects ]
    ; "may_intersect", [ test_case "all" `Quick test_may_intersect ]
    ; "subsample", [ test_case "all" `Quick test_subsample ]
    ; "approx_equal", [ test_case "all" `Quick test_approx_equal ]
    ; "equals", [ test_case "all" `Quick test_equals ]
    ; "validation", [ test_case "all" `Quick test_validation ]
    ; "shapes", [ test_case "all" `Quick test_shapes ]
    ; "type_tag", [ test_case "constant" `Quick test_type_tag ]
    ]
;;
