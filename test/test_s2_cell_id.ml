(* C++ test parity: s2geometry/src/s2/s2cell_id_test.cc
   -  TEST(S2CellId, FaceDefinitions) / FromFace       - faces
   -  TEST(S2CellId, ParentChildRelationships)         - hierarchy, parent_child
   -  TEST(S2CellId, CenterSiTi)                       - points
   -  TEST(S2CellId, Advance)                          - advance, advance_extended, advance_wrap_equiv
   -  TEST(S2CellId, SentinelRangeMinMax)              - sentinel_range
   -  TEST(S2CellId, Wrapping)                         - wrapping
   -  TEST(S2CellId, Tokens)                           - tokens, token_invalid
   -  TEST(S2CellId, Containment)                      - containment
   -  TEST(S2CellId, Continuity)                       - continuity_level8, continuity_geometry
   -  TEST(S2CellId, DistanceFromBegin)                - distance_from_begin
   -  TEST(S2CellId, GetCommonAncestorLevel)           - common_ancestor
   -  TEST(S2CellId, MaximumTile)                      - maximum_tile
   -  TEST(S2CellId, Coverage)                         - coverage_sample

   Generator-only (no TEST(...) macro in s2cell_id_test.cc):
   -  TEST(S2CellIdGolden, LatLngFace) - latlng_face (S2LatLng::FromDegrees + face/leaf)

   -  TEST(S2CellId, DefaultConstructor)                - default_constructor
   -  TEST(S2CellId, Inverses)                         - quickcheck: leaf cell -> latlng ->
      cell id roundtrip

   Deliberately omitted (no OCaml equivalent yet, or I/O-only in C++):
   -  S2CellIdHash, EncodeDecode*, LegacyCoder*,
      Neighbors*, ExpandedByDistanceUV, ToString, FromDebugString, OutputOperator,
      S2CoderWorks, AbslParseFlag*, SupportsAbslHash. *)

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

let test_default_constructor () =
  let id = S2.S2_cell_id.of_int64 #0L in
  assert (S2.S2_cell_id.equal id S2.S2_cell_id.none);
  assert (not (S2.S2_cell_id.is_valid id))
;;

(* TEST(S2CellId, Inverses) - leaf cell -> latlng -> cell id roundtrip *)
module Leaf_cell_id_int = struct
  type t = Int64.t [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let rec descend depth id =
      let cell = cid_of_int64 id in
      if S2.S2_cell_id.is_leaf cell
      then return id
      else if depth = 0
      then return (int64_of_cid (S2.S2_cell_id.child_exn cell 0))
      else
        bind (int_uniform_inclusive 0 3) ~f:(fun k ->
          descend (depth - 1) (int64_of_cid (S2.S2_cell_id.child_exn cell k)))
    in
    bind (int_uniform_inclusive 0 5) ~f:(fun f ->
      descend 30 (int64_of_cid (S2.S2_cell_id.from_face_exn f)))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let quickcheck_inverses () =
  Base_quickcheck.Test.run_exn
    (module Leaf_cell_id_int)
    ~config:{ qc_config with test_count = 1000 }
    ~f:(fun id ->
      let cell = cid_of_int64 id in
      assert (S2.S2_cell_id.is_leaf cell);
      assert (S2.S2_cell_id.level cell = S2.S2_cell_id.max_level);
      let center = S2.S2_latlng.of_point (S2.S2_cell_id.to_point cell) in
      let roundtripped = S2.S2_cell_id.from_latlng center in
      assert (S2.S2_cell_id.equal cell roundtripped))
;;

let quickcheck_token_roundtrip () =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let t = cid_of_int64 id in
    assert (S2.S2_cell_id.equal (S2.S2_cell_id.from_token (S2.S2_cell_id.to_token t)) t))
;;

let quickcheck_contains_immediate_children () =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let t = cid_of_int64 id in
    if S2.S2_cell_id.is_leaf t
    then ()
    else
      for k = 0 to 3 do
        assert (S2.S2_cell_id.contains t (S2.S2_cell_id.child_exn t k))
      done)
;;

let quickcheck_parent_of_child () =
  Base_quickcheck.Test.run_exn (module Cell_id_int) ~config:qc_config ~f:(fun id ->
    let t = cid_of_int64 id in
    if S2.S2_cell_id.is_leaf t
    then ()
    else
      for k = 0 to 3 do
        assert (
          S2.S2_cell_id.equal (S2.S2_cell_id.parent_exn (S2.S2_cell_id.child_exn t k)) t)
      done)
;;

(* C++ vs OCaml geometry can differ slightly; full Continuity walk (~400k steps)
   needs a margin above ~1e-9 on some cells. *)
let angle_eps = #5e-9

let uint64_ge (a : S2.S2_cell_id.t) (b : S2.S2_cell_id.t) =
  Stdlib_upstream_compatible.Int64_u.unsigned_compare
    (S2.S2_cell_id.id a)
    (S2.S2_cell_id.id b)
  >= 0
;;

let uint64_le (a : S2.S2_cell_id.t) (b : S2.S2_cell_id.t) =
  Stdlib_upstream_compatible.Int64_u.unsigned_compare
    (S2.S2_cell_id.id a)
    (S2.S2_cell_id.id b)
  <= 0
;;

let check_cell_props msg expected_json actual =
  let expected_id = s2_cell_id_of_json (member "id" expected_json) in
  check_cell_id (msg ^ " id") expected_id actual;
  (check int)
    (msg ^ " face")
    (int_of_json_exn (member "face" expected_json))
    (S2.S2_cell_id.face actual);
  (check int)
    (msg ^ " level")
    (int_of_json_exn (member "level" expected_json))
    (S2.S2_cell_id.level actual);
  (check bool)
    (msg ^ " is_valid")
    (bool_of_json_exn (member "is_valid" expected_json))
    (S2.S2_cell_id.is_valid actual);
  (check string)
    (msg ^ " token")
    (string_of_json_exn (member "token" expected_json))
    (S2.S2_cell_id.to_token actual)
;;

(** Matches [S2LatLng::FromDegrees] -> unit-sphere point used by [S2CellId(point)]. *)
let r3_from_lat_lng_deg ~lat_deg ~lng_deg =
  let open Float_u.O in
  let lat = lat_deg * Float_u.pi () / #180. in
  let lng = lng_deg * Float_u.pi () / #180. in
  let clat = Float_u.cos lat in
  S2.R3_vector.create
    ~x:(clat * Float_u.cos lng)
    ~y:(clat * Float_u.sin lng)
    ~z:(Float_u.sin lat)
;;

let test_faces fixture () =
  let cases = to_list (member "faces" fixture) in
  List.iter cases ~f:(fun c ->
    let face = int_of_json_exn (member "face" c) in
    let id = S2.S2_cell_id.from_face_exn face in
    check_cell_props (sprintf "face %d" face) c id)
;;

let test_hierarchy fixture () =
  let cases = to_list (member "hierarchy" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "hierarchy %d" i in
    let cell_json = member "cell" c in
    let id : S2.S2_cell_id.t = s2_cell_id_of_json (member "id" cell_json) in
    check_cell_props msg cell_json id;
    let expected_child0 = s2_cell_id_of_json (member "id" (member "child0" c)) in
    let expected_child1 = s2_cell_id_of_json (member "id" (member "child1" c)) in
    let expected_child2 = s2_cell_id_of_json (member "id" (member "child2" c)) in
    let expected_child3 = s2_cell_id_of_json (member "id" (member "child3" c)) in
    check_cell_id (msg ^ " child0") expected_child0 (S2.S2_cell_id.child_exn id 0);
    check_cell_id (msg ^ " child1") expected_child1 (S2.S2_cell_id.child_exn id 1);
    check_cell_id (msg ^ " child2") expected_child2 (S2.S2_cell_id.child_exn id 2);
    check_cell_id (msg ^ " child3") expected_child3 (S2.S2_cell_id.child_exn id 3);
    match member "parent" c with
    | `Null -> ()
    | parent_json ->
      let expected_parent = s2_cell_id_of_json (member "id" parent_json) in
      check_cell_id (msg ^ " parent") expected_parent (S2.S2_cell_id.parent_exn id);
      (check int)
        (msg ^ " child_pos")
        (int_of_json_exn (member "child_pos" c))
        (S2.S2_cell_id.child_position id))
;;

let test_points fixture () =
  let cases = to_list (member "points" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "points %d" i in
    let p = r3_vector_of_json (member "p" c) in
    let expected_id = s2_cell_id_of_json (member "id" c) in
    let actual_id = S2.S2_cell_id.from_point p in
    check_cell_id (msg ^ " id") expected_id actual_id;
    let expected_to_p = r3_vector_of_json (member "to_p" c) in
    let actual_to_p = S2.S2_cell_id.to_point actual_id in
    check_r3_vector (msg ^ " to_p") ~expected:expected_to_p ~actual:actual_to_p [@nontail])
;;

let test_advance fixture () =
  let cases = to_list (member "advance" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "advance %d" i in
    let id = s2_cell_id_of_json (member "id" c) in
    let steps = int64_u_of_json_exn (member "steps" c) in
    let expected_adv = s2_cell_id_of_json (member "advance" c) in
    let expected_adv_wrap = s2_cell_id_of_json (member "advance_wrap" c) in
    check_cell_id (msg ^ " advance") expected_adv (S2.S2_cell_id.advance id steps);
    check_cell_id
      (msg ^ " advance_wrap")
      expected_adv_wrap
      (S2.S2_cell_id.advance_wrap id steps))
;;

let test_parent_child fixture () =
  let pc = member "parent_child" fixture in
  let face = int_of_json_exn (member "construct_face" pc) in
  let pos = int64_u_of_json_exn (member "construct_pos" pc) in
  let level = int_of_json_exn (member "construct_level" pc) in
  let id = S2.S2_cell_id.from_face_pos_level face pos level in
  check_cell_id "parent_child id" (s2_cell_id_of_json (member "id" pc)) id;
  (check int)
    "parent_child face"
    (int_of_json_exn (member "face" pc))
    (S2.S2_cell_id.face id);
  (check bool)
    "parent_child is_leaf"
    (bool_of_json_exn (member "is_leaf" pc))
    (S2.S2_cell_id.is_leaf id);
  (check int)
    "parent_child level"
    (int_of_json_exn (member "level" pc))
    (S2.S2_cell_id.level id);
  let expected_pos = int64_u_of_json_exn (member "pos" pc) in
  check_bool
    "parent_child pos"
    ~expected:true
    ~actual:(Int64_u.equal expected_pos (S2.S2_cell_id.pos id));
  let l2 = S2.S2_cell_id.level id + 2 in
  let child_begin_l2 = S2.S2_cell_id.child_begin_at_level id l2 in
  check_bool
    "parent_child child_begin_l2_pos"
    ~expected:true
    ~actual:
      (Int64_u.equal
         (int64_u_of_json_exn (member "child_begin_l2_pos" pc))
         (S2.S2_cell_id.pos child_begin_l2));
  let child_begin_immediate = S2.S2_cell_id.child_begin id in
  check_bool
    "parent_child child_begin_pos"
    ~expected:true
    ~actual:
      (Int64_u.equal
         (int64_u_of_json_exn (member "child_begin_pos" pc))
         (S2.S2_cell_id.pos child_begin_immediate));
  check_bool
    "parent_child parent_pos"
    ~expected:true
    ~actual:
      (Int64_u.equal
         (int64_u_of_json_exn (member "parent_pos" pc))
         (S2.S2_cell_id.pos (S2.S2_cell_id.parent_exn id)));
  let pll = S2.S2_cell_id.level id - 2 in
  check_bool
    "parent_child parent_level_minus_2_pos"
    ~expected:true
    ~actual:
      (Int64_u.equal
         (int64_u_of_json_exn (member "parent_level_minus_2_pos" pc))
         (S2.S2_cell_id.pos (S2.S2_cell_id.parent_level id pll)));
  check_cell_id
    "parent_child range_min"
    (s2_cell_id_of_json (member "range_min" pc))
    (S2.S2_cell_id.range_min id);
  check_cell_id
    "parent_child range_max"
    (s2_cell_id_of_json (member "range_max" pc))
    (S2.S2_cell_id.range_max id);
  check_cell_id
    "parent_child child_end_max"
    (s2_cell_id_of_json (member "child_end_max" pc))
    (S2.S2_cell_id.child_end_at_level id S2.S2_cell_id.max_level);
  let id64 = S2.S2_cell_id.id id in
  let lo = S2.S2_cell_id.id (S2.S2_cell_id.range_min id) in
  let hi = S2.S2_cell_id.id (S2.S2_cell_id.range_max id) in
  (check bool)
    "parent_child hilbert_midpoint"
    (bool_of_json_exn (member "hilbert_midpoint" pc))
    Int64_u.O.(id64 + id64 = lo + hi);
  let next_child_begin_max =
    S2.S2_cell_id.advance
      (S2.S2_cell_id.child_begin_at_level (S2.S2_cell_id.next id) S2.S2_cell_id.max_level)
      #256L
  in
  check_cell_id
    "parent_child next_child_begin_max"
    (s2_cell_id_of_json (member "next_child_begin_max" pc))
    next_child_begin_max;
  let child_begin_max_adv =
    S2.S2_cell_id.advance
      (S2.S2_cell_id.child_begin_at_level id S2.S2_cell_id.max_level)
      #256L
  in
  check_cell_id
    "parent_child child_begin_max_adv"
    (s2_cell_id_of_json (member "child_begin_max_adv" pc))
    child_begin_max_adv;
  let expected_raw = r3_vector_of_json (member "to_point_raw" pc) in
  let actual_raw = S2.S2_cell_id.to_point_raw id in
  check_r3_vector "parent_child to_point_raw" ~expected:expected_raw ~actual:actual_raw;
  let expected_point = r3_vector_of_json (member "to_point" pc) in
  let actual_point = S2.S2_cell_id.to_point id in
  check_r3_vector
    "parent_child to_point"
    ~expected:expected_point
    ~actual:actual_point [@nontail]
;;

let test_sentinel_range fixture () =
  let sr = member "sentinel_range" fixture in
  let s = S2.S2_cell_id.sentinel in
  check_cell_id
    "sentinel range_min"
    (s2_cell_id_of_json (member "min" sr))
    (S2.S2_cell_id.range_min s);
  check_cell_id
    "sentinel range_max"
    (s2_cell_id_of_json (member "max" sr))
    (S2.S2_cell_id.range_max s)
;;

let test_wrapping fixture () =
  let cases = to_list (member "wrapping" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "wrapping %d (%s)" i (string_of_json_exn (member "name" c)) in
    let name = string_of_json_exn (member "name" c) in
    let expected = s2_cell_id_of_json (member "a" c) in
    let actual =
      if String.equal name "End0_prev_vs_Begin0_prev_wrap"
      then S2.S2_cell_id.prev_wrap (S2.S2_cell_id.hilbert_begin 0)
      else if String.equal name "BeginMax_prev_wrap_vs_corner_cell"
      then S2.S2_cell_id.prev_wrap (S2.S2_cell_id.hilbert_begin S2.S2_cell_id.max_level)
      else if String.equal name "BeginMax_advance_wrap_m1_vs_prev_wrap"
      then
        S2.S2_cell_id.advance_wrap
          (S2.S2_cell_id.hilbert_begin S2.S2_cell_id.max_level)
          (-#1L)
      else if String.equal name "End4_prev_next_wrap_vs_Begin4"
      then S2.S2_cell_id.next_wrap (S2.S2_cell_id.prev (S2.S2_cell_id.hilbert_end 4))
      else if String.equal name "EndMax_prev_next_wrap_vs_face0_leaf_min"
      then
        S2.S2_cell_id.next_wrap
          (S2.S2_cell_id.prev (S2.S2_cell_id.hilbert_end S2.S2_cell_id.max_level))
      else S2.S2_cell_id.none
    in
    check_cell_id msg expected actual)
;;

let test_advance_extended fixture () =
  let cases = to_list (member "advance_extended" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg =
      sprintf "advance_extended %d (%s)" i (string_of_json_exn (member "name" c))
    in
    let id = s2_cell_id_of_json (member "from_id" c) in
    let steps_j = member "steps" c in
    let steps_wrap_j = member "steps_wrap" c in
    (match steps_j with
     | `Null -> ()
     | j ->
       check_cell_id
         msg
         (s2_cell_id_of_json (member "expected" c))
         (S2.S2_cell_id.advance id (int64_u_of_json_exn j)));
    (match steps_wrap_j with
     | `Null -> ()
     | j ->
       check_cell_id
         (msg ^ " wrap")
         (s2_cell_id_of_json (member "expected_wrap" c))
         (S2.S2_cell_id.advance_wrap id (int64_u_of_json_exn j)));
    match steps_j, steps_wrap_j with
    | `Null, `Null ->
      let name = string_of_json_exn (member "name" c) in
      let expected = s2_cell_id_of_json (member "expected" c) in
      if String.equal name "next_child_begin_max_adv256"
      then
        check_cell_id
          msg
          expected
          (S2.S2_cell_id.advance
             (S2.S2_cell_id.child_begin_at_level
                (S2.S2_cell_id.next id)
                S2.S2_cell_id.max_level)
             #256L)
      else if String.equal name "child_begin_max_adv256"
      then
        check_cell_id
          msg
          expected
          (S2.S2_cell_id.advance
             (S2.S2_cell_id.child_begin_at_level id S2.S2_cell_id.max_level)
             #256L)
      else ()
    | _ -> ())
;;

let test_latlng_face fixture () =
  let cases = to_list (member "latlng_face" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "latlng_face %d" i in
    let lat = float_u_of_json_exn (member "lat" c) in
    let lng = float_u_of_json_exn (member "lng" c) in
    let p = r3_from_lat_lng_deg ~lat_deg:lat ~lng_deg:lng in
    let id = S2.S2_cell_id.from_point p in
    (check int)
      (msg ^ " face")
      (int_of_json_exn (member "face" c))
      (S2.S2_cell_id.face id);
    check_cell_id (msg ^ " leaf_id") (s2_cell_id_of_json (member "leaf_id" c)) id)
;;

let test_tokens fixture () =
  let cases = to_list (member "tokens" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "tokens %d" i in
    let raw_id = s2_cell_id_of_json (member "id" c) in
    (check string)
      (msg ^ " to_token")
      (string_of_json_exn (member "token" c))
      (S2.S2_cell_id.to_token raw_id);
    check_bool
      (msg ^ " from_token round_trip")
      ~expected:true
      ~actual:
        (S2.S2_cell_id.equal
           (S2.S2_cell_id.from_token (string_of_json_exn (member "token" c)))
           raw_id))
;;

let test_token_invalid fixture () =
  let cases = to_list (member "token_invalid" fixture) in
  List.iteri cases ~f:(fun i j ->
    let s =
      match j with
      | `String s -> s
      | _ -> failwith "token_invalid entry must be string"
    in
    check_bool
      (sprintf "token_invalid %d" i)
      ~expected:true
      ~actual:(S2.S2_cell_id.equal (S2.S2_cell_id.from_token s) S2.S2_cell_id.none))
;;

let test_containment fixture () =
  let cases = to_list (member "containment" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "containment %d" i in
    let end_id = s2_cell_id_of_json (member "end" c) in
    let begin_id = s2_cell_id_of_json (member "begin" c) in
    check_bool
      (msg ^ " contains")
      ~expected:(bool_of_json_exn (member "contains" c))
      ~actual:(S2.S2_cell_id.contains end_id begin_id);
    check_bool
      (msg ^ " contains_by_range")
      ~expected:(bool_of_json_exn (member "contains_by_range" c))
      ~actual:
        (uint64_ge begin_id (S2.S2_cell_id.range_min end_id)
         && uint64_le begin_id (S2.S2_cell_id.range_max end_id));
    check_bool
      (msg ^ " intersects")
      ~expected:(bool_of_json_exn (member "intersects" c))
      ~actual:(S2.S2_cell_id.intersects end_id begin_id);
    check_bool
      (msg ^ " intersects_expected")
      ~expected:(bool_of_json_exn (member "intersects_expected" c))
      ~actual:
        (S2.S2_cell_id.contains end_id begin_id || S2.S2_cell_id.contains begin_id end_id))
;;

let test_continuity_level8 fixture () =
  let cases = to_list (member "continuity_level8" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "continuity_level8 %d" i in
    let id = s2_cell_id_of_json (member "id" c) in
    check_cell_id
      (msg ^ " next")
      (s2_cell_id_of_json (member "next" c))
      (S2.S2_cell_id.next id);
    check_cell_id
      (msg ^ " next_wrap")
      (s2_cell_id_of_json (member "next_wrap" c))
      (S2.S2_cell_id.next_wrap id);
    check_cell_id
      (msg ^ " advance_wrap_1")
      (s2_cell_id_of_json (member "advance_wrap_1" c))
      (S2.S2_cell_id.advance_wrap id #1L))
;;

let test_distance_from_begin fixture () =
  let cases = to_list (member "distance_from_begin" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "distance_from_begin %d" i in
    let id = s2_cell_id_of_json (member "id" c) in
    let dist = int64_u_of_json_exn (member "distance" c) in
    check_bool
      (msg ^ " distance")
      ~expected:true
      ~actual:(Int64_u.equal dist (S2.S2_cell_id.distance_from_begin id));
    match member "roundtrip" c with
    | `Bool true ->
      let lvl = S2.S2_cell_id.level id in
      let from_begin = S2.S2_cell_id.hilbert_begin lvl in
      check_cell_id
        (msg ^ " advance(begin(level), dist)")
        id
        (S2.S2_cell_id.advance from_begin dist)
    | _ -> ())
;;

let test_common_ancestor fixture () =
  let cases = to_list (member "common_ancestor" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "common_ancestor %d" i in
    let a = s2_cell_id_of_json (member "a" c) in
    let b = s2_cell_id_of_json (member "b" c) in
    (check int)
      (msg ^ " level")
      (int_of_json_exn (member "level" c))
      (S2.S2_cell_id.get_common_ancestor_level a b))
;;

let test_maximum_tile fixture () =
  let cases = to_list (member "maximum_tile" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "maximum_tile %d" i in
    let tile = s2_cell_id_of_json (member "tile" c) in
    let limit = s2_cell_id_of_json (member "limit" c) in
    let expect = s2_cell_id_of_json (member "expect" c) in
    check_cell_id msg expect (S2.S2_cell_id.maximum_tile tile limit))
;;

let test_advance_wrap_equiv fixture () =
  let j = member "advance_wrap_equiv" fixture in
  let b = S2.S2_cell_id.hilbert_begin 5 in
  let a = S2.S2_cell_id.advance_wrap b #6644L in
  let c = S2.S2_cell_id.advance_wrap b (-#11788L) in
  check_bool
    "advance_wrap_equiv equal"
    ~expected:(bool_of_json_exn (member "eq" j))
    ~actual:(S2.S2_cell_id.equal a c);
  check_cell_id "advance_wrap_equiv id" (s2_cell_id_of_json (member "id" j)) a
;;

let test_continuity_geometry fixture () =
  let cases = to_list (member "continuity_geometry" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "continuity_geometry %d" i in
    let id = s2_cell_id_of_json (member "id" c) in
    let next_w = S2.S2_cell_id.next_wrap id in
    let p = S2.S2_cell_id.to_point_raw id in
    let q = S2.S2_cell_id.to_point_raw next_w in
    let ang = S2.S1_angle.radians (S2.R3_vector.angle p q) in
    let expected_ang = float_u_of_json_exn (member "angle" c) in
    let diff = Float_u.abs Float_u.O.(expected_ang - ang) in
    if Float_u.O.(diff > angle_eps)
    then
      Alcotest.failf
        "%s: angle expected %.17g, got %.17g (diff %g)"
        msg
        (Float_u.to_float expected_ang)
        (Float_u.to_float ang)
        (Float_u.to_float diff);
    let max_ang = float_u_of_json_exn (member "max_angle" c) in
    if Float_u.O.(ang > max_ang)
    then
      Alcotest.failf
        "%s: angle %g exceeds C++ kMaxEdge bound %g"
        msg
        (Float_u.to_float ang)
        (Float_u.to_float max_ang))
;;

let test_coverage_sample fixture () =
  let cases = to_list (member "coverage_sample" fixture) in
  List.iteri cases ~f:(fun i c ->
    let msg = sprintf "coverage_sample %d" i in
    let p = r3_vector_of_json (member "p" c) in
    let cell = S2.S2_cell_id.from_point p in
    let q = S2.S2_cell_id.to_point_raw cell in
    let ang = S2.S1_angle.radians (S2.R3_vector.angle p q) in
    let expected_ang = float_u_of_json_exn (member "angle" c) in
    let diff = Float_u.abs Float_u.O.(expected_ang - ang) in
    if Float_u.O.(diff > angle_eps)
    then
      Alcotest.failf
        "%s: angle expected %.17g, got %.17g (diff %g)"
        msg
        (Float_u.to_float expected_ang)
        (Float_u.to_float ang)
        (Float_u.to_float diff);
    let max_ang = float_u_of_json_exn (member "max_angle" c) in
    if Float_u.O.(ang > max_ang)
    then
      Alcotest.failf
        "%s: angle %g exceeds C++ kMaxDiag bound %g"
        msg
        (Float_u.to_float ang)
        (Float_u.to_float max_ang))
;;

let () =
  let fixture = load_fixture "s2cellid.json" in
  Alcotest.run
    "S2_cell_id"
    [ "faces", [ test_case "FaceDefinitions" `Quick (test_faces fixture) ]
    ; ( "hierarchy"
      , [ test_case "ParentChildRelationships" `Quick (test_hierarchy fixture) ] )
    ; "points", [ test_case "CenterSiTi" `Quick (test_points fixture) ]
    ; "advance", [ test_case "Advance" `Quick (test_advance fixture) ]
    ; ( "parent_child"
      , [ test_case "ParentChildRelationships" `Quick (test_parent_child fixture) ] )
    ; ( "sentinel_range"
      , [ test_case "SentinelRangeMinMax" `Quick (test_sentinel_range fixture) ] )
    ; "wrapping", [ test_case "Wrapping" `Quick (test_wrapping fixture) ]
    ; ( "advance_extended"
      , [ test_case "AdvanceExtended" `Quick (test_advance_extended fixture) ] )
    ; ( "advance_wrap_equiv"
      , [ test_case "AdvanceWrapEquiv" `Quick (test_advance_wrap_equiv fixture) ] )
    ; "latlng_face", [ test_case "LatLngFace" `Quick (test_latlng_face fixture) ]
    ; "tokens", [ test_case "Tokens" `Quick (test_tokens fixture) ]
    ; ( "token_invalid"
      , [ test_case "LegacyCoderTokenInvalid" `Quick (test_token_invalid fixture) ] )
    ; "containment", [ test_case "Containment" `Quick (test_containment fixture) ]
    ; ( "continuity_level8"
      , [ test_case "ContinuityLevel8" `Quick (test_continuity_level8 fixture) ] )
    ; ( "continuity_geometry"
      , [ test_case "ContinuityGeometry" `Quick (test_continuity_geometry fixture) ] )
    ; ( "distance_from_begin"
      , [ test_case "DistanceFromBegin" `Quick (test_distance_from_begin fixture) ] )
    ; ( "common_ancestor"
      , [ test_case "GetCommonAncestorLevel" `Quick (test_common_ancestor fixture) ] )
    ; "maximum_tile", [ test_case "MaximumTile" `Quick (test_maximum_tile fixture) ]
    ; "coverage_sample", [ test_case "Coverage" `Quick (test_coverage_sample fixture) ]
    ; ( "default_constructor"
      , [ test_case "DefaultConstructor" `Quick test_default_constructor ] )
    ; ( "quickcheck"
      , [ test_case "token_roundtrip" `Quick quickcheck_token_roundtrip
        ; test_case
            "contains_immediate_children"
            `Quick
            quickcheck_contains_immediate_children
        ; test_case "parent_of_child" `Quick quickcheck_parent_of_child
        ; test_case "inverses" `Quick quickcheck_inverses
        ] )
    ]
;;
