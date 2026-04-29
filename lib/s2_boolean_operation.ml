open Core

(* Implementation note: this port replaces the C++ CrossingProcessor state machine
   (s2boolean_operation.cc:1095-2009, ~1,500 lines) with a much simpler per-edge
   containment test that works for the polygon-polygon SEMI_OPEN predicate case only.
   Completing parity with the reference library means porting CrossingProcessor; the
   features that would unlock and their C++ entry points are:

   {ul
    {- Set-operation output via an S2Builder layer: BuildOpType (cc:2282), AddBoundary
       (cc:2028), ProcessEdge / ProcessEdge

      ,1,2

      dispatching on a_dimension_ (cc:1416, 1449, 1546, 1678), EdgeClippingLayer (cc:730),
      AddEdge / AddPointEdge (cc:1215-1264).
   }
    {- Polyline inputs: ProcessEdge1 (cc:1546), IsPolylineVertexInside /
       IsPolylineEdgeInside (cc:1616, 1636).
   }
    {- Point inputs: ProcessEdge0 (cc:1449), ProcessPointCrossings (cc:1485). }
    {- Open / Closed polygon boundary models: the per-branch model checks in ProcessEdge2
       (cc:1771-1823) and the degenerate-point paths in ProcessEdge0 (cc:1461).
   }
    {- Open / SEMI_OPEN / Closed polyline boundary models: PolylineModel plumbing in
       StartBoundary (cc:1383) and ProcessEdge1.
   }
    {- Mixed-dimension operands: the dimension dispatch in ProcessEdge plus
       IsPolylineEdgeInside for polyline-in-polygon tests.
   }
    {- Two-pass sibling-pair handling for shared-edge degeneracies: emit_shared (cc:1712)
       and is_degenerate_hole_ (cc:1741).
   }
    {- Early-exit optimisation via ProcessIncidentEdges (cc:2143). }
    {- AreRegionsIdentical fast path (cc:2509) for DIFFERENCE and SYMMETRIC_DIFFERENCE. }
   }

   Separately, is_full_polygon_result below uses a coarse face-mask + area heuristic; the
   symmetric-difference branch's hemisphere tolerance (cc:2476-2505) is approximated
   conservatively. Porting that heuristic accurately requires the snap-radius plumbing
   tracked in s2_builder. *)

module Op_type = struct
  type t =
    | Union
    | Intersection
    | Difference
    | Symmetric_difference
  [@@deriving sexp_of, equal]
end

module Polygon_model = struct
  type t =
    | Open
    | Semi_open
    | Closed
  [@@deriving sexp_of, equal]

  let default = Semi_open
end

module Polygon_input = struct
  type t =
    { shape : S2_shape.t
    ; shape_index : S2_shape_index.t
    ; shape_id : int
    ; area : float#
    ; is_empty : bool
    ; is_full : bool
    }

  let create ~shape ~shape_index ~shape_id ~area ~is_empty ~is_full =
    { shape; shape_index; shape_id; area; is_empty; is_full }
  ;;
end

(* --- Directed edge membership ------------------------------------------- *)

(* Encode a directed edge as three int64 bit patterns so it can key an OCaml [Hashtbl]
   (S2_point.t is an unboxed product and can't key polymorphic hashes directly). *)

type edge_key =
  { x0 : int64
  ; y0 : int64
  ; z0 : int64
  ; x1 : int64
  ; y1 : int64
  ; z1 : int64
  }
[@@deriving compare, equal, hash, sexp]

let edge_key_of (v0 : S2_point.t) (v1 : S2_point.t) =
  let bits f = Int64.bits_of_float (Float_u.to_float f) in
  { x0 = bits (S2_point.x v0)
  ; y0 = bits (S2_point.y v0)
  ; z0 = bits (S2_point.z v0)
  ; x1 = bits (S2_point.x v1)
  ; y1 = bits (S2_point.y v1)
  ; z1 = bits (S2_point.z v1)
  }
;;

module Edge_table = Hashtbl.Make (struct
    type t = edge_key [@@deriving compare, hash, sexp]
  end)

(* Build a hashtable of the directed boundary edges of a polygon shape. The shape is
   assumed to expose oriented edges (interior on the left). *)
let build_edge_table (shape : S2_shape.t) =
  let t = Edge_table.create () in
  let n = shape.#num_edges in
  for e = 0 to n - 1 do
    let ed : S2_shape.Edge.t = shape.#edge e in
    if not (S2_point.equal ed.#v0 ed.#v1)
    then Hashtbl.set t ~key:(edge_key_of ed.#v0 ed.#v1) ~data:()
  done;
  t
;;

(* Iterate directed boundary edges, stopping early when [f] returns false. Returns [true]
   iff iteration ran to completion. *)
let iter_boundary_edges (shape : S2_shape.t) ~f =
  let n = shape.#num_edges in
  let keep_going = ref true in
  let e = ref 0 in
  while !keep_going && !e < n do
    let ed : S2_shape.Edge.t = shape.#edge !e in
    if not (S2_point.equal ed.#v0 ed.#v1)
    then if not (f ed.#v0 ed.#v1) then keep_going := false;
    incr e
  done;
  !keep_going
;;

(* Does polygon [p] contain the directed edge (v0, v1) under SEMI_OPEN?
   - If (v0, v1) is an oriented edge of [p]: yes.
   - If (v1, v0) is an oriented edge of [p]: no (the reversed edge belongs to the
     neighbouring polygon).
   - Otherwise: the edge is strictly interior or strictly exterior; test the midpoint with
     [S2_contains_point_query]. *)
let polygon_contains_directed_edge ~edges ~pq ~shape_id v0 v1 =
  if S2_point.equal v0 v1
  then S2_contains_point_query.shape_contains pq ~shape_id v0
  else if Hashtbl.mem edges (edge_key_of v0 v1)
  then true
  else if Hashtbl.mem edges (edge_key_of v1 v0)
  then false
  else (
    let mid = S2_edge_distances.interpolate v0 v1 #0.5 in
    S2_contains_point_query.shape_contains pq ~shape_id mid)
;;

(* --- Crossing detection ------------------------------------------------- *)

(* True iff any edge of [a_shape] has an interior crossing with any edge in [b_index]. Any
   interior crossing is a proof of a non-empty result for any of the four boolean ops. *)
let has_interior_crossing (a_shape : S2_shape.t) b_index =
  let ceq = S2_crossing_edge_query.create b_index in
  let found = ref false in
  let _ =
    iter_boundary_edges a_shape ~f:(fun v0 v1 ->
      if !found
      then false
      else (
        let crossings =
          S2_crossing_edge_query.get_crossing_edges
            ceq
            ~a:v0
            ~b:v1
            ~crossing_type:S2_crossing_edge_query.Crossing_type.Interior
        in
        if Array.length crossings = 0
        then true
        else (
          found := true;
          false)))
  in
  !found
;;

(* --- Face mask for IsFullPolygonResult ---------------------------------- *)

let k_all_faces_mask = 0x3F

(* Returns the set of S2 cube faces touched by the shape index. *)
let face_mask (index : S2_shape_index.t) =
  let it = S2_shape_index.iterator index in
  let mask = ref 0 in
  S2_shape_index.Iterator.begin_at it;
  while not (S2_shape_index.Iterator.is_done it) do
    let cid = S2_shape_index.Iterator.cell_id it in
    let face = S2_cell_id.face cid in
    mask := !mask lor (1 lsl face);
    (* Seek to the first cell id on the next face. *)
    if face < 5
    then S2_shape_index.Iterator.seek it (S2_cell_id.from_face_exn (face + 1))
    else S2_shape_index.Iterator.at_end it
  done;
  !mask
;;

(* --- IsFullPolygonResult for each op type ------------------------------- *)

let four_pi_f = Float.O.( * ) 4.0 Float.pi

let is_full_polygon_union (a : Polygon_input.t) (b : Polygon_input.t) =
  if face_mask a.shape_index lor face_mask b.shape_index <> k_all_faces_mask
  then false
  else (
    let a_area = Float_u.to_float a.area in
    let b_area = Float_u.to_float b.area in
    let min_area = Float.max a_area b_area in
    let max_area = Float.min four_pi_f (a_area +. b_area) in
    Float.O.(min_area > four_pi_f -. max_area))
;;

let is_full_polygon_intersection (a : Polygon_input.t) (b : Polygon_input.t) =
  if face_mask a.shape_index land face_mask b.shape_index <> k_all_faces_mask
  then false
  else (
    let a_area = Float_u.to_float a.area in
    let b_area = Float_u.to_float b.area in
    let min_area = Float.max 0.0 (a_area +. b_area -. four_pi_f) in
    let max_area = Float.min a_area b_area in
    Float.O.(min_area > four_pi_f -. max_area))
;;

let is_full_polygon_difference (a : Polygon_input.t) (b : Polygon_input.t) =
  if face_mask a.shape_index <> k_all_faces_mask
  then false
  else (
    let a_area = Float_u.to_float a.area in
    let b_area = Float_u.to_float b.area in
    let min_area = Float.max 0.0 (a_area -. b_area) in
    let max_area = Float.min a_area (four_pi_f -. b_area) in
    Float.O.(min_area > four_pi_f -. max_area))
;;

let is_full_polygon_symmetric_difference (a : Polygon_input.t) (b : Polygon_input.t) =
  if face_mask a.shape_index lor face_mask b.shape_index <> k_all_faces_mask
  then false
  else (
    let a_area = Float_u.to_float a.area in
    let b_area = Float_u.to_float b.area in
    let diff = Float.abs (a_area -. b_area) in
    let sum = a_area +. b_area in
    let min_area = Float.max diff (Float.max 0.0 (sum -. four_pi_f)) in
    let max_area = Float.min sum (four_pi_f -. diff) in
    (* When the sym-diff area brackets 2pi we'd need the snap-tolerance hemisphere
       heuristic (s2boolean_operation.cc:2476-2505). For the simplified port we
       conservatively return false in the ambiguous range. *)
    Float.O.(min_area > four_pi_f -. max_area))
;;

let is_full_polygon_result op_type a b =
  match (op_type : Op_type.t) with
  | Union -> is_full_polygon_union a b
  | Intersection -> is_full_polygon_intersection a b
  | Difference -> is_full_polygon_difference a b
  | Symmetric_difference -> is_full_polygon_symmetric_difference a b
;;

(* --- Edge-level predicates --------------------------------------------- *)

let any_edge_not_inside ~source ~target =
  let target_edges = build_edge_table target.Polygon_input.shape in
  let target_pq = S2_contains_point_query.create target.shape_index () in
  let found = ref false in
  let _ =
    iter_boundary_edges source.Polygon_input.shape ~f:(fun v0 v1 ->
      if !found
      then false
      else if not
                (polygon_contains_directed_edge
                   ~edges:target_edges
                   ~pq:target_pq
                   ~shape_id:target.shape_id
                   v0
                   v1)
      then (
        found := true;
        false)
      else true)
  in
  !found
;;

let any_edge_inside ~source ~target =
  let target_edges = build_edge_table target.Polygon_input.shape in
  let target_pq = S2_contains_point_query.create target.shape_index () in
  let found = ref false in
  let _ =
    iter_boundary_edges source.Polygon_input.shape ~f:(fun v0 v1 ->
      if !found
      then false
      else if polygon_contains_directed_edge
                ~edges:target_edges
                ~pq:target_pq
                ~shape_id:target.shape_id
                v0
                v1
      then (
        found := true;
        false)
      else true)
  in
  !found
;;

(* Identical boundary check: same directed edges, same multiplicity. *)
let polygons_have_identical_boundaries (a : Polygon_input.t) (b : Polygon_input.t) =
  if a.shape.#num_edges <> b.shape.#num_edges
  then false
  else (
    let ea = build_edge_table a.shape in
    let eb = build_edge_table b.shape in
    Hashtbl.length ea = Hashtbl.length eb
    && Hashtbl.fold ea ~init:true ~f:(fun ~key ~data:() acc -> acc && Hashtbl.mem eb key))
;;

(* --- Core predicate implementation ------------------------------------- *)

let raw_is_empty op_type (a : Polygon_input.t) (b : Polygon_input.t) =
  let open Op_type in
  match op_type with
  | Union -> a.is_empty && b.is_empty
  | Intersection ->
    if a.is_empty || b.is_empty
    then true
    else if a.is_full
    then b.is_empty
    else if b.is_full
    then a.is_empty
    else if has_interior_crossing a.shape b.shape_index
    then false
    else (
      let a_has_edge_in_b = any_edge_inside ~source:a ~target:b in
      let b_has_edge_in_a = any_edge_inside ~source:b ~target:a in
      not (a_has_edge_in_b || b_has_edge_in_a))
  | Difference ->
    (* DIFFERENCE(a, b) = a - b, empty iff a ⊆ b. Note that [Contains] swaps arguments so
       [is_empty Difference b a] evaluates "b - a is empty" = "a contains b". *)
    if a.is_empty
    then true
    else if b.is_full
    then true
    else if a.is_full
    then false
    else if b.is_empty
    then false
    else if has_interior_crossing a.shape b.shape_index
    then false
    else not (any_edge_not_inside ~source:a ~target:b)
  | Symmetric_difference ->
    if polygons_have_identical_boundaries a b
    then true
    else if a.is_empty
    then b.is_empty
    else if b.is_empty
    then false
    else if has_interior_crossing a.shape b.shape_index
    then false
    else
      (* A xor B is empty iff A and B have the same boundary as multisets. *)
      not
        (any_edge_not_inside ~source:a ~target:b
         || any_edge_not_inside ~source:b ~target:a)
;;

let is_empty ?(polygon_model = Polygon_model.default) op_type a b =
  (match polygon_model with
   | Polygon_model.Semi_open -> ()
   | Open | Closed ->
     raise_s
       [%message
         "S2_boolean_operation: only Polygon_model.Semi_open is implemented"
           (polygon_model : Polygon_model.t)]);
  if not (raw_is_empty op_type a b)
  then false
  else not (is_full_polygon_result op_type a b)
;;

let intersects ?polygon_model a b = not (is_empty ?polygon_model Op_type.Intersection a b)
let contains ?polygon_model a b = is_empty ?polygon_model Op_type.Difference b a
let equals ?polygon_model a b = is_empty ?polygon_model Op_type.Symmetric_difference a b
