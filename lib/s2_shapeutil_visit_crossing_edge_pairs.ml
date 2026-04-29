open Core
module Crossing_type = S2_crossing_edge_query.Crossing_type

type visitor = S2_shapeutil_shape_edge.t -> S2_shapeutil_shape_edge.t -> bool -> bool

(* Sentinel used to grow / pre-fill arrays of [S2_shapeutil_shape_edge.t]. The
   id is set to the canonical "none" pair and the edge endpoints are the
   origin point. The sentinel is never read back: writes always overwrite
   the slot before any read. *)
let none_shape_edge =
  let edge = S2_shape.Edge.create ~v0:S2_point.origin ~v1:S2_point.origin in
  S2_shapeutil_shape_edge.create ~shape_id:(-1) ~edge_id:(-1) ~edge
;;

(* Growable buffer of [S2_shapeutil_shape_edge.t]: lists cannot hold non-value
   elements, so accumulating cell edges needs a small array that doubles when
   full. *)
type edge_buf =
  { mutable data : S2_shapeutil_shape_edge.t array
  ; mutable len : int
  }

let edge_buf_create () = { data = Array.create ~len:16 none_shape_edge; len = 0 }
let edge_buf_clear buf = buf.len <- 0

let[@inline] edge_buf_push buf e =
  let cap = Array.length buf.data in
  if buf.len >= cap
  then (
    let new_data = Array.create ~len:(cap * 2) none_shape_edge in
    for i = 0 to buf.len - 1 do
      new_data.(i) <- buf.data.(i)
    done;
    buf.data <- new_data);
  buf.data.(buf.len) <- e;
  buf.len <- buf.len + 1
;;

(* Append every edge of [cell] (across all clipped shapes) to [buf]. *)
let append_cell_edges buf index cell =
  let num_clipped = S2_shape_index.Index_cell.num_clipped cell in
  for s = 0 to num_clipped - 1 do
    let clipped = S2_shape_index.Index_cell.clipped cell s in
    let shape_id = S2_shape_index.Clipped_shape.shape_id clipped in
    let shape = S2_shape_index.shape index shape_id in
    let n = S2_shape_index.Clipped_shape.num_edges clipped in
    for i = 0 to n - 1 do
      let edge_id = S2_shape_index.Clipped_shape.edge clipped i in
      let edge = shape.#edge edge_id in
      edge_buf_push buf (S2_shapeutil_shape_edge.create ~shape_id ~edge_id ~edge)
    done
  done
;;

let min_sign_of (crossing_type : Crossing_type.t) =
  match crossing_type with
  | All -> 0
  | Interior -> 1
;;

(* Visit all crossings within a single buffer of edges. Returns [false] if
   the visitor terminated early. *)
let visit_crossings_within buf ~min_sign ~need_adjacent ~visitor =
  let n = buf.len in
  let mutable cont = true in
  let mutable i = 0 in
  while cont && i + 1 < n do
    let a = buf.data.(i) in
    let mutable j = i + 1 in
    (* When [need_adjacent] is false, skip a single (a.v1 == b.v0) "adjacent"
       neighbour. The crossing search for self-intersection only cares about
       (AB, AC) pairs, not (AB, BC) pairs, so this skip removes the very
       common chain-step pair without missing any real error. The skip
       applies even across chain boundaries since the buffer is sorted by
       (shape_id, edge_id) and adjacency is purely positional. *)
    if (not need_adjacent) && S2_point.equal a.#edge.#v1 buf.data.(j).#edge.#v0
    then j <- j + 1;
    if j >= n
    then i <- n
    else (
      let mutable crosser = S2_edge_crosser.create ~a:a.#edge.#v0 ~b:a.#edge.#v1 in
      while cont && j < n do
        let b = buf.data.(j) in
        let #{ state; sign } : S2_edge_crosser.with_sign =
          S2_edge_crosser.crossing_sign crosser b.#edge.#v0 b.#edge.#v1
        in
        crosser <- state;
        if sign >= min_sign && not (visitor a b (sign = 1)) then cont <- false;
        j <- j + 1
      done;
      i <- i + 1)
  done;
  cont
;;

(* Internal entry point for the single-index walk; exposed so
   [find_self_intersection] can ask for [need_adjacent = false]. *)
let visit_crossings_internal index ~crossing_type ~need_adjacent ~visitor =
  let min_sign = min_sign_of crossing_type in
  let iter = S2_shape_index.iterator index in
  let buf = edge_buf_create () in
  let mutable cont = true in
  while cont && not (S2_shape_index.Iterator.is_done iter) do
    let cell = S2_shape_index.Iterator.index_cell iter in
    edge_buf_clear buf;
    append_cell_edges buf index cell;
    if not (visit_crossings_within buf ~min_sign ~need_adjacent ~visitor)
    then cont <- false;
    if cont then S2_shape_index.Iterator.next iter
  done;
  cont
;;

let visit_crossing_edge_pairs index ~crossing_type ~visitor =
  let need_adjacent =
    match (crossing_type : Crossing_type.t) with
    | All -> true
    | Interior -> false
  in
  visit_crossings_internal index ~crossing_type ~need_adjacent ~visitor
;;

(* Two-index walk: for each edge in A, query B for candidate crossings, then
   run an edge-crosser to filter the candidates and to recover [is_interior]
   (the crosser's sign is +1 for strictly interior crossings, 0 for vertex
   crossings). The crosser is created once per A-edge and reused across all
   B candidates; the chain optimisation kicks in automatically when
   consecutive candidates share an endpoint. *)
let visit_crossing_edge_pairs_two ~a_index ~b_index ~crossing_type ~visitor =
  let min_sign = min_sign_of crossing_type in
  let b_query = S2_crossing_edge_query.create b_index in
  let a_iter = S2_shapeutil_edge_iterator.create a_index in
  let mutable cont = true in
  while cont && not (S2_shapeutil_edge_iterator.is_done a_iter) do
    let a_edge = S2_shapeutil_edge_iterator.edge a_iter in
    let a_shape_id = S2_shapeutil_edge_iterator.shape_id a_iter in
    let a_edge_id = S2_shapeutil_edge_iterator.edge_id a_iter in
    let a_shape_edge =
      S2_shapeutil_shape_edge.create ~shape_id:a_shape_id ~edge_id:a_edge_id ~edge:a_edge
    in
    let candidates =
      S2_crossing_edge_query.get_candidates b_query ~a:a_edge.#v0 ~b:a_edge.#v1
    in
    let mutable crosser = S2_edge_crosser.create ~a:a_edge.#v0 ~b:a_edge.#v1 in
    let n = Array.length candidates in
    let mutable k = 0 in
    while cont && k < n do
      let b_id = candidates.(k) in
      let b_shape = S2_shape_index.shape b_index b_id.#shape_id in
      let b_edge = b_shape.#edge b_id.#edge_id in
      let #{ state; sign } : S2_edge_crosser.with_sign =
        S2_edge_crosser.crossing_sign crosser b_edge.#v0 b_edge.#v1
      in
      crosser <- state;
      if sign >= min_sign
      then (
        let b_shape_edge =
          S2_shapeutil_shape_edge.create
            ~shape_id:b_id.#shape_id
            ~edge_id:b_id.#edge_id
            ~edge:b_edge
        in
        if not (visitor a_shape_edge b_shape_edge (sign = 1)) then cont <- false);
      k <- k + 1
    done;
    if cont then S2_shapeutil_edge_iterator.next a_iter
  done;
  cont
;;

(* Helpers for [find_self_intersection]. Each error path produces a string
   in one of two shapes: bare "Edge X ..." for single-loop shapes, or
   "Loop K: Edge X ..." / "Loop K edge X ..." for multi-loop shapes. The
   exact wording is what callers can pin in their error messages. *)

let loop_prefix ~is_polygon ~chain_id =
  if is_polygon then sprintf "Loop %d: " chain_id else ""
;;

let format_loop_error ~is_polygon ~ap_chain_id ~ap_offset ~bp_offset fmt =
  loop_prefix ~is_polygon ~chain_id:ap_chain_id ^ sprintf fmt ap_offset bp_offset
;;

(* Inspect a pair of edges that the crossing visitor has flagged. Returns
   [Some message] when the pair represents a validation error and the walk
   should terminate. *)
let find_crossing_error
  (shape : S2_shape.t)
  (a : S2_shapeutil_shape_edge.t)
  (b : S2_shapeutil_shape_edge.t)
  ~is_interior
  =
  let is_polygon = shape.#num_chains > 1 in
  let ap : S2_shape.Chain_position.t = shape.#chain_position a.#id.#edge_id in
  let bp : S2_shape.Chain_position.t = shape.#chain_position b.#id.#edge_id in
  if is_interior
  then
    if ap.#chain_id <> bp.#chain_id
    then
      Some
        (sprintf
           "Loop %d edge %d crosses loop %d edge %d"
           ap.#chain_id
           ap.#offset
           bp.#chain_id
           bp.#offset)
    else
      Some
        (format_loop_error
           ~is_polygon
           ~ap_chain_id:ap.#chain_id
           ~ap_offset:ap.#offset
           ~bp_offset:bp.#offset
           "Edge %d crosses edge %d")
  else if not (S2_point.equal a.#edge.#v1 b.#edge.#v1)
  then None
  else if ap.#chain_id = bp.#chain_id
  then
    Some
      (format_loop_error
         ~is_polygon
         ~ap_chain_id:ap.#chain_id
         ~ap_offset:ap.#offset
         ~bp_offset:bp.#offset
         "Edge %d has duplicate vertex with edge %d")
  else (
    let a_chain : S2_shape.Chain.t = shape.#chain ap.#chain_id in
    let b_chain : S2_shape.Chain.t = shape.#chain bp.#chain_id in
    let a_len = a_chain.#length in
    let b_len = b_chain.#length in
    let a_next = if ap.#offset + 1 = a_len then 0 else ap.#offset + 1 in
    let b_next = if bp.#offset + 1 = b_len then 0 else bp.#offset + 1 in
    let a2 = (shape.#chain_edge ap.#chain_id a_next).#v1 in
    let b2 = (shape.#chain_edge bp.#chain_id b_next).#v1 in
    if S2_point.equal a.#edge.#v0 b.#edge.#v0 || S2_point.equal a.#edge.#v0 b2
    then
      Some
        (sprintf
           "Loop %d edge %d has duplicate near loop %d edge %d"
           ap.#chain_id
           ap.#offset
           bp.#chain_id
           bp.#offset)
    else (
      (* Two non-overlapping wedges that share the middle vertex [ab1] cross at
         that vertex iff each wedge properly overlaps the other one's reflexion
         around the shared edge. *)
      let r1 =
        S2_wedge_relations.get_wedge_relation
          ~a0:a.#edge.#v0
          ~ab1:a.#edge.#v1
          ~a2
          ~b0:b.#edge.#v0
          ~b2
      in
      let r2 =
        S2_wedge_relations.get_wedge_relation
          ~a0:a.#edge.#v0
          ~ab1:a.#edge.#v1
          ~a2
          ~b0:b2
          ~b2:b.#edge.#v0
      in
      if S2_wedge_relations.Relation.equal r1 Properly_overlaps
         && S2_wedge_relations.Relation.equal r2 Properly_overlaps
      then
        Some
          (sprintf
             "Loop %d edge %d crosses loop %d edge %d"
             ap.#chain_id
             ap.#offset
             bp.#chain_id
             bp.#offset)
      else None))
;;

let find_self_intersection index =
  if S2_shape_index.num_shape_ids index = 0
  then None
  else (
    if S2_shape_index.num_shape_ids index <> 1
    then
      raise_s
        [%message
          "S2_shapeutil_visit_crossing_edge_pairs.find_self_intersection: index must \
           contain at most one shape"];
    let shape = S2_shape_index.shape index 0 in
    let result = ref None in
    let _completed =
      visit_crossings_internal
        index
        ~crossing_type:All
        ~need_adjacent:false
        ~visitor:(fun a b is_interior ->
          match find_crossing_error shape a b ~is_interior with
          | None -> true
          | Some _ as msg ->
            result := msg;
            false)
    in
    !result)
;;
