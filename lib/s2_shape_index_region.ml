open Core

type t =
  { index : S2_shape_index.t
  ; contains_query : S2_contains_point_query.t
  ; iter : S2_shape_index.Iterator.t
  }

let create index =
  S2_shape_index.build index;
  let contains_query =
    S2_contains_point_query.create
      index
      ~vertex_model:S2_contains_point_query.Vertex_model.Semi_open
      ()
  in
  { index; contains_query; iter = S2_shape_index.iterator index }
;;

let index t = t.index

(* Build the smallest [S2_cell_id.t] covering the (first, last) range in id order. When
   the range collapses to a single id we keep that id; otherwise we lift up to the common
   ancestor. The two ids must share a common ancestor (which is true for any pair of valid
   cell ids, since all share face level). *)
let cover_range first last buf len =
  if S2_cell_id.equal first last
  then (
    buf.(len) <- first;
    len + 1)
  else (
    let level = S2_cell_id.get_common_ancestor_level first last in
    buf.(len) <- S2_cell_id.parent_level first level;
    len + 1)
;;

(* The result is bounded by 6 cells: at most one per face the index touches, or up to four
   child cells of a single face when the index lives on one face. *)
let max_bound_cells = 6

let cell_union_bound t =
  let buf = Array.create ~len:max_bound_cells S2_cell_id.none in
  let mutable len = 0 in
  let it = t.iter in
  S2_shape_index.Iterator.at_end it;
  if S2_shape_index.Iterator.prev it
  then (
    let last_index_id = S2_shape_index.Iterator.cell_id it in
    S2_shape_index.Iterator.begin_at it;
    let first_index_id = S2_shape_index.Iterator.cell_id it in
    if not (S2_cell_id.equal first_index_id last_index_id)
    then (
      let level = S2_cell_id.get_common_ancestor_level first_index_id last_index_id + 1 in
      let last_id = S2_cell_id.parent_level last_index_id level in
      let mutable id = S2_cell_id.parent_level first_index_id level in
      while not (S2_cell_id.equal id last_id) do
        let cur_id = S2_shape_index.Iterator.cell_id it in
        if S2_cell_id.compare (S2_cell_id.range_max id) cur_id < 0
        then id <- S2_cell_id.next id
        else (
          let first = cur_id in
          S2_shape_index.Iterator.seek it (S2_cell_id.next (S2_cell_id.range_max id));
          let (_ : bool) = S2_shape_index.Iterator.prev it in
          len <- cover_range first (S2_shape_index.Iterator.cell_id it) buf len;
          S2_shape_index.Iterator.next it;
          id <- S2_cell_id.next id)
      done);
    len <- cover_range (S2_shape_index.Iterator.cell_id it) last_index_id buf len);
  (Array.sub [@kind bits64]) buf ~pos:0 ~len
;;

let cap_bound t =
  let cu = S2_cell_union.create (cell_union_bound t) in
  S2_cell_union.cap_bound cu
;;

let rect_bound t =
  let cu = S2_cell_union.create (cell_union_bound t) in
  S2_cell_union.rect_bound cu
;;

(* Conservative test: returns true when at least one edge of [clipped] crosses (or comes
   within the index error bound of) the [target] cell on its native face. The padding
   matches the worst-case face-clip + rect-intersect error so a [false] answer is
   reliable. *)
let any_edge_intersects t clipped target =
  let max_error =
    Float_u.O.(
      S2_edge_clipping.face_clip_error_uv_coord
      + S2_edge_clipping.intersects_rect_error_uv_dist)
  in
  let bound = R2_rect.expanded_scalar (S2_cell.bound_uv target) max_error in
  let face = S2_cell.face target in
  let shape_id = S2_shape_index.Clipped_shape.shape_id clipped in
  let shape = S2_shape_index.shape t.index shape_id in
  let n = S2_shape_index.Clipped_shape.num_edges clipped in
  let mutable found = false in
  let i = ref 0 in
  while (not found) && !i < n do
    let eid = S2_shape_index.Clipped_shape.edge clipped !i in
    let #{ v0; v1 } : S2_shape.Edge.t = shape.#edge eid in
    (match%optional_u.S2_edge_clipping.Clipped_uv.Option
       S2_edge_clipping.clip_to_padded_face v0 v1 face ~padding:max_error
     with
     | None -> ()
     | Some clipped_uv ->
       let p0 = S2_edge_clipping.Clipped_uv.a clipped_uv in
       let p1 = S2_edge_clipping.Clipped_uv.b clipped_uv in
       if S2_edge_clipping.intersects_rect p0 p1 bound then found <- true);
    incr i
  done;
  found
;;

let contains_cell t target =
  let target_id = S2_cell.id target in
  match S2_shape_index.Iterator.locate_cell_id t.iter target_id with
  | Disjoint | Subdivided -> false
  | Indexed ->
    (* The iterator points at an index cell that contains [target]. The cell is contained
       only if at least one of its clipped shapes covers [target] on its own. *)
    let cell = S2_shape_index.Iterator.index_cell t.iter in
    let cur_id = S2_shape_index.Iterator.cell_id t.iter in
    let n = S2_shape_index.Index_cell.num_clipped cell in
    let mutable result = false in
    let s = ref 0 in
    while (not result) && !s < n do
      let clipped = S2_shape_index.Index_cell.clipped cell !s in
      if S2_cell_id.equal cur_id target_id
      then (
        if S2_shape_index.Clipped_shape.num_edges clipped = 0
           && S2_shape_index.Clipped_shape.contains_center clipped
        then result <- true)
      else (
        let shape_id = S2_shape_index.Clipped_shape.shape_id clipped in
        let shape = S2_shape_index.shape t.index shape_id in
        if shape.#dimension = 2
           && (not (any_edge_intersects t clipped target))
           && S2_contains_point_query.shape_contains_at
                t.contains_query
                cur_id
                clipped
                (S2_cell.center target)
        then result <- true);
      incr s
    done;
    result
;;

let may_intersect_cell t target =
  let target_id = S2_cell.id target in
  match S2_shape_index.Iterator.locate_cell_id t.iter target_id with
  | Disjoint -> false
  | Subdivided -> true
  | Indexed ->
    let cur_id = S2_shape_index.Iterator.cell_id t.iter in
    if S2_cell_id.equal cur_id target_id
    then
      (* [target] is itself an index cell, so by construction at least one shape touches
         it: index cells are only created when an edge passes through or the cell is
         entirely covered by a polygon. *)
      true
    else (
      let cell = S2_shape_index.Iterator.index_cell t.iter in
      let n = S2_shape_index.Index_cell.num_clipped cell in
      let mutable result = false in
      let s = ref 0 in
      while (not result) && !s < n do
        let clipped = S2_shape_index.Index_cell.clipped cell !s in
        if any_edge_intersects t clipped target
        then result <- true
        else if S2_contains_point_query.shape_contains_at
                  t.contains_query
                  cur_id
                  clipped
                  (S2_cell.center target)
        then result <- true;
        incr s
      done;
      result)
;;

let contains_point t p =
  if not (S2_shape_index.Iterator.locate_point t.iter p)
  then false
  else (
    let cell = S2_shape_index.Iterator.index_cell t.iter in
    let cur_id = S2_shape_index.Iterator.cell_id t.iter in
    let n = S2_shape_index.Index_cell.num_clipped cell in
    let mutable result = false in
    let s = ref 0 in
    while (not result) && !s < n do
      let clipped = S2_shape_index.Index_cell.clipped cell !s in
      if S2_contains_point_query.shape_contains_at t.contains_query cur_id clipped p
      then result <- true;
      incr s
    done;
    result)
;;

let to_region t =
  S2_region.custom
    #{ cap_bound = (fun () -> cap_bound t)
     ; rect_bound = (fun () -> rect_bound t)
     ; contains_cell = (fun cell -> contains_cell t cell)
     ; intersects_cell = (fun cell -> may_intersect_cell t cell)
     ; contains_point = (fun p -> contains_point t p)
     ; cell_union_bound = (fun () -> cell_union_bound t)
     }
;;
