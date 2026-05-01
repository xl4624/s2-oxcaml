open Core

module Vertex_model = struct
  type t =
    | Open
    | Semi_open
    | Closed
end

type t =
  { index : S2_shape_index.t
  ; vertex_model : Vertex_model.t
  ; mutable iter : S2_shape_index.Iterator.t
  }

let index t = t.index

let create index ?(vertex_model = Vertex_model.Semi_open) () =
  S2_shape_index.build index;
  { index; vertex_model; iter = S2_shape_index.iterator index }
;;

let shape_contains_cell
  ~(vertex_model : Vertex_model.t)
  ~(shape : S2_shape.t)
  cell_id
  clipped
  p
  =
  let inside = S2_shape_index.Clipped_shape.contains_center clipped in
  let n = S2_shape_index.Clipped_shape.num_edges clipped in
  if n = 0
  then inside
  else if shape.#dimension < 2
  then (
    match vertex_model with
    | Closed ->
      let found = ref false in
      for i = 0 to n - 1 do
        let eid = S2_shape_index.Clipped_shape.edge clipped i in
        let #{ v0; v1 } : S2_shape.Edge.t = shape.#edge eid in
        if S2_point.equal v0 p || S2_point.equal v1 p then found := true
      done;
      !found
    | Open | Semi_open -> false)
  else (
    (* 2D shape. Start from [contains_center] - the parity of [cell_id]'s center with
       respect to this clipped shape - and draw a segment from the cell center to [p].
       Each interior edge crossing flips the parity, giving the answer for [p]. When
       [crossing_sign] returns 0 we are at a shared vertex: if [p] matches an edge
       endpoint the Open and Closed models can answer immediately without completing the
       scan (Open: outside, Closed: inside); otherwise we defer to [vertex_crossing],
       which implements the Semi_open tie-breaking rule required for
       exactly-one-polygon-wins behaviour. *)
    let center_pt = S2_cell_id.to_point cell_id in
    let mutable crosser = S2_edge_crosser.create ~a:center_pt ~b:p in
    let mutable inside = inside in
    let boundary_answer = ref None in
    let i = ref 0 in
    while Option.is_none !boundary_answer && !i < n do
      let eid = S2_shape_index.Clipped_shape.edge clipped !i in
      let #{ v0; v1 } : S2_shape.Edge.t = shape.#edge eid in
      let r : S2_edge_crosser.with_sign = S2_edge_crosser.crossing_sign crosser v0 v1 in
      crosser <- r.#state;
      let sign = r.#sign in
      if sign > 0
      then inside <- not inside
      else if sign = 0
      then (
        let is_vertex_hit = S2_point.equal v0 p || S2_point.equal v1 p in
        match vertex_model with
        | Open when is_vertex_hit -> boundary_answer := Some false
        | Closed when is_vertex_hit -> boundary_answer := Some true
        | _ ->
          if S2_edge_crossings.vertex_crossing
               (S2_edge_crosser.a crosser)
               (S2_edge_crosser.b crosser)
               v0
               v1
          then inside <- not inside);
      incr i
    done;
    match !boundary_answer with
    | Some b -> b
    | None -> inside)
;;

let shape_contains_at t cell_id clipped p =
  let shape_id = S2_shape_index.Clipped_shape.shape_id clipped in
  let shape = S2_shape_index.shape t.index shape_id in
  shape_contains_cell ~vertex_model:t.vertex_model ~shape cell_id clipped p
;;

let shape_contains t ~shape_id p =
  if not (S2_shape_index.Iterator.locate_point t.iter p)
  then false
  else (
    match%optional_u.S2_shape_index.Clipped_shape.Option
      S2_shape_index.Index_cell.find_clipped
        (S2_shape_index.Iterator.index_cell t.iter)
        ~shape_id
    with
    | None -> false
    | Some clipped ->
      let shape = S2_shape_index.shape t.index shape_id in
      shape_contains_cell
        ~vertex_model:t.vertex_model
        ~shape
        (S2_shape_index.Iterator.cell_id t.iter)
        clipped
        p)
;;

let visit_containing_shapes t p ~f =
  if not (S2_shape_index.Iterator.locate_point t.iter p)
  then true
  else (
    let cell = S2_shape_index.Iterator.index_cell t.iter in
    let cid = S2_shape_index.Iterator.cell_id t.iter in
    let n = S2_shape_index.Index_cell.num_clipped cell in
    let continue_ = ref true in
    let i = ref 0 in
    while !continue_ && !i < n do
      let clipped = S2_shape_index.Index_cell.clipped cell !i in
      let shape_id = S2_shape_index.Clipped_shape.shape_id clipped in
      let shape = S2_shape_index.shape t.index shape_id in
      if shape_contains_cell ~vertex_model:t.vertex_model ~shape cid clipped p
         && not (f shape_id)
      then continue_ := false;
      incr i
    done;
    !continue_)
;;
