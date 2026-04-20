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
    match vertex_model with
    | Open | Closed ->
      raise_s
        [%message
          "S2_contains_point_query: only Vertex_model.Semi_open is supported for 2d \
           shapes"]
    | Semi_open ->
      let center_pt = S2_cell_id.to_point cell_id in
      let open S2_edge_crosser in
      let mutable crosser = create ~a:center_pt ~b:p in
      let mutable inside = inside in
      for i = 0 to n - 1 do
        let eid = S2_shape_index.Clipped_shape.edge clipped i in
        let #{ v0; v1 } : S2_shape.Edge.t = shape.#edge eid in
        let #{ state; crossing } = edge_or_vertex_crossing crosser v0 v1 in
        crosser <- state;
        if crossing then inside <- not inside
      done;
      inside)
;;

let shape_contains t ~shape_id p =
  if not (S2_shape_index.Iterator.locate_point t.iter p)
  then false
  else (
    match
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
