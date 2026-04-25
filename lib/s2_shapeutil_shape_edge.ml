open Core

type t =
  { id : S2_shapeutil_shape_edge_id.t
  ; edge : S2_shape.Edge.t
  }
[@@deriving sexp_of]

let create ~shape_id ~edge_id ~edge =
  { id = S2_shapeutil_shape_edge_id.create ~shape_id ~edge_id; edge }
;;

let v0 t = t.edge.#v0
let v1 t = t.edge.#v1
