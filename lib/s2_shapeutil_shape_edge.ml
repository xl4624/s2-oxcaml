open Core

type t =
  #{ id : S2_shapeutil_shape_edge_id.t
   ; edge : S2_shape.Edge.t
   }

let sexp_of_t #{ id; edge } =
  Sexp.List
    [ Sexp.Atom "Shape_edge"
    ; S2_shapeutil_shape_edge_id.sexp_of_t id
    ; S2_shape.Edge.sexp_of_t edge
    ]
;;

let[@inline] [@zero_alloc] create ~shape_id ~edge_id ~edge =
  #{ id = S2_shapeutil_shape_edge_id.create ~shape_id ~edge_id; edge }
;;

let[@inline] [@zero_alloc] v0 t = t.#edge.#v0
let[@inline] [@zero_alloc] v1 t = t.#edge.#v1
