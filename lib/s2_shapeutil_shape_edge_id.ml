open Core

type t =
  { shape_id : int
  ; edge_id : int
  }
[@@deriving compare, equal, sexp_of]

let none = { shape_id = -1; edge_id = -1 }
let create ~shape_id ~edge_id = { shape_id; edge_id }
