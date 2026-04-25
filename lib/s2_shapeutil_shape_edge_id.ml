open Core

type t =
  #{ shape_id : int
   ; edge_id : int
   }

let sexp_of_t #{ shape_id; edge_id } =
  Sexp.List
    [ Sexp.Atom "Shape_edge_id"
    ; Sexp.List [ Sexp.Atom "shape_id"; Sexp.Atom (Int.to_string shape_id) ]
    ; Sexp.List [ Sexp.Atom "edge_id"; Sexp.Atom (Int.to_string edge_id) ]
    ]
;;

let none = #{ shape_id = -1; edge_id = -1 }
let[@inline] [@zero_alloc] create ~shape_id ~edge_id = #{ shape_id; edge_id }

let[@inline] [@zero_alloc] compare a b =
  match Int.compare a.#shape_id b.#shape_id with
  | 0 -> Int.compare a.#edge_id b.#edge_id
  | c -> c
;;

let[@inline] [@zero_alloc] equal a b =
  a.#shape_id = b.#shape_id && a.#edge_id = b.#edge_id
;;
