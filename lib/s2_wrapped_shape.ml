open Core

(* Boxed record around [S2_shape.t], which is itself an unboxed product.
   Gives the wrapper a nameable [value]-layout type for callers that need to
   pass an [S2_wrapped_shape.t] around without immediately turning it back
   into an [S2_shape.t]. *)
type t = { shape : S2_shape.t }

let sexp_of_t _ = Sexp.Atom "<S2_wrapped_shape>"
let create shape = { shape }
let num_edges t = t.shape.#num_edges
let edge t e = t.shape.#edge e
let dimension t = t.shape.#dimension
let num_chains t = t.shape.#num_chains
let chain t i = t.shape.#chain i
let chain_edge t i j = t.shape.#chain_edge i j
let chain_position t e = t.shape.#chain_position e
let reference_point t = t.shape.#reference_point
let type_tag = S2_shape.Type_tag.none

let to_shape t : S2_shape.t =
  #{ num_edges = t.shape.#num_edges
   ; num_chains = t.shape.#num_chains
   ; dimension = t.shape.#dimension
   ; type_tag
   ; reference_point = t.shape.#reference_point
   ; edge = t.shape.#edge
   ; chain = t.shape.#chain
   ; chain_edge = t.shape.#chain_edge
   ; chain_position = t.shape.#chain_position
   }
;;
