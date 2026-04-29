open Core

(* [reference_point] is materialised at construction time rather than looked up lazily:
   [S2_shape.get_reference_point] can allocate closures internally and we want every
   post-construction accessor to be zero-alloc. *)
type t =
  { vertices : S2_point.t array
  ; reference_point : S2_shape.Reference_point.t
  }

let sexp_of_t { vertices; reference_point = _ } =
  let n = Array.length vertices in
  let acc = ref [] in
  for i = n - 1 downto 0 do
    acc := S2_point.sexp_of_t vertices.(i) :: !acc
  done;
  Sexp.List [ Sexp.Atom "S2_lax_loop"; Sexp.List !acc ]
;;

let edge_of vertices n e =
  let e1 = if e + 1 = n then 0 else e + 1 in
  S2_shape.Edge.create ~v0:vertices.(e) ~v1:vertices.(e1)
;;

let of_vertices src =
  let n = Array.length src in
  (* Copy the caller's array so later mutations do not invalidate the pre-computed
     reference point. The branch on [n = 0] avoids indexing [src.(0)] when building the
     seed for [Array.create]. *)
  let vertices =
    if n = 0
    then [||]
    else (
      let dst = Array.create ~len:n src.(0) in
      for i = 1 to n - 1 do
        dst.(i) <- src.(i)
      done;
      dst)
  in
  let edge = edge_of vertices n in
  let chain _ = S2_shape.Chain.create ~start:0 ~length:n in
  let num_chains = Int.min 1 n in
  let reference_point =
    S2_shape.get_reference_point ~num_edges:n ~num_chains ~edge ~chain
  in
  { vertices; reference_point }
;;

let num_vertices t = Array.length t.vertices
let vertex t i = t.vertices.(i)
let num_edges t = Array.length t.vertices
let edge t e = edge_of t.vertices (Array.length t.vertices) e
let dimension _ = 2
let num_chains t = Int.min 1 (Array.length t.vertices)
let chain t _ = S2_shape.Chain.create ~start:0 ~length:(Array.length t.vertices)
let chain_edge t _ j = edge_of t.vertices (Array.length t.vertices) j
let chain_position _ e = S2_shape.Chain_position.create ~chain_id:0 ~offset:e
let reference_point t = t.reference_point
let type_tag = S2_shape.Type_tag.none

let to_shape t : S2_shape.t =
  #{ num_edges = num_edges t
   ; num_chains = num_chains t
   ; dimension = 2
   ; type_tag
   ; reference_point = reference_point t
   ; edge = edge t
   ; chain = chain t
   ; chain_edge = chain_edge t
   ; chain_position = chain_position t
   }
;;
