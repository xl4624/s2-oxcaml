open Core

type t = { vertices : S2_point.t array }

let sexp_of_t { vertices } =
  let n = Array.length vertices in
  let acc = ref [] in
  for i = n - 1 downto 0 do
    acc := S2_point.sexp_of_t vertices.(i) :: !acc
  done;
  Sexp.List [ Sexp.Atom "S2_lax_polyline"; Sexp.List !acc ]
;;

let of_vertices src =
  let n = Array.length src in
  if n = 0
  then { vertices = [||] }
  else (
    let dst = Array.create ~len:n src.(0) in
    for i = 1 to n - 1 do
      dst.(i) <- src.(i)
    done;
    { vertices = dst })
;;

let num_vertices t = Array.length t.vertices
let vertex t i = t.vertices.(i)
let num_edges t = Int.max 0 (num_vertices t - 1)
let edge t e = S2_shape.Edge.create ~v0:t.vertices.(e) ~v1:t.vertices.(e + 1)
let dimension _ = 1
let num_chains t = Int.min 1 (num_edges t)
let chain t _ = S2_shape.Chain.create ~start:0 ~length:(num_edges t)
let chain_edge t _ j = S2_shape.Edge.create ~v0:t.vertices.(j) ~v1:t.vertices.(j + 1)
let chain_position _ e = S2_shape.Chain_position.create ~chain_id:0 ~offset:e
let reference_point _ = S2_shape.Reference_point.contained false
let type_tag = 4

let to_shape t : S2_shape.t =
  #{ num_edges = num_edges t
   ; num_chains = num_chains t
   ; dimension = 1
   ; type_tag
   ; reference_point = reference_point t
   ; edge = edge t
   ; chain = chain t
   ; chain_edge = chain_edge t
   ; chain_position = chain_position t
   }
;;
