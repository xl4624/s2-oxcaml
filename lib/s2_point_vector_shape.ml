open Core

type t = { points : S2_point.t array }

let sexp_of_t { points } =
  let items = ref [] in
  for i = Array.length points - 1 downto 0 do
    items := S2_point.sexp_of_t points.(i) :: !items
  done;
  Sexp.List
    [ Sexp.Atom "S2_point_vector_shape"
    ; Sexp.List [ Sexp.Atom "points"; Sexp.List !items ]
    ]
;;

let zero_point = S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#0.0
let create () = { points = [||] }

let of_points points =
  if Array.length points = 0
  then { points = [||] }
  else (
    let copy = Array.create ~len:(Array.length points) zero_point in
    for i = 0 to Array.length points - 1 do
      copy.(i) <- points.(i)
    done;
    { points = copy })
;;

let num_points t = Array.length t.points
let point t i = t.points.(i)
let num_edges t = num_points t

let edge t e =
  let p = t.points.(e) in
  S2_shape.Edge.create ~v0:p ~v1:p
;;

let dimension (_ : t) = 0
let num_chains t = num_points t
let chain (_ : t) i = S2_shape.Chain.create ~start:i ~length:1
let chain_edge t i (_ : int) = edge t i
let chain_position (_ : t) e = S2_shape.Chain_position.create ~chain_id:e ~offset:0
let reference_point (_ : t) = S2_shape.Reference_point.contained false
let type_tag = S2_shape.Type_tag.none

let to_shape t : S2_shape.t =
  #{ num_edges = num_edges t
   ; num_chains = num_chains t
   ; dimension = 0
   ; type_tag
   ; reference_point = reference_point t
   ; edge = edge t
   ; chain = chain t
   ; chain_edge = chain_edge t
   ; chain_position = chain_position t
   }
;;
