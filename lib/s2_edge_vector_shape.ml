open Core

let default_capacity = 8
let zero_point = S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#0.0

(* Two parallel growable arrays of [S2_point.t] rather than a single [S2_shape.Edge.t]
   array: [S2_shape.Edge.t] is an unboxed product, but maintaining one buffer per endpoint
   sidesteps the missing kind-polymorphic [Array.blit] over products when growing. *)
type t =
  { mutable v0s : S2_point.t array
  ; mutable v1s : S2_point.t array
  ; mutable len : int
  ; mutable dimension : int
  }

let sexp_of_t { v0s; v1s; len; dimension } =
  let edges = ref [] in
  for i = len - 1 downto 0 do
    edges
    := Sexp.List [ S2_point.sexp_of_t v0s.(i); S2_point.sexp_of_t v1s.(i) ] :: !edges
  done;
  Sexp.List
    [ Sexp.Atom "S2_edge_vector_shape"
    ; Sexp.List [ Sexp.Atom "dimension"; Sexp.Atom (Int.to_string dimension) ]
    ; Sexp.List [ Sexp.Atom "edges"; Sexp.List !edges ]
    ]
;;

let create () =
  { v0s = Array.create ~len:default_capacity zero_point
  ; v1s = Array.create ~len:default_capacity zero_point
  ; len = 0
  ; dimension = 1
  }
;;

let grow t =
  let old_cap = Array.length t.v0s in
  let new_cap = Int.max default_capacity (old_cap * 2) in
  let new_v0s = Array.create ~len:new_cap zero_point in
  let new_v1s = Array.create ~len:new_cap zero_point in
  for i = 0 to t.len - 1 do
    new_v0s.(i) <- t.v0s.(i);
    new_v1s.(i) <- t.v1s.(i)
  done;
  t.v0s <- new_v0s;
  t.v1s <- new_v1s
;;

let add t ~v0 ~v1 =
  if t.len >= Array.length t.v0s then grow t;
  t.v0s.(t.len) <- v0;
  t.v1s.(t.len) <- v1;
  t.len <- t.len + 1
;;

let of_edges es =
  let n = Array.length es in
  let cap = Int.max default_capacity n in
  let v0s = Array.create ~len:cap zero_point in
  let v1s = Array.create ~len:cap zero_point in
  for i = 0 to n - 1 do
    let (#{ v0; v1 } : S2_shape.Edge.t) = es.(i) in
    v0s.(i) <- v0;
    v1s.(i) <- v1
  done;
  { v0s; v1s; len = n; dimension = 1 }
;;

let singleton ~v0 ~v1 =
  let t = create () in
  add t ~v0 ~v1;
  t
;;

let set_dimension t dim = t.dimension <- dim
let num_edges t = t.len
let edge t e = S2_shape.Edge.create ~v0:t.v0s.(e) ~v1:t.v1s.(e)
let dimension t = t.dimension
let num_chains t = t.len
let chain (_ : t) i = S2_shape.Chain.create ~start:i ~length:1
let chain_edge t i (_ : int) = edge t i
let chain_position (_ : t) e = S2_shape.Chain_position.create ~chain_id:e ~offset:0
let reference_point (_ : t) = S2_shape.Reference_point.contained false
let type_tag = S2_shape.Type_tag.none

let to_shape t : S2_shape.t =
  #{ num_edges = num_edges t
   ; num_chains = num_chains t
   ; dimension = t.dimension
   ; type_tag
   ; reference_point = reference_point t
   ; edge = edge t
   ; chain = chain t
   ; chain_edge = chain_edge t
   ; chain_position = chain_position t
   }
;;
