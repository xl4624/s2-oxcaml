open Core

let shape_to_points (shape : S2_shape.t) =
  let n = shape.#num_edges in
  if n = 0
  then [||]
  else (
    let first = (shape.#edge 0).#v0 in
    let out = Array.create ~len:n first in
    for i = 1 to n - 1 do
      out.(i) <- (shape.#edge i).#v0
    done;
    out)
;;

(* Returns the [num_vertices] of [chain_id] in [shape]: [chain.length] for
   dimension 0 or 2 (closed), and [chain.length + 1] for dimension 1 (open).
   The vertex sequence walks the chain edges in order, taking [v0] of each edge
   plus the final [v1] when the chain is open. *)
let chain_vertices (shape : S2_shape.t) chain_id =
  let chain = shape.#chain chain_id in
  let len : int = chain.#length in
  let dim = shape.#dimension in
  let num_vertices = if dim = 1 then len + 1 else len in
  if num_vertices = 0
  then [||]
  else (
    (* Seed with the first vertex so we can size the array. For [len = 0] in
       dimension other than 1 the case above already returned. *)
    let first = (shape.#chain_edge chain_id 0).#v0 in
    let out = Array.create ~len:num_vertices first in
    for i = 1 to len - 1 do
      out.(i) <- (shape.#chain_edge chain_id i).#v0
    done;
    if dim = 1 && len > 0
    then out.(num_vertices - 1) <- (shape.#chain_edge chain_id (len - 1)).#v1;
    out)
;;

let shape_to_polyline (shape : S2_shape.t) =
  S2_polyline.of_vertices ~validate:false (chain_vertices shape 0)
;;

let shape_to_polygon (shape : S2_shape.t) =
  if S2_shape.is_full shape
  then S2_polygon.full ()
  else (
    let n = shape.#num_chains in
    let loops =
      Array.init n ~f:(fun i ->
        S2_loop.of_vertices ~validate:false (chain_vertices shape i))
    in
    if n = 1 then S2_polygon.of_loops loops else S2_polygon.of_oriented_loops loops)
;;
