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

let shape_to_polyline (shape : S2_shape.t) =
  S2_polyline.of_vertices ~validate:false (S2_shape_measures.chain_vertices shape 0)
;;

let shape_to_polygon (shape : S2_shape.t) =
  if S2_shape.is_full shape
  then S2_polygon.full ()
  else (
    let n = shape.#num_chains in
    let loops =
      Array.init n ~f:(fun i ->
        S2_loop.of_vertices ~validate:false (S2_shape_measures.chain_vertices shape i))
    in
    if n = 1 then S2_polygon.of_loops loops else S2_polygon.of_oriented_loops loops)
;;
