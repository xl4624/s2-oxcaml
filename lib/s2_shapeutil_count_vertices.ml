open Core

let[@zero_alloc] of_shape (shape : S2_shape.t) =
  match shape.#dimension with
  | 0 -> shape.#num_chains
  | 1 -> shape.#num_edges + shape.#num_chains
  | 2 -> shape.#num_edges
  | d ->
    raise_s [%message "S2_shapeutil_count_vertices: invalid shape dimension" (d : int)]
;;

let[@zero_alloc] of_index idx =
  let n = S2_shape_index.num_shape_ids idx in
  let mutable total = 0 in
  for i = 0 to n - 1 do
    total <- total + of_shape (S2_shape_index.shape idx i)
  done;
  total
;;
