open Core

let[@zero_alloc] count_edges_up_to idx ~max_edges =
  let n = S2_shape_index.num_shape_ids idx in
  let mutable total = 0 in
  let mutable i = 0 in
  let mutable stop = false in
  while (not stop) && i < n do
    let shape = S2_shape_index.shape idx i in
    total <- total + shape.#num_edges;
    if total >= max_edges then stop <- true;
    i <- i + 1
  done;
  total
;;

let[@zero_alloc] count_edges idx = count_edges_up_to idx ~max_edges:Int.max_value
