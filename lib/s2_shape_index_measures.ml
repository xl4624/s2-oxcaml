open Core

let dimension index =
  let n = S2_shape_index.num_shape_ids index in
  let mutable dim = -1 in
  for i = 0 to n - 1 do
    let shape = S2_shape_index.shape index i in
    let d = shape.#dimension in
    if d > dim then dim <- d
  done;
  dim
;;

let num_points index =
  let n = S2_shape_index.num_shape_ids index in
  let mutable count = 0 in
  for i = 0 to n - 1 do
    let shape = S2_shape_index.shape index i in
    if shape.#dimension = 0 then count <- count + shape.#num_edges
  done;
  count
;;

let length index =
  let n = S2_shape_index.num_shape_ids index in
  let mutable acc = S1_angle.zero in
  for i = 0 to n - 1 do
    let shape = S2_shape_index.shape index i in
    acc <- S1_angle.add acc (S2_shape_measures.length shape)
  done;
  acc
;;

let perimeter index =
  let n = S2_shape_index.num_shape_ids index in
  let mutable acc = S1_angle.zero in
  for i = 0 to n - 1 do
    let shape = S2_shape_index.shape index i in
    acc <- S1_angle.add acc (S2_shape_measures.perimeter shape)
  done;
  acc
;;

let area index =
  let n = S2_shape_index.num_shape_ids index in
  let mutable acc = #0.0 in
  for i = 0 to n - 1 do
    let shape = S2_shape_index.shape index i in
    acc <- Float_u.O.(acc + S2_shape_measures.area shape)
  done;
  acc
;;

let approx_area index =
  let n = S2_shape_index.num_shape_ids index in
  let mutable acc = #0.0 in
  for i = 0 to n - 1 do
    let shape = S2_shape_index.shape index i in
    acc <- Float_u.O.(acc + S2_shape_measures.approx_area shape)
  done;
  acc
;;

let centroid index =
  let dim = dimension index in
  let n = S2_shape_index.num_shape_ids index in
  let mutable acc = R3_vector.zero in
  for i = 0 to n - 1 do
    let shape = S2_shape_index.shape index i in
    if shape.#dimension = dim
    then acc <- R3_vector.add acc (S2_shape_measures.centroid shape)
  done;
  acc
;;
