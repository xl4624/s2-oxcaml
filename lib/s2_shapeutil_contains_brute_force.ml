open Core

let contains_brute_force (shape : S2_shape.t) ~point =
  if shape.#dimension < 2
  then false
  else (
    let ref_point = shape.#reference_point in
    if S2_point.equal ref_point.#point point
    then ref_point.#contained
    else (
      let inside = ref ref_point.#contained in
      let n = shape.#num_edges in
      for e = 0 to n - 1 do
        let edge = shape.#edge e in
        if S2_edge_crossings.edge_or_vertex_crossing
             ref_point.#point
             point
             edge.#v0
             edge.#v1
        then inside := not !inside
      done;
      !inside))
;;
