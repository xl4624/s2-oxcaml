open Core

let next_edge_wrap (shape : S2_shape.t) ~edge_id =
  let pos = shape.#chain_position edge_id in
  let chain_id = pos.#chain_id in
  let offset = pos.#offset in
  let chain = shape.#chain chain_id in
  match shape.#dimension with
  | 2 -> chain.#start + ((offset + 1) mod chain.#length)
  | 1 ->
    if offset = chain.#length - 1
    then (
      (* A polyline chain wraps only when its last edge ends where the first edge starts.
         Otherwise the chain is open and we return the [-1] sentinel. *)
      let curr = shape.#chain_edge chain_id offset in
      let next = shape.#chain_edge chain_id 0 in
      if S2_point.equal curr.#v1 next.#v0 then chain.#start else -1)
    else chain.#start + offset + 1
  | _ -> -1
;;

let prev_edge_wrap (shape : S2_shape.t) ~edge_id =
  let pos = shape.#chain_position edge_id in
  let chain_id = pos.#chain_id in
  let offset = pos.#offset in
  let chain = shape.#chain chain_id in
  match shape.#dimension with
  | 2 ->
    let off = offset - 1 in
    let off = if off < 0 then off + chain.#length else off in
    chain.#start + off
  | 1 ->
    if offset = 0
    then (
      let last = chain.#length - 1 in
      let curr = shape.#chain_edge chain_id 0 in
      let prev = shape.#chain_edge chain_id last in
      if S2_point.equal prev.#v1 curr.#v0 then chain.#start + last else -1)
    else chain.#start + offset - 1
  | _ -> -1
;;
