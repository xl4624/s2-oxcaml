open Core

(* Reusable seed for [Array.create] when the caller has not yet seen any vertex. The
   sentinel is never observed: [chain_vertices] either returns the empty array or
   overwrites every slot before returning. *)
let zero_point = S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#0.0

(* For closed shapes (dimension 0 or 2) the chain has [chain.length] vertices; for an open
   polyline (dimension 1) the closing vertex bumps the count to [chain.length + 1]. We
   pull pairs of vertices out by walking every other edge, which halves the number of
   [chain_edge] dispatches compared with reading [v0] one edge at a time. The "odd" prefix
   handles the case where [num_vertices] is odd. *)
let chain_vertices (shape : S2_shape.t) chain_id =
  let chain = shape.#chain chain_id in
  let len : int = chain.#length in
  let num_vertices = len + if shape.#dimension = 1 then 1 else 0 in
  if num_vertices = 0
  then [||]
  else (
    let out = Array.create ~len:num_vertices zero_point in
    let mutable e = 0 in
    if num_vertices land 1 = 1
    then (
      out.(0) <- (shape.#chain_edge chain_id 0).#v0;
      e <- 1);
    while e < num_vertices do
      let edge = shape.#chain_edge chain_id e in
      out.(e) <- edge.#v0;
      out.(e + 1) <- edge.#v1;
      e <- e + 2
    done;
    out)
;;

let length (shape : S2_shape.t) =
  if shape.#dimension <> 1
  then S1_angle.zero
  else (
    let mutable acc = S1_angle.zero in
    let n = shape.#num_chains in
    for chain_id = 0 to n - 1 do
      acc
      <- S1_angle.add acc (S2_polyline_measures.length (chain_vertices shape chain_id))
    done;
    acc)
;;

let perimeter (shape : S2_shape.t) =
  if shape.#dimension <> 2
  then S1_angle.zero
  else (
    let mutable acc = S1_angle.zero in
    let n = shape.#num_chains in
    for chain_id = 0 to n - 1 do
      acc <- S1_angle.add acc (S2_loop_measures.perimeter (chain_vertices shape chain_id))
    done;
    acc)
;;

(* Per-loop signed areas keep their sign so that holes (which run "backwards" relative to
   their enclosing shells) cancel against shells. Adding unsigned per-loop areas would
   lose accuracy because polygon holes typically have areas near [4 * pi]: a small shell
   with an even smaller hole would otherwise compute as [(4*pi - tiny) + (small) mod 4*pi]
   and lose the relative precision of the small differences. *)
let area (shape : S2_shape.t) =
  if shape.#dimension <> 2
  then #0.0
  else (
    let mutable acc = #0.0 in
    let n = shape.#num_chains in
    for chain_id = 0 to n - 1 do
      acc
      <- Float_u.O.(acc + S2_loop_measures.signed_area (chain_vertices shape chain_id))
    done;
    (* [signed_area] guarantees the full loop has a tiny negative area; lifting back into
       [[0, 4 * pi]] keeps the sum monotone with respect to the true (unsigned) area. *)
    if Float_u.O.(acc < #0.0) then Float_u.O.(acc + (#4.0 * Float_u.pi)) else acc)
;;

let approx_area (shape : S2_shape.t) =
  if shape.#dimension <> 2
  then #0.0
  else (
    let mutable acc = #0.0 in
    let n = shape.#num_chains in
    for chain_id = 0 to n - 1 do
      acc
      <- Float_u.O.(acc + S2_loop_measures.approx_area (chain_vertices shape chain_id))
    done;
    let four_pi = Float_u.O.(#4.0 * Float_u.pi) in
    if Float_u.O.(acc <= four_pi) then acc else Float_u.mod_float acc four_pi)
;;

(* Point shapes use chain edges directly; each edge of a dimension-0 shape encodes a
   single vertex as a degenerate self-loop. *)
let centroid (shape : S2_shape.t) =
  let dimension = shape.#dimension in
  let n = shape.#num_chains in
  let mutable acc = R3_vector.zero in
  for chain_id = 0 to n - 1 do
    match dimension with
    | 0 ->
      let edge = shape.#edge chain_id in
      acc <- R3_vector.add acc edge.#v0
    | 1 ->
      acc
      <- R3_vector.add acc (S2_polyline_measures.centroid (chain_vertices shape chain_id))
    | _ ->
      acc <- R3_vector.add acc (S2_loop_measures.centroid (chain_vertices shape chain_id))
  done;
  acc
;;
