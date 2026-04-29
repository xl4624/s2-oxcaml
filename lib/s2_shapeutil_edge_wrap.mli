(** Convenience helpers for stepping along the chain that contains a given edge.

    Given an edge id inside an {!S2_shape.t}, [next_edge_wrap] and [prev_edge_wrap] return
    the edge id of the chain neighbour, wrapping around at the boundaries of any chain
    that is closed:

    - {b dimension 2} (polygons): chains always wrap, so the result is always a valid edge
      id in the same chain.
    - {b dimension 1} (polylines): the chain wraps only when its last vertex equals its
      first - i.e. it is a closed polyline. Otherwise reaching either end yields the
      sentinel [-1].
    - {b dimension 0} (points): every "edge" is a single-edge chain by itself, so both
      neighbours are always [-1].

    Each call performs a [chain_position] lookup followed by one [chain] (and possibly two
    [chain_edge]) calls. This is intended for one-off probes; iterate the chain directly
    if you need many lookups. *)

(** [next_edge_wrap shape ~edge_id] returns the edge id immediately following [edge_id]
    within its chain, or [-1] when no such edge exists (open polylines reaching their last
    edge, or any point shape). The caller is responsible for ensuring
    [0 <= edge_id < shape.num_edges]; behaviour is otherwise unspecified. *)
val next_edge_wrap : S2_shape.t -> edge_id:int -> int

(** [prev_edge_wrap shape ~edge_id] returns the edge id immediately preceding [edge_id]
    within its chain, or [-1] when no such edge exists. Mirrors {!next_edge_wrap} with the
    closed/open polyline rule reversed at the chain start. *)
val prev_edge_wrap : S2_shape.t -> edge_id:int -> int
