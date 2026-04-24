(** Vertex-counting helpers for {!S2_shape.t} and {!S2_shape_index.t}.

    Vertex counts depend on a shape's dimension:

    - dimension 0 (points): each point is a degenerate edge in its own chain, so the
      vertex count equals [num_chains].
    - dimension 1 (polylines): an open polyline with [k] edges has [k + 1] vertices.
      Summing over all chains gives [num_edges + num_chains].
    - dimension 2 (polygons): a closed loop with [k] edges visits [k] distinct vertices,
      so the count equals [num_edges].

    These totals ignore whether individual vertices coincide: a degenerate polyline like
    [[p, p, p]] still contributes three vertices (two edges plus one chain), and a
    degenerate polygon with four identical vertices contributes four. *)

open Core

(** [of_shape shape] returns the number of vertices in [shape]. Requires
    [0 <= shape.#dimension <= 2]. *)
val of_shape : S2_shape.t -> int

(** [of_index idx] returns the total number of vertices across every shape in [idx]. Runs
    in time linear in the number of shapes. *)
val of_index : S2_shape_index.t -> int
