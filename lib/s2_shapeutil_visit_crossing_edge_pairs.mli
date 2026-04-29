(** Visit all pairs of crossing edges within an {!S2_shape_index.t}, or between two such
    indexes, and find self-intersections in a single-shape index.

    Two helpers are provided for the visitor-based crossing search:

    - {!visit_crossing_edge_pairs} walks every cell of one index and reports each pair of
      edges within that cell whose endpoints cross.
    - {!visit_crossing_edge_pairs_two} walks one index using {!S2_crossing_edge_query} on
      the other, reporting each pair of edges [(a, b)] where [a] is from the first index
      and [b] is from the second.

    A "crossing" is either an edge crossing (two open edges intersect at an interior
    point) or a vertex crossing (two edges share a vertex). The {!Crossing_type.t}
    controls whether vertex crossings are reported.

    The visitor is called with the two crossing edges packaged as
    {!S2_shapeutil_shape_edge.t} values plus a flag indicating whether the crossing is at
    points strictly interior to both edges. The visitor may return [false] to terminate
    the walk early, in which case the function also returns [false].

    Crossings may be visited more than once (a single crossing pair can appear in multiple
    cells of the underlying index).

    {!find_self_intersection} is a higher-level helper that runs
    {!visit_crossing_edge_pairs} on an index containing one polygonal shape and returns a
    human-readable error string if any loop has a duplicate vertex, shares an edge with
    another loop, or properly crosses another loop.

    {1 Performance}

    The two-index crossing search runs in time roughly proportional to
    [|A| * (avg cells of B traversed by an A-edge)]: for each edge of [a_index] it walks
    the cells of [b_index] that may contain a crossing edge. This is asymptotically
    suboptimal on large indexes - a cell-pair walk that descends both indexes in lockstep
    would amortise the cost - but the simpler strategy is correct and adequate for the
    polygon-validation workloads this module currently serves. *)

open Core

(** Re-exported for convenience: identical to {!S2_crossing_edge_query.Crossing_type}. *)
module Crossing_type = S2_crossing_edge_query.Crossing_type

(** Visitor signature: receives the two crossing edges together with a flag indicating
    whether the crossing is strictly interior to both edges. Return [false] to stop the
    walk. *)
type visitor = S2_shapeutil_shape_edge.t -> S2_shapeutil_shape_edge.t -> bool -> bool

(** [visit_crossing_edge_pairs index ~crossing_type ~visitor] visits every pair of
    crossing edges within [index]. Returns [true] if the walk completed, [false] if
    [visitor] terminated it early. *)
val visit_crossing_edge_pairs
  :  S2_shape_index.t
  -> crossing_type:Crossing_type.t
  -> visitor:visitor
  -> bool

(** [visit_crossing_edge_pairs_two ~a_index ~b_index ~crossing_type ~visitor] visits every
    pair [(a_edge, b_edge)] where [a_edge] belongs to [a_index] and [b_edge] belongs to
    [b_index] and the two edges cross. *)
val visit_crossing_edge_pairs_two
  :  a_index:S2_shape_index.t
  -> b_index:S2_shape_index.t
  -> crossing_type:Crossing_type.t
  -> visitor:visitor
  -> bool

(** [find_self_intersection index] returns [Some message] if any loop in the single shape
    stored at [index] has a self-intersection (including duplicate vertices), shares an
    edge with another loop, or properly crosses another loop. Returns [None] otherwise.

    [index] must contain at most one shape; the function returns [None] when the index is
    empty. The shape is expected to be a polygonal shape ([dimension = 2]) with one or
    more chains.

    The returned message is intended for use as an error string and is not guaranteed to
    be stable across versions. *)
val find_self_intersection : S2_shape_index.t -> string option
