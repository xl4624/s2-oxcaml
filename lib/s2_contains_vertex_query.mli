(** Decide whether a polygon contains a given vertex, using only the edges of the polygon
    incident to that vertex.

    Callers describe the local topology around the vertex by supplying each incident edge
    via {!add_edge} together with a signed direction ([+1] for outgoing, [-1] for
    incoming, [0] for a degenerate edge). After all edges have been added,
    {!contains_sign} returns:

    - [+1] if the polygon contains the target vertex,
    - [-1] if the polygon does not contain it,
    - [0] if the incident edges consist entirely of matched sibling pairs. In this case
      the answer cannot be determined from the local data alone and the caller must
      resolve it with global information.

    Point containment follows the "semi-open" boundary model: if several polygons tile the
    region around a vertex, exactly one of those polygons contains that vertex. This
    convention is compatible with the vertex crossing rule used throughout S2.

    This type is not thread-safe; each thread should construct its own instance. *)

open Core

[@@@zero_alloc all]

type t

(** [create target] returns a new query whose containment will be determined for the
    vertex [target]. *)
val create : S2_point.t -> t
[@@zero_alloc ignore]

(** [target t] returns the vertex whose containment [t] is tracking. *)
val target : t -> S2_point.t

(** [add_edge t v ~direction] records that the polygon has an edge between [target t] and
    [v] with the given [direction]: [+1] for outgoing, [-1] for incoming, [0] for a
    degenerate edge. *)
val add_edge : t -> S2_point.t -> direction:int -> unit
[@@zero_alloc ignore]

(** [contains_sign t] returns [+1] if the target vertex is contained by the polygon, [-1]
    if it is not, and [0] if the incident edges consisted of matched sibling pairs. *)
val contains_sign : t -> int
[@@zero_alloc ignore]

(** [duplicate_edges t] returns [true] if the same directed edge incident to [target] has
    been added more than once since the query was created. An incoming and an outgoing
    edge to the same vertex do not count as duplicates because they cancel each other in
    the direction sum. *)
val duplicate_edges : t -> bool
[@@zero_alloc ignore]
