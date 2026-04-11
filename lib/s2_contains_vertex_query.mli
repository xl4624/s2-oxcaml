(** Determines whether a polygon contains one of its vertices given the edges incident to
    that vertex.

    The result of {!contains_sign} is [+1] if the target vertex is contained, [-1] if it
    is not contained, and [0] if the incident edges consist of matched sibling pairs (in
    which case the result cannot be determined locally).

    Point containment is defined according to the "semi-open" boundary model, which means
    that if several polygons tile the region around a vertex then exactly one of those
    polygons contains that vertex.

    This type is not thread-safe; each thread should construct its own instance.

    Mirrors C++ [S2ContainsVertexQuery] in [s2contains_vertex_query.h]. *)

open Core

type t

(** [create target] returns a new query whose containment will be determined for the
    vertex [target]. *)
val create : S2_point.t -> t

(** [target t] returns the vertex whose containment [t] is tracking. *)
val target : t -> S2_point.t

(** [add_edge t v ~direction] records that the polygon has an edge between [target t] and
    [v] with the given [direction]: [+1] for outgoing, [-1] for incoming, [0] for a
    degenerate edge. *)
val add_edge : t -> S2_point.t -> direction:int -> unit

(** [contains_sign t] returns [+1] if the target vertex is contained by the polygon, [-1]
    if it is not, and [0] if the incident edges consisted of matched sibling pairs. *)
val contains_sign : t -> int

(** [duplicate_edges t] returns [true] if the same directed edge incident to [target] has
    been added more than once since the query was created. An incoming and an outgoing
    edge to the same vertex do not count as duplicates because they cancel each other in
    the direction sum. *)
val duplicate_edges : t -> bool
