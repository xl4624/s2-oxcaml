(** Helper for implementing the [reference_point] field of an {!S2_shape.t} on closed
    polygonal shapes (dimension 2).

    Given a shape whose interior is the region to the left of all edges (oriented
    consistently), this picks an arbitrary vertex of the shape and returns it together
    with whether that vertex is contained.

    Unlike {!S2_loop.t} and {!S2_polygon.t}, this routine tolerates duplicate vertices and
    edges. The rule is that an edge cancels its reverse, so a shape consisting only of
    canceling degenerate loops is empty unless it contains at least one chain with zero
    edges (the convention that marks the full polygon).

    Determining containment on the sphere is harder than in the plane: there is no "point
    at infinity" guaranteed to lie outside a loop, so the algorithm must inspect the local
    edge geometry around an unbalanced vertex via {!S2_contains_vertex_query}. *)

open Core

(** [get_reference_point shape] returns a {!S2_shape.Reference_point.t} for [shape]. The
    caller is responsible for ensuring [shape] has dimension [2]; behaviour is otherwise
    unspecified.

    Runs in O([num_edges]) for shapes whose first vertex is unbalanced (the common case)
    and O([num_edges] log [num_edges]) when a sort-based fallback is needed. *)
val get_reference_point : S2_shape.t -> S2_shape.Reference_point.t
