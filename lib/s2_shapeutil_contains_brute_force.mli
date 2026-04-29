(** Linear-time point-in-shape containment test.

    Walks every edge of a {!S2_shape.t} once: starting from the shape's
    {!S2_shape.Reference_point.t} (whose containment is known) it counts edge-or-vertex
    crossings of the segment from the reference point to the query point and returns the
    parity-flipped containment.

    Polygon boundaries are treated as semi-open, matching the C++ default in
    [S2ContainsPointQuery]. Shapes of dimension less than 2 always return [false] - they
    have no interior.

    {b Cost}: this is [O(num_edges)]. Most callers should index the shape once and use a
    {!S2_contains_point_query}; this helper exists for the rare cases where the shape has
    very few edges or the index has not been built yet. *)

(** [contains_brute_force shape ~point] returns [true] when [point] lies inside [shape]
    under the semi-open boundary rule. *)
val contains_brute_force : S2_shape.t -> point:S2_point.t -> bool
