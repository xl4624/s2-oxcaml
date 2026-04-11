(** Edge crossing predicates and intersection computations for geodesic edges on the
    sphere. Provides robust predicates that produce correct, consistent results even in
    pathological cases (collinear points, nearly-degenerate edges, etc.). *)

open Core

(** {1 Robust sign predicate} *)

(** [sign a b c] returns +1 if the points A, B, C are in counter-clockwise order, -1 if
    they are in clockwise order, and 0 if any two points are the same. This is the robust
    version that uses exact arithmetic and symbolic perturbations to ensure consistency. *)
val sign : S2_point.t -> S2_point.t -> S2_point.t -> int

(** [triage_sign a b c] is a fast triage version of [sign] that returns 0 whenever the
    orientation is numerically indeterminate. Callers should fall back to [expensive_sign]
    if 0 is returned. Used internally by [S2_edge_crosser] to avoid redundant work when
    cached state already rules out a crossing. *)
val triage_sign : S2_point.t -> S2_point.t -> S2_point.t -> int

(** [expensive_sign a b c] is the slow path of [sign] that handles degenerate cases and
    uses stable/exact arithmetic with symbolic perturbations to guarantee a non-zero
    result whenever A, B, C are distinct. Returns 0 only if two of the arguments are
    equal. *)
val expensive_sign : S2_point.t -> S2_point.t -> S2_point.t -> int

(** {1 Ordering} *)

(** [ordered_ccw a b c o] returns true if the edges OA, OB, and OC are encountered in that
    order while sweeping CCW around the point O. Equivalently, B is contained in the range
    of angles (inclusive) that starts at A and extends CCW to C. *)
val ordered_ccw : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> bool

(** {1 Crossing predicates} *)

(** [crossing_sign a b c d] reports whether edge AB intersects edge CD. Returns +1 if AB
    crosses CD at a point interior to both edges, 0 if any two vertices from different
    edges are the same, and -1 otherwise. *)
val crossing_sign : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> int

(** [vertex_crossing a b c d] reports whether two edges "cross" in a way suitable for
    point-in-polygon containment tests, given that at least two vertices from different
    edges are the same (i.e., [crossing_sign a b c d = 0]). It is an error to call this
    with 4 distinct vertices. *)
val vertex_crossing : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> bool

(** [edge_or_vertex_crossing a b c d] is a convenience function that calls [crossing_sign]
    to handle cases where all four vertices are distinct, and [vertex_crossing] to handle
    cases where two or more vertices are the same. This defines a crossing function
    suitable for point-in-polygon containment tests. *)
val edge_or_vertex_crossing : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> bool

(** {1 Angle predicates} *)

(** [angle_contains_vertex a b c] returns true if the angle ABC contains its vertex B.
    Containment is defined such that if several polygons tile the region around a vertex,
    then exactly one of those polygons contains that vertex. Returns false for degenerate
    angles of the form ABA.

    Requires: [a <> b] and [b <> c]. *)
val angle_contains_vertex : S2_point.t -> S2_point.t -> S2_point.t -> bool

(** {1 Intersection} *)

(** [get_intersection a0 a1 b0 b1] returns the intersection point of two edges that cross
    ([crossing_sign a0 a1 b0 b1 > 0]). The result is guaranteed to be close to the true
    intersection point. *)
val get_intersection : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t

(** {1 Error bounds} *)

(** [intersection_error] is an upper bound on the distance from the intersection point
    returned by [get_intersection] to the true intersection point. Equal to
    [8 * dbl_error] radians. *)
val intersection_error : S1_angle.t

(** [intersection_merge_radius] can be used as a snap radius to ensure that edges
    displaced by up to [intersection_error] are merged back together. Equal to
    [2 * intersection_error]. *)
val intersection_merge_radius : S1_angle.t
