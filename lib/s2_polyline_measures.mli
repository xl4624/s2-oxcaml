(** Low-level measures for polylines on the sphere.

    These helpers work directly on an array of {!S2_point.t} vertices so that they can be
    reused by higher-level types (polylines, shapes, shape indices) once those are ported. *)

open Core

[@@@zero_alloc all]

(** [length polyline] returns the total geodesic length of the polyline, i.e. the sum of
    the distances between consecutive vertices. Returns {!S1_angle.zero} for polylines
    with fewer than two vertices. *)
val length : S2_point.t array -> S1_angle.t

(** [centroid polyline] returns the true centroid of the polyline multiplied by the length
    of the polyline (see {!S2_centroids} for details on centroids). The result is not unit
    length, so you may want to normalize it.

    Scaling by the polyline length makes it easy to compute the centroid of several
    polylines (by simply adding up their centroids).

    Returns the zero vector for degenerate polylines (e.g. fewer than two vertices, or all
    vertices equal). The result of this function is a line integral over the polyline,
    whose value is always zero in the degenerate case. *)
val centroid : S2_point.t array -> S2_point.t
