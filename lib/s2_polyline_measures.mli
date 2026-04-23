(** Low-level length and centroid measures for polylines on the unit sphere.

    Each function takes an {!S2_point.t} array [v] and treats it as an open polyline whose
    edges are [(v.(0), v.(1)), ..., (v.(n-2), v.(n-1))]. No per-polyline object is
    constructed, which is why these helpers are convenient building blocks for the
    higher-level {!S2_polyline} / {!S2_lax_polyline} types or any custom carrier that
    exposes a vertex sequence. *)

open Core

[@@@zero_alloc all]

(** [length polyline] returns the total geodesic length of the polyline: the sum of the
    great-circle distances between consecutive vertices. Polylines with fewer than two
    vertices return {!S1_angle.zero}. *)
val length : S2_point.t array -> S1_angle.t

(** [centroid polyline] returns the true centroid of the polyline scaled by its length
    (see {!S2_centroids} for the definition). The result is not unit length, so callers
    that want the centroid's direction must normalise it.

    Scaling by length makes it easy to combine the centroids of several polylines: add
    their centroid vectors and divide by the total length at the end.

    Degenerate polylines (fewer than two vertices or every vertex equal to the first)
    return the zero vector. This is the correct value: the centroid is defined as a line
    integral over the polyline, which vanishes in the degenerate case. *)
val centroid : S2_point.t array -> S2_point.t
