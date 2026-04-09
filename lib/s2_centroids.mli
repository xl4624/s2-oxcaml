(** Centroid computations for spherical edges and triangles.

    There are several notions of the "centroid" of a triangle. The planar centroid is the
    centroid of the ordinary (non-spherical) triangle. The surface centroid is the
    intersection of the three medians of the spherical triangle (equal to the planar
    centroid projected onto the sphere). The true centroid (mass centroid) is the surface
    integral of (x,y,z) over the triangle divided by area.

    The true centroid is best for most purposes because it behaves linearly: if you split
    a triangle into pieces and average their centroids (weighted by area), you get the
    centroid of the original triangle. *)

open Core

[@@@zero_alloc all]

(** [planar_centroid a b c] returns the centroid of the planar triangle ABC. This can be
    normalized to unit length to obtain the "surface centroid" of the corresponding
    spherical triangle, i.e. the intersection of the three medians. For large spherical
    triangles the surface centroid may be far from the intuitive "center". *)
val planar_centroid : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t

(** [true_centroid a b c] returns the true centroid of the spherical triangle ABC
    multiplied by the signed area. The reasons for multiplying by the signed area are
    (1) this is the quantity that needs to be summed to compute the centroid of a union or
        difference of triangles, and (2) it is easier to calculate this way. All points
        must have unit length.

    Returns the zero vector if the triangle is degenerate. *)
val true_centroid : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t

(** [edge_true_centroid a b] returns the true centroid of the spherical geodesic edge AB
    multiplied by the length of the edge. The true centroid of a collection of edges may
    be computed by summing the result of this function for each edge.

    Note that the planar centroid of a geodesic edge is simply [0.5 * (a + b)], while the
    surface centroid is [(a + b).normalize()]. Neither of these is appropriate for
    computing the centroid of a collection of edges (such as a polyline).

    Returns the zero vector if the edge is degenerate (including antipodal endpoints). *)
val edge_true_centroid : S2_point.t -> S2_point.t -> S2_point.t
