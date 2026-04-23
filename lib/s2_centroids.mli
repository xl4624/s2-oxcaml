(** Centroid computations for spherical edges and triangles.

    There are three different notions of the centroid of a triangle ABC on the sphere:

    - The {e planar} centroid is the arithmetic mean [(A+B+C)/3] of the three vertices,
      treated as ordinary points in R3.
    - The {e surface} centroid is the intersection of the three spherical medians. It
      equals the planar centroid projected onto the sphere.
    - The {e true} centroid (mass centroid) is the surface integral of [(x,y,z)] over the
      triangle divided by the triangle's area; it is the point the triangle would rotate
      around if spinning freely.

    Most callers want the true centroid because it behaves linearly under area-weighted
    averaging: splitting a triangle into pieces and averaging their centroids (weighted by
    area) recovers the centroid of the original triangle. The planar and surface variants
    do not. The surface centroid can also land far from the intuitive "center" of a large
    spherical triangle, so it is rarely the right choice. *)

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
