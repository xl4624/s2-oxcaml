(** Convert between spherical geodesic edges and planar edges in a 2D projection.

    Given an edge in some 2D projection (e.g. Mercator), an {!t} converts the edge into a
    chain of spherical geodesic edges such that the maximum distance between the original
    edge and the geodesic edge chain is at most [tolerance]. Similarly, it can convert a
    spherical geodesic edge into a chain of edges in a given 2D projection such that the
    maximum distance between the geodesic edge and the chain of projected edges is at most
    [tolerance]. *)

open Core

type t : (immediate & float64 & float64 & float64) & float64

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constants} *)

(** Minimum supported tolerance (less than 1 micrometer on the Earth's surface, but still
    much larger than expected projection and interpolation errors). *)
val min_tolerance : S1_angle.t

(** {1 Constructors} *)

(** [create ~projection ~tolerance] constructs a tessellator. [tolerance] is clamped up to
    {!min_tolerance}. *)
val create : projection:S2_projections.t -> tolerance:S1_angle.t -> t

(** {1 Tessellation} *)

(** [project t points] tessellates the polyline [points] (interpreted as a chain of
    spherical geodesic edges) into a chain of planar vertices in the projection. If
    [points] is a single vertex, the result is the single projected point. If the
    projection wraps along some axis, successive output vertices are as close as possible
    to their predecessor, which may yield coordinates outside the canonical range. *)
val project : t -> S2_point.t array -> R2_point.t array

(** [unproject t points] tessellates the planar polyline [points] (in the projection) into
    a chain of spherical geodesic edges, returning the corresponding vertices on the
    sphere. Note: for loops, the first and last vertex may not be exactly equal when
    coordinate wrapping is involved. *)
val unproject : t -> R2_point.t array -> S2_point.t array
