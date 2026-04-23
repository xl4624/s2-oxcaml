(** Convert between spherical geodesic edges and planar edges in a 2D projection.

    A geodesic on the sphere and its straight-line image in a projection (Mercator, Plate
    Carree, ...) generally disagree. A tessellator subdivides one side of the pair until
    the maximum distance between the curve and its approximating polyline drops below a
    user-supplied [tolerance].

    Given a {!t} and a polyline you can either

    - {!project} a spherical polyline (interpreted as a chain of geodesics) into a planar
      polyline in the projection's coordinate system, or
    - {!unproject} a planar polyline in the projection into a spherical polyline of
      geodesics.

    Wrapping projections (for example anything built on longitude) keep every successive
    output vertex as close as possible to the previous one, which can yield coordinates
    outside the canonical range - for example [(0:170)] then [(0:-170)] tessellates to
    vertices whose longitudes pass through [190] rather than jumping back across the
    dateline. *)

open Core

[@@@zero_alloc all]

(** An immutable tessellator bound to a specific projection and tolerance. *)
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
[@@zero_alloc ignore]

(** [unproject t points] tessellates the planar polyline [points] (in the projection) into
    a chain of spherical geodesic edges, returning the corresponding vertices on the
    sphere. Note: for loops, the first and last vertex may not be exactly equal when
    coordinate wrapping is involved. *)
val unproject : t -> R2_point.t array -> S2_point.t array
[@@zero_alloc ignore]
