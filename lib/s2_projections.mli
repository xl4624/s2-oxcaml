(** Map projections between the unit sphere and the 2D plane.

    Mirrors C++ [S2::Projection] (in [s2/s2projections.h]) and Go [Projection]. The
    upstream C++ uses a virtual class hierarchy; here we encode the two concrete
    projections ([PlateCarreeProjection] and [MercatorProjection]) as a single tagged
    record, dispatching on {!kind}. This keeps the API flat and avoids the
    first-class-module indirection while still supporting both projections uniformly.

    The plate carree projection maps the sphere to [(longitude, latitude)] pairs, with a
    configurable scale. The Mercator projection maps longitude the same way but distorts
    latitude by [0.5 * log((1+sin phi)/(1-sin phi))], yielding an unbounded y range (poles
    are at +/-infinity).

    Note that [(x, y)] coordinates are ordered with x = longitude and y = latitude,
    matching the C++/Go convention. *)

open Core

[@@@zero_alloc all]

(** Which concrete projection a {!t} represents. *)
type kind =
  | Plate_carree
  | Mercator

type t : immediate & float64 & float64 & float64

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [plate_carree ~x_scale] constructs a plate carree projection where the x coordinate
    (longitude) spans [[-x_scale, x_scale]] and the y coordinate (latitude) spans
    [[-x_scale/2, x_scale/2]]. For example, [x_scale = #180.0] gives the degree-valued
    range. With [x_scale = pi] the range is [[-pi, pi]] / [[-pi/2, pi/2]] (radians). *)
val plate_carree : x_scale:float# -> t

(** [mercator ~max_x] constructs a spherical Mercator projection where x corresponds to
    longitude in [[-max_x, max_x]] and y corresponds to a function of latitude that can
    take any real value. *)
val mercator : max_x:float# -> t

(** {1 Accessors} *)

val kind : t -> kind

(** {1 Core operations} *)

(** Convert a point on the sphere to a projected 2D point. *)
val project : t -> S2_point.t -> R2_point.t

(** Convert a projected 2D point back to a point on the sphere. If the projection wraps
    along a given axis, any real number is accepted for that coordinate. *)
val unproject : t -> R2_point.t -> S2_point.t

(** [from_latlng t ll] is equivalent to [project t (S2_latlng.to_point ll)] but more
    efficient. *)
val from_latlng : t -> S2_latlng.t -> R2_point.t

(** [to_latlng t p] is equivalent to [S2_latlng.of_point (unproject t p)] but more
    efficient. *)
val to_latlng : t -> R2_point.t -> S2_latlng.t

(** [interpolate t ~f a b] returns the point obtained by interpolating the given fraction
    of the distance along the 2D line from [a] to [b]. Fractions outside [[0, 1]]
    extrapolate. Both projections use linear interpolation in the 2D projected space. *)
val interpolate : t -> f:float# -> R2_point.t -> R2_point.t -> R2_point.t

(** {1 Wrapping} *)

(** [wrap_distance t] is the coordinate wrapping distance along each axis. A zero
    component means that axis does not wrap. For both built-in projections this is
    [(2 * x_scale, 0)]: x wraps with period [2 * x_scale] and y does not wrap. *)
val wrap_distance : t -> R2_point.t

(** [wrap_destination t ~a ~b] returns [b] potentially wrapped along any wrapping axis so
    that the segment from [a] to the result is the shortest edge. For example, with a
    plate carree projection scaled in degrees,
    [wrap_destination t ~a:(170, 20) ~b:(-170, 20)] returns [(190, 20)], shortening the
    x-span from 340 to 20. *)
val wrap_destination : t -> a:R2_point.t -> b:R2_point.t -> R2_point.t
