(** Map projections between the unit sphere and the 2D plane.

    Two projections are supplied: {!plate_carree} (the "square plate" projection that maps
    sphere coordinates to [(longitude, latitude)] pairs at a configurable scale) and
    {!mercator} (the spherical Mercator, which stretches latitude by
    [0.5 * log((1 + sin phi) / (1 - sin phi))] and sends the poles to +/- infinity). Both
    share the same {!t} type so callers can hold a projection in a single variable and
    dispatch on {!kind} if needed.

    The 2D coordinates follow the graph convention (x horizontal, y vertical). That is the
    reverse of the usual (latitude, longitude) tuple ordering: the x coordinate is
    longitude and the y coordinate is latitude.

    Limitations:
    - Custom {!Projection} subclasses are not supported: only the two projections exposed
      here are available.
    - {!wrap_destination} wraps only along the x axis. The C++ default wraps along any
      axis with non-zero {!wrap_distance}; for both built-in projections the y wrap is
      zero, so the two implementations agree. Custom projections with a wrapping y axis
      would diverge but cannot be constructed here. *)

open Core

[@@@zero_alloc all]

(** Which concrete projection a {!t} represents. *)
type kind =
  | Plate_carree
  | Mercator

(** A projection value. The layout is immediate + three [float64] slots, so values of this
    type are allocation-free. *)
type t : immediate & float64 & float64 & float64

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [plate_carree ~x_scale] constructs a plate carree projection whose x coordinate
    (longitude) spans [[-x_scale, x_scale]] and whose y coordinate (latitude) spans
    [[-x_scale/2, x_scale/2]]. For example [x_scale = #180.0] gives a degree-valued range,
    [x_scale = Float_u.pi ()] gives a radian range of [[-pi, pi] x [-pi/2, pi/2]]. *)
val plate_carree : x_scale:float# -> t

(** [mercator ~max_x] constructs a spherical Mercator projection whose x coordinate
    (longitude) spans [[-max_x, max_x]]. The y coordinate encodes latitude and takes any
    real value; [y = +/- infinity] corresponds to the poles. *)
val mercator : max_x:float# -> t

(** {1 Accessors} *)

(** [kind t] returns the tag identifying which projection [t] represents. *)
val kind : t -> kind

(** {1 Core operations} *)

(** [project t p] returns the 2D projection of the sphere point [p]. *)
val project : t -> S2_point.t -> R2_point.t

(** [unproject t p] returns the sphere point that projects to [p]. Any real value is
    accepted for a coordinate along an axis that wraps; out-of-range coordinates along a
    non-wrapping axis are accepted but may produce points near the poles. *)
val unproject : t -> R2_point.t -> S2_point.t

(** [from_latlng t ll] is equivalent to [project t (S2_latlng.to_point ll)] but avoids the
    round-trip through Cartesian coordinates. *)
val from_latlng : t -> S2_latlng.t -> R2_point.t

(** [to_latlng t p] is equivalent to [S2_latlng.of_point (unproject t p)] but avoids the
    round-trip through Cartesian coordinates. *)
val to_latlng : t -> R2_point.t -> S2_latlng.t

(** [interpolate t ~f a b] returns the point at fraction [f] along the straight 2D line
    from [a] to [b]. Values outside [[0, 1]] extrapolate. Both built-in projections
    interpret edges as straight lines in the projected plane, so this is a simple linear
    interpolation. *)
val interpolate : t -> f:float# -> R2_point.t -> R2_point.t -> R2_point.t

(** {1 Wrapping} *)

(** [wrap_distance t] returns the per-axis wrapping period. A zero component means that
    axis does not wrap. Both built-in projections return [(2 * x_scale, 0)]: the x axis
    wraps with period [2 * x_scale] and the y axis does not wrap. *)
val wrap_distance : t -> R2_point.t

(** [wrap_destination t ~a ~b] returns [b] possibly translated by an integer multiple of
    {!wrap_distance} so that the segment from [a] to the returned point is the shortest
    wrapped edge. For example, with a plate carree projection in degrees,
    [wrap_destination t ~a:(170, 20) ~b:(-170, 20)] returns [(190, 20)], shrinking the x
    span from 340 to 20. Only the x axis is wrapped; y passes through unchanged. *)
val wrap_destination : t -> a:R2_point.t -> b:R2_point.t -> R2_point.t
