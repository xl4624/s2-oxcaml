(** Points on the unit sphere expressed as a pair of latitude and longitude.

    Latitude is measured from the equator toward the poles and lies in [[-pi/2, pi/2]]
    radians. Longitude is measured from the prime meridian and lies in [[-pi, pi]]
    radians. A valid latlng satisfies both bounds; see {!is_valid} and {!normalized} for
    how out-of-range inputs are handled.

    This type models spherical geometry as pure mathematics. Earth-specific conversions
    such as easting/northing belong in separate modules.

    {2 Typical usage}

    Build a value with {!of_degrees}, {!of_radians}, or one of the [of_eN] constructors;
    convert to/from a {!S2_point.t} with {!to_point} / {!of_point}; measure distance with
    {!distance} (Haversine formula, accurate for small distances).

    {2 Limitations}

    The [FromUnsignedE6] / [FromUnsignedE7] constructors are not exposed. *)
open Core

[@@@zero_alloc all]

type t : float64 & float64 [@@deriving sexp_of, unboxed_option { sentinel = true }]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val pp : Format.formatter -> t -> unit [@@zero_alloc ignore]
val to_string : t -> string [@@zero_alloc ignore]

(** [to_string_in_degrees t] returns the latitude and longitude of [t] (after
    {!normalized}) in degrees, separated by a comma, each formatted to six digits after
    the decimal point. For example,
    [to_string_in_degrees (of_degrees ~lat:#94.518 ~lng:#150.300)] returns
    ["90.000000,150.300000"] (the [94.518] latitude is clamped to [90] by normalization). *)
val to_string_in_degrees : t -> string
[@@zero_alloc ignore]

(** {1 Constructors} *)

(** [create ~lat ~lng] builds the latlng with the given angles. The inputs may lie outside
    the {!is_valid} range; see {!normalized} if you need to bring them back. *)
val create : lat:S1_angle.t -> lng:S1_angle.t -> t

(** The latlng with both components zero; useful for initialization. *)
val zero : t

(** A sentinel latlng with lat = pi and lng = 2*pi; always fails {!is_valid}.
    {!normalized} returns this value when given a non-finite input. *)
val invalid : t

(** [of_radians ~lat ~lng] builds a latlng directly from radian values without going
    through {!S1_angle.t}. *)
val of_radians : lat:float# -> lng:float# -> t

(** [of_degrees ~lat ~lng] builds a latlng from degree values. *)
val of_degrees : lat:float# -> lng:float# -> t

(** {2 E5 / E6 / E7 constructors}

    Integer representations where the value is the angle in degrees times [1eN]. The safe
    variants return [Option.none] if [lat] is not in the [-90 * 10^N, 90 * 10^N] range;
    the [_exn] variants raise via {!Core.raise_s} on the same condition. *)

val of_e5 : lat:int -> lng:int -> Option.t
val of_e5_exn : lat:int -> lng:int -> t
val of_e6 : lat:int -> lng:int -> Option.t
val of_e6_exn : lat:int -> lng:int -> t
val of_e7 : lat:int -> lng:int -> Option.t
val of_e7_exn : lat:int -> lng:int -> t

(** [of_point p] recovers the latitude and longitude of a 3D direction vector [p]. The
    vector need not be unit length; only its direction matters. *)
val of_point : S2_point.t -> t

(** {1 Accessors} *)

(** [lat t] returns the latitude of [t] as an {!S1_angle.t}. *)
val lat : t -> S1_angle.t

(** [lng t] returns the longitude of [t] as an {!S1_angle.t}. *)
val lng : t -> S1_angle.t

(** [latitude p] computes the latitude of a direction vector [p]. Uses [atan2] rather than
    [asin] because [p] need not be unit length and [atan2] is more accurate near the
    poles. *)
val latitude : S2_point.t -> S1_angle.t

(** [longitude p] computes the longitude of a direction vector [p]. *)
val longitude : S2_point.t -> S1_angle.t

(** {1 Predicates} *)

(** [is_valid t] is [true] iff [|lat| <= pi/2], [|lng| <= pi], and both components are
    finite. *)
val is_valid : t -> bool

(** {1 Normalization} *)

(** [normalized t] clamps the latitude to [[-pi/2, pi/2]] and wraps the longitude into
    [[-pi, pi]] by IEEE remainder. Returns {!invalid} if either component is not finite. *)
val normalized : t -> t

(** {1 Conversion} *)

(** [to_point t] returns the unit-length 3D vector corresponding to [t]. The maximum error
    is [1.5 * epsilon]; this does not include any error from converting degrees or
    E5/E6/E7 values to radians. Both components must be finite. *)
val to_point : t -> S2_point.t

(** {1 Distance} *)

(** [distance a b] returns the surface angular distance between [a] and [b] using the
    Haversine formula. Accurate for small distances; accuracy degrades to roughly 8 digits
    near antipodal pairs (about 10 cm on Earth). For full precision across all distances,
    convert both to {!S2_point.t} and call {!S2_point.distance}. Both [a] and [b] must be
    {!normalized}. *)
val distance : t -> t -> S1_angle.t

(** {1 Arithmetic}

    These operators treat a latlng as a raw 2-vector of radians; the result is {b not}
    re-normalized. Use {!normalized} if you need a valid latlng afterwards. *)

(** [add a b] is [(a.lat + b.lat, a.lng + b.lng)]. *)
val add : t -> t -> t

(** [sub a b] is [(a.lat - b.lat, a.lng - b.lng)]. *)
val sub : t -> t -> t

(** [mul t k] is [(k * t.lat, k * t.lng)]. *)
val mul : t -> float# -> t

(** {1 Comparison} *)

(** [approx_equal ~max_error a b] is [true] iff each coordinate differs by at most
    [max_error] radians. Because this compares the rectangular coordinates rather than the
    spherical points, two nearby points close to the poles can still compare unequal. Use
    {!distance} for spherical-closeness comparisons. Default tolerance is [1e-15] when
    [max_error] is [none]. *)
val approx_equal : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool
[@@zero_alloc]

val equal : t -> t -> bool
