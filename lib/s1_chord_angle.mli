(** Distances on the unit sphere stored as squared chord length.

    A chord angle is the angle subtended at the center of the unit sphere by the straight
    line segment (the chord) connecting two points on the sphere. Internally it is stored
    as the squared chord length, a value in [0, 4]. Two advantages follow:

    - Comparing and ordering chord angles reduces to comparing a single double, with no
      trigonometric calls.
    - Constructing a chord angle from two unit-length points requires only a subtraction
      and a dot product.

    Unlike {!S1_angle.t}, a chord angle can only represent values in [0, pi] radians.
    Prefer this type when many distances must be computed and compared; prefer
    {!S1_angle.t} for a single angle or for exact trigonometric operations.

    {2 Accuracy near pi}

    Accuracy degrades as the angle approaches pi radians. Three distinct sources matter:

    - {i Representational}: how accurately chord angles near pi can be encoded as a
      squared length.
    - {i Conversion}: precision lost when converting from {!S1_angle.t}.
    - {i Measurement}: accuracy of constructing a chord angle from two near-antipodal
      points (the dominant source in practice).

    For the measurement error, let the true angle between two points be (pi -. x) radians.
    Then:

    [{| dx = min(1e-15 /. tan(x /. 2.), sqrt(2e-15)) |}]

    (approximately [min(2e-15 /. x, sqrt(2e-15))] when x is small).

    On the Earth's surface (radius 6371 km), worst-case measurement errors:

    {v
     Accuracy:             Unless antipodal to within:
     ---------             ---------------------------
     6.4 nanometers        10,000 km (90 degrees)
     1 micrometer          81.2 kilometers
     1 millimeter          81.2 meters
     1 centimeter          8.12 meters
     28.5 centimeters      28.5 centimeters
    v}

    The maximum distance between adjacent representable chord-angle values is 13.5 cm on
    Earth, and the worst-case rounding error is 9.5 cm. *)
open Core

[@@@zero_alloc all]

type t : float64 [@@deriving sexp_of, unboxed_option { sentinel = true }]

val%template sexp_of_t : t -> Sexp.t @ m
[@@alloc a @ m = (heap @ global, stack @ local)] [@@zero_alloc ignore]

(** {1 Constants} *)

(** Relative error incurred when summing two chord angles via {!add}. The absolute error
    is [length2 result *. relative_sum_error]. *)
val relative_sum_error : float#

(** {1 Constructors} *)

(** The zero chord angle (length2 = 0). *)
val zero : t

(** A chord angle of 90 degrees (length2 = 2). *)
val right : t

(** A chord angle of 180 degrees (length2 = 4). This is the maximum finite chord angle. *)
val straight : t

(** A chord angle larger than any finite chord angle. The only valid operations on
    [infinity] are comparisons, angle conversions, and {!successor} / {!predecessor};
    arithmetic and trigonometry are undefined. *)
val infinity : t

(** A chord angle smaller than {!zero} (length2 < 0). The only valid operations on
    [negative] are comparisons, angle conversions, and {!successor} / {!predecessor};
    arithmetic and trigonometry are undefined. *)
val negative : t

(** [of_angle a] builds the chord angle whose underlying S1Angle is [a]. Angles outside
    [0, pi] are handled as follows: [S1_angle.infinity] maps to {!infinity}, negative
    angles map to {!negative}, and finite angles larger than pi are clamped to
    {!straight}. This call is relatively expensive (one sine); structure code so
    conversion happens only at the boundaries. *)
val of_angle : S1_angle.t -> t

(** [of_length2 r2] builds a chord angle directly from a squared chord length. The
    argument is clamped to at most 4.0 to absorb roundoff; it must be non-negative. *)
val of_length2 : float# -> t

(** {2 Convenience constructors}

    Each of the following is equivalent to converting to {!S1_angle.t} first and then
    calling {!of_angle}. *)

val of_radians : float# -> t
val of_degrees : float# -> t
val of_e5 : int -> t
val of_e6 : int -> t
val of_e7 : int -> t

(** [fast_upper_bound_from a] is a cheap chord-angle upper bound for [a]: the result
    [u] satisfies [to_angle u >= a]. Faster than {!of_angle} (no [sin]) but the bound
    is loose - within about 1% of the true chord-angle for distances up to roughly
    3100 km on Earth's surface. Useful as a coarse pruning bound before a precise
    distance computation. *)
val fast_upper_bound_from : S1_angle.t -> t

(** {1 Accessors} *)

(** [length2 t] returns the squared chord length. Most callers should not need this. *)
val length2 : t -> float#

(** [to_angle t] converts [t] to an equivalent {!S1_angle.t} using
    [2 * asin(sqrt(length2) / 2)]. {!infinity} maps to [S1_angle.infinity]; {!negative}
    maps to an unspecified negative angle. This uses trigonometry and should be avoided in
    inner loops. *)
val to_angle : t -> S1_angle.t

(** [radians t] is [S1_angle.radians (to_angle t)]. Same cost considerations. *)
val radians : t -> float#

(** [degrees t] is [S1_angle.degrees (to_angle t)]. Same cost considerations. *)
val degrees : t -> float#

(** {1 Predicates} *)

val is_zero : t -> bool
val is_negative : t -> bool
val is_infinity : t -> bool

(** [is_special t] is [true] iff [t] is {!negative} or {!infinity}. Arithmetic and
    trigonometric operations require [not (is_special t)]. *)
val is_special : t -> bool

(** [is_valid t] is [true] iff [length2 t] is in [0, 4] or [t] is special. *)
val is_valid : t -> bool

(** {1 Successor / Predecessor} *)

(** [successor t] is the smallest representable chord angle strictly greater than [t].
    Useful for converting a strict comparison into a non-strict one. Special cases:
    [successor negative = zero], [successor straight = infinity],
    [successor infinity = infinity]. *)
val successor : t -> t

(** [predecessor t] is the largest representable chord angle strictly less than [t].
    Special cases: [predecessor infinity = straight], [predecessor zero = negative],
    [predecessor negative = negative]. *)
val predecessor : t -> t

(** {1 Arithmetic} *)

(** [add a b] returns the chord angle whose underlying S1Angle is the sum of the S1Angles
    of [a] and [b], clamped to [0, pi]. Much cheaper than converting to S1Angles; requires
    only one square root plus a few adds and multiplies.

    Requires [not (is_special a)] and [not (is_special b)]. *)
val add : t -> t -> t

(** [sub a b] returns the chord angle whose underlying S1Angle is the difference of the
    S1Angles of [a] and [b], clamped to [0, pi]. Formulated with two square roots to avoid
    catastrophic cancellation when [a] and [b] are nearly equal.

    Requires [not (is_special a)] and [not (is_special b)]. *)
val sub : t -> t -> t

(** {1 Trigonometry}

    These call sites are cheaper and more accurate than converting to {!S1_angle.t} first,
    and never allocate. All require [not (is_special t)]. *)

(** [sin t] is the sine of the angle. *)
val sin : t -> float#

(** [sin2 t] is [(sin t) *. (sin t)], computed more efficiently than squaring {!sin}. *)
val sin2 : t -> float#

(** [cos t] is the cosine of the angle. *)
val cos : t -> float#

(** [tan t] is [sin t /. cos t]. *)
val tan : t -> float#

(** {1 Error bounds} *)

(** [plus_error t e] adds [e] (positive or negative) to the squared chord length and
    clamps the result to [0, 4]. [e] is intended to be the output of one of the error
    bound functions below. If [t] is {!negative} or {!infinity}, [t] is returned
    unchanged. *)
val plus_error : t -> float# -> t

(** [max_point_error t] is the maximum error in [length2 t] when [t] was constructed from
    two near-unit-length {!S2_point.t} values. It accounts for rounding in the squared
    distance plus the fact that the input norms may differ from 1 by up to [2 *. epsilon]. *)
val max_point_error : t -> float#

(** [max_angle_error t] is the maximum error in [length2 t] when [t] was constructed via
    the {!S1_angle.t} conversion. *)
val max_angle_error : t -> float#

(** {1 Comparison} *)

val compare : t -> t -> int
val equal : t -> t -> bool
