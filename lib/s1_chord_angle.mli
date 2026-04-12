(** S1_chord_angle represents the angle subtended by a chord (the straight line segment
    connecting two points on the sphere). Its representation makes it very efficient for
    computing and comparing distances, but unlike {!S1_angle.t} it is only capable of
    representing angles between 0 and pi radians. It is intended for applications where
    many angles need to be computed and compared, otherwise it is simpler to use
    {!S1_angle.t}.

    S1_chord_angle loses accuracy as the angle approaches pi radians. There are three
    error sources: representational error (how accurately angles near pi can be
    represented), conversion error (precision lost converting from {!S1_angle.t}), and
    measurement error (accuracy of constructing from two points near antipodal). All
    differ by a small constant factor.

    For the measurement error (the largest), let the angle between two points be (pi -. x)
    radians. The relative error in the chord length is roughly [5e-16]. Converting to an
    equivalent angle gives:

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

    Representational and conversion errors are smaller. The maximum distance between
    adjacent representable values is 13.5 cm. Worst-case rounding error is 9.5 cm. *)
open Core

[@@@zero_alloc all]

type t : float64 [@@deriving sexp_of, unboxed_option { sentinel = true }]

val%template sexp_of_t : t -> Sexp.t @ m
[@@alloc a @ m = (heap @ global, stack @ local)] [@@zero_alloc ignore]

(** {1 Constants} *)

(** Maximum relative error when summing two chord angles. *)
val relative_sum_error : float#

(** {1 Constructors} *)

(** The zero chord angle. *)
val zero : t

(** A chord angle of 90 degrees. *)
val right : t

(** A chord angle of 180 degrees. This is the maximum finite chord angle. *)
val straight : t

(** A chord angle larger than any finite chord angle. The only valid operations on
    [infinity] are comparisons, angle conversions, and [successor]/[predecessor]. *)
val infinity : t

(** A chord angle smaller than [zero]. The only valid operations on [negative] are
    comparisons, angle conversions, and [successor]/[predecessor]. *)
val negative : t

(** Construct from an {!S1_angle.t}. Angles outside [0, pi] are handled as follows:
    [S1_angle.infinity] maps to [infinity], negative angles map to [negative], and finite
    angles larger than pi map to [straight]. *)
val of_angle : S1_angle.t -> t

(** Construct from an unboxed squared chord length. The argument is clamped to at most
    4.0. *)
val of_length2 : float# -> t

(** Convenience constructors via {!S1_angle.t}. *)

val of_radians : float# -> t
val of_degrees : float# -> t
val of_e5 : int -> t
val of_e6 : int -> t
val of_e7 : int -> t

(** {1 Accessors} *)

(** The squared chord length. *)
val length2 : t -> float#

(** Convert to {!S1_angle.t}. [infinity] becomes [S1_angle.infinity], [negative] becomes a
    negative angle. *)
val to_angle : t -> S1_angle.t

(** Convert to radians via {!to_angle}. *)
val radians : t -> float#

(** Convert to degrees via {!to_angle}. *)
val degrees : t -> float#

(** {1 Predicates} *)

val is_zero : t -> bool
val is_negative : t -> bool
val is_infinity : t -> bool

(** [true] if [negative] or [infinity]. *)
val is_special : t -> bool

(** [true] if length2 is in [0, 4] or the angle is special. *)
val is_valid : t -> bool

(** {1 Successor / Predecessor} *)

(** The smallest representable chord angle larger than [t]. Special cases:
    [negative -> zero], [straight -> infinity], [infinity -> infinity]. *)
val successor : t -> t

(** The largest representable chord angle smaller than [t]. Special cases:
    [infinity -> straight], [zero -> negative], [negative -> negative]. *)
val predecessor : t -> t

(** {1 Arithmetic} *)

(** Add two chord angles (as the corresponding S1Angles, clamped to [0, pi]). Both must be
    non-special. *)
val add : t -> t -> t

(** Subtract chord angles (as the corresponding S1Angles, clamped to [0, pi]). Both must
    be non-special. *)
val sub : t -> t -> t

(** {1 Trigonometry} *)

(** Sine of the angle. More efficient than converting to S1_angle first. *)
val sin : t -> float#

(** Square of the sine. More efficient than [sin]. *)
val sin2 : t -> float#

(** Cosine of the angle. *)
val cos : t -> float#

(** Tangent of the angle. *)
val tan : t -> float#

(** {1 Error bounds} *)

(** Adjust by [error] (positive or negative), clamping to [0, 4]. Special angles are
    unchanged. *)
val plus_error : t -> float# -> t

(** Maximum error in [length2] for a chord angle constructed from two unit-length points. *)
val max_point_error : t -> float#

(** Maximum error in [length2] for a chord angle constructed from an S1Angle. *)
val max_angle_error : t -> float#

(** {1 Comparison} *)

val compare : t -> t -> int
val equal : t -> t -> bool

(** {1 Optional Chord Angle}

    An optional chord angle representation that avoids allocating an [option] wrapper.
    Uses a NaN sentinel to represent absence. *)
module Option : sig
  include module type of Option

  val%template sexp_of_t : t -> Sexp.t @ m
  [@@alloc a @ m = (heap @ global, stack @ local)] [@@zero_alloc ignore]
end
