open Core

(** A one-dimensional angle (as opposed to a two-dimensional solid angle).

    The internal representation is a double-precision value in radians, so conversion to
    and from radians is exact. Conversions between E5, E6, E7, and degrees are not always
    exact; for example, [of_degrees 3.1] differs from [of_e6 3100000].

    For any integer [n] in range, the following identities hold:

    {v
      of_degrees n = of_e6 (1000000 * n)
      of_degrees n = of_e7 (10000000 * n)
      of_e6 n      = of_e7 (10 * n)
    v}

    These do not hold for E5.

    Exact degree-to-radian conversions include [of_degrees 180 = of_radians pi] and
    [of_degrees (45 * k) = of_radians (k *. pi /. 4.)] for [k = 0..8], but
    [of_degrees 60 <> of_radians (pi /. 60.)]. Similarly, [degrees (of_degrees 60) <> 60].
    When testing equality, allow for numerical errors or compare discrete E5/E6/E7 values. *)

[@@@zero_alloc all]

type t : float64 [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val pp : Format.formatter -> t -> unit [@@zero_alloc ignore]
val to_string : t -> string [@@zero_alloc ignore]

(** {1 Constructors} *)

(** The zero angle. *)
val zero : t

(** An angle larger than any finite angle. *)
val infinity : t

(** Construct an angle from its measure in radians. *)
val of_radians : float# -> t

(** Construct an angle from its measure in degrees. *)
val of_degrees : float# -> t

(** Construct an angle from hundred-thousandths of degrees (E5 representation). *)
val of_e5 : int -> t

(** Construct an angle from millionths of degrees (E6 representation). *)
val of_e6 : int -> t

(** Construct an angle from ten-millionths of degrees (E7 representation). *)
val of_e7 : int -> t

(** [of_unsigned_e6 u] reinterprets [u] (mod [2^32]) as unsigned, then as signed [int32]
    and scales like [of_e6]. *)
val of_unsigned_e6 : int -> t

(** [of_unsigned_e7 u]: same as [of_unsigned_e6] for the E7 scale. *)
val of_unsigned_e7 : int -> t

(** {1 Accessors} *)

(** Return the angle in radians. *)
val radians : t -> float#

(** Return the angle in degrees. *)
val degrees : t -> float#

(** Return [Some e5] representation (degrees * 1e5, rounded to nearest integer).
    The angle in degrees must be in the interval (-180, 180]. *)
val e5 : t -> int option
[@@zero_alloc ignore]

(** [e5_exn t] is like [e5] but raises if the angle is out of range. *)
val e5_exn : t -> int
[@@zero_alloc ignore]

(** Return [Some e6] representation (degrees * 1e6, rounded to nearest integer).
    The angle in degrees must be in the interval (-180, 180]. *)
val e6 : t -> int option
[@@zero_alloc ignore]

(** [e6_exn t] is like [e6] but raises if the angle is out of range. *)
val e6_exn : t -> int
[@@zero_alloc ignore]

(** Return [Some e7] representation (degrees * 1e7, rounded to nearest integer).
    The angle in degrees must be in the interval (-180, 180]. *)
val e7 : t -> int option
[@@zero_alloc ignore]

(** [e7_exn t] is like [e7] but raises if the angle is out of range. *)
val e7_exn : t -> int
[@@zero_alloc ignore]

(** {1 Predicates} *)

val is_inf : t -> bool
val is_zero : t -> bool

(** {1 Arithmetic} *)

(** Absolute value of the angle. *)
val abs : t -> t

(** Negate the angle. *)
val neg : t -> t

(** Sum of two angles. *)
val add : t -> t -> t

(** Difference of two angles. *)
val sub : t -> t -> t

(** Multiply angle by a scalar. *)
val mul : t -> float# -> t

(** Divide angle by a scalar. *)
val div : t -> float# -> t

(** Ratio of two angles (a / b as a float). *)
val ratio : t -> t -> float#

(** {1 Trigonometry} *)

val sin : t -> float#
val cos : t -> float#
val tan : t -> float#

(** Sine and cosine together. *)
val sin_cos : t -> #(float# * float#)

(** {1 Normalization} *)

(** Return the angle normalized to the range (-pi, pi] (i.e., (-180, 180] degrees). *)
val normalized : t -> t

(** {1 Comparison} *)

val compare : t -> t -> int
val equal : t -> t -> bool
