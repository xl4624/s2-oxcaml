open Core

(** A one-dimensional angle (as opposed to a two-dimensional solid angle).

    Angles are stored as radians in an unboxed [float#]. Arithmetic, comparison, and
    trigonometry operate on that internal representation. Construct angles with
    [of_radians], [of_degrees], or one of the fixed-point [of_eN] variants for integer
    degrees scaled by [10^N].

    {1 Precision}

    Conversion between the internal radians representation and radians is exact.
    Conversions between radians and degrees, or between degrees and E5/E6/E7, generally
    are not. For any integer [n] small enough to be represented exactly, the following
    identities hold:

    {v
      of_degrees n = of_e6 (1000000 * n)
      of_degrees n = of_e7 (10000000 * n)
      of_e6 n      = of_e7 (10 * n)
    v}

    These do not hold for E5.

    Exact degree-to-radian conversions include [of_degrees 180 = of_radians pi] and
    [of_degrees (45 * k) = of_radians (k *. pi /. 4.)] for [k = 0..8], but
    [of_degrees 60 <> of_radians (pi /. 60.)] and [degrees (of_degrees 60) <> 60]. When
    testing equality, allow for small numerical error or compare discrete E5/E6/E7 values
    instead.

    The angle between two points or two latlngs is exposed as {!S2_point.distance} and
    {!S2_latlng.distance} rather than as constructors here, since this module sits below
    both of those in the dependency order.

    {1 Limitations}

    The [Coder] interface for serialization is not currently provided. *)

[@@@zero_alloc all]

type t : float64 [@@deriving sexp_of]

val%template sexp_of_t : t -> Sexp.t @ m
[@@alloc a @ m = (heap @ global, stack @ local)] [@@zero_alloc ignore]

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

(** [of_unsigned_e6 u] interprets the low 32 bits of [u] as an unsigned integer, then
    reinterprets that bit pattern as a signed [int32] before applying [of_e6]. This
    matches the convention of fixed-width unsigned proto fields where large values
    round-trip as negative angles. *)
val of_unsigned_e6 : int -> t

(** [of_unsigned_e7 u] is [of_unsigned_e6] for the E7 scale. *)
val of_unsigned_e7 : int -> t

(** {1 Optional Integer}

    An unboxed optional integer used by the [eN] accessors to report overflow without
    allocating. [Int.min_value] serves as the [none] sentinel, so the valid range of
    [some] is [Int.min_value + 1 .. Int.max_value]. *)
module Int_option : sig
  type value := int
  type t : immediate = int

  val none : t
  val some : value -> t
  val is_none : t -> bool
  val is_some : t -> bool
  val value_exn : t -> value

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool
      val unsafe_value : t -> value
    end
  end
end

(** {1 Accessors} *)

(** [radians t] returns the angle in radians. *)
val radians : t -> float#

(** [degrees t] returns the angle in degrees. *)
val degrees : t -> float#

(** [e5 t] returns the E5 representation ([1e5 * degrees t], rounded half-to-even), or
    [Int_option.none] if rounding would overflow OCaml's [int]. [t] should be in
    [(-180, 180]] degrees; {!normalized} ensures this. *)
val e5 : t -> Int_option.t

(** [e5_exn t] is like [e5] but raises when the rounded value would overflow. *)
val e5_exn : t -> int

(** [e6 t] returns the E6 representation ([1e6 * degrees t], rounded half-to-even), or
    [Int_option.none] if rounding would overflow OCaml's [int]. [t] should be in
    [(-180, 180]] degrees. *)
val e6 : t -> Int_option.t

(** [e6_exn t] is like [e6] but raises when the rounded value would overflow. *)
val e6_exn : t -> int

(** [e7 t] returns the E7 representation ([1e7 * degrees t], rounded half-to-even), or
    [Int_option.none] if rounding would overflow OCaml's [int]. [t] should be in
    [(-180, 180]] degrees. *)
val e7 : t -> Int_option.t

(** [e7_exn t] is like [e7] but raises when the rounded value would overflow. *)
val e7_exn : t -> int

(** {1 Predicates} *)

(** [is_inf t] returns true iff [t] is a positive or negative infinite angle. *)
val is_inf : t -> bool

(** [is_zero t] returns true iff [t] is exactly zero radians (positive or negative zero). *)
val is_zero : t -> bool

(** {1 Arithmetic} *)

(** [abs t] returns the absolute value of [t]. *)
val abs : t -> t

(** [neg t] returns [-t]. *)
val neg : t -> t

(** [add a b] returns [a + b]. No normalization is applied; use {!normalized} if you
    need a result in [(-pi, pi]]. *)
val add : t -> t -> t

(** [sub a b] returns [a - b]. No normalization is applied. *)
val sub : t -> t -> t

(** [mul t k] returns [k * t]. *)
val mul : t -> float# -> t

(** [div t k] returns [t / k]. *)
val div : t -> float# -> t

(** [ratio a b] returns [a / b] as a scalar in radians per radian (i.e. a plain [float#]). *)
val ratio : t -> t -> float#

(** {1 Trigonometry} *)

(** [sin t] returns [sin (radians t)]. *)
val sin : t -> float#

(** [cos t] returns [cos (radians t)]. *)
val cos : t -> float#

(** [tan t] returns [tan (radians t)]. *)
val tan : t -> float#

(** [sin_cos t] returns [#(sin t, cos t)] as an unboxed pair. *)
val sin_cos : t -> #(float# * float#)

(** {1 Normalization} *)

(** [normalized t] returns [t] reduced into the half-open range [(-pi, pi]]
    (equivalently [(-180, 180]] degrees). *)
val normalized : t -> t

(** {1 Comparison} *)

(** [compare a b] compares [a] and [b] by their radian values. *)
val compare : t -> t -> int

(** [equal a b] returns true iff [radians a] and [radians b] compare equal under IEEE
    float equality (so any NaN makes the result false). *)
val equal : t -> t -> bool
