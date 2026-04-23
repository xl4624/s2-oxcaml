(** A vector in three-dimensional Euclidean space.

    Values of this type are the underlying representation of 3D points used throughout S2:
    unit-length instances are treated as points on the unit sphere, but most arithmetic
    operations do not require or preserve unit length. Use [normalize] when you need a
    unit vector.

    All arithmetic is component-wise IEEE 754 double precision; no rounding or range
    checks are performed. [angle] returns an [S1_angle.t] in [[0, pi]] using
    [atan2(|cross|, dot)], which stays accurate near [0] and near [pi] unlike [acos(dot)].

    Underflow-safety helpers are provided for vectors with very small components:
    [is_normalizable] and [ensure_normalizable] scale a vector so that squaring its
    largest component cannot underflow. See those functions for details.

    The type is unboxed, so values live in registers and no allocation occurs.

    {1 Limitations}

    Component-wise [Sqrt], [Floor], [Ceil], and [FRound] from the C++ [Vector3_d] base
    class are not exposed; add them if a caller needs them. *)

open Core

[@@@zero_alloc all]

type t : (float64 & float64 & float64) mod external_
[@@deriving unboxed_option { sentinel = true }]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val pp : Format.formatter -> t -> unit [@@zero_alloc ignore]
val to_string : t -> string [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [create ~x ~y ~z] constructs a vector with the given coordinates. *)
val create : x:float# -> y:float# -> z:float# -> t

(** The zero vector [(0, 0, 0)]. *)
val zero : t

(** {1 Accessors} *)

(** [x t] returns the x-coordinate. *)
val x : t -> float#

(** [y t] returns the y-coordinate. *)
val y : t -> float#

(** [z t] returns the z-coordinate. *)
val z : t -> float#

(** {1 Arithmetic} *)

(** [add a b] returns the component-wise sum [a + b]. *)
val add : t -> t -> t

(** [sub a b] returns the component-wise difference [a - b]. *)
val sub : t -> t -> t

(** [mul t k] returns the scalar product [k * t]. *)
val mul : t -> float# -> t

(** [neg t] returns the negated vector [(-x, -y, -z)]. *)
val neg : t -> t

(** [dot a b] returns the dot product [a.x * b.x + a.y * b.y + a.z * b.z]. *)
val dot : t -> t -> float#

(** [abs t] returns the vector with nonnegative components [(|x|, |y|, |z|)]. *)
val abs : t -> t

(** [cross a b] returns the cross product [a x b], a vector orthogonal to both inputs
    whose magnitude equals the area of the parallelogram they span. Follows the right-hand
    rule. *)
val cross : t -> t -> t

(** [mul_components a b] returns the component-wise product
    [(a.x * b.x, a.y * b.y, a.z * b.z)]. *)
val mul_components : t -> t -> t

(** [div_components a b] returns the component-wise quotient
    [(a.x /. b.x, a.y /. b.y, a.z /. b.z)]. *)
val div_components : t -> t -> t

(** [max a b] returns the vector of component-wise maxima. *)
val max : t -> t -> t

(** [min a b] returns the vector of component-wise minima. *)
val min : t -> t -> t

(** [norm2 t] returns the squared Euclidean norm [x^2 + y^2 + z^2]. *)
val norm2 : t -> float#

(** [norm t] returns the Euclidean norm [sqrt(x^2 + y^2 + z^2)]. *)
val norm : t -> float#

(** [normalize t] returns a unit vector in the same direction as [t]. If [t] is the zero
    vector, the zero vector is returned instead of producing NaN. For vectors with
    extremely small magnitudes, the squared norm can underflow; see [ensure_normalizable]
    for a workaround. *)
val normalize : t -> t

(** [is_normalizable t] returns true if the vector can be normalized without losing
    precision in a subsequent [norm2] call - equivalently, its largest absolute component
    is at least [2^(-242)]. The zero vector is also reported as normalizable (callers
    handle it specially). *)
val is_normalizable : t -> bool

(** [ensure_normalizable t] returns a vector pointing in the same direction as [t] but
    scaled up by a power of two if its largest absolute component is smaller than
    [2^(-242)]. This threshold guarantees that [norm2] and downstream operations on the
    result will not underflow, so callers can safely take cross products, squared norms,
    or [atan2] on results of [ensure_normalizable]. Inputs whose largest component is
    already at least [2^(-242)] are returned unchanged. The zero vector is returned
    unchanged. *)
val ensure_normalizable : t -> t

(** [distance a b] returns the Euclidean distance between [a] and [b], i.e.
    [norm (sub a b)]. *)
val distance : t -> t -> float#

(** [angle a b] returns the unsigned angle between [a] and [b] in radians, in the range
    [[0, pi]]. If either vector is zero-length or nearly zero-length, the result is zero. *)
val angle : t -> t -> S1_angle.t

(** [ortho t] returns a unit vector orthogonal to [t]. The axis used to construct it is
    chosen based on [largest_abs_component t] to avoid near-degenerate cross products. The
    result satisfies [dot t (ortho t) = 0] and [norm (ortho t) = 1] (up to floating-point
    rounding). *)
val ortho : t -> t

(** {1 Component analysis} *)

(** [largest_abs_component t] returns the index ([0], [1], or [2]) of the component with
    the largest absolute value. Ties are broken in favor of the higher index. *)
val largest_abs_component : t -> int

(** [smallest_component t] returns the index ([0], [1], or [2]) of the component with the
    smallest absolute value. Ties are broken in favor of the higher index. *)
val smallest_component : t -> int

(** {1 Comparison} *)

val compare : t -> t -> int
val equal : t -> t -> bool

(** [approx_equal ~max_error a b] returns true iff each coordinate differs by at most
    [max_error] (default [1e-16] when [max_error] is [none]). *)
val approx_equal : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool
[@@zero_alloc]
