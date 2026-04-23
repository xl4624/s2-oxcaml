(** A point in the two-dimensional Cartesian plane.

    Values of this type are used interchangeably as points and as 2D displacement vectors
    with [x] and [y] coordinates; which interpretation is intended depends on the calling
    context. Arithmetic operators treat the value as a vector, so [add] gives you a
    translation and [sub] gives you the displacement between two points.

    All operations are component-wise unless otherwise noted. [angle a b] returns the
    signed angle from [a] to [b] measured counterclockwise, computed as
    [atan2(cross, dot)] and thus numerically stable for both near-parallel and
    near-antiparallel inputs.

    The type is unboxed and value-copyable. The nested [Option] module provides an unboxed
    option variant (via the [unboxed_option] deriver) for APIs that need to return an
    optional point without allocating.

    {1 Limitations}

    The underlying [Vector2_d] in C++ offers many additional component-wise operations
    ([Min], [Max], [Sqrt], [Floor], [Ceil], lexicographic comparison, casts to other
    element types). Only the subset used elsewhere in the port is exposed here; see
    {!R3_vector} for examples of what a fuller interface looks like.

    The [R2Edge] struct from the same C++ header is not represented in this module.
    Callers that need a pair of endpoints pass them separately. *)
open Core

[@@@zero_alloc all]

type t : (float64 & float64) mod external_
[@@deriving sexp_of, unboxed_option { sentinel = true }]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val pp : Format.formatter -> t -> unit [@@zero_alloc ignore]
val to_string : t -> string [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [create ~x ~y] constructs a point with the given coordinates. *)
val create : x:float# -> y:float# -> t

(** The zero vector [(0, 0)]. *)
val zero : t

(** {1 Accessors} *)

(** [x t] returns the x-coordinate. *)
val x : t -> float#

(** [y t] returns the y-coordinate. *)
val y : t -> float#

(** {1 Arithmetic} *)

(** [add a b] returns the component-wise sum [a + b]. *)
val add : t -> t -> t

(** [sub a b] returns the component-wise difference [a - b]. *)
val sub : t -> t -> t

(** [mul t k] returns the scalar product [t * k]. *)
val mul : t -> float# -> t

(** [div t k] returns the scalar quotient [(x/k, y/k)]. If [k] is zero each component
    becomes an IEEE infinity or NaN; no exception is raised. *)
val div : t -> float# -> t

(** [neg t] returns the negated vector [(-x, -y)]. *)
val neg : t -> t

(** {1 Geometry} *)

(** [ortho t] returns a counterclockwise orthogonal vector with the same norm. For
    [(x, y)], this returns [(-y, x)]. *)
val ortho : t -> t

(** [dot a b] returns the dot product [a.x * b.x + a.y * b.y]. *)
val dot : t -> t -> float#

(** [cross a b] returns the z-component of the 3D cross product [a.x * b.y - a.y * b.x].
    This is the signed area of the parallelogram spanned by [a] and [b]. *)
val cross : t -> t -> float#

(** [angle a b] returns the signed angle from [a] to [b] in radians, measured
    counterclockwise and in the range [(-pi, pi]]. Computed as [atan2(cross, dot)],
    which is accurate across the full range. If either vector is zero, the result is
    zero. *)
val angle : t -> t -> float#

(** [fabs t] returns the component-wise absolute value [(|x|, |y|)]. *)
val fabs : t -> t

(** [norm2 t] returns the squared Euclidean norm [x^2 + y^2]. *)
val norm2 : t -> float#

(** [norm t] returns the Euclidean norm [sqrt(x^2 + y^2)]. *)
val norm : t -> float#

(** [normalize t] returns a unit vector in the same direction as [t]. If [t] is the zero
    vector, the zero vector is returned instead of producing NaN from division by zero. *)
val normalize : t -> t

(** [equal a b] returns true iff both coordinates are exactly equal. *)
val equal : t -> t -> bool
