(** A point in the two-dimensional Cartesian plane.

    Represents a vector in R^2 with [x] and [y] coordinates.

    All operations are component-wise unless otherwise noted. The [angle] function
    computes the signed angle from one vector to another using [atan2(cross, dot)]. *)
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

(** [div t k] returns the component-wise division [t / k]. *)
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
    counterclockwise. If either vector is zero, the result is zero. *)
val angle : t -> t -> float#

(** [fabs t] returns the component-wise absolute value [(|x|, |y|)]. *)
val fabs : t -> t

(** [norm2 t] returns the squared Euclidean norm [x^2 + y^2]. *)
val norm2 : t -> float#

(** [norm t] returns the Euclidean norm [sqrt(x^2 + y^2)]. *)
val norm : t -> float#

(** [normalize t] returns a unit vector in the same direction as [t]. If [t] is the zero
    vector, the zero vector is returned (no division by zero). *)
val normalize : t -> t

(** [equal a b] returns true iff both coordinates are exactly equal. *)
val equal : t -> t -> bool
