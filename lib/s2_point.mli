(** Points on the unit sphere represented as 3D Cartesian vectors.

    Internally an [S2_point.t] is an {!R3_vector.t}. Most S2 operations assume unit-length
    inputs, and the constructors in this module (e.g. {!of_coords}, {!S2_latlng.to_point})
    normalize accordingly. A few helpers in the {!section:Component-wise_arithmetic}
    section treat the value as a raw 3D vector and neither require nor preserve unit
    length; they are useful for building up intermediate values before a final
    normalization.

    Equality and ordering are lexicographic on the raw coordinates; two points that differ
    by sub-epsilon rounding will not be {!equal}. Use {!approx_equal} for tolerance-aware
    comparison.

    Precision: {!robust_cross_prod} falls back to exact arithmetic and symbolic
    perturbation when necessary so that the returned vector is non-zero even for nearly
    parallel or identical inputs. *)

open Core

[@@@zero_alloc all]

type t = R3_vector.t [@@deriving unboxed_option]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val pp : Format.formatter -> t -> unit [@@zero_alloc ignore]
val to_string : t -> string [@@zero_alloc ignore]

(** {1 Constructors} *)

(** A fixed unit-length point used as the "point at infinity" for edge-crossing-based
    point-in-polygon tests. Not equal to any point that would otherwise arise from
    geographic construction, and deliberately not on the boundary of any low-level S2
    cell. {b Not} the origin of the coordinate system. *)
val origin : t

(** [of_coords ~x ~y ~z] creates a unit-length point by normalizing [(x, y, z)]. If all
    three components are zero, returns {!origin} as a sentinel. *)
val of_coords : x:float# -> y:float# -> z:float# -> t

(** {1 Accessors} *)

(** [x t] returns the x-coordinate. *)
val x : t -> float#

(** [y t] returns the y-coordinate. *)
val y : t -> float#

(** [z t] returns the z-coordinate. *)
val z : t -> float#

(** [to_r3 t] returns the underlying R3 vector. *)
val to_r3 : t -> R3_vector.t

(** {1 Predicates} *)

(** [is_unit_length t] is [true] iff [norm2 t - 1] is bounded by [5 * epsilon] in absolute
    value. Useful in assertions; non-unit inputs to distance and edge primitives violate
    the expected error analysis. *)
val is_unit_length : t -> bool

(** {1 Geometry} *)

(** [ortho t] returns a unit-length vector orthogonal to [t]. Satisfies
    [ortho (neg t) = neg (ortho t)]. Deliberately biases away from the coordinate axes to
    reduce degeneracies downstream. *)
val ortho : t -> t

(** [robust_cross_prod a b] returns a non-zero vector orthogonal to both [a] and [b],
    provided the two inputs are not exactly equal. Uses the numerically stable
    [(a + b) x (b - a)] formula and, if that underflows, falls back to exact arithmetic;
    for truly collinear inputs it applies a symbolic perturbation that is antisymmetric in
    its arguments. The result is {i not} normalized, but it is guaranteed to be safely
    passable to [atan2(|cross|, dot)] without underflow. *)
val robust_cross_prod : t -> t -> t

(** [distance a b] returns the angle between [a] and [b] as an {!S1_angle.t}. *)
val distance : t -> t -> S1_angle.t

(** [stable_angle a b] returns the angle between [a] and [b] using
    [2 * atan2(|a - b|, |a + b|)], which remains accurate for very small angles where
    [acos(dot)] loses precision. *)
val stable_angle : t -> t -> S1_angle.t

(** [chord_angle_between a b] returns the chord angle between [a] and [b]. Both inputs
    should be unit length. *)
val chord_angle_between : t -> t -> S1_chord_angle.t

(** [rotate p ~axis ~angle] returns [p] rotated about [axis] by [angle] radians. Both [p]
    and [axis] must be unit length; [angle] has no restrictions. The result is
    re-normalized so numerical errors do not accumulate. *)
val rotate : t -> axis:t -> angle:S1_angle.t -> t

(** {1 Component-wise arithmetic}

    These operators treat an [S2_point] as a raw 3D vector. They do not assume or preserve
    unit length, so they are intended for building up or transforming intermediate values
    before a final normalization. *)

(** [add a b] returns the component-wise sum [a + b]. *)
val add : t -> t -> t

(** [sub a b] returns the component-wise difference [a - b]. *)
val sub : t -> t -> t

(** [mul t k] returns the scalar product [k * t]. *)
val mul : t -> float# -> t

(** [neg t] returns the negated point [(-x, -y, -z)]. *)
val neg : t -> t

(** [mul_components a b] returns the component-wise product
    [(a.x * b.x, a.y * b.y, a.z * b.z)]. *)
val mul_components : t -> t -> t

(** [div_components a b] returns the component-wise quotient
    [(a.x / b.x, a.y / b.y, a.z / b.z)]. *)
val div_components : t -> t -> t

(** [max a b] returns the component-wise maximum of [a] and [b]. *)
val max : t -> t -> t

(** [min a b] returns the component-wise minimum of [a] and [b]. *)
val min : t -> t -> t

(** [sqrt t] returns the component-wise square root of [t]. *)
val sqrt : t -> t

(** [floor t] returns the component-wise floor of [t]. *)
val floor : t -> t

(** [ceil t] returns the component-wise ceiling of [t]. *)
val ceil : t -> t

(** [fround t] returns the component-wise nearest integer of [t], using banker's rounding
    (round-half-to-even). *)
val fround : t -> t

(** [nan ()] returns a point whose three components are all NaN. *)
val nan : unit -> t

(** [is_nan t] returns true if any component of [t] is NaN. *)
val is_nan : t -> bool

(** {1 Comparison} *)

val compare : t -> t -> int
val equal : t -> t -> bool

(** [approx_equal ~max_error a b] returns true if the angle between [a] and [b] is at most
    [max_error]. Default tolerance is [1e-15] when [max_error] is [none]. *)
val approx_equal : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool
[@@zero_alloc]

(** {1 Frames} *)

(** A 3x3 right-handed orthonormal matrix, stored as three column vectors. *)
type frame =
  #{ col0 : R3_vector.t
   ; col1 : R3_vector.t
   ; col2 : R3_vector.t
   }

(** [get_frame z] returns an orthonormal frame whose third column is [z] (which must be
    unit length). The first two columns form an orthonormal basis for the tangent space at
    [z]. *)
val get_frame : t -> frame

(** [to_frame frame p] returns the coordinates of [p] relative to [frame]. The result [q]
    satisfies [from_frame frame q = p]. *)
val to_frame : frame -> t -> t

(** [from_frame frame q] returns [frame * q], i.e. the point whose coordinates in [frame]
    are [q]. *)
val from_frame : frame -> t -> t
