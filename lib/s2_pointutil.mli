(** Sphere-geometry helpers that supplement the raw vector operations on {!S2_point.t}.

    Almost every function here is a thin forwarder to an operation already defined on
    {!S2_point}. The separate namespace exists because the analogous C++ utilities live in
    a top-level [S2::] namespace rather than on [S2Point] itself; grouping them here makes
    porting code that uses [S2::Origin()], [S2::Ortho()], etc. straightforward. *)

open Core

[@@@zero_alloc all]

(** [origin ()] is the fixed "point at infinity" used by edge-crossing algorithms such as
    point-in-polygon. It is a unit-length vector chosen specifically to avoid lying on the
    boundary of any low-level S2 cell and to be unlikely to be collinear with common
    edges; it is {b not} the coordinate origin (0, 0, 0). *)
val origin : unit -> S2_point.t

(** [is_unit_length p] is [true] iff [norm2 p - 1] is bounded by [5 * epsilon] in absolute
    value. Primarily useful in assertions. *)
val is_unit_length : S2_point.t -> bool

(** [approx_equals ~max_error_radians a b] is [true] iff the angle between [a] and [b] is
    at most [max_error_radians]. Default tolerance is [1e-15] radians when
    [max_error_radians] is [none]. Both [a] and [b] must be non-zero. *)
val approx_equals
  :  max_error_radians:Packed_float_option.Unboxed.t
  -> S2_point.t
  -> S2_point.t
  -> bool
[@@zero_alloc]

(** [ortho a] returns a unit-length vector orthogonal to [a] and satisfying
    [ortho (neg a) = neg (ortho a)]. Biases away from the coordinate axes to keep
    downstream predicates out of their degenerate cases. *)
val ortho : S2_point.t -> S2_point.t

(** [ref_dir a] returns a unit-length vector distinct from [a], used as the reference
    direction when deciding which incident edge "owns" a vertex in a semi-open polygon
    boundary. Currently equal to {!ortho}. *)
val ref_dir : S2_point.t -> S2_point.t

(** [rotate p ~axis ~angle] rotates [p] about [axis] by [angle] radians. Both [p] and
    [axis] must be unit length; [angle] may be any value (positive, negative, or larger
    than one full turn). The result is re-normalized. *)
val rotate : S2_point.t -> axis:S2_point.t -> angle:S1_angle.t -> S2_point.t

(** A right-handed orthonormal 3x3 matrix stored as three column vectors. *)
type frame = S2_point.frame

(** [get_frame z] extends [z] (which must be unit length) into a right-handed orthonormal
    coordinate frame whose third column is [z]. The first two columns are an orthonormal
    basis for the tangent space at [z]. *)
val get_frame : S2_point.t -> frame

(** [to_frame m p] returns the coordinates of [p] with respect to the orthonormal basis
    [m], i.e. [transpose m * p]. *)
val to_frame : frame -> S2_point.t -> S2_point.t

(** [from_frame m q] returns [m * q], inverting {!to_frame}. *)
val from_frame : frame -> S2_point.t -> S2_point.t
