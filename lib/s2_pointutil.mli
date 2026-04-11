(** Utility operations for points on the unit sphere.

    This module mirrors [s2pointutil.h] in the C++ S2 library. The underlying point
    representation is [S2_point.t], and most operations are thin wrappers around functions
    already exposed by [S2_point]. The purpose of this module is to group the upstream
    "S2::" utility helpers under a single namespace. *)

open Core

[@@@zero_alloc all]

(** [origin ()] returns the "point at infinity" used for point-in-polygon testing (by
    counting the number of edge crossings). This is NOT (0, 0, 0); it is a carefully
    chosen unit-length point that is unlikely to be collinear with commonly used edges. *)
val origin : unit -> S2_point.t
[@@zero_alloc ignore]

(** [is_unit_length p] returns true if [p] is approximately unit length, using the C++
    tolerance of [5 * DBL_EPSILON] on [|p|^2 - 1]. *)
val is_unit_length : S2_point.t -> bool

(** [approx_equals ~max_error_radians a b] returns true if the angle between [a] and [b]
    is at most [max_error_radians]. The default tolerance (when [max_error_radians] is
    [none]) matches C++'s [S1Angle::Radians(1e-15)]. Both [a] and [b] must be non-zero
    vectors. *)
val approx_equals
  :  max_error_radians:Packed_float_option.Unboxed.t
  -> S2_point.t
  -> S2_point.t
  -> bool
[@@zero_alloc]

(** [ortho a] returns a unit-length vector orthogonal to [a]. Satisfies
    [ortho (-a) = -(ortho a)]. Matches C++ [S2::Ortho]; prefers results whose coordinates
    are all non-zero to reduce degeneracies. *)
val ortho : S2_point.t -> S2_point.t

(** [ref_dir a] returns a unit-length vector different from [a], used as the reference
    direction for deciding whether a polygon with semi-open boundaries contains the vertex
    [a]. Currently aliased to [ortho], matching C++ [S2::RefDir]. *)
val ref_dir : S2_point.t -> S2_point.t

(** [rotate p ~axis ~angle] rotates [p] about [axis] by [angle]. Both [p] and [axis] must
    be unit length; [angle] has no restrictions. *)
val rotate : S2_point.t -> axis:S2_point.t -> angle:S1_angle.t -> S2_point.t

(** A 3x3 orthonormal matrix stored as three column vectors, as produced by [get_frame]. *)
type frame = S2_point.frame

(** [get_frame z] extends [z] (which must be unit length) into a right-handed orthonormal
    coordinate frame whose third column is [z]. *)
val get_frame : S2_point.t -> frame

(** [to_frame m p] returns the coordinates of [p] with respect to the orthonormal basis
    [m]. The result [q] satisfies [from_frame m q = p]. *)
val to_frame : frame -> S2_point.t -> S2_point.t

(** [from_frame m q] returns the point [p] such that [to_frame m p = q]. *)
val from_frame : frame -> S2_point.t -> S2_point.t
