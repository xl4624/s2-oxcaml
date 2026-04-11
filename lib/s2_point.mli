(** An S2Point represents a point on the unit sphere as a 3D vector. Points are usually
    normalized to unit length, but some methods do not require this. *)

open Core

[@@@zero_alloc all]

type t = R3_vector.t [@@deriving unboxed_option]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val pp : Format.formatter -> t -> unit [@@zero_alloc ignore]
val to_string : t -> string [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [origin] is a point that is guaranteed to be different from any valid point created
    from coordinates. It is used as a sentinel value. *)
val origin : t

(** [of_coords ~x ~y ~z] creates a new point from the given coordinates and normalizes it.
    If all coordinates are zero, returns [origin]. *)
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

(** [is_unit_length t] returns true if the point is within a small epsilon of unit length. *)
val is_unit_length : t -> bool

(** {1 Geometry} *)

(** [ortho t] returns a unit-length vector orthogonal to [t]. Satisfies the property that
    [ortho (-t) = -(ortho t)]. Matches [S2Point::Ortho] in the C++ library. *)
val ortho : t -> t

(** [robust_cross_prod a b] returns a vector orthogonal to both [a] and [b]. Uses
    high-precision arithmetic if necessary to ensure the result is non-zero even when [a]
    and [b] are nearly parallel. The result is not normalized. *)
val robust_cross_prod : t -> t -> t
[@@zero_alloc ignore]

(** [distance a b] returns the angle between [a] and [b]. *)
val distance : t -> t -> S1_angle.t

(** [stable_angle a b] returns the angle between [a] and [b] using a numerically stable
    formula that is accurate even for very small angles. *)
val stable_angle : t -> t -> S1_angle.t

(** [chord_angle_between a b] returns the chord angle between [a] and [b]. *)
val chord_angle_between : t -> t -> S1_chord_angle.t

(** [rotate p ~axis ~angle] rotates the point [p] about the given [axis] by the given
    [angle]. *)
val rotate : t -> axis:t -> angle:S1_angle.t -> t

(** {1 Comparison} *)

val compare : t -> t -> int
val equal : t -> t -> bool

(** [approx_equal ~max_error a b] returns true if the angle between [a] and [b] is at most
    [max_error]. Default tolerance is [1e-15] when [max_error] is [none]. *)
val approx_equal : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool
[@@zero_alloc]

(** {1 Frames} *)

(** A 3x3 orthonormal matrix stored as three column vectors. *)
type frame =
  #{ col0 : R3_vector.t
   ; col1 : R3_vector.t
   ; col2 : R3_vector.t
   }

(** [get_frame z] returns an orthonormal frame where [col2 = z]. *)
val get_frame : t -> frame

(** [to_frame frame p] returns the coordinates of [p] in the given frame. *)
val to_frame : frame -> t -> t

(** [from_frame frame q] returns the point [p] such that [to_frame frame p = q]. *)
val from_frame : frame -> t -> t
