(** An axis-aligned rectangle in the (x,y) plane.

    Represents a closed region defined by x and y intervals. This type is the OCaml
    equivalent of C++ [R2Rect] (in [s2/r2rect.h]) and Go [r2.Rect].

    Rectangles are valid if both their x and y intervals are either empty or non-empty.
    The canonical empty rectangle is returned by [empty]. *)

open Core

[@@@zero_alloc all]

type t : (float64 & float64) & (float64 & float64) [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val pp : Format.formatter -> t -> unit [@@zero_alloc ignore]
val to_string : t -> string [@@zero_alloc ignore]

(** A sentinel-based option type for [R2_rect.t] that avoids boxing.

    Uses NaN in the x-interval lo field as the none sentinel. *)
module Option : sig
  type nonrec t = t

  val none : t
  val some : t -> t
  val is_none : t -> bool
  val is_some : t -> bool
  val value : t -> default:t -> t
  val value_exn : t -> t
  val unchecked_value : t -> t

  val%template sexp_of_t : t -> Sexp.t @ m
  [@@alloc a @ m = (heap @ global, stack @ local)] [@@zero_alloc ignore]

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool
      val unsafe_value : t -> t
    end
  end
end

(** {1 Constructors} *)

(** [create lo hi] constructs a rectangle from its lower-left and upper-right corners.
    Returns [Option.none] if the resulting intervals are invalid (one empty, one not). *)
val create : lo:R2_point.t -> hi:R2_point.t -> Option.t

(** [create_intervals x y] constructs a rectangle from its x and y intervals. Returns
    [Option.none] if the intervals are invalid. *)
val create_intervals : x:R1_interval.t -> y:R1_interval.t -> Option.t

(** [create_exn lo hi] constructs a rectangle from its lower-left and upper-right corners.
    Raises if the resulting intervals are invalid (one empty, one not). *)
val create_exn : lo:R2_point.t -> hi:R2_point.t -> t

(** [create_intervals_exn x y] constructs a rectangle from its x and y intervals. Raises
    if the intervals are invalid. *)
val create_intervals_exn : x:R1_interval.t -> y:R1_interval.t -> t

(** The canonical empty rectangle. Use [is_empty] to test for empty rectangles. *)
val empty : t

(** The rectangle representing the entire UV plane [[-1, 1]x[-1, 1]]. *)
val full : t

(** [from_center_size center size] constructs a rectangle from its center point and size.
    Both components of [size] should be non-negative. *)
val from_center_size : center:R2_point.t -> size:R2_point.t -> t

(** [from_point p] constructs the degenerate rectangle containing the single point [p]. *)
val from_point : R2_point.t -> t

(** [from_point_pair p1 p2] constructs the minimal bounding rectangle containing [p1] and
    [p2]. *)
val from_point_pair : R2_point.t -> R2_point.t -> t

(** {1 Accessors} *)

(** [x t] returns the x-interval of the rectangle. *)
val x : t -> R1_interval.t

(** [y t] returns the y-interval of the rectangle. *)
val y : t -> R1_interval.t

(** [lo t] returns the lower-left corner of the rectangle. *)
val lo : t -> R2_point.t

(** [hi t] returns the upper-right corner of the rectangle. *)
val hi : t -> R2_point.t

(** [is_valid t] returns true if the rectangle is valid (both x and y intervals are either
    empty or non-empty). *)
val is_valid : t -> bool

(** [is_empty t] returns true if the rectangle contains no points. *)
val is_empty : t -> bool

(** [get_vertex t k] returns the k-th vertex of the rectangle in CCW order starting from
    the lower-left corner. [k] is reduced modulo 4. *)
val get_vertex : t -> int -> R2_point.t

(** [get_vertex_ij t i j] returns the vertex in direction [i] along the x-axis (0=lo,
    1=hi) and direction [j] along the y-axis (0=lo, 1=hi). *)
val get_vertex_ij : t -> int -> int -> R2_point.t

(** [center t] returns the center of the rectangle in (x,y)-space. *)
val center : t -> R2_point.t

(** [size t] returns the width and height of the rectangle. Empty rectangles have negative
    width and height. *)
val size : t -> R2_point.t

(** {1 Predicates} *)

(** [contains_point t p] returns true if the rectangle contains the point [p]. Rectangles
    are closed, so they contain their boundary. *)
val contains_point : t -> R2_point.t -> bool

(** [interior_contains_point t p] returns true if the interior of the rectangle contains
    [p] (excluding the boundary). *)
val interior_contains_point : t -> R2_point.t -> bool

(** [contains_rect t other] returns true if the rectangle contains [other]. *)
val contains_rect : t -> t -> bool

(** [interior_contains_rect t other] returns true if the interior of the rectangle
    contains all points of [other] (including its boundary). *)
val interior_contains_rect : t -> t -> bool

(** [intersects t other] returns true if the two rectangles have any points in common. *)
val intersects : t -> t -> bool

(** [interior_intersects t other] returns true if the interior of the rectangle intersects
    any point of [other] (including its boundary). *)
val interior_intersects : t -> t -> bool

(** {1 Operations} *)

(** [add_point t p] expands the rectangle to include point [p]. The rectangle is expanded
    by the minimum amount possible. *)
val add_point : t -> R2_point.t -> t

(** [add_rect t other] expands the rectangle to include [other]. This is equivalent to
    [union t other] but is more efficient. *)
val add_rect : t -> t -> t

(** [project t p] returns the closest point in the rectangle to [p], or
    [R2_point.Option.none] if the rectangle is empty. *)
val project : t -> R2_point.t -> R2_point.Option.t

(** [project_exn t p] returns the closest point in the rectangle to [p]. Raises if the
    rectangle is empty. *)
val project_exn : t -> R2_point.t -> R2_point.t

(** [expanded t margin] expands the rectangle on each side by the given [margin]. If
    either component of [margin] is negative, the rectangle is shrunk on the corresponding
    sides. The resulting rectangle may be empty. *)
val expanded : t -> R2_point.t -> t

(** [expanded_scalar t margin] expands the rectangle on each side by the scalar [margin]. *)
val expanded_scalar : t -> float -> t

(** [union t other] returns the smallest rectangle containing both rectangles. *)
val union : t -> t -> t

(** [intersection t other] returns the smallest rectangle containing the intersection of
    the two rectangles. If the intersection is empty, returns [empty]. *)
val intersection : t -> t -> t

(** {1 Comparison} *)

(** [approx_equal ?max_error t other] returns true if the x- and y-intervals of the two
    rectangles are within [max_error] of each other. Default [max_error] is [1e-15]. *)
val approx_equal : ?max_error:float -> t -> t -> bool
[@@zero_alloc ignore]

val equal : t -> t -> bool
