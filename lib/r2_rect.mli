(** An axis-aligned closed rectangle in the (x, y) plane.

    A rectangle is a pair of closed intervals, one in each axis. The rectangle contains
    every point [(x, y)] with [x] in the x-interval and [y] in the y-interval. Because
    rectangles are closed, the boundary edges are included in [contains_point].

    A rectangle is {b valid} iff its x- and y-intervals are either both empty or both
    non-empty; the constructors enforce this invariant, returning [Option.none] (or
    raising) on a mismatch. Test for emptiness with [is_empty] rather than structural
    equality because there is more than one empty representation. The canonical empty
    rectangle is [empty].

    Typical usage is to compute bounding rectangles by starting from [empty] and calling
    [add_point] or [add_rect], then intersect or expand them as needed. The type is
    unboxed (four [float64]s laid out as two [R1_interval.t]s), so operations incur no
    allocation. *)

open Core

[@@@zero_alloc all]

type t : (float64 & float64) & (float64 & float64)
[@@deriving sexp_of, unboxed_option { sentinel = true }]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val pp : Format.formatter -> t -> unit [@@zero_alloc ignore]
val to_string : t -> string [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [create ~lo ~hi] constructs a rectangle from its lower-left and upper-right corners.
    Returns [Option.none] if exactly one of the induced intervals is empty (the other
    being non-empty), which would make the rectangle invalid. *)
val create : lo:R2_point.t -> hi:R2_point.t -> Option.t

(** [create_intervals ~x ~y] constructs a rectangle from its x- and y-intervals. Returns
    [Option.none] if exactly one of the two is empty. *)
val create_intervals : x:R1_interval.t -> y:R1_interval.t -> Option.t

(** [create_exn ~lo ~hi] is like [create] but raises if exactly one of the induced
    intervals is empty. *)
val create_exn : lo:R2_point.t -> hi:R2_point.t -> t

(** [create_intervals_exn ~x ~y] is like [create_intervals] but raises if exactly one of
    the two intervals is empty. *)
val create_intervals_exn : x:R1_interval.t -> y:R1_interval.t -> t

(** The canonical empty rectangle. Use [is_empty] to test for empty rectangles. *)
val empty : t

(** The rectangle [[-1, 1] x [-1, 1]], covering the canonical UV domain used by S2's
    face-based coordinate system. *)
val full : t

(** [from_center_size ~center ~size] constructs a rectangle centered at [center] with the
    given width and height. Both components of [size] should be non-negative; this
    constructor cannot produce an empty rectangle. *)
val from_center_size : center:R2_point.t -> size:R2_point.t -> t

(** [from_point p] constructs the degenerate rectangle containing the single point [p]. *)
val from_point : R2_point.t -> t

(** [from_point_pair p1 p2] constructs the minimal bounding rectangle containing [p1] and
    [p2]. Unlike [create], neither argument has to be the lower-left corner; this is
    equivalent to starting with [empty] and calling [add_point] twice. *)
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

(** [get_vertex t k] returns the [k]-th vertex of the rectangle in CCW order starting from
    the lower-left corner (vertex 0). [k] is reduced modulo 4, so any integer is accepted. *)
val get_vertex : t -> int -> R2_point.t

(** [get_vertex_ij t i j] returns the vertex selected by endpoint [i] of the x-interval (0
    = lo, 1 = hi) and endpoint [j] of the y-interval (0 = lo, 1 = hi). *)
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

(** [project t p] returns the point of [t] nearest to [p] under Euclidean distance, or
    [R2_point.Option.none] if [t] is empty. The result is [p] itself when [p] is already
    in [t]. *)
val project : t -> R2_point.t -> R2_point.Option.t

(** [project_exn t p] is like [project] but raises if [t] is empty. *)
val project_exn : t -> R2_point.t -> R2_point.t

(** [expanded t margin] expands the rectangle on each side by [margin.x] along the x-axis
    and [margin.y] along the y-axis. Negative components shrink the rectangle instead; if
    shrinking makes either axis empty, the result is [empty]. *)
val expanded : t -> R2_point.t -> t

(** [expanded_scalar t margin] is [expanded t (R2_point.create ~x:margin ~y:margin)]. *)
val expanded_scalar : t -> float# -> t

(** [union t other] returns the smallest rectangle containing both rectangles. *)
val union : t -> t -> t

(** [intersection t other] returns the smallest rectangle containing the intersection of
    the two rectangles. If the intersection is empty, returns [empty]. *)
val intersection : t -> t -> t

(** {1 Comparison} *)

(** [approx_equal ~max_error t other] returns true if each endpoint of the x- and
    y-intervals differs by at most [max_error]. Default tolerance is [1e-15] when
    [max_error] is [none]. Empty intervals participate the same way as in
    {!R1_interval.approx_equal}: an empty interval matches any sufficiently short interval
    in the same axis. *)
val approx_equal : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool
[@@zero_alloc]

(** [equal t other] returns true iff the two rectangles contain exactly the same set of
    points. All empty rectangles compare equal regardless of their representation. *)
val equal : t -> t -> bool
