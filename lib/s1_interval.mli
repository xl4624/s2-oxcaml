(** An S1Interval represents a closed interval on a unit circle (also known
    as a 1-dimensional sphere). It is capable of representing the empty
    interval (containing no points), the full interval (containing all
    points), and zero-length intervals (containing a single point).

    Points are represented by the angle they make with the positive x-axis in
    the range [-Pi, Pi]. An interval is represented by its lower and upper
    bounds (both inclusive, since the interval is closed). The lower bound may
    be greater than the upper bound, in which case the interval is "inverted"
    (i.e. it passes through the point (-1, 0)).

    Note that the point (-1, 0) has two valid representations, Pi and -Pi.
    The normalized representation of this point internally is Pi, so that
    endpoints of normal intervals are in the range (-Pi, Pi]. However, we
    take advantage of the point -Pi to construct two special intervals:
    the full interval is [-Pi, Pi], and the empty interval is [Pi, -Pi]. *)
open Core

[@@@zero_alloc all]

type t : float64 & float64 [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val pp : Format.formatter -> t -> unit [@@zero_alloc ignore]
val to_string : t -> string [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [create ~lo ~hi] creates an interval with the given endpoints. Both endpoints must be
    in the range [-Pi, Pi]. If either endpoint is -Pi, it is converted internally to Pi,
    except for [full] and [empty] intervals. *)
val create : lo:float# -> hi:float# -> t

(** The empty interval. *)
val empty : t

(** The full interval [-Pi, Pi]. *)
val full : t

(** Construct an interval containing a single point. [p] must be in [-Pi, Pi]. *)
val from_point : float# -> t

(** Construct the minimal interval containing the two given points. Both [p1] and [p2]
    must be in [-Pi, Pi]. *)
val from_point_pair : float# -> float# -> t

(** [is_valid_point p] is true iff [abs p <= Pi]. *)
val is_valid_point : float# -> bool

(** {1 Accessors} *)

val lo : t -> float#
val hi : t -> float#

(** {1 Predicates} *)

(** [is_valid t] returns true if neither bound exceeds Pi in absolute value, and the value
    -Pi appears only in the [empty] and [full] intervals. *)
val is_valid : t -> bool

(** [is_full t] returns true if the interval contains all points on the unit circle. *)
val is_full : t -> bool

(** [is_empty t] returns true if the interval contains no points. *)
val is_empty : t -> bool

(** [is_inverted t] returns true if [lo > hi]. (This is true for empty intervals.) *)
val is_inverted : t -> bool

(** {1 Operations} *)

(** [center t] returns the midpoint of the interval. For full and empty intervals, the
    result is arbitrary. *)
val center : t -> float#

(** [length t] returns the length of the interval. The length of an empty interval is
    negative. *)
val length : t -> float#

(** [complement t] returns the complement of the interior of the interval. *)
val complement : t -> t

(** [complement_center t] returns the midpoint of the complement of the interval. *)
val complement_center : t -> float#

(** [contains t p] returns true if the interval contains the point [p]. *)
val contains : t -> float# -> bool

(** [interior_contains t p] returns true if the interior of the interval contains [p]. *)
val interior_contains : t -> float# -> bool

(** [contains_interval t other] returns true if the interval contains [other]. *)
val contains_interval : t -> t -> bool

(** [interior_contains_interval t other] returns true if the interior of the interval
    contains [other]. *)
val interior_contains_interval : t -> t -> bool

(** [intersects t other] returns true if the two intervals have any points in common. *)
val intersects : t -> t -> bool

(** [interior_intersects t other] returns true if the interior of the interval contains
    any point of [other]. *)
val interior_intersects : t -> t -> bool

(** [directed_hausdorff_distance t other] returns the Hausdorff distance to [other]. *)
val directed_hausdorff_distance : t -> t -> float#

(** [add_point t p] returns the smallest interval that contains [t] and [p]. *)
val add_point : t -> float# -> t

(** [project t p] returns the closest point in the interval to [p]. The interval must be
    non-empty. *)
val project : t -> float# -> float#

(** [expanded t margin] returns an interval expanded on each side by [margin]. *)
val expanded : t -> float# -> t

(** [union t other] returns the smallest interval that contains both intervals. *)
val union : t -> t -> t

(** [intersection t other] returns the smallest interval that contains the intersection of
    the two intervals. *)
val intersection : t -> t -> t

(** [approx_equal ~max_error t other] returns true if the two intervals are approximately
    equal. Default tolerance is [1e-15] when [max_error] is [none]. *)
val approx_equal : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool
[@@zero_alloc]

(** [equal t other] returns true iff the two intervals contain the same set of points. *)
val equal : t -> t -> bool
