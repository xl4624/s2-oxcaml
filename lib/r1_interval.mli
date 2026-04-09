(** A closed interval on the real line.

    Zero-length intervals (where [lo = hi]) represent single points. If [lo > hi], the
    interval is empty. *)
open Core

[@@@zero_alloc all]

type t : float64 & float64 [@@deriving sexp_of, unboxed_option { sentinel = true }]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val pp : Format.formatter -> t -> unit [@@zero_alloc ignore]
val to_string : t -> string [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [create ~lo ~hi] creates an interval with the given endpoints. If [lo > hi], the
    interval is empty. *)
val create : lo:float# -> hi:float# -> t

(** The empty interval. *)
val empty : t

(** [from_point p] constructs the interval containing the single point [p]. *)
val from_point : float# -> t

(** [from_point_pair p1 p2] constructs the minimal interval containing [p1] and [p2]. This
    is equivalent to starting with [empty] and adding both points, but is more efficient. *)
val from_point_pair : float# -> float# -> t

(** {1 Accessors} *)

(** [lo t] returns the low endpoint of the interval. *)
val lo : t -> float#

(** [hi t] returns the high endpoint of the interval. *)
val hi : t -> float#

(** {1 Predicates} *)

(** [is_empty t] returns true iff the interval contains no points. *)
val is_empty : t -> bool

(** {1 Operations} *)

(** [center t] returns the midpoint of the interval. For empty intervals, the result is
    arbitrary. *)
val center : t -> float#

(** [length t] returns the length of the interval. The length of an empty interval is
    negative. *)
val length : t -> float#

(** [contains t p] returns true if [p] is in the closed interval [[lo, hi]]. *)
val contains : t -> float# -> bool

(** [interior_contains t p] returns true if [p] is in the open interval [(lo, hi)]. *)
val interior_contains : t -> float# -> bool

(** [contains_interval t other] returns true if [t] contains [other]. *)
val contains_interval : t -> t -> bool

(** [interior_contains_interval t other] returns true if the interior of [t] contains
    [other], including [other]'s boundary. *)
val interior_contains_interval : t -> t -> bool

(** [intersects t other] returns true if the two intervals have any points in common. *)
val intersects : t -> t -> bool

(** [interior_intersects t other] returns true if the interior of [t] intersects any point
    of [other], including [other]'s boundary. *)
val interior_intersects : t -> t -> bool

(** [intersection t other] returns the interval containing all points common to [t] and
    [other]. Empty intervals do not need to be special-cased. *)
val intersection : t -> t -> t

(** [union t other] returns the smallest interval that contains both intervals. *)
val union : t -> t -> t

(** [add_point t p] returns the interval expanded so that it contains [p]. *)
val add_point : t -> float# -> t

(** [project t p] returns the closest point in [t] to [p], or
    [Packed_float_option.Unboxed.none] if [t] is empty. *)
val project : t -> float# -> Packed_float_option.Unboxed.t

(** [project_exn t p] returns the closest point in [t] to [p]. The interval must be
    non-empty. Matches [R1Interval::Project] in the C++ library. *)
val project_exn : t -> float# -> float#

(** [expanded t margin] returns an interval expanded on each side by [margin]. If [margin]
    is negative, the interval is shrunk instead. The result may be empty. Any expansion of
    an empty interval remains empty. *)
val expanded : t -> float# -> t

(** [directed_hausdorff_distance t other] returns the directed Hausdorff distance to
    [other]: [max_{p in t} min_{q in other} d(p, q)]. *)
val directed_hausdorff_distance : t -> t -> float#

(** [approx_equal ?max_error t other] returns true if [t] can be transformed into [other]
    by moving each endpoint by at most [max_error]. The empty interval is considered to be
    positioned arbitrarily on the real line, so any interval with sufficiently small
    length matches it. *)
val approx_equal : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool

(** [equal t other] returns true iff the two intervals contain the same set of points. *)
val equal : t -> t -> bool
