(** Closed intervals on the unit circle (1-sphere).

    An [S1_interval] represents a closed arc of the unit circle. Points are identified
    with the angle they make with the positive x-axis, in the range [[-pi, pi]]. Each
    interval is stored as a pair [(lo, hi)] of endpoints. Possible shapes:

    - {i Ordinary}: [lo <= hi]. The interval is the arc from [lo] to [hi] going
      counter-clockwise without crossing the point (-1, 0).
    - {i Inverted}: [lo > hi]. The interval wraps through (-1, 0), running from [lo]
      counter-clockwise to [pi], and separately from [-pi] up to [hi].
    - {i Empty}: represented internally by [(pi, -pi)].
    - {i Full}: the entire circle, represented by [(-pi, pi)].
    - {i Singleton}: [lo = hi], a zero-length interval containing one point.

    The point (-1, 0) has two valid angular representations, [pi] and [-pi]. Endpoints
    of ordinary intervals are normalized to [(-pi, pi]]; [-pi] appears only in the
    {!full} and {!empty} sentinels. Inputs of [-pi] are silently rewritten to [pi] by
    {!create} and the point-accepting operations.

    Intervals returned by this module are always valid (see {!is_valid}). Callers
    typically build one with {!create}, {!from_point}, or {!from_point_pair} and then
    query membership with {!contains} / {!intersects} or combine with {!union} /
    {!intersection}. *)
open Core

[@@@zero_alloc all]

type t : float64 & float64 [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val pp : Format.formatter -> t -> unit [@@zero_alloc ignore]
val to_string : t -> string [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [create ~lo ~hi] builds an interval with the given endpoints. Both must lie in
    [[-pi, pi]]. An endpoint value of [-pi] is normalized to [pi] unless the pair is the
    full or empty sentinel. *)
val create : lo:float# -> hi:float# -> t

(** The empty interval, containing no points. Encoded as [(pi, -pi)]. *)
val empty : t

(** The full interval [[-pi, pi]], containing every point on the circle. *)
val full : t

(** [from_point p] builds a zero-length interval containing only [p]. [p] must lie in
    [[-pi, pi]]; the value [-pi] is normalized to [pi]. *)
val from_point : float# -> t

(** [from_point_pair p1 p2] builds the smallest interval containing both [p1] and [p2].
    Equivalent to adding the two points to an empty interval but faster. Both inputs must
    lie in [[-pi, pi]]. *)
val from_point_pair : float# -> float# -> t

(** [is_valid_point p] is [true] iff [Float_u.abs p <= pi]. *)
val is_valid_point : float# -> bool

(** {1 Accessors} *)

val lo : t -> float#
val hi : t -> float#

(** {1 Predicates} *)

(** [is_valid t] is [true] iff neither endpoint exceeds [pi] in absolute value and [-pi]
    appears only in the empty and full sentinels. *)
val is_valid : t -> bool

(** [is_full t] is [true] iff [t] covers the entire circle. *)
val is_full : t -> bool

(** [is_empty t] is [true] iff [t] contains no points. *)
val is_empty : t -> bool

(** [is_inverted t] is [true] iff [lo t > hi t]. This is true for the empty interval but
    not for the full interval. *)
val is_inverted : t -> bool

(** {1 Operations} *)

(** [center t] returns the midpoint of the interval. For full and empty intervals, the
    result is arbitrary. *)
val center : t -> float#

(** [length t] returns the angular length of the interval. The length of the empty
    interval is negative; the length of the full interval is [2 * pi]. *)
val length : t -> float#

(** [complement t] returns the complement of the interior of [t]. Both share the same
    boundary but no interior points. This is not a bijection: the complement of any
    singleton equals the complement of the empty interval. *)
val complement : t -> t

(** [complement_center t] returns the midpoint of [complement t]. For full and empty
    intervals the result is arbitrary; for a singleton the result is its antipodal point. *)
val complement_center : t -> float#

(** [contains t p] is [true] iff the (closed) interval contains [p]. [p] equal to [-pi] is
    treated as [pi]. *)
val contains : t -> float# -> bool

(** [interior_contains t p] is [true] iff the interior of [t] contains [p]. *)
val interior_contains : t -> float# -> bool

(** [contains_interval t other] is [true] iff every point of [other] is in [t]. Works for
    any combination of empty, full, singleton, and inverted intervals. *)
val contains_interval : t -> t -> bool

(** [interior_contains_interval t other] is [true] iff the interior of [t] contains
    [other] entirely. Note that [interior_contains_interval t t] is true only when [t] is
    empty or full. *)
val interior_contains_interval : t -> t -> bool

(** [intersects t other] is [true] iff the two intervals share any point. Because (-1, 0)
    has representations [pi] and [-pi], the intervals [[-pi, -3]] and [[2, pi]] do
    intersect. *)
val intersects : t -> t -> bool

(** [interior_intersects t other] is [true] iff the interior of [t] meets [other]
    (including its boundary). *)
val interior_intersects : t -> t -> bool

(** [directed_hausdorff_distance t other] is [max_{p in t} min_{q in other} d(p, q)],
    where [d] is measured along the unit circle. *)
val directed_hausdorff_distance : t -> t -> float#

(** [add_point t p] returns the smallest interval containing every point of [t] and [p].
    If [p] is outside [[-pi, pi]] the interval is returned unchanged. *)
val add_point : t -> float# -> t

(** [project t p] returns the closest point of [t] to [p]. [t] must be non-empty. *)
val project : t -> float# -> float#

(** [expanded t margin] returns an interval grown by [margin] radians on each side. A
    negative [margin] shrinks the interval. Expanding a full interval yields a full
    interval and expanding an empty interval yields an empty interval; other shrinkages
    may collapse to empty and other growths may saturate to full. *)
val expanded : t -> float# -> t

(** [union t other] returns the smallest interval containing both [t] and [other]. *)
val union : t -> t -> t

(** [intersection t other] returns the smallest interval containing the intersection of
    [t] and [other]. Note that the true intersection of two arcs may consist of two
    disjoint pieces; this function returns the minimal single arc covering them. *)
val intersection : t -> t -> t

(** [approx_equal ~max_error t other] is [true] iff [t] can be transformed into [other] by
    moving each endpoint by at most [max_error] radians without inverting. Empty and full
    intervals match any interval whose length is [<= 2 * max_error] or
    [>= 2 * pi - 2 * max_error] respectively. Default tolerance is [1e-15] when
    [max_error] is [none]. *)
val approx_equal : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool
[@@zero_alloc]

(** [equal t other] is [true] iff the two intervals contain exactly the same set of
    points. *)
val equal : t -> t -> bool
