(** A disc-shaped region on the unit sphere.

    A cap is the set of points on the sphere within a given angular distance of a center
    point. Geometrically it is the portion of the sphere cut off by a plane; the boundary
    is the circle where that plane meets the sphere. The cap is a closed set - it contains
    its boundary circle.

    The radius is measured along the surface of the sphere, not as a straight-line chord
    through the interior. A cap of radius [pi /. 2.] is a hemisphere; a cap of radius [pi]
    covers the sphere.

    An equivalent parameterization is in terms of the center point and the cap height,
    where the height is the distance from the center to the cutoff plane:

    {[
      h = 1 -. cos r = 2. *. (sin (r /. 2.) ** 2.)
    ]}

    Radii are stored internally as an {!S1_chord_angle.t} (squared chord length) rather
    than an {!S1_angle.t} to make all comparisons, containment tests, and set operations
    exact up to a small roundoff. Round-tripping the radius through {!radius_angle} and
    back to a cap therefore loses a trigonometric-worth of precision. Two sentinel values
    are supported: {!empty} (contains no points; negative internal radius) and {!full}
    (contains the whole sphere; radius [pi]).

    Set-theoretic predicates such as {!contains_cap}, {!intersects}, and the centroid/area
    accessors all work on both sentinels. *)

open Core

[@@@zero_alloc all]

type t : (float64 & float64 & float64) & float64
[@@deriving sexp_of, unboxed_option { sentinel = true }]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** The empty cap - contains no points. *)
val empty : t

(** The full cap - contains every point on the sphere. *)
val full : t

(** [of_point p] returns a singleton cap containing exactly [p] (radius zero). [p] should
    be unit length. *)
val of_point : S2_point.t -> t

(** [of_center_angle center radius] builds a cap from [center] (which must be unit length)
    and an angular [radius]. Infinite radii and radii greater than [pi] yield a full cap;
    negative radii yield an empty cap after conversion to a chord angle. *)
val of_center_angle : S2_point.t -> S1_angle.t -> t

(** [of_center_chord_angle center radius] is the chord-angle form of {!of_center_angle},
    which avoids the trig conversion. [center] should be unit length; [radius] must
    satisfy {!S1_chord_angle.is_valid} ideally, but empty- and full-encoded chord angles
    are accepted. *)
val of_center_chord_angle : S2_point.t -> S1_chord_angle.t -> t

(** [of_center_height center height] builds a cap from its [height] along [center] (the
    distance from [center] to the cutoff plane). Negative heights produce an empty cap;
    [height >= 2] produces a full cap. *)
val of_center_height : S2_point.t -> float# -> t

(** [of_center_area center area] builds a cap whose surface area is [area] (a solid angle,
    in steradians). Negative [area] yields empty; [area >= 4 * pi] yields full. *)
val of_center_area : S2_point.t -> float# -> t

(** {1 Accessors} *)

(** [center t] returns the cap center. Always defined; for {!empty} and {!full} it is the
    canonical [(1, 0, 0)]. *)
val center : t -> S2_point.t

(** [radius_chord t] returns the stored chord-angle radius. *)
val radius_chord : t -> S1_chord_angle.t

(** [height t] returns the cap height ([1 - cos r]). For {!empty} this is negative. *)
val height : t -> float#

(** [radius_angle t] converts the stored chord-angle radius to an angle. May differ by a
    few ULPs from the angle passed to {!of_center_angle} because of the [chord -> angle]
    round trip. *)
val radius_angle : t -> S1_angle.t

(** [area t] returns the cap's surface area ([2 * pi * height]). Returns 0 for {!empty}. *)
val area : t -> float#

(** [centroid t] returns the cap's centroid scaled by its area. For {!empty} the result is
    the zero vector. The result lies on the ray from the origin through {!center t} but is
    not unit length. *)
val centroid : t -> S2_point.t

(** {1 Predicates} *)

(** [is_valid t] is [true] iff the center is unit length and the stored radius is a valid
    chord angle with length at most [4] (covering sphere diameter). *)
val is_valid : t -> bool

(** [is_empty t] is [true] iff the cap's radius is the negative sentinel. *)
val is_empty : t -> bool

(** [is_full t] is [true] iff the cap's radius covers the entire sphere (chord-length
    squared equals [4]). *)
val is_full : t -> bool

(** {1 Set operations} *)

(** [complement t] returns the cap of all points not in [t]. The complement of {!empty} is
    {!full} and vice versa. *)
val complement : t -> t

(** [contains_cap t other] is [true] iff every point of [other] is in [t]. Any cap
    contains {!empty}, and {!full} contains every cap. *)
val contains_cap : t -> t -> bool

(** [intersects t other] is [true] iff the two caps share at least one point. *)
val intersects : t -> t -> bool

(** [interior_intersects t other] is [true] iff the open interior of [t] meets [other].
    Unlike {!intersects}, a caps that touch only at the boundary do not count. *)
val interior_intersects : t -> t -> bool

(** [interior_contains_point t p] is [true] iff [p] lies strictly inside [t] (in the open
    interior, excluding the boundary circle). *)
val interior_contains_point : t -> S2_point.t -> bool

(** [contains_point t p] is [true] iff [p] lies in [t]'s closed disc. [p] must be unit
    length. *)
val contains_point : t -> S2_point.t -> bool

(** [add_point t p] returns the smallest cap with the same center as [t] that also
    contains [p]. If [t] is {!empty}, the result is a singleton cap at [p]. *)
val add_point : t -> S2_point.t -> t

(** [add_cap t other] returns the smallest cap with the same center as [t] that also
    contains [other]. Includes a small rounding bump on the chord-angle sum so that
    [contains_cap (add_cap t other) other] holds modulo roundoff. *)
val add_cap : t -> t -> t

(** [expanded t distance] returns a cap with the same center and radius
    [radius + distance]. Returns [Option.none] when [distance] is negative. Expanding
    {!empty} yields {!empty}. *)
val expanded : t -> S1_angle.t -> Option.t

(** [expanded_exn t distance] is {!expanded} but raises when [distance] is negative.
    @raise [Invalid_argument] if [distance] is negative. *)
val expanded_exn : t -> S1_angle.t -> t

(** [union t other] returns the smallest cap containing both [t] and [other]. Unlike
    {!add_cap}, the center of the result is allowed to move along the great circle through
    the two centers so that the covering radius is minimized. *)
val union : t -> t -> t

(** {1 Bounding} *)

(** [cell_union_bound t] returns a small set of cell ids whose union covers [t]. The
    result generally has at most 4 cells; very large caps fall back to the 6 face cells
    covering the sphere. Ids are not sorted. *)
val cell_union_bound : t -> S2_cell_id.t array
[@@zero_alloc ignore]

(** {1 Comparison} *)

(** [equal t other] is [true] iff the two caps have identical centers and radii, or are
    both {!empty}, or are both {!full}. *)
val equal : t -> t -> bool

(** [approx_equal ~max_error t other] tests whether the caps are equal within a tolerance:
    centers must be close in radians on the sphere, and radii must be close in squared
    chord length ({!S1_chord_angle.length2}). [max_error] defaults to [1e-14] when
    {!Packed_float_option.Unboxed.none} is supplied. Empty-or-full sentinels compare
    approximately equal to other caps that are within [max_error] of the corresponding
    extreme. *)
val approx_equal : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool
[@@zero_alloc]

(** {1 Encoding}

    Binary layout: four little-endian IEEE doubles, [x; y; z; length2], for a total of 32
    bytes. *)

(** [encode t] serializes [t] to a 32-byte little-endian binary representation. *)
val encode : t -> string
[@@zero_alloc ignore]

(** [decode s] parses a 32-byte encoding produced by {!encode}. Returns [Option.none] if
    the length is wrong or if the decoded cap fails {!is_valid}. *)
val decode : string -> Option.t
[@@zero_alloc ignore]

(** [decode_exn s] is {!decode} but raises on failure. *)
val decode_exn : string -> t
[@@zero_alloc ignore]
