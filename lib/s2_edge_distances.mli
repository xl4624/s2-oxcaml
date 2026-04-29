(** Distances to and along geodesic edges on the unit sphere.

    An edge here is a shortest-arc geodesic segment between two unit-length points A and

    B. This module answers the common geometric questions about such edges:

    - how far is a point X from an edge AB, and where on AB is the closest point? (the
      [*_distance], [project], and [get_distance_fraction] family);
    - how to walk along AB or along an orthogonal direction at a given distance? (the
      [interpolate], [get_point_on_line], [get_point_on_ray], and [get_point_to_*]
      family);
    - what is the minimum or maximum distance between two edges, and where is it attained?
      (the [update_edge_pair_*_distance] and [get_edge_pair_closest_points] family);
    - is one edge everywhere close to another? ({!is_edge_b_near_edge_a}).

    Inputs are assumed to be unit-length unless stated otherwise. The module handles
    degenerate edges ([a = b]) and antipodal or near-antipodal pairs correctly; the line
    AB has a well-defined direction even at antipodality via a robust cross-product
    primitive.

    Numerical strategy: the "min distance" functions work entirely in
    {!S1_chord_angle}-squared space (no [atan2] calls) so they can skip the exact geodesic
    computation when a cheap bound already rules the distance out. That makes
    {!is_distance_less}, {!update_min_distance}, and friends much faster than calling
    {!get_distance} and comparing. Accuracy degrades near 0 and near pi; see
    {!get_update_min_distance_max_error}.

    All functions are pure and thread-safe.

    {2 Limitations}

    - The constants [kProjectPerpendicularError], [kGetPointOnLineError], and
      [kGetPointOnRayPerpendicularError] are not exposed. *)

[@@@zero_alloc all]

(** {1 Point-to-Edge Distance} *)

(** [get_distance x a b] returns the minimum geodesic distance from [x] to any point on
    edge AB. All inputs must be unit length. Accuracy is excellent for small distances and
    degrades gradually as the distance approaches [pi/2] (see
    {!get_update_min_distance_max_error}). Handles [a = b] by returning the distance from
    [x] to [a]. When you only need to compare the distance against a fixed threshold,
    {!is_distance_less} is significantly faster. *)
val get_distance : S2_point.t -> S2_point.t -> S2_point.t -> S1_angle.t

(** [is_distance_less x a b limit] returns [true] iff the minimum distance from [x] to
    edge AB is strictly less than [limit]. Pass [S1_chord_angle.successor limit] to get a
    less-than-or-equal test. Avoids the expensive final angle conversion that
    {!get_distance} performs. *)
val is_distance_less : S2_point.t -> S2_point.t -> S2_point.t -> S1_chord_angle.t -> bool

(** [update_min_distance x a b min_dist] is the "running minimum" primitive: returns
    [Some d] if the distance from [x] to AB is strictly less than [min_dist], where [d] is
    that new minimum; otherwise returns [None]. Designed for loops that compute many
    distances and keep the smallest - internal bounds let the function bail out without
    computing the full distance when it is obviously larger than [min_dist]. Handles
    [a = b]. *)
val update_min_distance
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S1_chord_angle.t
  -> S1_chord_angle.Option.t

(** [update_max_distance x a b max_dist] is the companion of {!update_min_distance} for
    maximum distance: returns [Some d] if the maximum distance from [x] to AB is strictly
    greater than [max_dist], and [None] otherwise. Handles [a = b]. *)
val update_max_distance
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S1_chord_angle.t
  -> S1_chord_angle.Option.t

(** {1 Interior distance}

    "Interior" means the closest point on AB is strictly between the two endpoints, not at
    A or B. These variants are useful when endpoint cases must be treated separately (for
    example, to decide whether a point is closer to a polygon edge than to any vertex). *)

(** [is_interior_distance_less x a b limit] returns [true] iff the closest point on AB to
    [x] lies strictly in the interior of AB and the corresponding distance is strictly
    less than [limit]. *)
val is_interior_distance_less
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S1_chord_angle.t
  -> bool

(** [update_min_interior_distance x a b min_dist] is like {!update_min_distance} but
    considers only configurations where the closest point lies strictly in the interior of
    AB. Returns [None] when the minimum is attained at an endpoint or is not smaller than
    [min_dist]. *)
val update_min_interior_distance
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S1_chord_angle.t
  -> S1_chord_angle.Option.t

(** {1 Error Bounds} *)

(** [get_update_min_distance_max_error dist] returns an upper bound on the error in
    results produced by {!update_min_distance} (and the related interior and
    [is_distance_less] variants) when the distance is around [dist] and all inputs are
    unit-length. Feed the returned error into [S1_chord_angle.plus_error] to widen or
    narrow a comparison.

    Accuracy notes: near 0 degrees the error is about [1.2e-15] radians (approximately 8
    nanometers on Earth); near exact antipodality the error can be as large as 0.5 meters
    but drops quickly as the points move away from antipodal (roughly 1 mm at 50 m from
    antipodal, 1 um at 50 km from antipodal). *)
val get_update_min_distance_max_error : S1_chord_angle.t -> float#

(** {1 Projection} *)

(** [project x a b] returns the point on edge AB closest to [x]. If the perpendicular foot
    from [x] to the great circle through A and B lies inside the segment, that foot is
    returned; otherwise the nearer endpoint is returned. The fractional position of the
    result along AB can be recovered via {!get_distance_fraction}. All inputs must be unit
    length. *)
val project : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t

(** [project_with_cross x a b a_cross_b] is a slightly faster {!project} for callers who
    have already computed the cross product of the edge endpoints. The cross product does
    not need to be normalized but should come from {!S2_point.robust_cross_prod} for the
    most accurate result. All of [x], [a], [b] must be unit length. *)
val project_with_cross
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S2_point.t

(** {1 Interpolation} *)

(** [get_distance_fraction x a b] returns the distance ratio AX / (AX + BX). If [x] is on
    edge [ab], this is the fraction [t] such that [x = interpolate a b t]. Requires
    [a <> b]. *)
val get_distance_fraction : S2_point.t -> S2_point.t -> S2_point.t -> float#

(** [interpolate a b t] returns the point along edge [ab] whose distance from [a] is
    fraction [t] of the distance [ab]. Does not require [t] to be in [[0, 1]]. Distances
    are measured on the sphere surface. *)
val interpolate : S2_point.t -> S2_point.t -> float# -> S2_point.t

(** [get_point_on_line a b r] returns the point at distance [r] from [a] along the line
    [ab]. The line has a well-defined direction even when [a] and [b] are antipodal or
    nearly so. *)
val get_point_on_line : S2_point.t -> S2_point.t -> S1_angle.t -> S2_point.t

(** [get_point_on_ray origin dir r] returns the point at distance [r] along the ray with
    the given [origin] and [dir]. [dir] must be perpendicular to [origin]. Both should be
    normalized. This is faster than [get_point_on_line]. *)
val get_point_on_ray : S2_point.t -> S2_point.t -> S1_angle.t -> S2_point.t

(** [get_point_to_left a b r] returns the point to the left of edge [ab] at distance [r]
    from [a], orthogonal to the edge. *)
val get_point_to_left : S2_point.t -> S2_point.t -> S1_angle.t -> S2_point.t

(** [get_point_to_right a b r] returns the point to the right of edge [ab] at distance [r]
    from [a], orthogonal to the edge. *)
val get_point_to_right : S2_point.t -> S2_point.t -> S1_angle.t -> S2_point.t

(** Chord-angle variants of {!get_point_on_ray}, {!get_point_on_line},
    {!get_point_to_left}, {!get_point_to_right}. They take the input distance as an
    {!S1_chord_angle.t} and use its native [sin]/[cos] accessors, which are cheaper than
    the radian-based trig calls. Accuracy degrades for distances close to 180 degrees
    because the chord-angle representation collapses there; callers that may pass
    distances near pi should prefer the {!S1_angle.t} forms. *)
val get_point_on_ray_chord : S2_point.t -> S2_point.t -> S1_chord_angle.t -> S2_point.t

val get_point_on_line_chord : S2_point.t -> S2_point.t -> S1_chord_angle.t -> S2_point.t
val get_point_to_left_chord : S2_point.t -> S2_point.t -> S1_chord_angle.t -> S2_point.t
val get_point_to_right_chord : S2_point.t -> S2_point.t -> S1_chord_angle.t -> S2_point.t

(** {1 Edge-Pair Distance} *)

(** Unboxed pair of points returned by {!get_edge_pair_closest_points}. *)
type closest_points =
  #{ a : S2_point.t
   ; b : S2_point.t
   }

(** [update_edge_pair_min_distance a0 a1 b0 b1 min_dist] returns [Some dist] if the
    minimum distance between edges [a0a1] and [b0b1] is less than [min_dist], where [dist]
    is the new minimum. Returns [None] otherwise. If the two edges cross, the distance is
    zero. The cases [a0 = a1] and [b0 = b1] are handled correctly. *)
val update_edge_pair_min_distance
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S1_chord_angle.t
  -> S1_chord_angle.Option.t

(** [update_edge_pair_max_distance a0 a1 b0 b1 max_dist] returns [Some dist] if the
    maximum distance between edges [a0a1] and [b0b1] is greater than [max_dist]. Returns
    [None] otherwise. If one edge crosses the antipodal reflection of the other, the
    distance is pi. *)
val update_edge_pair_max_distance
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S1_chord_angle.t
  -> S1_chord_angle.Option.t

(** [is_edge_pair_distance_less a0 a1 b0 b1 limit] returns [true] iff the minimum distance
    between edges [a0a1] and [b0b1] is strictly less than [limit]. Faster than
    {!update_edge_pair_min_distance} when you only need the boolean answer. *)
val is_edge_pair_distance_less
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S1_chord_angle.t
  -> bool

(** [get_edge_pair_closest_points a0 a1 b0 b1] returns the pair [(a, b)] of points that
    achieves the minimum distance between edges [a0a1] and [b0b1], where [a] is a point on
    [a0a1] and [b] is a point on [b0b1]. If the two edges intersect, [a] and [b] are both
    equal to the intersection point. Handles [a0 = a1] and [b0 = b1] correctly. *)
val get_edge_pair_closest_points
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> closest_points

(** [is_edge_b_near_edge_a a0 a1 b0 b1 tolerance] returns true if every point on edge
    [b0b1] is no further than [tolerance] from some point on edge [a0a1]. Equivalently,
    returns true if the directed Hausdorff distance from [b0b1] to [a0a1] is no more than
    [tolerance]. Requires [tolerance] to be strictly less than 90 degrees and strictly
    greater than zero. *)
val is_edge_b_near_edge_a
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S1_angle.t
  -> bool
