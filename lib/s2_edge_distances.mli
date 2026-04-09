(** Distance computations for edges (geodesic segments) on the unit sphere.

    Provides functions for computing the distance from a point to an edge, projecting
    points onto edges, interpolating along edges, and related operations. *)

open Core

[@@@zero_alloc all]

(** {1 Point-to-Edge Distance} *)

(** [get_distance x a b] returns the minimum distance from point [x] to edge [ab]. All
    arguments should be unit length. The result is very accurate for small distances but
    may have some numerical error if the distance is large (approximately pi/2 or
    greater). The case [a = b] is handled correctly. *)
val get_distance : S2_point.t -> S2_point.t -> S2_point.t -> S1_angle.t
[@@zero_alloc ignore]

(** [is_distance_less x a b limit] returns true if the distance from [x] to edge [ab] is
    less than [limit]. For less-than-or-equal, pass [S1_chord_angle.successor limit]. This
    is faster than [get_distance]. *)
val is_distance_less : S2_point.t -> S2_point.t -> S2_point.t -> S1_chord_angle.t -> bool
[@@zero_alloc ignore]

(** [update_min_distance x a b min_dist] returns [Some dist] if the distance from [x] to
    edge [ab] is less than [min_dist], where [dist] is the new minimum. Returns [None]
    otherwise. The case [a = b] is handled correctly. *)
val update_min_distance
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S1_chord_angle.t
  -> S1_chord_angle.Option.t
[@@zero_alloc ignore]

(** [update_max_distance x a b max_dist] returns [Some dist] if the maximum distance from
    [x] to edge [ab] is greater than [max_dist], where [dist] is the new maximum. Returns
    [None] otherwise. *)
val update_max_distance
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S1_chord_angle.t
  -> S1_chord_angle.Option.t
[@@zero_alloc ignore]

(** {1 Interior Distance} *)

(** [is_interior_distance_less x a b limit] returns true if the minimum distance from [x]
    to edge [ab] is attained at an interior point of [ab] (not an endpoint) and that
    distance is less than [limit]. *)
val is_interior_distance_less
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S1_chord_angle.t
  -> bool
[@@zero_alloc ignore]

(** [update_min_interior_distance x a b min_dist] returns [Some dist] if the minimum
    distance from [x] to [ab] is attained at an interior point and that distance is less
    than [min_dist]. Returns [None] otherwise. *)
val update_min_interior_distance
  :  S2_point.t
  -> S2_point.t
  -> S2_point.t
  -> S1_chord_angle.t
  -> S1_chord_angle.Option.t
[@@zero_alloc ignore]

(** {1 Error Bounds} *)

(** [get_update_min_distance_max_error dist] returns the maximum error in the result of
    [update_min_distance] and associated functions, assuming all input points are
    normalized. The error can be applied via [S1_chord_angle.plus_error]. *)
val get_update_min_distance_max_error : S1_chord_angle.t -> float#
[@@zero_alloc ignore]

(** {1 Projection} *)

(** [project x a b] returns the point along edge [ab] that is closest to [x]. All
    arguments must be unit length. *)
val project : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t
[@@zero_alloc ignore]

(** {1 Interpolation} *)

(** [get_distance_fraction x a b] returns the distance ratio AX / (AX + BX). If [x] is on
    edge [ab], this is the fraction [t] such that [x = interpolate a b t]. Requires
    [a <> b]. *)
val get_distance_fraction : S2_point.t -> S2_point.t -> S2_point.t -> float#
[@@zero_alloc ignore]

(** [interpolate a b t] returns the point along edge [ab] whose distance from [a] is
    fraction [t] of the distance [ab]. Does not require [t] to be in [[0, 1]]. Distances
    are measured on the sphere surface. *)
val interpolate : S2_point.t -> S2_point.t -> float# -> S2_point.t
[@@zero_alloc ignore]

(** [get_point_on_line a b r] returns the point at distance [r] from [a] along the line
    [ab]. The line has a well-defined direction even when [a] and [b] are antipodal or
    nearly so. *)
val get_point_on_line : S2_point.t -> S2_point.t -> S1_angle.t -> S2_point.t
[@@zero_alloc ignore]

(** [get_point_on_ray origin dir r] returns the point at distance [r] along the ray with
    the given [origin] and [dir]. [dir] must be perpendicular to [origin]. Both should be
    normalized. This is faster than [get_point_on_line]. *)
val get_point_on_ray : S2_point.t -> S2_point.t -> S1_angle.t -> S2_point.t
[@@zero_alloc ignore]

(** [get_point_to_left a b r] returns the point to the left of edge [ab] at distance [r]
    from [a], orthogonal to the edge. *)
val get_point_to_left : S2_point.t -> S2_point.t -> S1_angle.t -> S2_point.t
[@@zero_alloc ignore]

(** [get_point_to_right a b r] returns the point to the right of edge [ab] at distance [r]
    from [a], orthogonal to the edge. *)
val get_point_to_right : S2_point.t -> S2_point.t -> S1_angle.t -> S2_point.t
[@@zero_alloc ignore]
