(** A closed latitude-longitude rectangle on the unit sphere.

    The rectangle is capable of representing the empty and full rectangles as well as
    single points. The latitude-longitude space has cylindrical topology: longitudes wrap
    around at +/-180 degrees, while latitudes are clamped to [-90, 90] degrees.

    An S2LatLngRect may be defined so that it includes some representations of a pole but
    not others. Use {!polar_closure} to expand a rectangle so that it contains all
    possible representations of any contained poles.

    The rectangle is stored as an [R1_interval] (latitude in radians) and an [S1_interval]
    (longitude in radians). *)
open Core

[@@@zero_alloc all]

type t : (float64 & float64) & (float64 & float64) [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** The empty rectangle (contains no points). *)
val empty : t

(** The full rectangle (contains all points). *)
val full : t

(** [create ~lat ~lng] constructs a rectangle from latitude and longitude intervals. Both
    intervals must be either both empty or both non-empty. The latitude interval must not
    extend outside [-pi/2, pi/2]. *)
val create : lat:R1_interval.t -> lng:S1_interval.t -> t

(** [of_lo_hi ~lo ~hi] constructs a rectangle from two corner points. [lo.lat <= hi.lat]
    is required. If [lo.lng > hi.lng], the rectangle crosses the antimeridian. *)
val of_lo_hi : lo:S2_latlng.t -> hi:S2_latlng.t -> t

(** [of_point ll] constructs a rectangle containing a single normalized point. *)
val of_point : S2_latlng.t -> t

(** [of_point_pair p1 p2] constructs the minimal bounding rectangle containing the two
    given normalized points. *)
val of_point_pair : S2_latlng.t -> S2_latlng.t -> t

(** [of_center_size ~center ~size] constructs a rectangle with the given size centered
    around the given point. [center] must be normalized; [size] does not. The latitude is
    clamped to [-90,90] and the longitude becomes full if [size.lng >= 360 degrees]. *)
val of_center_size : center:S2_latlng.t -> size:S2_latlng.t -> t

(** {1 Accessors} *)

val lat : t -> R1_interval.t
val lng : t -> S1_interval.t

(** Lower-left corner. *)
val lo : t -> S2_latlng.t

(** Upper-right corner. *)
val hi : t -> S2_latlng.t

(** Center point in latitude-longitude space (not generally the center on the sphere). *)
val center : t -> S2_latlng.t

(** Width and height in latitude-longitude space. Empty rectangles have negative width and
    height. *)
val size : t -> S2_latlng.t

(** [vertex k] returns the k-th vertex (k = 0,1,2,3) in CCW order: lower-left,
    lower-right, upper-right, upper-left. [k] is reduced modulo 4. *)
val vertex : t -> int -> S2_latlng.t

(** Surface area on the unit sphere. *)
val area : t -> float# [@@zero_alloc ignore]

(** Area-weighted centroid (not unit length). See [S2LatLngRect::GetCentroid]. *)
val centroid : t -> S2_point.t
[@@zero_alloc ignore]

(** {1 Predicates} *)

val is_valid : t -> bool
val is_empty : t -> bool
val is_full : t -> bool
val is_point : t -> bool
val is_inverted : t -> bool

(** {1 Containment and intersection} *)

(** [contains_latlng t ll] reports whether the rectangle contains the given point. [ll]
    must be valid. *)
val contains_latlng : t -> S2_latlng.t -> bool

(** [interior_contains_latlng t ll] reports whether the interior of the rectangle contains
    the given point. *)
val interior_contains_latlng : t -> S2_latlng.t -> bool

(** [contains_point t p] reports whether the rectangle contains the given S2Point. *)
val contains_point : t -> S2_point.t -> bool

(** [interior_contains_point t p] reports whether the interior contains the S2Point. *)
val interior_contains_point : t -> S2_point.t -> bool

(** [contains t other] reports whether [t] contains [other]. *)
val contains : t -> t -> bool

(** [interior_contains t other] reports whether the interior of [t] contains [other]. *)
val interior_contains : t -> t -> bool

(** [intersects t other] reports whether [t] and [other] have any points in common. *)
val intersects : t -> t -> bool

(** [interior_intersects t other] reports whether the interior of [t] intersects [other]. *)
val interior_intersects : t -> t -> bool

(** {1 Set operations} *)

(** Smallest rectangle containing the union of both rectangles. *)
val union : t -> t -> t

(** Smallest rectangle containing the intersection. The intersection may be two disjoint
    rectangles; in that case a single rectangle spanning both is returned. *)
val intersection : t -> t -> t

(** Expand the rectangle to include the given point. [ll] must be normalized. *)
val add_point : t -> S2_latlng.t -> t

(** Expand by [margin.lat] on each side in latitude and [margin.lng] on each side in
    longitude. See C++ [S2LatLngRect::Expanded] for full semantics. *)
val expanded : t -> S2_latlng.t -> t

(** If the rectangle includes a pole, expand longitude to full. Otherwise return
    unchanged. *)
val polar_closure : t -> t

(** {1 Bounding} *)

(** A bounding cap (the smaller of center-cap and pole-cap). *)
val cap_bound : t -> S2_cap.t
[@@zero_alloc ignore]

(** Returns itself. *)
val rect_bound : t -> t

(** {1 Distance} *)

(** Minimum distance on the sphere from the rectangle to the given point. The rectangle
    must be non-empty and [ll] must be valid. *)
val distance_to_latlng : t -> S2_latlng.t -> S1_angle.t
[@@zero_alloc ignore]

(** Minimum distance on the sphere between two non-empty rectangles. *)
val distance : t -> t -> S1_angle.t
[@@zero_alloc ignore]

(** Directed Hausdorff distance: [max_{p in t} min_{q in other} d(p,q)]. *)
val directed_hausdorff_distance : t -> t -> S1_angle.t
[@@zero_alloc ignore]

(** Undirected Hausdorff distance: [max(h(t,other), h(other,t))]. *)
val hausdorff_distance : t -> t -> S1_angle.t
[@@zero_alloc ignore]

(** {1 Comparison} *)

val equal : t -> t -> bool

(** Approximate equality. Default tolerance is [1e-15] radians when [max_error] is [none]. *)
val approx_equal : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool
[@@zero_alloc]

(** Approximate equality with separate latitude and longitude tolerances. *)
val approx_equal_latlng : max_error:S2_latlng.t -> t -> t -> bool
[@@zero_alloc ignore]
