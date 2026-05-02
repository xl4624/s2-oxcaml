(** A closed latitude-longitude rectangle on the unit sphere.

    Latitude-longitude space is treated as a cylinder rather than a sphere: longitudes
    wrap around at +/-180 degrees, while latitudes are clamped to [-90, 90] degrees. The
    rectangle is the product of a latitude interval (in radians) and a longitude interval
    (in radians); when the longitude interval is inverted, the rectangle crosses the
    antimeridian. The empty and full rectangles, as well as single points, all have
    canonical representations.

    Because the poles have infinitely many longitude representatives, a rectangle can
    include some representations of a pole but not others. Use {!polar_closure} to expand
    a rectangle so that it contains every representation of any pole it touches.

    Typical use is as a conservative bounding box for geometry: construct a rectangle from
    points, cells, or caps, and then test containment/intersection against other regions.
    Most operations are O(1). *)
open Core

[@@@zero_alloc all]

type t : (float64 & float64) & (float64 & float64)
[@@deriving sexp_of, unboxed_option { sentinel = true }]

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

(** [lat t] returns the latitude interval of [t] (radians). *)
val lat : t -> R1_interval.t

(** [lng t] returns the longitude interval of [t] (radians). *)
val lng : t -> S1_interval.t

(** [lo t] returns the lower-left corner ([lat_lo], [lng_lo]). *)
val lo : t -> S2_latlng.t

(** [hi t] returns the upper-right corner ([lat_hi], [lng_hi]). *)
val hi : t -> S2_latlng.t

(** [center t] returns the center in latitude-longitude space. This is generally not the
    centroid of the region on the sphere. *)
val center : t -> S2_latlng.t

(** [size t] returns the (lat, lng) extent in radians. Empty rectangles have negative
    width and height. *)
val size : t -> S2_latlng.t

(** [vertex t k] returns the [k]-th vertex in CCW order: lower-left, lower-right,
    upper-right, upper-left. [k] is reduced modulo 4. *)
val vertex : t -> int -> S2_latlng.t

(** [area t] returns the surface area of the rectangle on the unit sphere, in steradians. *)
val area : t -> float#

(** [centroid t] returns the true centroid of the rectangle multiplied by its surface
    area. The result is not unit length, and in general does not lie inside the rectangle.
    Multiplying by the area makes centroids of disjoint regions additive. *)
val centroid : t -> S2_point.t

(** {1 Predicates} *)

(** [is_valid t] is true when latitude bounds stay within [-pi/2, pi/2], the longitude
    interval is valid (as per {!S1_interval}), and latitude/longitude are either both
    empty or both non-empty. *)
val is_valid : t -> bool

(** [is_empty t] is true when the rectangle contains no points. *)
val is_empty : t -> bool

(** [is_full t] is true when the rectangle contains every point on the sphere. *)
val is_full : t -> bool

(** [is_point t] is true when the latitude and longitude intervals each consist of a
    single value. *)
val is_point : t -> bool

(** [is_inverted t] is true when [lng_lo > lng_hi], i.e. the rectangle crosses the
    antimeridian. *)
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

(** [union t other] returns the smallest rectangle containing both inputs. *)
val union : t -> t -> t

(** [intersection t other] returns the smallest rectangle containing the intersection of
    the two inputs. The intersection may actually consist of two disjoint rectangles (when
    the longitude intervals split); in that case a single spanning rectangle is returned. *)
val intersection : t -> t -> t

(** [add_point t ll] returns the smallest rectangle that contains [t] together with [ll].
    [ll] must be normalized. *)
val add_point : t -> S2_latlng.t -> t

(** [expanded t margin] expands the rectangle by [margin.lat] on each latitude side and
    [margin.lng] on each longitude side. Negative margins shrink the rectangle; if either
    interval becomes empty the result is the canonical empty rectangle. Latitude is
    clamped to [-pi/2, pi/2]. See also {!polar_closure} if the result should include every
    representation of a pole it contains. *)
val expanded : t -> S2_latlng.t -> t

(** [polar_closure t] expands the longitude interval to full whenever [t] touches a pole,
    and returns [t] unchanged otherwise. Use this when an expanded rectangle must contain
    every longitude at a contained pole. *)
val polar_closure : t -> t

(** [expanded_by_distance t distance] expands [t] so that it contains every point within
    the given spherical [distance] of the rectangle (measured along the sphere, rather
    than independently in latitude and longitude).

    A negative [distance] shrinks the rectangle so that it excludes every point within
    [|distance|] of the original boundary, and may collapse to {!empty} when [|distance|]
    exceeds half the rectangle's latitude or longitude width. The full and empty
    rectangles, and rectangles that cover the full longitude range and include a pole,
    have no boundary at the corresponding side and are not adjusted there. *)
val expanded_by_distance : t -> S1_angle.t -> t

(** {1 Boundary intersections} *)

(** [boundary_intersects t v0 v1] reports whether the boundary of [t] intersects the
    geodesic edge from [v0] to [v1]. Returns [false] for empty and full rectangles, which
    have no boundary on the sphere. *)
val boundary_intersects : t -> S2_point.t -> S2_point.t -> bool

(** [intersects_lng_edge a b ~lat ~lng] reports whether the geodesic edge from [a] to [b]
    intersects the rectangle edge of constant longitude [lng] spanning the latitude
    interval [lat]. *)
val intersects_lng_edge
  :  S2_point.t
  -> S2_point.t
  -> lat:R1_interval.t
  -> lng:float#
  -> bool

(** [intersects_lat_edge a b ~lat ~lng] reports whether the geodesic edge from [a] to [b]
    intersects the (curved) rectangle edge of constant latitude [lat] spanning the
    longitude interval [lng]. [a] and [b] must be unit length. *)
val intersects_lat_edge
  :  S2_point.t
  -> S2_point.t
  -> lat:float#
  -> lng:S1_interval.t
  -> bool

(** {1 Bounding} *)

(** [cap_bound t] returns a bounding {!S2_cap} for the rectangle, chosen as the smaller of
    a cap centered at the latitude-longitude center and a cap centered at the appropriate
    pole. *)
val cap_bound : t -> S2_cap.t

(** [from_cap c] returns a latitude-longitude rectangle that bounds the given cap. Returns
    the empty rectangle if [c] is empty. *)
val from_cap : S2_cap.t -> t

(** [from_cell c] returns a tight latitude-longitude rectangle bounding the given cell.
    Accounts for numerical error in the (u, v) to lat/lng projection. *)
val from_cell : S2_cell.t -> t

(** [rect_bound t] returns [t] itself; provided for uniformity with other region types. *)
val rect_bound : t -> t

(** [contains_cell t c] reports whether the rectangle contains the given cell. *)
val contains_cell : t -> S2_cell.t -> bool

(** [intersects_cell t c] is a conservative cheap rect-vs-cell test: returns [true]
    whenever [t] could intersect [c]. May return [true] for cells whose spherical geometry
    does not actually intersect [t] but whose lat/lng bound does. Used by region coverers
    and by other modules' bound-rejection paths; for a precise answer use
    {!intersects_s2_cell}. *)
val intersects_cell : t -> S2_cell.t -> bool

(** [intersects_s2_cell t c] reports whether [t] and [c] share any point. Exact (modulo
    cell vertex rounding): handles the curved nature of latitude edges, so unlike
    {!intersects_cell} it does not return [true] for spherically disjoint cells whose
    lat/lng bound merely overlaps [t]. *)
val intersects_s2_cell : t -> S2_cell.t -> bool
[@@zero_alloc ignore]

(** [cell_union_bound t] returns a small set of cell ids whose union covers [t]. *)
val cell_union_bound : t -> S2_cell_id.t array
[@@zero_alloc ignore]

(** {1 Distance} *)

(** [distance_to_latlng t ll] returns the minimum distance along the sphere from the
    rectangle (boundary and interior) to the point [ll]. Requires [t] non-empty and [ll]
    valid. *)
val distance_to_latlng : t -> S2_latlng.t -> S1_angle.t

(** [distance t other] returns the minimum distance along the sphere between the two
    rectangles. Both must be non-empty. *)
val distance : t -> t -> S1_angle.t

(** [directed_hausdorff_distance t other] returns [max_{p in t} min_{q in other} d(p, q)],
    measured along the sphere. *)
val directed_hausdorff_distance : t -> t -> S1_angle.t

(** [hausdorff_distance t other] returns
    [max (directed_hausdorff_distance t other) (directed_hausdorff_distance other t)]. *)
val hausdorff_distance : t -> t -> S1_angle.t

(** {1 Comparison} *)

(** [equal t other] is structural equality on the latitude and longitude intervals. *)
val equal : t -> t -> bool

(** [approx_equal ~max_error t other] is approximate equality on both intervals. When
    [max_error] is [none] the tolerance defaults to [1e-15] radians. *)
val approx_equal : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool
[@@zero_alloc]

(** [approx_equal_latlng ~max_error t other] is like {!approx_equal} but allows separate
    tolerances for latitude and longitude. *)
val approx_equal_latlng : max_error:S2_latlng.t -> t -> t -> bool
[@@zero_alloc ignore]

(** {1 Limitations}

    The following C++ features are not exposed:
    - [Encode] / [Decode]: binary serialization. *)
