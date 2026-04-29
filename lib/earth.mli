(** Earth modeled as a sphere: conversions between distances on the Earth's surface and
    angles on the unit sphere.

    S2 geometry lives on a unit sphere, so lengths and areas come out as angles (radians)
    and solid angles (steradians). This module multiplies those values by the Earth's mean
    radius to express them in physical units. The mean radius used is 6371.01 km
    (6371010 m) - the radius of the equivalent sphere with the same surface area as the
    Earth.

    One radian on the unit sphere corresponds to {!radius_meters} meters on the surface,
    and one steradian corresponds to {!radius_meters}{^ 2} square meters. Because the
    model is a sphere (not an ellipsoid) every angle maps to a unique distance, so the
    conversions are invertible.

    Typical uses:
    - Converting an {!S1_angle.t} distance bound to meters.
    - Converting a ground distance in meters into the {!S1_angle.t} needed to build an
      {!S2_cap.t} or to call one of the [_within_distance] queries.
    - Turning the steradian result of {!S2_polygon.area} into square meters. *)

[@@@zero_alloc all]

(** {1 Constants} *)

(** The Earth's mean radius in meters (6371010 m). This is the radius of the equivalent
    sphere with the same surface area. According to NASA, this value is 6371.01 +/- 0.02
    km. The equatorial radius is 6378.136 km and the polar radius is 6356.752 km. *)
val radius_meters : float#

(** The Earth's mean radius in kilometers (6371.01 km). *)
val radius_km : float#

(** Altitude of the lowest known point on Earth (Challenger Deep), measured from the
    surface of the spherical earth, in meters. *)
val lowest_altitude_meters : float#

(** Altitude of the highest known point on Earth (Mount Everest), measured from the
    surface of the spherical earth, in meters. *)
val highest_altitude_meters : float#

(** {1 Angle/length conversions} *)

(** [angle_from_length meters] returns the central angle subtended by a geodesic of length
    [meters] on the Earth's surface. Equivalent to [meters / radius_meters] radians. *)
val angle_from_length : float# -> S1_angle.t

(** [length_from_angle angle] returns the geodesic length in meters subtended by [angle]
    on the Earth's surface. *)
val length_from_angle : S1_angle.t -> float#

(** [length_from_points a b] returns the great-circle distance in meters between the two
    unit-length points [a] and [b]. *)
val length_from_points : S2_point.t -> S2_point.t -> float#

(** [length_from_latlngs a b] returns the great-circle distance in meters between the two
    lat/lng points. Both arguments must be normalized. *)
val length_from_latlngs : S2_latlng.t -> S2_latlng.t -> float#

(** {1 Area/steradians conversions} *)

(** [area_from_steradians s] returns the area in square meters on the Earth's surface
    covered by the solid angle [s] (in steradians). Pass values produced by S2 area
    routines such as {!S2_polygon.area} or {!S2_cap.area}. *)
val area_from_steradians : float# -> float#

(** [steradians_from_area m2] returns the solid angle (in steradians) covered by an area
    of [m2] square meters on the Earth's surface. The value is in [0, 4 * pi] when [m2]
    does not exceed the surface area of the Earth. *)
val steradians_from_area : float# -> float#

(** {1 Bearing} *)

(** [initial_bearing_from_latlngs a b] computes the initial great-circle bearing from [a]
    toward [b]: the bearing an observer at [a] has when facing [b]. A bearing of 0 degrees
    is north, increasing clockwise (90 is east, 180 is south, 270 is west).

    The return value is undefined when [a = b], when [a = -b], or when [a] is one of the
    poles (the direction "north" is degenerate there). *)
val initial_bearing_from_latlngs : S2_latlng.t -> S2_latlng.t -> S1_angle.t
