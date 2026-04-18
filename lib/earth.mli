(** Earth modeled as a sphere: conversions between distances on the Earth's surface and
    angles on the unit sphere.

    The mean radius is 6371.01 km (6371010 m).

    Because the Earth is modeled as a sphere, a given angle corresponds to a unique
    distance along the surface, so conversions are possible in both directions. The angle
    is measured from the center of the (unit) sphere, so one radian corresponds to
    {!radius_meters} meters on the surface. *)

open Core

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

(** [angle_from_length meters] returns the angle subtended from the sphere's center by a
    geodesic of length [meters] on the Earth's surface. *)
val angle_from_length : float# -> S1_angle.t

(** [length_from_angle angle] returns the geodesic length in meters on the Earth's surface
    subtended by [angle]. *)
val length_from_angle : S1_angle.t -> float#

(** [length_from_points a b] returns the distance in meters between two points on the
    Earth's surface. *)
val length_from_points : S2_point.t -> S2_point.t -> float#

(** [length_from_latlngs a b] returns the distance in meters between two lat/lng points on
    the Earth's surface. Both arguments must be normalized. *)
val length_from_latlngs : S2_latlng.t -> S2_latlng.t -> float#

(** {1 Area/steradians conversions} *)

(** [area_from_steradians s] returns the area in square meters on the Earth's surface
    covered by [s] steradians, as returned by [Area] methods on S2 geometry types. *)
val area_from_steradians : float# -> float#

(** [steradians_from_area m2] returns the number of steradians covered by an area of [m2]
    square meters on the Earth's surface. The value will be between 0 and [4 * pi] if the
    area does not exceed the area of the Earth. *)
val steradians_from_area : float# -> float#

(** {1 Bearing} *)

(** [initial_bearing_from_latlngs a b] computes the initial bearing from [a] to [b] - the
    bearing an observer at point [a] has when facing point [b]. A bearing of 0 degrees is
    north, and it increases clockwise (90 degrees is east, etc.).

    If [a = b], [a = -b], or [a] is one of the Earth's poles, the return value is
    undefined. *)
val initial_bearing_from_latlngs : S2_latlng.t -> S2_latlng.t -> S1_angle.t
