(** Angle and area measures on the unit sphere.

    These functions operate on triples of unit-length {!S2_point.t} values forming
    spherical triangles. They match Google's C++ [S2::Angle], [S2::TurnAngle], [S2::Area],
    [S2::GirardArea], and [S2::SignedArea]. *)

open Core

(** [angle a b c] returns the interior angle at vertex [b] in the spherical triangle
    [abc]. The result is always in the range [[0, pi]]. All points should be normalized.
    Satisfies [angle a b c = angle c b a] for all [a], [b], [c].

    The angle is undefined if [a] or [c] is diametrically opposite from [b], and becomes
    numerically unstable as the length of edge [ab] or [bc] approaches 180 degrees. *)
val angle : S2_point.t -> S2_point.t -> S2_point.t -> S1_angle.t

(** [turn_angle a b c] returns the exterior angle at vertex [b] in the spherical triangle
    [abc]. The result is positive if [abc] is counter-clockwise and negative otherwise.
    If you imagine an ant walking from [a] to [b] to [c], this is the angle that the ant
    turns at vertex [b] (positive = left = CCW, negative = right = CW). This quantity is
    also known as the "geodesic curvature" at [b].

    Satisfies [turn_angle a b c = -turn_angle c b a] for all distinct [a], [b], [c]. The
    result is undefined if [a = b] or [b = c], but is either [-pi] or [pi] if [a = c]. All
    points should be normalized. *)
val turn_angle : S2_point.t -> S2_point.t -> S2_point.t -> S1_angle.t

(** [area a b c] returns the area of the spherical triangle [abc]. Combines l'Huilier's
    theorem and Girard's formula to obtain accurate results for both large and small
    triangles. The maximum error is about [5e-15] (about 0.25 square meters on the
    Earth's surface).

    All points should be unit length, and no two points should be antipodal. The result is
    always non-negative. *)
val area : S2_point.t -> S2_point.t -> S2_point.t -> float#

(** [girard_area a b c] returns the area of the spherical triangle [abc] using Girard's
    formula. About twice as fast as {!area} but has poor relative accuracy for small
    triangles.

    All points should be unit length, and no two points should be antipodal. The result is
    clamped to be non-negative. *)
val girard_area : S2_point.t -> S2_point.t -> S2_point.t -> float#

(** [signed_area a b c] is like {!area} but returns a positive value for counter-clockwise
    triangles and a negative value otherwise. *)
val signed_area : S2_point.t -> S2_point.t -> S2_point.t -> float#
