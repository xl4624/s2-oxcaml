(** A point on the unit sphere as a pair of latitude-longitude coordinates.

    Latitude is measured from the equator towards the poles and ranges from [-pi/2, pi/2].
    Longitude is measured from the prime meridian and ranges from [-pi, pi].

    The intent is to represent spherical geometry as a mathematical abstraction; functions
    related to the Earth's geometry (e.g. easting/northing) belong elsewhere. *)
open Core

[@@@zero_alloc all]

type t : float64 & float64 [@@deriving sexp_of, unboxed_option { sentinel = true }]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val pp : Format.formatter -> t -> unit [@@zero_alloc ignore]
val to_string : t -> string [@@zero_alloc ignore]

(** {1 Optional LatLng}

    An optional latlng representation that avoids allocating an [option] wrapper. Uses a
    NaN sentinel to represent absence. *)
module Option : sig
  include module type of Option

  val%template sexp_of_t : t -> Sexp.t @ m
  [@@alloc a @ m = (heap @ global, stack @ local)] [@@zero_alloc ignore]
end

(** {1 Constructors} *)

(** Construct from latitude and longitude angles. The values are allowed to be outside the
    {!is_valid} range; see {!normalized}. *)
val create : lat:S1_angle.t -> lng:S1_angle.t -> t

(** The default value: latitude and longitude both zero. *)
val zero : t

(** An invalid S2LatLng (lat = pi, lng = 2*pi) for use as a sentinel. *)
val invalid : t

(** Convenience constructor from radians. *)
val of_radians : lat:float# -> lng:float# -> t

(** Convenience constructor from degrees. *)
val of_degrees : lat:float# -> lng:float# -> t

(** Convenience constructors from E5/E6/E7 integer representations. These return
    [Option.some t] if both coordinates are within their valid ranges (latitude in
    [-90, 90] degrees), and [Option.none] otherwise. *)
val of_e5 : lat:int -> lng:int -> Option.t

val of_e5_exn : lat:int -> lng:int -> t
val of_e6 : lat:int -> lng:int -> Option.t
val of_e6_exn : lat:int -> lng:int -> t
val of_e7 : lat:int -> lng:int -> Option.t
val of_e7_exn : lat:int -> lng:int -> t

(** Construct from a direction vector (not necessarily unit length). *)
val of_point : S2_point.t -> t

(** {1 Accessors} *)

(** Latitude angle. *)
val lat : t -> S1_angle.t

(** Longitude angle. *)
val lng : t -> S1_angle.t

(** Compute the latitude of a point. *)
val latitude : S2_point.t -> S1_angle.t

(** Compute the longitude of a point. *)
val longitude : S2_point.t -> S1_angle.t

(** {1 Predicates} *)

(** [true] if latitude is in [-pi/2, pi/2] and longitude is in [-pi, pi]. *)
val is_valid : t -> bool

(** {1 Normalization} *)

(** Clamp latitude to [-pi/2, pi/2] and wrap longitude to [-pi, pi]. Returns {!invalid} if
    either component is not finite. *)
val normalized : t -> t

(** {1 Conversion} *)

(** Convert to the equivalent unit-length point on the sphere. The maximum error in the
    result is 1.5 * epsilon_float. *)
val to_point : t -> S2_point.t

(** {1 Distance} *)

(** Angular distance using the Haversine formula. Both values must be normalized. *)
val distance : t -> t -> S1_angle.t

(** {1 Arithmetic} *)

(** Component-wise addition (result is not normalized). *)
val add : t -> t -> t

(** Component-wise subtraction (result is not normalized). *)
val sub : t -> t -> t

(** Scalar multiplication (result is not normalized). *)
val mul : t -> float# -> t

(** {1 Comparison} *)

(** Approximate equality: each coordinate differs by at most [max_error] radians (default
    1e-15). *)
val approx_equal : ?max_error:float -> t -> t -> bool
[@@zero_alloc ignore]

val equal : t -> t -> bool
