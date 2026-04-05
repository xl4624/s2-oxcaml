open Core

(** A disc-shaped region on the unit sphere, defined by a center and a radius. Technically
    this is a "spherical cap" rather than a planar disc, because it is not flat -- it
    represents the portion of the sphere cut off by a plane. The boundary of the cap is
    the circle formed by the intersection of the sphere and that plane.

    The cap is a closed set, meaning it contains its boundary. The radius is measured
    along the surface of the sphere (not the straight-line chord distance through the
    interior). A cap of radius [pi /. 2.] is a hemisphere; a cap of radius [pi] covers the
    entire sphere.

    A cap can also be defined by its center and height, where the height is the distance
    from the center point to the cutoff plane. The relationships are:

    {[
      h = 1 -. cos r = 2. *. (sin (r /. 2.) ** 2.)
    ]}

    Empty and full caps are supported, containing no points and all points respectively. *)

(* TODO: replace with [S2_latlng_rect.t] once ported.
   Bounding rectangle in latitude-longitude space. *)
type lat_lng_rect =
  { lat : R1_interval.t
  ; lng : S1_interval.t
  }
[@@deriving sexp_of]

[@@@zero_alloc all]

type t : (float64 & float64 & float64) & float64 [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Optional Cap}
    An optional cap representation that avoids allocating an [option] wrapper. Uses a NaN
    sentinel. *)
module Option : sig
  type value := t
  type nonrec t = t

  val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

  (** The absent value. *)
  val none : t

  (** [some v] wraps [v] as a present value. *)
  val some : value -> t

  val is_none : t -> bool
  val is_some : t -> bool

  (** [value t ~default] returns the wrapped cap, or [default] if [t] is [none]. *)
  val value : t -> default:value -> value

  (** [value_exn t] returns the wrapped cap, or raises if [t] is [none]. *)
  val value_exn : t -> value

  (** [unchecked_value t] returns the wrapped cap without checking for [none]. *)
  val unchecked_value : t -> value

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool
      val unsafe_value : t -> value
    end
  end
end

(** {1 Constructors} *)

val empty : t
val full : t

(** Singleton cap (zero radius). *)
val of_point : S2_point.t -> t

(** Center must be unit length. Angle is clamped to [[0, pi]] before conversion; angles
    larger than [pi] yield a full cap. *)
val of_center_angle : S2_point.t -> S1_angle.t -> t

(** [center] should be unit length. *)
val of_center_chord_angle : S2_point.t -> S1_chord_angle.t -> t

(** [height] is distance from center to the cutoff plane along [center]. Negative height
    gives an empty cap; [height >= 2] gives a full cap. *)
val of_center_height : S2_point.t -> float -> t
[@@zero_alloc ignore]

(** [area] is surface area on the unit sphere (solid angle). Negative yields empty;
    [area >= 4 * pi] yields full. *)
val of_center_area : S2_point.t -> float -> t
[@@zero_alloc ignore]

(** {1 Accessors} *)

val center : t -> S2_point.t
val radius_chord : t -> S1_chord_angle.t
val height : t -> float [@@zero_alloc ignore]

(** Angular radius (may differ slightly from the angle passed to {!of_center_angle}). *)
val radius_angle : t -> S1_angle.t

val area : t -> float# [@@zero_alloc ignore]

(** Area-weighted centroid; for an empty cap this is [(0,0,0)]. *)
val centroid : t -> S2_point.t
[@@zero_alloc ignore]

(** Latitude-longitude bounds (see [S2Cap::GetRectBound]). *)
val rect_bound : t -> lat_lng_rect
[@@zero_alloc ignore]

(** {1 Predicates} *)

val is_valid : t -> bool
val is_empty : t -> bool
val is_full : t -> bool
val equal : t -> t -> bool [@@zero_alloc ignore]

(** Center angles and squared chord radii within [max_error] (radians on the sphere for
    centers, absolute on [length2] for radii). *)
val approx_equal : ?max_error:float -> t -> t -> bool
[@@zero_alloc ignore]

(** {1 Set operations} *)

val complement : t -> t [@@zero_alloc ignore]
val contains_cap : t -> t -> bool [@@zero_alloc ignore]
val intersects : t -> t -> bool [@@zero_alloc ignore]
val interior_intersects : t -> t -> bool [@@zero_alloc ignore]
val interior_contains_point : t -> S2_point.t -> bool [@@zero_alloc ignore]

(** [p] must be unit length. *)
val contains_point : t -> S2_point.t -> bool
[@@zero_alloc ignore]

(** Smallest cap containing [cap] and [p]. If [cap] is empty, center becomes [p]. *)
val add_point : t -> S2_point.t -> t
[@@zero_alloc ignore]

(** Including the rounding bump on the chord sum. *)
val add_cap : t -> t -> t
[@@zero_alloc ignore]

(** Expanding an empty cap yields empty. *)
val expanded : t -> S1_angle.t -> Option.t

(** @raise [Invalid_argument] if [distance] is negative. *)
val expanded_exn : t -> S1_angle.t -> t
[@@zero_alloc ignore]

(** Smallest cap enclosing both operands. *)
val union : t -> t -> t [@@zero_alloc ignore]

(** {1 Encoding}

    Binary layout: four IEEE doubles [x; y; z; length2]. *)

val encode : t -> string [@@zero_alloc ignore]
val decode : string -> Option.t [@@zero_alloc ignore]
val decode_exn : string -> t [@@zero_alloc ignore]
