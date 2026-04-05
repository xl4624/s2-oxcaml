open Core

(** S2Cell represents a cell on the unit sphere.

    An S2Cell is an S2Region object that represents a cell. Unlike S2CellIds, it supports
    efficient containment and intersection tests. However, it is also a more expensive
    representation. *)
type t [@@deriving sexp_of]

(** {1 Constructors} *)

(** [of_cell_id id] constructs an S2Cell corresponding to the given S2CellId. *)
val of_cell_id : S2_cell_id.t -> t

(** [of_point p] constructs a cell for the given point. *)
val of_point : S2_point.t -> t

(** [of_latlng ll] constructs a cell for the given LatLng. *)
val of_latlng : S2_latlng.t -> t

(** [from_face face] returns the cell corresponding to the given S2 cube face. *)
val from_face : int -> t

(** [from_face_pos_level face pos level] returns a cell given its face, Hilbert curve
    position within that face, and level. *)
val from_face_pos_level : face:int -> pos:int64 -> level:int -> t

(** {1 Accessors} *)

val id : t -> S2_cell_id.t
val face : t -> int
val level : t -> int
val orientation : t -> int
val is_leaf : t -> bool
val bound_uv : t -> R2_rect.t

(** [size_ij t] returns the edge length of this cell in (i,j)-space. *)
val size_ij : t -> int

(** [size_st t] returns the edge length of this cell in (s,t)-space. *)
val size_st : t -> float

(** {1 Geometry} *)

(** [vertex k] returns the normalized k-th vertex of the cell (k = 0,1,2,3). Vertices are
    returned in CCW order (lower left, lower right, upper right, upper left in the UV
    plane). *)
val vertex : t -> int -> S2_point.t

(** [vertex_raw k] returns the unnormalized k-th vertex of the cell. *)
val vertex_raw : t -> int -> S2_point.t

(** [edge k] returns the normalized inward-facing normal of the great circle passing
    through the edge from vertex k to vertex k+1 (mod 4). *)
val edge : t -> int -> S2_point.t

(** [edge_raw k] returns the unnormalized inward-facing normal of the edge. *)
val edge_raw : t -> int -> S2_point.t

(** [center] returns the direction vector corresponding to the center in (s,t)-space of
    the given cell. *)
val center : t -> S2_point.t

(** [center_raw] returns the unnormalized center of the cell. *)
val center_raw : t -> S2_point.t

(** {1 Measures} *)

(** [exact_area] returns the area of this cell as accurately as possible. *)
val exact_area : t -> float

(** [approx_area] returns the approximate area of this cell. *)
val approx_area : t -> float

(** [average_area level] returns the average area of cells at the given level. *)
val average_area : int -> float

(** {1 Relations} *)

(** [contains_point] reports whether this cell contains the given point. *)
val contains_point : t -> S2_point.t -> bool

(** [contains_cell] reports whether this cell contains the other cell. *)
val contains_cell : t -> t -> bool

(** [intersects_cell] reports whether the intersection of this cell and the other cell is
    not empty. *)
val intersects_cell : t -> t -> bool

(** {1 Subdivide} *)

(** [subdivide] returns the four direct children of this cell. Returns [None] if the cell
    is a leaf. *)
val subdivide : t -> t list option

(** {1 Bounding} *)

(** [cap_bound] returns a bounding cap for this cell. *)
val cap_bound : t -> S2_cap.t

(** {1 Distance} *)

(** [distance_to_point] returns the distance from the cell to the given point. Returns
    zero if the point is inside the cell. *)
val distance_to_point : t -> S2_point.t -> S1_chord_angle.t

(** [boundary_distance_to_point] returns the distance from the cell boundary to the given
    point. *)
val boundary_distance_to_point : t -> S2_point.t -> S1_chord_angle.t

(** [max_distance_to_point] returns the maximum distance from the cell (including its
    interior) to the given point. *)
val max_distance_to_point : t -> S2_point.t -> S1_chord_angle.t

(** [uv_coord_of_edge k] returns either U or V for the given edge, whichever is constant
    along it. *)
val uv_coord_of_edge : t -> int -> float#

(** [ij_coord_of_edge k] returns either I or J for the given edge, whichever is constant
    along it. *)
val ij_coord_of_edge : t -> int -> int

(** {1 TODO *)

(* TODO: [rect_bound] returns a bounding latitude-longitude rectangle for this cell. *)
(* val rect_bound : t -> S2_latlng_rect.t *)

(* TODO: [distance_to_edge] returns the distance from the cell to the given edge (a, b). *)
(* val distance_to_edge : t -> S2_point.t -> S2_point.t -> S1_chord_angle.t *)

(* TODO: [max_distance_to_edge] returns the max distance from the cell to the given edge. *)
(* val max_distance_to_edge : t -> S2_point.t -> S2_point.t -> S1_chord_angle.t *)

(* TODO: [distance_to_cell] returns the distance from this cell to the other cell. *)
(* val distance_to_cell : t -> t -> S1_chord_angle.t *)

(* TODO: [max_distance_to_cell] returns the max distance from this cell to the other cell. *)
(* val max_distance_to_cell : t -> t -> S1_chord_angle.t *)
