open Core

[@@@zero_alloc all]

(** A cell in the S2 cell decomposition with its [(u, v)] bounds pre-computed.

    An {!S2_cell.t} wraps an {!S2_cell_id.t} together with the cached face, level, Hilbert
    orientation, and the cell's bounding rectangle in cube-face [(u, v)] coordinates.
    Keeping these values around makes point containment, vertex/edge queries, and distance
    computations cheap, so {!S2_cell.t} is the right type whenever you need to treat the
    cell as a 2D region rather than a bare 64-bit handle. If you only need an identifier
    or want to compare ranges along the Hilbert curve, prefer {!S2_cell_id.t}.

    A cell is a closed quadrilateral bounded by four great-circle arcs. Vertices are
    returned in CCW order when the cell is viewed from outside the sphere (lower left,
    lower right, upper right, upper left in the UV plane). Edges are indexed so that edge
    [k] runs from vertex [k] to vertex [k + 1 mod 4].

    The type is a fully unboxed record (id plus three [int]s plus the four bounds of the
    [R2_rect]). Values are cheap to pass around and copy.

    Limitations:
    - The latitude-longitude bounding rectangle of a cell is computed by
      {!S2_latlng_rect.from_cell}; {!S2_cell} itself does not expose a [rect_bound]
      function to avoid a module cycle between [s2_cell] and [s2_latlng_rect]. *)
type t :
  bits64 & immediate & immediate & immediate & ((float64 & float64) & (float64 & float64))
[@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [of_cell_id id] builds the {!S2_cell.t} denoted by [id]. Cheap: it reads [id]'s
    [(face, i, j, orientation)] and materializes the UV bounds. *)
val of_cell_id : S2_cell_id.t -> t

(** [of_point p] returns the leaf cell containing [p]. [p] need not be unit length; see
    {!S2_cell_id.from_point} for tie-breaking on cell boundaries. *)
val of_point : S2_point.t -> t

(** [of_latlng ll] returns the leaf cell containing the point corresponding to [ll]. *)
val of_latlng : S2_latlng.t -> t

(** [from_face face] returns the top-level cell for cube [face], [face] in [[0, 5]]. *)
val from_face : int -> t

(** [from_face_pos_level ~face ~pos ~level] reconstructs the cell with the given face,
    Hilbert position, and level. See {!S2_cell_id.from_face_pos_level}. *)
val from_face_pos_level : face:int -> pos:int64# -> level:int -> t

(** {1 Accessors} *)

(** The cell id underlying this cell. *)
val id : t -> S2_cell_id.t

(** Cube face containing the cell ([0..5]). *)
val face : t -> int

(** Subdivision level ([0..max_level]). *)
val level : t -> int

(** Hilbert orientation of the cell ([0..3], encoding [swap] and [invert] bits as in
    {!S2_cell_id}). *)
val orientation : t -> int

(** [is_leaf t] is [true] iff [t] has level [S2_cell_id.max_level]. *)
val is_leaf : t -> bool

(** [bound_uv t] is the [(u, v)] bounding rectangle of [t] on its face. *)
val bound_uv : t -> R2_rect.t

(** [size_ij t] is the edge length of [t] in leaf-cell units. *)
val size_ij : t -> int

(** [size_st t] is the edge length of [t] in cell-space [(s, t)] coordinates. *)
val size_st : t -> float#

(** {1 Geometry} *)

(** [vertex t k] returns the [k]-th vertex of [t] as a unit vector ([k] in [[0, 3]]).
    Vertices are in CCW order (lower left, lower right, upper right, upper left in the UV
    plane). *)
val vertex : t -> int -> S2_point.t

(** [vertex_raw t k] is {!vertex} without the final [normalize], so the result lies on the
    biunit cube rather than the sphere. Cheaper when magnitude does not matter. *)
val vertex_raw : t -> int -> S2_point.t

(** [edge t k] returns the unit-length inward normal of the great circle through vertex
    [k] and vertex [k + 1 mod 4]. The plane defined by this normal contains the edge; the
    inward-facing sign is consistent with cell containment. *)
val edge : t -> int -> S2_point.t

(** [edge_raw t k] is {!edge} without normalization. *)
val edge_raw : t -> int -> S2_point.t

(** [center t] returns the center of [t] in [(s, t)] coordinates projected to the sphere
    as a unit vector. *)
val center : t -> S2_point.t

(** [center_raw t] is {!center} without normalization (on the biunit cube). *)
val center_raw : t -> S2_point.t

(** {1 Measures} *)

(** [exact_area t] returns [t]'s surface area on the unit sphere, computed from the actual
    spherical quadrilateral vertices. Accurate to roundoff. *)
val exact_area : t -> float#

(** [approx_area t] returns an approximate surface area. Uses {!average_area} at levels
    [0] and [1] and a flat-quadrilateral approximation corrected for curvature at deeper
    levels; accuracy improves with level and is within ~0.1% by level 5. *)
val approx_area : t -> float#

(** [average_area level] is the sphere's area ([4 * pi]) divided by the number of cells at
    [level] ([6 * 4^level]). *)
val average_area : int -> float#

(** {1 Relations} *)

(** [contains_point t p] is [true] iff [p] lies in [t]'s closed cube-face region. The test
    is performed in [(u, v)] space expanded by a small rounding margin, so boundary points
    are treated consistently even after projection roundoff. *)
val contains_point : t -> S2_point.t -> bool

(** [contains_cell t other] is equivalent to [S2_cell_id.contains (id t) (id other)]. *)
val contains_cell : t -> t -> bool

(** [intersects_cell t other] is equivalent to [S2_cell_id.intersects (id t) (id other)]. *)
val intersects_cell : t -> t -> bool

(** {1 Subdivide} *)

(** [child t ~pos] returns the [pos]-th Hilbert child of [t] ([pos] in [[0, 3]]). Raises
    if [t] is a leaf cell. *)
val child : t -> pos:int -> t

(** {1 Bounding} *)

(** [cap_bound t] returns a spherical cap that contains [t]. Built from the cell center
    plus all four vertices so the radius is tight. *)
val cap_bound : t -> S2_cap.t

(** [cell_union_bound t] returns a single-cell covering of [t] (namely [[| id t |]]). *)
val cell_union_bound : t -> S2_cell_id.t array
[@@zero_alloc ignore]

(** {1 Distance} *)

(** [distance_to_point t p] returns the minimum distance from [t] (treated as a closed
    region including its interior) to [p]. Returns [S1_chord_angle.zero] if [p] lies
    inside [t]. *)
val distance_to_point : t -> S2_point.t -> S1_chord_angle.t

(** [boundary_distance_to_point t p] returns the distance from [t]'s boundary (the four
    great-circle arcs only) to [p]. Unlike {!distance_to_point}, this is nonzero for
    interior points. *)
val boundary_distance_to_point : t -> S2_point.t -> S1_chord_angle.t

(** [max_distance_to_point t p] returns the maximum distance from any point in [t]
    (including interior) to [p]. *)
val max_distance_to_point : t -> S2_point.t -> S1_chord_angle.t

(** [distance_to_edge t a b] returns the minimum distance from [t] to the edge [ab].
    Returns [S1_chord_angle.zero] if [ab] intersects [t] or if either endpoint lies inside
    [t]. *)
val distance_to_edge : t -> S2_point.t -> S2_point.t -> S1_chord_angle.t

(** [max_distance_to_edge t a b] returns the maximum distance from any point in [t] to any
    point on edge [ab]. If the antipode of [ab] crosses [t] the result is
    [S1_chord_angle.straight] ([pi]). *)
val max_distance_to_edge : t -> S2_point.t -> S2_point.t -> S1_chord_angle.t

(** [distance_to_cell t other] returns the minimum distance between [t] and [other].
    Returns [S1_chord_angle.zero] if the cells intersect. *)
val distance_to_cell : t -> t -> S1_chord_angle.t

(** [max_distance_to_cell t other] returns the maximum distance between any point of [t]
    and any point of [other]. If either cell intersects the antipode of the other, the
    result is [S1_chord_angle.straight] ([pi]). *)
val max_distance_to_cell : t -> t -> S1_chord_angle.t

(** [uv_coord_of_edge t k] returns [v] when edge [k] is a horizontal edge (bottom/top, [k]
    even) and [u] when it is vertical (right/left, [k] odd) - that is, whichever
    coordinate stays constant along the edge. *)
val uv_coord_of_edge : t -> int -> float#

(** [ij_coord_of_edge t k] is {!uv_coord_of_edge} converted back to the corresponding
    leaf-cell [i] or [j] index (via half-to-even rounding of [limit_ij * st_of_uv]). *)
val ij_coord_of_edge : t -> int -> int

(* The latitude-longitude bounding rectangle of an [S2_cell.t] is computed by
   {!S2_latlng_rect.from_cell}, which lives in [s2_latlng_rect] to avoid a
   dependency cycle between [s2_cell] and [s2_latlng_rect]. *)
