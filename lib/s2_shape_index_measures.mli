(** Aggregate angle and area measures over an {!S2_shape_index.t}.

    Each function visits every shape in the index and combines the per-shape result from
    {!S2_shape_measures}. The shape-level helpers already key off the shape's dimension,
    so these aggregates correctly ignore shapes that do not contribute (e.g. polylines do
    not contribute to {!area}).

    All edges are modeled as spherical geodesics; results are on the unit sphere. To
    convert to Earth-surface lengths or areas use the helpers in {!Earth}. *)

(** [dimension index] is the maximum {!S2_shape.dimension} of any shape in [index], or
    [-1] if [index] is empty. The dimension does not depend on whether each shape contains
    any geometry: an empty point set still has dimension 0 and an empty polygon still has
    dimension 2. *)
val dimension : S2_shape_index.t -> int

(** [num_points index] is the total number of dimension-0 points across all shapes in
    [index]. Polyline and polygon vertices are not counted. *)
val num_points : S2_shape_index.t -> int

(** [length index] is the sum of {!S2_shape_measures.length} over every shape in [index].
    Returns {!S1_angle.zero} when no polylines are present. *)
val length : S2_shape_index.t -> S1_angle.t
[@@zero_alloc ignore]

(** [perimeter index] is the sum of {!S2_shape_measures.perimeter} over every shape in
    [index]. Returns {!S1_angle.zero} when no polygons are present. *)
val perimeter : S2_shape_index.t -> S1_angle.t
[@@zero_alloc ignore]

(** [area index] is the sum of {!S2_shape_measures.area} over every shape in [index].
    Returns [0.0] when no polygons are present. The result may exceed [4 * pi] when
    [index] contains overlapping polygons. *)
val area : S2_shape_index.t -> float#
[@@zero_alloc ignore]

(** [approx_area index] is the sum of {!S2_shape_measures.approx_area} over every shape in
    [index]. Cheaper and slightly less accurate than {!area}. *)
val approx_area : S2_shape_index.t -> float#
[@@zero_alloc ignore]

(** [centroid index] is the sum of {!S2_shape_measures.centroid} over every shape whose
    dimension equals [dimension index]. Lower-dimensional shapes are ignored: when [index]
    contains both polylines and points, the points do not contribute. The result is scaled
    by the total per-shape measure (see {!S2_shape_measures.centroid}) and is [(0, 0, 0)]
    when [index] is empty. *)
val centroid : S2_shape_index.t -> S2_point.t
