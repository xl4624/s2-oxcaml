(** Length, perimeter, area, and centroid measures for arbitrary {!S2_shape.t} values.

    These functions abstract away the storage choice for shape data: they read everything
    they need through the public {!S2_shape.t} interface, so the same algorithm can be
    applied to {!S2_loop.t}, {!S2_polyline.t}, {!S2_lax_polygon.t}, and any other shape
    type. The polygon-only and polyline-only methods on those concrete types remain the
    fastest path when you already hold the underlying value.

    All edges are modeled as spherical geodesics; results are on the unit sphere. To
    convert to Earth-surface lengths or areas use the helpers in {!Earth}. *)

(** [length shape] is the sum of all polyline lengths on the unit sphere when [shape] has
    dimension 1. Returns {!S1_angle.zero} for shapes of any other dimension. *)
val length : S2_shape.t -> S1_angle.t
[@@zero_alloc ignore]

(** [perimeter shape] is the sum of all loop perimeters on the unit sphere when [shape]
    has dimension 2. Returns {!S1_angle.zero} for shapes of any other dimension. *)
val perimeter : S2_shape.t -> S1_angle.t
[@@zero_alloc ignore]

(** [area shape] is the area of [shape] on the unit sphere when [shape] has dimension 2.
    The result is in steradians, in [[0, 4 * pi]]. Returns [0.0] for shapes of any other
    dimension. Has good relative accuracy for both very large and very small regions.

    Edge cases: very rarely (when the true area is very close to either [0] or [4 * pi]
    and the polygon has multiple loops) the result may be the area of the complementary
    region. Detecting and correcting that requires inferring the loop nesting structure
    and is not yet implemented. *)
val area : S2_shape.t -> float#
[@@zero_alloc ignore]

(** [approx_area shape] is like {!area} but cheaper and less accurate. The additional
    error is at most [2.22e-15] steradians per vertex (about 0.09 m^2 per vertex on
    Earth's surface). Returns [0.0] for shapes whose dimension is not 2. *)
val approx_area : S2_shape.t -> float#
[@@zero_alloc ignore]

(** [centroid shape] returns the centroid of [shape] multiplied by the {b measure} of the
    shape, defined as:

    - dimension 0: the number of edges (i.e. points);
    - dimension 1: the total {!length};
    - dimension 2: the total {!area}.

    The result is not unit length and may need to be normalised before passing to other S2
    functions. The scale factor lets callers combine centroids of multiple shapes by
    summing the per-shape centroids of the maximal-dimension contributors. *)
val centroid : S2_shape.t -> S2_point.t
[@@zero_alloc ignore]

(** [chain_vertices shape chain_id] returns the vertex sequence of the [chain_id]-th chain
    in [shape]: [chain.length + 1] vertices for an open polyline (dimension 1) and
    [chain.length] vertices otherwise. The returned array is freshly allocated. *)
val chain_vertices : S2_shape.t -> int -> S2_point.t array
[@@zero_alloc ignore]
