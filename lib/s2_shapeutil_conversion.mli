(** Conversions from a generic {!S2_shape.t} to typed geometry containers ({!S2_point.t}
    list, {!S2_polyline.t}, {!S2_polygon.t}). *)

open Core

(** [shape_to_points shape] returns the vertices of a 0-dimensional [shape] (one [v0] per
    edge). The empty shape returns an empty array. Caller must pass a shape with
    [dimension = 0]; behaviour is otherwise unspecified. *)
val shape_to_points : S2_shape.t -> S2_point.t array

(** [shape_to_polyline shape] returns an {!S2_polyline.t} whose vertices are the vertices
    of [shape]'s first chain. Requires [dimension = 1] and [num_chains = 1] and at least
    one edge; behaviour is otherwise unspecified.

    The resulting polyline is constructed without validation, so the input shape's vertex
    sequence is preserved bit-exactly. *)
val shape_to_polyline : S2_shape.t -> S2_polyline.t

(** [shape_to_polygon shape] returns an {!S2_polygon.t} reconstructed from the chains of
    [shape]. Each chain becomes one {!S2_loop.t}, and the loops are handed to
    {!S2_polygon.of_loops} (single-loop shape) or {!S2_polygon.of_oriented_loops}
    (multi-loop shape) so that holes are inverted as needed.

    If [S2_shape.is_full shape], the result is the full polygon regardless of [shape]'s
    dimension. Otherwise [shape] must have [dimension = 2]. *)
val shape_to_polygon : S2_shape.t -> S2_polygon.t
