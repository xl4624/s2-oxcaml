(** An open polyline on the unit sphere: a sequence of vertices connected by edges.

    A lax polyline has [n] vertices and [max 0 (n - 1)] edges connecting consecutive
    vertices. It is the degenerate-friendly, slightly more compact counterpart of
    {!S2_polyline}:

    - adjacent vertices may coincide or be antipodal,
    - a polyline with fewer than two vertices defines no edges,
    - a single degenerate edge is expressed by repeating the same vertex twice.

    Lax polylines carry no built-in measurements or predicates; they expose edge data
    through the generic {!S2_shape.t} interface for consumption by shape-index queries.
    Because the shape has no interior, {!reference_point} always reports "not contained"
    and {!dimension} is [1].

    {2 Limitations}

    - Encoding and decoding are not implemented.
    - Construction from an {!S2_polyline} is not provided.
    - The lazy [EncodedS2LaxPolylineShape] variant is not ported.
    - The closed-polyline variant [S2LaxClosedPolylineShape] (which wraps the last vertex
      back to the first) is not ported. *)

open Core

[@@@zero_alloc all]

type t [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [of_vertices vs] builds a lax polyline whose vertex sequence is a copy of [vs], so
    later mutation of [vs] does not affect the polyline. *)
val of_vertices : S2_point.t array -> t
[@@zero_alloc ignore]

(** {1 Accessors} *)

(** [num_vertices t] is the number of stored vertices. *)
val num_vertices : t -> int

(** [vertex t i] returns the [i]-th vertex. Requires [0 <= i < num_vertices t]. *)
val vertex : t -> int -> S2_point.t

(** {1 Shape interface}

    These functions implement the {!S2_shape.t} interface. *)

(** [num_edges t] is [max 0 (num_vertices t - 1)]. *)
val num_edges : t -> int

(** [edge t e] returns the [e]-th edge [(vertex t e, vertex t (e + 1))]. Requires
    [0 <= e < num_edges t]. *)
val edge : t -> int -> S2_shape.Edge.t

(** [dimension t] is [1]: a polyline is a one-dimensional shape. *)
val dimension : t -> int

(** [num_chains t] is [1] if the polyline has any edges, [0] otherwise. *)
val num_chains : t -> int

(** [chain t i] returns the single chain spanning every edge. Requires [i = 0]. *)
val chain : t -> int -> S2_shape.Chain.t

(** [chain_edge t i j] is [edge t j]. Requires [i = 0]. *)
val chain_edge : t -> int -> int -> S2_shape.Edge.t

(** [chain_position t e] is [{ chain_id = 0; offset = e }]. *)
val chain_position : t -> int -> S2_shape.Chain_position.t

(** [reference_point t] always returns a reference point with [contained = false]: a
    polyline has no interior. *)
val reference_point : t -> S2_shape.Reference_point.t

(** [type_tag] is the encoded-shape tag for lax polylines ([4]), matching the upstream
    [S2LaxPolylineShape::kTypeTag]. *)
val type_tag : S2_shape.Type_tag.t

(** [to_shape t] exposes [t] through the generic {!S2_shape.t} interface. *)
val to_shape : t -> S2_shape.t
[@@zero_alloc ignore]
