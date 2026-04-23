(** A closed loop of edges surrounding an interior region on the unit sphere.

    A lax loop is a sequence of {!S2_point.t} vertices [v.(0); ...; v.(n-1)] connected by
    edges [(v.(0), v.(1)), ..., (v.(n-1), v.(0))]. It is a compact, degenerate-friendly
    alternative to {!S2_loop}:

    - adjacent vertices may coincide or be antipodal,
    - a loop may have any number of vertices, including 0, 1, or 2,
    - a one-vertex loop defines a single degenerate edge from the vertex to itself.

    Lax loops are the cheapest way to expose a loop-shaped region through the generic
    {!S2_shape.t} interface so that index-based queries (boolean operations, closest-edge
    queries, etc.) can consume it. Unlike {!S2_loop}, they carry no built-in predicates
    like [contains] or [intersects]; use an {!S2_shape_index} instead.

    The containment of a {!reference_point} is computed once at construction time and
    stored in the value; all accessors are cheap and allocate no new shape state.

    {2 Limitations}

    - Encoding and decoding are not implemented; {!type_tag} is [S2_shape.Type_tag.none].
    - Construction from an {!S2_loop} is not provided; build vertex arrays directly.
    - The vertex-id variant [S2VertexIdLaxLoopShape] from the C++ library (which stores
      vertices as indices into an external array) is not ported.
    - The interior-less [S2LaxClosedPolylineShape] variant is not ported. *)

open Core

[@@@zero_alloc all]

type t [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [of_vertices vs] builds a lax loop whose vertex sequence is a copy of [vs]. The
    reference point and its containment status are computed eagerly. *)
val of_vertices : S2_point.t array -> t
[@@zero_alloc ignore]

(** {1 Accessors} *)

(** [num_vertices t] is the number of stored vertices. *)
val num_vertices : t -> int

(** [vertex t i] returns the [i]-th vertex. Requires [0 <= i < num_vertices t]; an
    out-of-range index raises the standard array-access exception. *)
val vertex : t -> int -> S2_point.t

(** {1 Shape interface}

    These functions implement the {!S2_shape.t} interface. They are safe to call directly
    on a [t] value but are most useful when paired with {!to_shape}. *)

(** [num_edges t] equals [num_vertices t]: a lax loop has one edge per vertex. *)
val num_edges : t -> int

(** [edge t e] is the [e]-th edge [(vertex t e, vertex t ((e + 1) mod num_vertices t))]. *)
val edge : t -> int -> S2_shape.Edge.t

(** [dimension t] is [2]: a lax loop bounds a two-dimensional interior. *)
val dimension : t -> int

(** [num_chains t] is [1] if the loop has at least one vertex and [0] otherwise. *)
val num_chains : t -> int

(** [chain t i] returns the single chain that spans every edge of the loop. Requires
    [i = 0]. *)
val chain : t -> int -> S2_shape.Chain.t

(** [chain_edge t i j] is [edge t j]. Requires [i = 0]. *)
val chain_edge : t -> int -> int -> S2_shape.Edge.t

(** [chain_position t e] is [{ chain_id = 0; offset = e }], locating edge [e] within the
    only chain. *)
val chain_position : t -> int -> S2_shape.Chain_position.t

(** [reference_point t] returns a point on the sphere together with a flag indicating
    whether it lies in the loop interior. The result is cached on [t]. *)
val reference_point : t -> S2_shape.Reference_point.t

(** [type_tag] is the encoded-shape tag used by downstream S2 tooling. Lax loops have no
    wire format, so this is {!S2_shape.Type_tag.none}. *)
val type_tag : S2_shape.Type_tag.t

(** [to_shape t] exposes [t] through the generic {!S2_shape.t} interface for use with
    shape-index consumers. *)
val to_shape : t -> S2_shape.t
[@@zero_alloc ignore]
