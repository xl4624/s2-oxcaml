(** A region bounded by a collection of zero or more closed loops on the unit sphere.

    The interior is the region that lies to the left of every loop. A lax polygon is the
    compact, degenerate-friendly alternative to {!S2_polygon}: it carries no built-in
    predicates or boolean operations, but exposes its vertex and edge structure through
    the {!S2_shape.t} interface so that shape-index queries (point-in-polygon, boolean
    operations, nearest edge, ...) can consume it.

    {2 Degeneracies}

    Unlike {!S2_polygon}, this representation permits two kinds of degeneracy:

    - degenerate edges (from a vertex to itself), and
    - sibling edge pairs (two oppositely-oriented copies of the same edge).

    These can represent either degenerate shells or degenerate holes depending on the loop
    that contains them.

    Loops with fewer than three vertices are interpreted as follows:

    - a loop with two vertices defines two edges in opposite directions,
    - a loop with one vertex defines a single degenerate edge,
    - a loop with no vertices is the "full loop" containing every point on the sphere. If
      a full loop is present then every other loop must be entirely degenerate (so that
      the polygon is well-defined); this invariant is not checked.

    {2 Validity}

    No error checking is performed: it is deliberately possible to construct lax polygons
    that violate S2's invariants (for example, to repair them later). To use such a
    polygon for point containment it must satisfy the interior-on-the-left rule (no
    crossing edges, and at most one of any set of duplicate edges may not belong to a
    sibling pair). To use it with boolean operations no degenerate edge or sibling pair
    may coincide with another edge.

    {2 Performance}

    Construction is proportional to the total vertex count and allocates two arrays
    (vertex buffer and loop-start offsets). Edge and chain-position lookup is amortised
    O(1) for sequential access thanks to a last-loop cache; for cold access it is O(k)
    when there are at most 12 loops and O(log k) otherwise.

    Per-thread mutation of the cache is safe (single-threaded OCaml) but two threads
    racing on the same polygon through the shape interface can corrupt the hint; treat a
    [t] as read-only when shared across threads.

    {2 Limitations}

    - Encoding and decoding are not implemented.
    - Construction from an existing {!S2_polygon} is not provided.
    - The lazy {!EncodedS2LaxPolygonShape} variant is not ported. *)

open Core

[@@@zero_alloc all]

type t [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [of_loops loops] builds a lax polygon from the given vertex loops. The outer array
    indexes loops; each inner array is that loop's vertex sequence. Both layers are copied
    so later mutation of the caller's arrays has no effect on the polygon. The reference
    point is computed eagerly. *)
val of_loops : S2_point.t array array -> t
[@@zero_alloc ignore]

(** {1 Accessors} *)

(** [num_loops t] is the number of loops (including any full or degenerate loops). *)
val num_loops : t -> int

(** [num_vertices t] is the total number of vertices summed over all loops. *)
val num_vertices : t -> int

(** [num_loop_vertices t i] is the number of vertices in loop [i]. Requires
    [0 <= i < num_loops t]. *)
val num_loop_vertices : t -> int -> int

(** [loop_vertex t i j] is the [j]-th vertex of loop [i]. Requires [0 <= i < num_loops t]
    and [0 <= j < num_loop_vertices t i]. *)
val loop_vertex : t -> int -> int -> S2_point.t

(** {1 Shape interface}

    These functions implement the {!S2_shape.t} interface, treating each loop as a
    separate chain. *)

(** [num_edges t] equals [num_vertices t]. Every vertex originates one outgoing edge. *)
val num_edges : t -> int

(** [edge t e] returns the [e]-th edge in concatenation order across all loops. Inside a
    loop the edges wrap around so that the last vertex is followed by the first. An
    internal cache accelerates sequential scans. *)
val edge : t -> int -> S2_shape.Edge.t

(** [dimension t] is [2]: a lax polygon bounds a two-dimensional region. *)
val dimension : t -> int

(** [num_chains t] equals [num_loops t]: each loop forms exactly one chain. *)
val num_chains : t -> int

(** [chain t i] returns the descriptor [{ start; length }] for loop [i]. Requires
    [0 <= i < num_loops t]. *)
val chain : t -> int -> S2_shape.Chain.t

(** [chain_edge t i j] is the [j]-th edge of loop [i], wrapping around at the loop
    boundary. Requires [0 <= i < num_loops t] and [0 <= j < num_loop_vertices t i]. *)
val chain_edge : t -> int -> int -> S2_shape.Edge.t

(** [chain_position t e] locates the global edge index [e] as a (chain_id, offset) pair.
    Shares the same internal cache as {!edge} and runs in amortised O(1) for sequential
    access. *)
val chain_position : t -> int -> S2_shape.Chain_position.t

(** [reference_point t] returns a point on the sphere together with a flag indicating
    whether that point lies inside the polygon. Cached on [t]. *)
val reference_point : t -> S2_shape.Reference_point.t

(** [type_tag] is the encoded-shape tag for lax polygons ([5]), matching the upstream
    [S2LaxPolygonShape::kTypeTag] so shape-index consumers can recognise the type. *)
val type_tag : S2_shape.Type_tag.t

(** [to_shape t] exposes [t] through the generic {!S2_shape.t} interface. *)
val to_shape : t -> S2_shape.t
[@@zero_alloc ignore]
