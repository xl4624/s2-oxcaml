(** A polygonal region on the unit sphere, defined by zero or more {!S2_loop} boundaries.

    The polygon's interior is the set of points contained by an odd number of its loops.
    During construction the input loops are reordered into a canonical "shells and holes"
    form: outer shells have even nesting depth (0, 2, ...) and holes have odd depth (1, 3,
    ...), and a loop's descendants in the nesting hierarchy occupy the array slots
    immediately after it.

    A polygon may represent any region with a polygonal boundary, including:
    - the {!empty} polygon (no loops)
    - the {!full} polygon (a single full loop covering the entire sphere)

    Polygon invariants:
    - Loops may not cross: one loop's boundary may not intersect both the interior and
      exterior of another loop.
    - Loops may not share edges.
    - Loops may share isolated vertices, but no vertex may appear twice in a single loop.
    - No loop may be empty. The full loop may appear only in the {!full} polygon.

    Boolean predicates ({!contains}, {!intersects}) match the reference library's
    shared-boundary semantics by routing through {!S2_boolean_operation} for non-trivial
    cases.

    {1 Limitations}

    The following operations are not yet ported:
    - Set operations with {!t} output: [InitToUnion], [InitToIntersection],
      [InitToDifference], [InitToSymmetricDifference], [InitToSnapped],
      [InitToSimplified].
    - [IntersectWithPolyline] / [SubtractFromPolyline] and related polyline operations.
    - [ApproxContains] / [ApproxDisjoint] and other distance-tolerant predicates.
    - [GetDistance] / [Project] and nearest-point queries.
    - [Encode] / [Decode] binary serialisation.
    - [BoundaryNear] and [Clone].
    - Index-driven validation (the index-free subset is implemented). *)

open Core

type t [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [of_loops loops] takes ownership of [loops], determines the nesting hierarchy, and
    assigns every loop a depth. [loops] are reordered so that every loop is immediately
    followed by its descendants. A single-element array containing the empty loop is
    interpreted as the empty polygon. *)
val of_loops : S2_loop.t array -> t

(** [of_oriented_loops loops] is like {!of_loops} but assumes the caller oriented the
    loops so that the polygon interior is on the left of all edges. Loops representing
    holes are automatically inverted during initialisation. *)
val of_oriented_loops : S2_loop.t array -> t

(** [of_cell cell] is the single-shell polygon whose boundary matches [cell]. *)
val of_cell : S2_cell.t -> t

(** [full ()] is the "full" polygon, which covers the entire sphere. *)
val full : unit -> t

(** [empty ()] is the empty polygon. *)
val empty : unit -> t

(** {1 Accessors} *)

(** [num_loops t] returns the number of loops. *)
val num_loops : t -> int

(** [loops t] returns a fresh array of the loops (shares the underlying loop values). *)
val loops : t -> S2_loop.t array

(** [loop t k] returns the loop at index [k]. Loops are indexed in pre-order traversal of
    the nesting hierarchy, so a loop's descendants occupy indices
    [k + 1 .. last_descendant t k]. *)
val loop : t -> int -> S2_loop.t

(** [parent t k] returns the index of the enclosing loop, or [-1] if [loop t k] has depth
    [0]. The [-1] sentinel matches the convention used by [last_descendant]. *)
val parent : t -> int -> int

(** [last_descendant t k] returns the index of the last loop contained (transitively) in
    [loop t k]. For [k < 0] returns [num_loops t - 1]. *)
val last_descendant : t -> int -> int

(** [num_vertices t] is the sum of [S2_loop.num_vertices] over all loops. *)
val num_vertices : t -> int

(** {1 Predicates} *)

(** [is_empty t] is true iff [t] contains no loops. *)
val is_empty : t -> bool

(** [is_full t] is true iff [t] is the full polygon (a single full loop). *)
val is_full : t -> bool

(** [has_holes t] is true iff any loop has odd depth. *)
val has_holes : t -> bool

(** {1 Geometry} *)

(** [area t] is the total area of the polygon interior in steradians, in [[0, 4 * pi]]. *)
val area : t -> float#

(** [centroid t] is the true centroid of the polygon multiplied by its area. The result is
    not unit length and may not be contained by the polygon. *)
val centroid : t -> S2_point.t

(** [invert t] returns the complement of [t]. *)
val invert : t -> t

(** {1 Validation} *)

(** [find_validation_error t] returns [None] if [t] is a well-formed polygon, otherwise
    the first problem found. Only performs checks that do not require building an index. *)
val find_validation_error : t -> string option

(** [is_valid t] is [Option.is_none (find_validation_error t)]. *)
val is_valid : t -> bool

(** {1 Region interface} *)

(** [cap_bound t] returns a bounding cap derived from {!rect_bound}. *)
val cap_bound : t -> S2_cap.t

(** [rect_bound t] returns a bounding latitude-longitude rectangle. The bound is the union
    of the shell loops' bounds; holes do not contribute. *)
val rect_bound : t -> S2_latlng_rect.t

(** [cell_union_bound t] returns a small set of cell ids whose union covers [t]. *)
val cell_union_bound : t -> S2_cell_id.t array

(** [contains_point t p] reports whether [p] lies in the polygon's interior under the
    semi-open vertex model. For small polygons (fewer than 32 vertices) this is a
    brute-force scan over loops; larger polygons use the internal shape index. *)
val contains_point : t -> S2_point.t -> bool

(** [contains_cell t cell] reports whether the polygon entirely contains [cell]. *)
val contains_cell : t -> S2_cell.t -> bool

(** [may_intersect_cell t cell] is a conservative test: returns [true] when [cell] may
    intersect the polygon, [false] only when they are guaranteed disjoint. *)
val may_intersect_cell : t -> S2_cell.t -> bool

(** [to_region t] exposes [t] through the generic region interface. *)
val to_region : t -> S2_region.t

(** {1 Polygon relations} *)

(** [contains a b] reports whether every point in [b] is also in [a]. *)
val contains : t -> t -> bool

(** [intersects a b] reports whether [a] and [b] share at least one point. *)
val intersects : t -> t -> bool

(** {1 Equality} *)

(** [equal a b] reports whether [a] and [b] have the same loops in the same order (no
    cyclic rotation allowed). *)
val equal : t -> t -> bool

(** {1 Shape interface} *)

(** [num_edges t] is the total number of boundary edges (sum over all loops). The full
    polygon has zero edges. *)
val num_edges : t -> int

(** [edge t e] returns the [e]-th edge in flattened order, oriented so that the polygon
    interior is on the left. Requires [0 <= e < num_edges t]. *)
val edge : t -> int -> S2_shape.Edge.t

(** [dimension t] is [2]. *)
val dimension : t -> int

(** [num_chains t] equals [num_loops t]. *)
val num_chains : t -> int

(** [chain t i] returns the chain corresponding to loop [i]. The full loop contributes a
    chain of length zero. *)
val chain : t -> int -> S2_shape.Chain.t

(** [chain_edge t i j] returns the [j]-th edge of loop [i], oriented so the interior is on
    the left. *)
val chain_edge : t -> int -> int -> S2_shape.Edge.t

(** [chain_position t e] decomposes a flat edge index [e] into [{ chain_id; offset }]. *)
val chain_position : t -> int -> S2_shape.Chain_position.t

(** [reference_point t] returns {!S2_pointutil.origin} paired with whether the polygon
    contains it (computed by XOR-ing the per-loop [contains_origin] bits). *)
val reference_point : t -> S2_shape.Reference_point.t

(** [type_tag] is the encoded shape tag for polygons ([1]). *)
val type_tag : S2_shape.Type_tag.t

(** [to_shape t] exposes [t] through the generic shape interface. *)
val to_shape : t -> S2_shape.t
