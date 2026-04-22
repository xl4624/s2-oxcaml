(** [S2_loop] represents a simple spherical polygon. It consists of a single chain of
    vertices where the first vertex is implicitly connected to the last. All loops are
    defined to have a CCW orientation, i.e. the interior of the loop is on the left side
    of the edges. A clockwise loop enclosing a small area therefore represents the
    complementary region (a CCW loop enclosing a very large area).

    Loops are not allowed to have any duplicate vertices (whether adjacent or not).
    Non-adjacent edges are not allowed to intersect, and edges of length 180 degrees are
    not allowed (i.e., adjacent vertices cannot be antipodal). Loops must have at least
    three vertices, except for the special {!empty} and {!full} loops which have exactly
    one vertex each.

    Point containment is defined such that if the sphere is subdivided into faces, every
    point is contained by exactly one face. This implies that loops do not necessarily
    contain their vertices.

    This port currently exposes the index-free subset of the API: containment and
    intersection tests against cells use brute-force enumeration of edges and vertices
    rather than a precomputed shape index. *)

open Core

[@@@zero_alloc all]

type t [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [of_vertices vs] copies [vs] and returns the resulting loop. The vertices are expected
    to satisfy {!is_valid}; pass [validate:false] to skip the check. *)
val of_vertices : ?validate:bool -> S2_point.t array -> t
[@@zero_alloc ignore]

(** [empty ()] is the special single-vertex loop containing no points. *)
val empty : unit -> t
[@@zero_alloc ignore]

(** [full ()] is the special single-vertex loop containing all points on the sphere. *)
val full : unit -> t
[@@zero_alloc ignore]

(** [of_cell cell] constructs a loop corresponding to the four corners of [cell]. The
    resulting loop and the cell do {e not} contain exactly the same set of points: a cell
    vertex is contained by all four neighboring cells but only by one of the four
    corresponding loops. *)
val of_cell : S2_cell.t -> t
[@@zero_alloc ignore]

(** {1 Accessors} *)

(** [num_vertices t] is the number of stored vertices. *)
val num_vertices : t -> int

(** [vertex t i] is the [i]-th vertex; indices wrap automatically with period
    [num_vertices t], so [vertex t (num_vertices t)] equals [vertex t 0]. *)
val vertex : t -> int -> S2_point.t

(** [oriented_vertex t i] is like {!vertex} except that for a hole loop the iteration
    order is reversed, ensuring the interior is always on the left side of the edge chain. *)
val oriented_vertex : t -> int -> S2_point.t

(** [vertices t] returns a copy of the vertex array. *)
val vertices : t -> S2_point.t array
[@@zero_alloc ignore]

(** [contains_origin t] reports whether the loop contains {!S2_pointutil.origin}. *)
val contains_origin : t -> bool

(** [depth t] is the loop's nesting depth within its containing polygon. Outer shells have
    depth 0, holes within them depth 1, and so on. Defaults to [0]. *)
val depth : t -> int

(** [set_depth t d] updates the nesting depth in place. Used by polygon construction. *)
val set_depth : t -> int -> unit

(** {1 Predicates} *)

(** [is_empty t] is true when [t] is the special empty loop. *)
val is_empty : t -> bool

(** [is_full t] is true when [t] is the special full loop. *)
val is_full : t -> bool

(** [is_empty_or_full t] is true when [t] is either the empty or full loop. *)
val is_empty_or_full : t -> bool

(** [is_normalized t] is true when the loop area is at most [2 * pi]. Degenerate loops are
    handled consistently with {!S2_predicates.robust_sign}. *)
val is_normalized : t -> bool
[@@zero_alloc ignore]

(** [is_hole t] is true when the loop is a hole in its containing polygon, i.e. the
    nesting depth is odd. *)
val is_hole : t -> bool

(** [sign t] is [-1] if [t] is a hole and [+1] otherwise. *)
val sign : t -> int

(** {1 Validation} *)

(** [is_valid t] is true when the no-index validation checks pass: every vertex is unit
    length, the loop has at least three vertices (or is the empty/full loop), and no
    adjacent vertices are identical or antipodal. Self-intersection is {e not} checked
    until a shape index is available. *)
val is_valid : t -> bool
[@@zero_alloc ignore]

(** [find_validation_error t] returns [None] if {!is_valid}, otherwise a short
    human-readable description of the first error encountered. *)
val find_validation_error : t -> string option
[@@zero_alloc ignore]

(** {1 Operations} *)

(** [invert t] returns a loop whose vertex order is reversed, effectively complementing
    the region represented by [t]. The empty loop becomes the full loop and vice versa. *)
val invert : t -> t
[@@zero_alloc ignore]

(** [normalize t] returns [t] if {!is_normalized}, otherwise {!invert} of [t]. *)
val normalize : t -> t
[@@zero_alloc ignore]

(** {1 Geometry} *)

(** [area t] is the area of the loop interior in steradians, between [0] and [4 * pi]. *)
val area : t -> float#
[@@zero_alloc ignore]

(** [centroid t] is the true centroid of the loop multiplied by its area. The result is
    not unit length and may not be contained by the loop. *)
val centroid : t -> S2_point.t
[@@zero_alloc ignore]

(** [curvature t] is the geodesic curvature of the loop (the sum of the turn angles at
    each vertex). Equals [2 * pi - area t]. *)
val curvature : t -> float#
[@@zero_alloc ignore]

(** [curvature_max_error t] is an upper bound on the error of {!curvature}. *)
val curvature_max_error : t -> float#

(** {1 Region interface} *)

(** [cap_bound t] returns a bounding cap. *)
val cap_bound : t -> S2_cap.t

(** [rect_bound t] returns a bounding latitude-longitude rectangle. *)
val rect_bound : t -> S2_latlng_rect.t

(** [cell_union_bound t] returns a small set of cell ids whose union covers [t]. *)
val cell_union_bound : t -> S2_cell_id.t array
[@@zero_alloc ignore]

(** [contains_point t p] reports whether the loop contains [p]. After the
    bounding-rectangle quick reject, this consults a lazily-built {!S2_shape_index.t} and
    {!S2_contains_point_query.t} (semi-open vertex model) so repeated tests reuse the same
    index. *)
val contains_point : t -> S2_point.t -> bool
[@@zero_alloc ignore]

(** [contains_cell t cell] reports whether the loop entirely contains [cell]. Uses the
    loop's lazily-built {!S2_shape_index.t} for both the per-cell-vertex point test and
    the cell-edge / loop-edge crossing test. *)
val contains_cell : t -> S2_cell.t -> bool
[@@zero_alloc ignore]

(** [may_intersect_cell t cell] reports whether the loop may intersect [cell]. Uses the
    loop's lazily-built {!S2_shape_index.t} for the point-in-loop test and the edge
    crossing test. *)
val may_intersect_cell : t -> S2_cell.t -> bool
[@@zero_alloc ignore]

(** [to_region t] exposes [t] through the generic region interface. *)
val to_region : t -> S2_region.t
[@@zero_alloc ignore]

(** {1 Equality} *)

(** [equal a b] is true when both loops have the same vertex sequence in the same linear
    order (no cyclic rotation allowed). *)
val equal : t -> t -> bool

(** [boundary_equals a b] is true when both loops have the same vertices in the same
    cyclic order. The empty and full loops are considered to have different boundaries. *)
val boundary_equals : t -> t -> bool

(** [boundary_approx_equals ~max_error a b] is true when both loops have the same vertices
    in the same cyclic order, up to vertex perturbations within [max_error] radians of
    each other. The default tolerance (when [max_error] is [none]) is [1e-15] radians. *)
val boundary_approx_equals : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool
[@@zero_alloc]

(** {1 Loop relations} *)

(** Canonical starting index and direction for the iteration produced by
    {!canonical_first_vertex}. *)
type canonical_first =
  #{ first : int
   ; dir : int
   }

(** [canonical_first_vertex t] returns a pair [(first, dir)] with [dir] in [[-1; 1]] such
    that iterating [vertex t (first + k * dir)] for [k = 0 .. n-1] produces a sequence
    that is invariant under cyclic rotation of the vertices. The returned [first] is
    chosen so that [first + k * dir] stays in [[0, 2*n-1]] for any [k] in [[0, n]]. Used
    by nesting-related tie breakers. *)
val canonical_first_vertex : t -> canonical_first
[@@zero_alloc]

(** [contains_nested a b] reports whether [b] is nested within [a] assuming both loops
    satisfy the polygon contract (no shared edges and either one contains the other or
    they are disjoint). Does not test for edge intersections. *)
val contains_nested : t -> t -> bool
[@@zero_alloc ignore]

(** [contains a b] reports whether the region enclosed by [a] is a superset of the region
    enclosed by [b]. Uses brute-force O(|a| * |b|) edge-pair crossing checks. *)
val contains : t -> t -> bool
[@@zero_alloc ignore]

(** [intersects a b] reports whether the regions enclosed by [a] and [b] overlap. Uses
    brute-force O(|a| * |b|) edge-pair crossing checks. *)
val intersects : t -> t -> bool
[@@zero_alloc ignore]

(** [compare_boundary a b] returns [+1] if [a] contains the boundary of [b], [-1] if [a]
    excludes the boundary of [b], and [0] if the boundaries of [a] and [b] cross. Shared
    edges are handled as follows: if [xy] is a shared edge and
    [reversed_in_b = xy appears reversed in b], then [a] contains [xy] iff
    [reversed_in_b = is_hole b]. Requires that neither loop is empty, and if [b] is full
    it must not be a hole. *)
val compare_boundary : t -> t -> int
[@@zero_alloc ignore]

(** [contains_non_crossing_boundary a b ~reverse] reports whether [a] contains the
    boundary of [b], assuming the boundaries do not cross (see {!compare_boundary}). When
    [reverse] is [true], the boundary of [b] is interpreted as reversed (which only
    affects the result at shared edges). Requires that neither loop is empty, and if [b]
    is full [reverse] must be [false]. *)
val contains_non_crossing_boundary : t -> t -> reverse:bool -> bool
[@@zero_alloc ignore]

(** {1 Shape interface} *)

(** [num_edges t] is [0] for the empty/full loop, and [num_vertices t] otherwise. *)
val num_edges : t -> int

(** [edge t e] is the [e]-th edge. Requires [0 <= e < num_edges t]. *)
val edge : t -> int -> S2_shape.Edge.t

(** [dimension t] is [2]. *)
val dimension : t -> int

(** [num_chains t] is [0] for the empty loop and [1] otherwise (the full loop has a single
    chain of length zero). *)
val num_chains : t -> int

(** [chain t i] returns the single chain spanning every edge. Requires [i = 0]. *)
val chain : t -> int -> S2_shape.Chain.t

(** [chain_edge t i j] is [edge t j]. Requires [i = 0]. *)
val chain_edge : t -> int -> int -> S2_shape.Edge.t

(** [chain_position t e] is [{ chain_id = 0; offset = e }]. *)
val chain_position : t -> int -> S2_shape.Chain_position.t

(** [reference_point t] returns {!S2_pointutil.origin} together with whether the loop
    contains it. *)
val reference_point : t -> S2_shape.Reference_point.t

(** [type_tag] is {!S2_shape.Type_tag.none}. Loops are not directly encodable; use
    [S2_polygon] for that. *)
val type_tag : S2_shape.Type_tag.t

(** [to_shape t] exposes [t] through the generic shape interface. *)
val to_shape : t -> S2_shape.t
[@@zero_alloc ignore]
