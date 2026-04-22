(** [S2_polygon] represents a region defined by a collection of zero or more {!S2_loop}s.
    When the polygon is initialized, the given loops are reordered so that they correspond
    to a pre-order traversal of the nesting hierarchy, and their depths are updated in
    place. Shells have even depth, and holes have odd depth.

    Polygons may represent any region of the sphere with a polygonal boundary, including
    the entire sphere (the "full" polygon, consisting of a single full loop). The zero
    value (no loops) is treated as the empty polygon.

    Polygons have the following restrictions:
    - Loops may not cross, i.e. the boundary of a loop may not intersect both the interior
      and exterior of any other loop.
    - Loops may not share edges.
    - Loops may share vertices, but no vertex may appear twice in a single loop.
    - No loop may be empty. The full loop may appear only in the full polygon.

    This port covers containment and intersection tests, area/centroid, invert, and the
    shape/region interfaces. Encode/decode, boolean set operations, snap/simplify, and
    polyline intersection are not ported. *)

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

(** [parent t k] returns [Some p] where [p] is the index of the enclosing loop, or [None]
    if [loop t k] has depth [0]. *)
val parent : t -> int -> int option

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

val cap_bound : t -> S2_cap.t
val rect_bound : t -> S2_latlng_rect.t
val cell_union_bound : t -> S2_cell_id.t array
val contains_point : t -> S2_point.t -> bool
val contains_cell : t -> S2_cell.t -> bool
val may_intersect_cell : t -> S2_cell.t -> bool
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

val num_edges : t -> int
val edge : t -> int -> S2_shape.Edge.t
val dimension : t -> int
val num_chains : t -> int
val chain : t -> int -> S2_shape.Chain.t
val chain_edge : t -> int -> int -> S2_shape.Edge.t
val chain_position : t -> int -> S2_shape.Chain_position.t
val reference_point : t -> S2_shape.Reference_point.t
val type_tag : S2_shape.Type_tag.t
val to_shape : t -> S2_shape.t
