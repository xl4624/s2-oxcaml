(** [S2_lax_polygon] represents a region defined by a collection of zero or more closed
    loops. The interior is the region to the left of all loops.

    Unlike {!S2_polygon} (not yet ported), this representation supports degeneracies:
    degenerate edges (from a vertex to itself) and sibling edge pairs. Loops with fewer
    than three vertices are interpreted as follows:
    - A loop with two vertices defines two edges (in opposite directions).
    - A loop with one vertex defines a single degenerate edge.
    - A loop with no vertices is interpreted as the "full loop" containing all points on
      the sphere. *)

open Core

type t [@@deriving sexp_of]

(** {1 Constructors} *)

(** [of_loops loops] constructs a lax polygon from the given vertex loops. The outer array
    indexes loops; each inner array is the vertex sequence of that loop. Both arrays are
    copied. *)
val of_loops : S2_point.t array array -> t

(** {1 Accessors} *)

(** [num_loops t] is the number of loops. *)
val num_loops : t -> int

(** [num_vertices t] is the total number of vertices across all loops. *)
val num_vertices : t -> int

(** [num_loop_vertices t i] is the number of vertices in loop [i]. Requires
    [0 <= i < num_loops t]. *)
val num_loop_vertices : t -> int -> int

(** [loop_vertex t i j] is the [j]-th vertex of loop [i]. Requires [0 <= i < num_loops t]
    and [0 <= j < num_loop_vertices t i]. *)
val loop_vertex : t -> int -> int -> S2_point.t

(** {1 Shape interface} *)

(** [num_edges t] equals [num_vertices t]. *)
val num_edges : t -> int

(** [edge t e] is the [e]-th edge across all loops in concatenation order. *)
val edge : t -> int -> S2_shape.Edge.t

(** [dimension t] is [2]. *)
val dimension : t -> int

(** [num_chains t] equals [num_loops t]: each loop is one chain. *)
val num_chains : t -> int

(** [chain t i] returns the chain for loop [i]. Requires [0 <= i < num_loops t]. *)
val chain : t -> int -> S2_shape.Chain.t

(** [chain_edge t i j] is the [j]-th edge of loop [i] (wrapping at the end). *)
val chain_edge : t -> int -> int -> S2_shape.Edge.t

(** [chain_position t e] locates the global edge [e] within its loop. *)
val chain_position : t -> int -> S2_shape.Chain_position.t

(** [reference_point t] returns a point and whether it lies in the polygon's interior.
    Computed once at construction time. *)
val reference_point : t -> S2_shape.Reference_point.t

(** [type_tag] is the encoded shape tag for lax polygons ([5]). *)
val type_tag : S2_shape.Type_tag.t

(** [to_shape t] exposes [t] through the generic shape interface. *)
val to_shape : t -> S2_shape.t
