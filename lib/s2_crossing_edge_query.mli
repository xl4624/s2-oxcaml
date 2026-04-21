(** [S2_crossing_edge_query] finds edges or shapes that are crossed by a query edge.

    Given a query edge [a]->[b] and an {!S2_shape_index.t}, the query returns the set of
    (shape_id, edge_id) pairs whose edges intersect [a]->[b]. A faster "candidates" API
    returns a superset of the actual crossings, which is useful when the caller wants to
    apply a different crossing test.

    For indices with few edges (27 or fewer) the query falls back to a brute-force scan;
    otherwise it walks the minimal subset of index cells that intersect the query edge. *)

open Core

(** A (shape_id, edge_id) pair identifying one edge of an indexed shape. *)
module Shape_edge_id : sig
  type t =
    { shape_id : int
    ; edge_id : int
    }
  [@@deriving compare, equal, sexp_of]
end

(** Controls whether crossings that only share a vertex are reported. *)
module Crossing_type : sig
  type t =
    | Interior (** Report only crossings that are interior to both edges. *)
    | All (** Report all crossings, including those that share a vertex. *)
end

type t

(** [create index] builds (or re-builds) the underlying index and returns a reusable query
    object. Repeated queries against the same index should reuse this value to avoid
    reallocating temporary storage. *)
val create : S2_shape_index.t -> t

(** [index t] returns the index this query was built against. *)
val index : t -> S2_shape_index.t

(** {1 High-level crossing queries} *)

(** [get_crossing_edges t ~a ~b ~crossing_type] returns the edges in the index that
    intersect [a]->[b]. The result is sorted and deduplicated. *)
val get_crossing_edges
  :  t
  -> a:S2_point.t
  -> b:S2_point.t
  -> crossing_type:Crossing_type.t
  -> Shape_edge_id.t list

(** [get_crossing_edges_for_shape t ~a ~b ~shape_id ~shape ~crossing_type] is like
    {!get_crossing_edges} but only considers edges from the given shape. *)
val get_crossing_edges_for_shape
  :  t
  -> a:S2_point.t
  -> b:S2_point.t
  -> shape_id:int
  -> shape:S2_shape.t
  -> crossing_type:Crossing_type.t
  -> Shape_edge_id.t list

(** {1 Low-level candidate queries}

    These return a superset of the edges that may intersect the query edge. They are
    useful when the caller wants to apply their own crossing test. Results are sorted and
    deduplicated. *)

val get_candidates : t -> a:S2_point.t -> b:S2_point.t -> Shape_edge_id.t list

val get_candidates_for_shape
  :  t
  -> a:S2_point.t
  -> b:S2_point.t
  -> shape_id:int
  -> shape:S2_shape.t
  -> Shape_edge_id.t list
