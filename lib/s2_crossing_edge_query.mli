(** Find edges of indexed geometry that are crossed by a given query edge.

    Given an {!S2_shape_index.t} and a query edge [a]->[b], the query returns the set of
    (shape_id, edge_id) pairs whose edges intersect [a]->[b]. A lower-level "candidates"
    API returns a superset of the actual crossings; it is useful when the caller wants to
    apply a custom crossing test (for example {!S2_edge_crossings.edge_or_vertex_crossing}
    instead of the strict edge crosser).

    Typical usage is to build one query object per index and reuse it across many query
    edges, which avoids re-allocating the temporary storage and iterator on each call.

    This type is not thread-safe. Each thread should construct its own instance.

    Limitations:

    - The general cell-visitor API from the C++ library ([VisitCells],
      [VisitRawCandidates], [GetCells]) is not exposed; only the high-level crossing and
      candidate lists are available.
    - There is no in-place variant that writes into a caller-provided buffer; every query
      allocates a fresh result list. *)

open Core

(** A (shape_id, edge_id) pair identifying one edge of an indexed shape. *)
module Shape_edge_id = S2_shapeutil_shape_edge_id

(** Controls whether crossings that only share a vertex are reported. *)
module Crossing_type : sig
  type t =
    | Interior (** Report only crossings that are interior to both edges. *)
    | All (** Report all crossings, including those that share a vertex. *)
end

type t

(** [create index] ensures [index] is built and returns a reusable query object. The index
    must not be modified for the lifetime of the returned value. Reuse the same query
    across many calls to amortize internal storage. *)
val create : S2_shape_index.t -> t

(** [index t] returns the index this query was built against. *)
val index : t -> S2_shape_index.t

(** {1 High-level crossing queries} *)

(** [get_crossing_edges t ~a ~b ~crossing_type] returns the edges in the index that
    intersect [a]->[b]. With [crossing_type = Interior] only crossings that lie strictly
    interior to both edges are reported; with [crossing_type = All] edges that share a
    vertex with [a]->[b] are included as well. Results are sorted by [(shape_id, edge_id)]
    and deduplicated. *)
val get_crossing_edges
  :  t
  -> a:S2_point.t
  -> b:S2_point.t
  -> crossing_type:Crossing_type.t
  -> Shape_edge_id.t list

(** [get_crossing_edges_for_shape t ~a ~b ~shape_id ~shape ~crossing_type] is like
    {!get_crossing_edges} but restricted to edges of the given [shape]. The [shape] must
    be the shape at index [shape_id] in the underlying index; [shape_id] is used to look
    up the clipped shape in each visited index cell. *)
val get_crossing_edges_for_shape
  :  t
  -> a:S2_point.t
  -> b:S2_point.t
  -> shape_id:int
  -> shape:S2_shape.t
  -> crossing_type:Crossing_type.t
  -> Shape_edge_id.t list

(** {1 Low-level candidate queries}

    These return a superset of the edges that may intersect the query edge - enough to
    guarantee no crossing is missed, but without running the final edge-crossing test.
    They are intended for callers that want to apply their own crossing predicate. Results
    are sorted by [(shape_id, edge_id)] and deduplicated. *)

(** [get_candidates t ~a ~b] returns every indexed edge that {i may} cross [a]->[b]. The
    result is a superset of the true crossings. *)
val get_candidates : t -> a:S2_point.t -> b:S2_point.t -> Shape_edge_id.t list

(** [get_candidates_for_shape t ~a ~b ~shape_id ~shape] is like {!get_candidates} but
    restricted to one shape, as in {!get_crossing_edges_for_shape}. *)
val get_candidates_for_shape
  :  t
  -> a:S2_point.t
  -> b:S2_point.t
  -> shape_id:int
  -> shape:S2_shape.t
  -> Shape_edge_id.t list
