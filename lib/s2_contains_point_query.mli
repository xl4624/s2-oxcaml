(** Point-in-shape queries against geometry stored in an {!S2_shape_index.t}.

    The index may contain any mix of points, polylines, and polygons, and those shapes may
    overlap. The boundary of each shape is handled according to the configured
    {!Vertex_model.t}:

    - [Open]: no shape contains its vertices (not even point shapes). For a polygon,
      [shape_contains] is [true] only when the query point is strictly in the interior.
    - [Semi_open]: if several polygons tile the region around a vertex, exactly one of
      them contains that vertex. Points and polylines still do not contain any vertices.
    - [Closed]: every shape (including points and polylines) contains its vertices. A
      polygon contains its boundary as well as its interior.

    Points that are not vertices of a polyline are never contained by that polyline
    regardless of the vertex model. Use {!S2_closest_edge_query.is_distance_less} with a
    small threshold to test proximity to a polyline instead.

    This type is not thread-safe; each thread should construct its own instance. Reusing a
    query object across many point tests against the same index is faster than building a
    fresh one each time. *)

open Core

module Vertex_model : sig
  type t =
    | Open
    | Semi_open
    | Closed
end

type t

(** [create index ?vertex_model ()] builds a query bound to [index]. The default vertex
    model is [Semi_open]. The index is built eagerly if it has not been built already. *)
val create : S2_shape_index.t -> ?vertex_model:Vertex_model.t -> unit -> t

(** [index t] returns the index [t] was built against. *)
val index : t -> S2_shape_index.t

(** [shape_contains t ~shape_id p] reports whether the shape with the given id contains
    [p] under the configured vertex model. If [shape_id] is not present in the index the
    result is [false]. *)
val shape_contains : t -> shape_id:int -> S2_point.t -> bool

(** [visit_containing_shapes t p ~f] calls [f shape_id] once for each indexed shape that
    contains [p] under the configured vertex model. Each shape is visited at most once.
    Iteration stops early if [f] returns [false]; the return value is [false] iff [f]
    requested early termination, otherwise [true]. *)
val visit_containing_shapes : t -> S2_point.t -> f:(int -> bool) -> bool
