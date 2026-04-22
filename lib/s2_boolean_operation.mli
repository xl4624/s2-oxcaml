(** [S2_boolean_operation] implements polygon-polygon boolean predicates with C++-parity
    shared-boundary semantics.

    This port covers the {e predicate-only, polygon-only} slice of the reference library:
    - [is_empty], [intersects], [contains], [equals] restricted to inputs that are two
      polygon shapes (dimension 2).
    - Default [Polygon_model.Semi_open] semantics, matching the C++ default. Under
      [Semi_open], a directed edge belongs to the polygon that contains it in the same
      direction; the reversed edge belongs to the neighbouring polygon. This resolves the
      divergence from C++ on shared-boundary edges noted in the older boundary-parity
      implementation of [S2_polygon.contains].

    To avoid a dependency cycle on [S2_polygon], the API takes each polygon as a small
    [Polygon_input.t] record that bundles a shape index with its area and empty/full
    flags. [S2_polygon] builds this record for callers.

    {1 Deferred: full CrossingProcessor port}

    The C++ reference library implements the four predicates (and the set ops) through a
    general [S2BooleanOperation::Impl::CrossingProcessor] in
    [s2boolean_operation.cc: 1095-2009] - a ~1,500-line state machine that walks edges of
    an arbitrary {!S2_shape_index.t} (points, polylines, and polygons intermixed) and
    decides whether each edge belongs to the result under any combination of
    [PolygonModel] / [PolylineModel] / [Op_type]. This port replaces that machinery, for
    the specific polygon-polygon SEMI_OPEN predicate case, with a much simpler per-edge
    containment test ({!S2_boolean_operation.ml}'s [polygon_contains_directed_edge] and
    friends).

    To complete parity with C++ the full CrossingProcessor still needs to be ported.
    Features that require it, and the corresponding C++ entry points:

    - Set-operation output via an {!S2_builder.Layer.t}: [BuildOpType] (cc:2282),
      [AddBoundary] (cc:2028), [ProcessEdge] / [ProcessEdge0] / [ProcessEdge1] /
      [ProcessEdge2] dispatching on [a_dimension_] (cc:1416, 1449, 1546, 1678), the
      [EdgeClippingLayer] wrapper (cc:730), [AddEdge] / [AddPointEdge] emission (cc:
      1215-1264).
    - Polyline inputs: [ProcessEdge1] (cc:1546) plus [IsPolylineVertexInside] /
      [IsPolylineEdgeInside] (cc:1616, 1636).
    - Point inputs: [ProcessEdge0] (cc:1449) and [ProcessPointCrossings] (cc:1485).
    - [Open] / [Closed] polygon boundary models: the per-branch model checks in
      [ProcessEdge2] (cc:1771-1823) and the degenerate-point paths in [ProcessEdge0]
      (cc:1461).
    - [Open] / [SEMI_OPEN] / [Closed] polyline boundary models: [PolylineModel] plumbing
      in [StartBoundary] (cc:1383) and [ProcessEdge1].
    - Mixed-dimension operands (e.g. a polyline intersected with a polygon): the dimension
      dispatch in [ProcessEdge] plus [IsPolylineEdgeInside] for polyline- in-polygon
      tests.
    - Two-pass sibling-pair handling for shared-edge degeneracies: the [emit_shared] trick
      at cc:1712 and [is_degenerate_hole_] map (cc:1741).
    - Early-exit optimisation via [ProcessIncidentEdges] (cc:2143) when a chain's start is
      known-inside.
    - [AreRegionsIdentical] fast path (cc:2509) for DIFFERENCE and SYMMETRIC_DIFFERENCE
      when the inputs are byte-identical.

    Additionally, the degenerate-boundary disambiguation in [IsFullPolygonResult]
    (cc:2333-2505) uses a coarse face-mask + area heuristic here; the symmetric-
    difference branch's hemisphere tolerance (cc:2476-2505) is approximated
    conservatively. Porting that heuristic accurately also requires the snap-radius
    plumbing described in the [s2_builder] TODO.

    These raise (or silently disagree with C++ in the [Polygon_model.Open] / [Closed]
    case) if reached today. *)

open Core

(** Which boolean operation to evaluate. *)
module Op_type : sig
  type t =
    | Union
    | Intersection
    | Difference
    | Symmetric_difference
  [@@deriving sexp_of, equal]
end

(** Polygon boundary semantics. Only {!Semi_open} is implemented. *)
module Polygon_model : sig
  type t =
    | Open
    | Semi_open
    | Closed
  [@@deriving sexp_of, equal]

  val default : t
end

(** A polygon shape packaged with the auxiliary data the predicates need. *)
module Polygon_input : sig
  type t =
    { shape : S2_shape.t
    ; shape_index : S2_shape_index.t
    ; shape_id : int
    ; area : float#
    ; is_empty : bool
    ; is_full : bool
    }

  (** [create ~shape ~shape_index ~shape_id ~area ~is_empty ~is_full] bundles the inputs
      without additional validation. *)
  val create
    :  shape:S2_shape.t
    -> shape_index:S2_shape_index.t
    -> shape_id:int
    -> area:float#
    -> is_empty:bool
    -> is_full:bool
    -> t
end

(** {1 Predicates} *)

(** [is_empty op_type a b] returns [true] iff the boolean operation [op_type] applied to
    polygon inputs [a] and [b] has an empty result. *)
val is_empty
  :  ?polygon_model:Polygon_model.t
  -> Op_type.t
  -> Polygon_input.t
  -> Polygon_input.t
  -> bool

(** [intersects a b] is [not (is_empty Intersection a b)]. *)
val intersects
  :  ?polygon_model:Polygon_model.t
  -> Polygon_input.t
  -> Polygon_input.t
  -> bool

(** [contains a b] is [is_empty Difference b a]. *)
val contains
  :  ?polygon_model:Polygon_model.t
  -> Polygon_input.t
  -> Polygon_input.t
  -> bool

(** [equals a b] is [is_empty Symmetric_difference a b]. *)
val equals : ?polygon_model:Polygon_model.t -> Polygon_input.t -> Polygon_input.t -> bool
