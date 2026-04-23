(** Boolean predicates on pairs of polygons on the sphere: {!is_empty}, {!intersects},
    {!contains}, and {!equals}.

    Each operand is a {!Polygon_input.t} - a bundle of a shape index and pre-computed area
    / empty / full flags. In typical use, construct one with {!S2_polygon.boolean_input}
    and pass it to the predicate.

    {2 Boundary model}

    Polygon boundary handling is controlled by {!Polygon_model.t}. Only
    {!Polygon_model.Semi_open} is implemented: around every shared vertex or edge, exactly
    one of the touching polygons contains it, consistent with the reference library. In
    particular, a polygon contains its own directed edges but not their reverses, which
    gives well-defined answers when two polygons share a boundary.

    {2 Relationship to {!S2_polygon}}

    {!S2_polygon.contains} and {!S2_polygon.intersects} for multi-loop inputs delegate to
    {!contains} and {!intersects} here, respectively.

    {1 Limitations}

    The following paths are not implemented and raise when reached:
    - Set-operation output (materialising the actual union, intersection, difference, or
      symmetric-difference geometry). Only the four boolean predicates work.
    - Polyline or point inputs, and mixed-dimension operands (e.g. clipping a polyline by
      a polygon).
    - {!Polygon_model.Open} and {!Polygon_model.Closed} boundary models.

    Inputs with invalid nesting - for example a "shell" whose winding order actually
    describes its complement - can yield results that differ silently from the reference
    library. Build polygons via {!S2_polygon.of_loops} or {!S2_polygon.of_oriented_loops}
    (which normalise the winding order) so they are well-formed before calling. *)

open Core

(** Which boolean operation to evaluate. *)
module Op_type : sig
  type t =
    | Union (** Points contained by either input. *)
    | Intersection (** Points contained by both inputs. *)
    | Difference (** Points contained by the first input but not the second. *)
    | Symmetric_difference (** Points contained by exactly one input. *)
  [@@deriving sexp_of, equal]
end

(** Polygon boundary semantics. Only {!Semi_open} is implemented; the others raise. *)
module Polygon_model : sig
  type t =
    | Open
    (** Polygons contain neither their vertices nor their edges. Not implemented. *)
    | Semi_open
    (** Around every shared vertex or edge, exactly one adjacent polygon contains it.
        Equivalently, polygons contain their directed edges but not the reversed edges.
        This is the default. *)
    | Closed
    (** Polygons contain their vertices, edges, and reversed edges. Not implemented. *)
  [@@deriving sexp_of, equal]

  (** [default] is {!Semi_open}. *)
  val default : t
end

(** A polygon shape packaged with the auxiliary data the predicates need. The area and
    empty/full flags are not validated; callers are expected to pass values consistent
    with [shape]. *)
module Polygon_input : sig
  type t =
    { shape : S2_shape.t
    ; shape_index : S2_shape_index.t (** Indexed form of [shape] for fast queries. *)
    ; shape_id : int
    (** Id of [shape] inside [shape_index], used to identify which shape point-in- polygon
        queries should consider. *)
    ; area : float#
    (** Signed area of [shape] in steradians. Used by the full-polygon heuristic. *)
    ; is_empty : bool (** [true] iff [shape] represents the empty polygon. *)
    ; is_full : bool (** [true] iff [shape] represents the full polygon. *)
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

(** [is_empty ?polygon_model op_type a b] returns [true] iff the geometry produced by
    applying [op_type] to [a] and [b] contains no points. [polygon_model] defaults to
    {!Polygon_model.Semi_open}; any other value raises. *)
val is_empty
  :  ?polygon_model:Polygon_model.t
  -> Op_type.t
  -> Polygon_input.t
  -> Polygon_input.t
  -> bool

(** [intersects ?polygon_model a b] is [true] iff [a] and [b] share at least one point.
    Defined as [not (is_empty Intersection a b)]. *)
val intersects
  :  ?polygon_model:Polygon_model.t
  -> Polygon_input.t
  -> Polygon_input.t
  -> bool

(** [contains ?polygon_model a b] is [true] iff every point of [b] lies in [a]. Equivalent
    to [is_empty Difference b a]: "b - a" is empty exactly when [a] contains [b]. *)
val contains
  :  ?polygon_model:Polygon_model.t
  -> Polygon_input.t
  -> Polygon_input.t
  -> bool

(** [equals ?polygon_model a b] is [true] iff [a] and [b] contain the same set of points.
    Equivalent to [is_empty Symmetric_difference a b]. *)
val equals : ?polygon_model:Polygon_model.t -> Polygon_input.t -> Polygon_input.t -> bool
