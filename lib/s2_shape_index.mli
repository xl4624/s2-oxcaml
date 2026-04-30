(** Spatial index over {!S2_shape.t} values, used to accelerate point containment,
    closest-edge, edge-crossing, and boolean-operation queries.

    The index is conceptually a map from {!S2_cell_id.t} to the set of shape edges (and
    whole shapes, when a cell lies fully inside a polygon) that may intersect that cell.
    Cells are adaptively subdivided so that no cell holds more than a bounded number of
    "short" edges (edges whose length is comparable to the cell's size). The result is
    linear in the total number of input edges, both in space and construction time.

    A shape is added with {!add}, which assigns the next dense integer id and queues it
    for indexing. Ids are never reused. The index is lazy: {!add} only queues the shape,
    and the actual subdivision runs on the first call to {!build} or {!iterator}.

    Usage pattern:
    {[
      let idx = create () in
      let _id = add idx shape in
      let it = iterator idx in
      if Iterator.locate_point it query then
        let cell = Iterator.index_cell it in
        ...
    ]}

    Thread-safety matches the C++ library: read-only operations may run concurrently, but
    adding a shape or triggering a build requires exclusive access.

    {1 Limitations}

    - Only addition is supported; [Remove] / [RemoveAll] are not implemented.
    - Encoding / decoding is not implemented.
    - [Minimize], [SpaceUsed], and the memory-tracker integration are not implemented.
    - Only the in-memory flavor (equivalent to C++ [MutableS2ShapeIndex]) is provided;
      there is no [EncodedS2ShapeIndex] analogue yet. *)

(** Relationship between a query [S2_cell_id.t] and the index:
    - [Indexed]: the query cell (or an ancestor) is itself an index cell.
    - [Subdivided]: the query cell is split across multiple index cells that are its
      strict descendants.
    - [Disjoint]: no index cell overlaps the query cell. *)
module Cell_relation : sig
  type t =
    | Indexed
    | Subdivided
    | Disjoint
end

(** The portion of one shape that intersects a particular index cell: the shape's id, a
    flag indicating whether the cell's center lies inside the shape, and the ids of edges
    that actually intersect the cell. Edges are sorted by ascending edge id. *)
module Clipped_shape : sig
  type t : value & value & value [@@deriving unboxed_option { none = #{ shape_id = -1 } }]

  (** [shape_id t] returns the shape id this clipped view refers to. *)
  val shape_id : t -> int

  (** [contains_center t] is true when the center of the enclosing index cell lies inside
      the shape. Always false for shapes without an interior (dimension < 2). *)
  val contains_center : t -> bool

  (** [num_edges t] returns the number of edges of this shape intersecting the cell. *)
  val num_edges : t -> int

  (** [edge t i] returns the global edge id of the [i]-th intersecting edge
      ([0 <= i < num_edges t]). Edges are in ascending order. *)
  val edge : t -> int -> int
end

(** The payload stored at a single {!S2_cell_id.t} key: a collection of {!Clipped_shape.t}
    values, sorted by ascending [shape_id]. The empty cell is reserved as the [none]
    sentinel for {!Index_cell.Option}; real cells always have at least one clipped shape. *)
module Index_cell : sig
  type t : value [@@deriving unboxed_option { none = #{ shapes = [||] } }]

  (** [num_clipped t] is the number of shapes that intersect this cell. *)
  val num_clipped : t -> int

  (** [clipped t i] returns the [i]-th clipped shape for [0 <= i < num_clipped t]. Raises
      if [i] is out of range. *)
  val clipped : t -> int -> Clipped_shape.t

  (** [find_clipped t ~shape_id] returns the clipped view for [shape_id], or
      [Clipped_shape.Option.none] if the shape does not intersect this cell. Branch via
      [match%optional_u.Clipped_shape.Option] to avoid allocation. *)
  val find_clipped : t -> shape_id:int -> Clipped_shape.Option.t
end

(** A random-access iterator over index cells in ascending {!S2_cell_id.t} order. *)
module Iterator : sig
  type t

  (** [begin_at it] positions [it] at the first index cell. *)
  val begin_at : t -> unit

  (** [at_end it] positions [it] one past the last index cell. *)
  val at_end : t -> unit

  (** [seek it target] positions [it] at the first index cell whose id is >= [target], or
      past the end if there is none. *)
  val seek : t -> S2_cell_id.t -> unit

  (** [next it] advances [it] by one cell. *)
  val next : t -> unit

  (** [prev it] moves [it] back by one cell. Returns [false] if the iterator was already
      at the first cell (in which case the position is unchanged). *)
  val prev : t -> bool

  (** [is_done it] is true when [it] is at the end. *)
  val is_done : t -> bool

  (** [cell_id it] returns the current cell id. Undefined when [is_done it]. *)
  val cell_id : t -> S2_cell_id.t

  (** [index_cell it] returns the payload at the current cell. Raises when [is_done it]. *)
  val index_cell : t -> Index_cell.t

  (** [locate_point it p] positions [it] at the index cell containing [p] and returns
      [true] if such a cell exists. On [false], the iterator position is unspecified. *)
  val locate_point : t -> S2_point.t -> bool

  (** [locate_cell_id it target] positions [it] according to the relation between [target]
      and the index cells. For [Indexed] or [Subdivided] the iterator points at a cell
      that witnesses the relation; for [Disjoint] the iterator position is unspecified. *)
  val locate_cell_id : t -> S2_cell_id.t -> Cell_relation.t
end

type t

(** [create ?max_edges_per_cell ()] allocates a fresh empty index. [max_edges_per_cell]
    bounds the number of "short" edges in any single index cell (default 10). Larger
    values reduce memory and construction time; smaller values speed up queries. *)
val create : ?max_edges_per_cell:int -> unit -> t

(** [add t shape] queues [shape] for indexing and returns its freshly assigned shape id.
    Ids are assigned densely starting at [0]. The index is marked stale; the next {!build}
    or {!iterator} call materializes the new geometry. *)
val add : t -> S2_shape.t -> int

(** [num_shape_ids t] is the number of distinct shape ids that have been assigned. *)
val num_shape_ids : t -> int
[@@zero_alloc]

(** [shape t id] returns the shape previously added with id [id]. Raises if [id] is
    outside [0 .. num_shape_ids t - 1]. *)
val shape : t -> int -> S2_shape.t
[@@zero_alloc]

(** [build t] materializes any pending additions into the cell map. Idempotent: a no-op
    when the index is already fresh. {!iterator} calls this implicitly. *)
val build : t -> unit

(** [is_fresh t] is true when the index has no pending additions. *)
val is_fresh : t -> bool

(** [iterator t] builds the index if necessary and returns a fresh iterator positioned at
    the first cell. *)
val iterator : t -> Iterator.t
