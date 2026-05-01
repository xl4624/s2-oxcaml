(** Wrap an {!S2_shape_index.t} as an {!S2_region.t}.

    {!S2_region.t} is the input type expected by region-coverer style algorithms: it
    exposes loose bounds and conservative cell-containment / cell-intersection tests, but
    not exact predicates against arbitrary geometry. This wrapper provides those answers
    by walking the index cells: it conservatively treats a cell as "contained" only when a
    single shape in the index demonstrably contains it, and as "may intersect" whenever
    any indexed shape could reach the cell.

    The {!contains_point} test uses the [Semi_open] vertex model: dimension-0 and
    dimension-1 shapes never contain a point, and exactly one of any tile of polygons that
    share a vertex contains that vertex. This matches the boundary handling used by
    {!S2_polygon}.

    All cell predicates are conservative but not exact; if a shape is just barely disjoint
    from a cell, or just barely contains it, the answer may swing the wrong way. The
    maximum error is on the order of [10 * Float.epsilon_float] radians.

    Not thread-safe: each thread should build its own [t]. *)

type t

(** [create index] returns a region view bound to [index]. The index is built eagerly. *)
val create : S2_shape_index.t -> t

(** [index t] returns the index [t] was built against. *)
val index : t -> S2_shape_index.t

(** [cap_bound t] returns a spherical cap that covers every shape in the index. *)
val cap_bound : t -> S2_cap.t

(** [rect_bound t] returns a latitude-longitude rectangle that covers every shape in the
    index. *)
val rect_bound : t -> S2_latlng_rect.t

(** [cell_union_bound t] returns up to 6 cell ids that together cover every shape in the
    index (4 cells when the index spans a single face). The result is sorted in cell id
    order and is not normalized: pass through {!S2_cell_union.create} or
    {!S2_region_coverer} if a tighter, deduplicated covering is needed. *)
val cell_union_bound : t -> S2_cell_id.t array

(** [contains_cell t cell] is [true] when some single shape in the index demonstrably
    contains [cell]. The check is conservative: it returns [false] if no single shape in
    the index covers [cell] on its own, even when a union of several shapes does. *)
val contains_cell : t -> S2_cell.t -> bool

(** [may_intersect_cell t cell] is [false] only when no shape in the index can intersect
    [cell]. A [true] result includes the [false]-positive case where an edge is just
    outside [cell] but within the conservative error tolerance. *)
val may_intersect_cell : t -> S2_cell.t -> bool

(** [contains_point t p] is [true] when [p] lies inside some dimension-2 shape under the
    [Semi_open] vertex model. Dimension-0 and dimension-1 shapes are ignored - use
    {!S2_contains_point_query} directly when other vertex models or lower-dimensional
    shapes matter. *)
val contains_point : t -> S2_point.t -> bool

(** [to_region t] returns an {!S2_region.t} backed by [t]. Each method dispatches back
    into [t], so the returned region shares state with [t] and is similarly not
    thread-safe. *)
val to_region : t -> S2_region.t
