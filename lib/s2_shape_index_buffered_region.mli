(** Wrap an {!S2_shape_index.t} as an {!S2_region.t} that represents the original geometry
    expanded ("buffered", "offset", or "Minkowski sum with a disc") by a fixed angular
    radius. The resulting region contains every point within [radius] of any point in the
    underlying index.

    The geometry itself is not buffered; the {!S2_region.t} interface is implemented by
    measuring the distance from candidate cells (or points) to the original geometry and
    comparing against [radius]. The class is mainly useful as input to
    {!S2_region_coverer} when computing a cell covering of buffered geometry. For example,
    if the input is a single point, the buffered region is exactly equivalent to an
    {!S2_cap.t} centered at that point with radius [radius] (without polygonal
    approximation).

    All cell predicates are conservative and use the upper-bound errors built into
    {!S2_closest_edge_query}. {!contains_cell} in particular is implemented with a
    cap-based heuristic - it returns [true] only when containment can be proven, so
    barely-contained cells may report [false].

    Not thread-safe: each thread should construct its own [t]. *)

type t

(** [create index radius] returns a region view of [index] expanded by the given chord
    angle [radius]. The index is built eagerly. *)
val create : S2_shape_index.t -> S1_chord_angle.t -> t

(** [create_angle index radius] is like {!create} but accepts an {!S1_angle.t} for
    convenience. [radius] must be non-negative. *)
val create_angle : S2_shape_index.t -> S1_angle.t -> t

(** [index t] returns the index [t] was built against. *)
val index : t -> S2_shape_index.t

(** [radius t] returns the buffer radius. *)
val radius : t -> S1_chord_angle.t

(** [cap_bound t] returns a spherical cap that covers the buffered region. *)
val cap_bound : t -> S2_cap.t

(** [rect_bound t] returns a latitude-longitude rectangle that covers the buffered region. *)
val rect_bound : t -> S2_latlng_rect.t

(** [cell_union_bound t] returns a small set of cell ids whose union covers the buffered
    region. The result is not optimal: it may include duplicate or overlapping cells, and
    is intended as a fast over-approximation rather than a tight covering. Pass through
    {!S2_region_coverer} to obtain a clean covering. *)
val cell_union_bound : t -> S2_cell_id.t array

(** [contains_cell t cell] is [true] when [cell] is provably contained in the buffered
    region. The implementation is conservative: it returns [false] in some cases where
    [cell] is in fact contained. *)
val contains_cell : t -> S2_cell.t -> bool

(** [may_intersect_cell t cell] is [true] when any buffered shape could intersect [cell],
    to within a very small error margin. *)
val may_intersect_cell : t -> S2_cell.t -> bool

(** [contains_point t p] is [true] when [p] lies within [radius t] of some shape stored in
    [index t]. *)
val contains_point : t -> S2_point.t -> bool

(** [to_region t] returns an {!S2_region.t} backed by [t]. Each method dispatches back
    into [t], so the returned region shares state with [t] and is similarly not
    thread-safe. *)
val to_region : t -> S2_region.t
