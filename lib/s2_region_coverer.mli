(** S2RegionCoverer approximates arbitrary regions as unions of cells (S2CellUnion).

    This is useful for implementing various sorts of search and precomputation operations.

    Typical usage:

    {[
      let rc = S2_region_coverer.create ~max_cells:5 () in
      let covering = S2_region_coverer.covering rc (S2_region.of_cap cap) in
      (* covering is an S2_cell_union.t with at most 5 cells *)
    ]}

    Note the following:

    - [min_level] takes priority over [max_cells], i.e. cells below the given level will
      never be used even if this causes a large number of cells to be returned.

    - For any setting of [max_cells], up to 6 cells may be returned if that is the minimum
      number of cells required (e.g. if the region intersects all six face cells). Up to 3
      cells may be returned even for very tiny convex regions if they happen to be located
      at the intersection of three cube faces.

    - If [max_cells] is less than 4, the area of the covering may be arbitrarily large
      compared to the area of the original region even if the region is convex.

    The approximation algorithm is not optimal but does a pretty good job in practice. The
    output does not always use the maximum number of cells allowed, both because this
    would not always yield a better approximation, and because [max_cells] is a limit on
    how much work is done exploring the possible covering as well as a limit on the final
    output size.

    One can also generate interior coverings, which are sets of cells which are entirely
    contained within a region. Interior coverings can be empty, even for non-empty
    regions, if there are no cells that satisfy the provided constraints and are contained
    by the region. For performance reasons, it is wise to specify a [max_level] when
    computing interior coverings. *)

open Core

[@@@zero_alloc all]

type t

(** {1 Constructors} *)

(** [create ?min_level ?max_level ?level_mod ?max_cells ()] constructs a region coverer
    with the given parameters.

    - [min_level]: minimum cell level to be used (default 0)
    - [max_level]: maximum cell level to be used (default 30)
    - [level_mod]: only cells where [(level - min_level)] is a multiple of [level_mod]
      will be used; allowed values are 1, 2, or 3 (default 1)
    - [max_cells]: maximum desired number of cells in the approximation (default 8) *)
val create
  :  ?min_level:int
  -> ?max_level:int
  -> ?level_mod:int
  -> ?max_cells:int
  -> unit
  -> t
[@@zero_alloc ignore]

(** {1 Covering} *)

(** [covering t region] returns a cell union that covers the given region and satisfies
    the restrictions. Note that if [min_level > 0] or [level_mod > 1], the result may not
    be normalized (groups of 4 child cells may not be replaced by their parent). *)
val covering : t -> S2_region.t -> S2_cell_union.t
[@@zero_alloc ignore]

(** [interior_covering t region] returns a cell union that is contained within the given
    region and satisfies the restrictions. *)
val interior_covering : t -> S2_region.t -> S2_cell_union.t
[@@zero_alloc ignore]

(** [fast_covering t region] returns a cell union that covers the given region similar to
    {!covering}, except that this method is much faster and the coverings are not as
    tight. All of the usual parameters are respected, except that the implementation makes
    no attempt to take advantage of large values of [max_cells]. *)
val fast_covering : t -> S2_region.t -> S2_cell_union.t
[@@zero_alloc ignore]

(** {1 Validation} *)

(** [is_canonical t cell_ids] reports whether the given cell id array represents a valid
    covering that conforms to the current covering parameters. *)
val is_canonical : t -> S2_cell_id.t array -> bool

(** [canonicalize_covering t cell_ids] modifies [cell_ids] if necessary so that it
    conforms to the current covering parameters. There are no restrictions on the input
    cell ids (they may be unsorted, overlapping, etc). Returns a new conforming array. *)
val canonicalize_covering : t -> S2_cell_id.t array -> S2_cell_id.t array
[@@zero_alloc ignore]
