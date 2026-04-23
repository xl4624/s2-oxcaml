(** Approximate arbitrary {!S2_region.t} values as unions of {!S2_cell_id.t} cells, for
    use in search and precomputation.

    A coverer is parameterised by four numbers: [min_level], [max_level], [level_mod], and
    [max_cells]. Given a region it produces a {!S2_cell_union.t} that covers it (or is
    covered by it, for interior coverings) while honouring those constraints.

    Typical usage:

    {[
      let rc = S2_region_coverer.create ~max_cells:5 () in
      let covering = S2_region_coverer.covering rc (S2_region.of_cap cap) in
      (* [covering] is an S2_cell_union.t with at most (roughly) 5 cells *)
    ]}

    Important caveats:

    - [min_level] takes priority over [max_cells]: cells below [min_level] are never used
      even if this forces the output to exceed [max_cells].
    - Up to 6 cells may always be returned if the region spans all six cube faces; up to 3
      may be returned for a tiny region located exactly at the intersection of three cube
      faces.
    - If [max_cells < 4], the area of the covering may be arbitrarily larger than that of
      the region even for convex inputs.
    - The algorithm is a best-effort approximation. It will not always use [max_cells]
      cells, both because more cells do not always give a better approximation and because
      [max_cells] doubles as a search-effort budget.

    Output stability is not guaranteed across library versions.

    Interior coverings (via {!interior_covering}) return cells wholly contained in the
    region. They can be empty even for non-empty regions if no allowed cell fits inside.
    Always set a finite [max_level] when computing interior coverings; otherwise the
    search can drill all the way to leaf level in regions with no interior. *)

open Core

[@@@zero_alloc all]

type t

(** {1 Constructors} *)

(** [create ?min_level ?max_level ?level_mod ?max_cells ()] constructs a coverer with the
    given parameters. Values are silently clamped to the legal range.

    - [min_level]: minimum cell level to be used (default [0]).
    - [max_level]: maximum cell level to be used (default [S2_cell_id.max_level = 30]).
      Cell edge length roughly halves per level.
    - [level_mod]: only cells where [(level - min_level) mod level_mod = 0] are used.
      Allowed values are [1], [2], or [3], raising the effective branching factor from [4]
      to [16] or [64] (default [1]).
    - [max_cells]: desired upper bound on the size of the output (default [8]). *)
val create
  :  ?min_level:int
  -> ?max_level:int
  -> ?level_mod:int
  -> ?max_cells:int
  -> unit
  -> t
[@@zero_alloc ignore]

(** {1 Covering} *)

(** [covering t region] returns a cell union that covers [region] and satisfies the
    parameters of [t]. If [min_level > 0] or [level_mod > 1], the result may not be
    normalized: groups of 4 sibling cells are not replaced by their parent when that
    parent would violate [min_level] or [level_mod]. *)
val covering : t -> S2_region.t -> S2_cell_union.t
[@@zero_alloc ignore]

(** [interior_covering t region] returns a cell union whose every cell lies entirely
    inside [region] and whose level fits the parameters of [t]. The result may be empty
    even when [region] is non-empty, for instance when no admissible cell fits inside. Set
    a finite [max_level] to bound the work. *)
val interior_covering : t -> S2_region.t -> S2_cell_union.t
[@@zero_alloc ignore]

(** [fast_covering t region] returns a cell union that covers [region], like {!covering},
    but runs much faster and produces looser coverings. All parameters are respected
    except that large values of [max_cells] do not tighten the result further. Useful as a
    starting point for algorithms that refine cells recursively. *)
val fast_covering : t -> S2_region.t -> S2_cell_union.t
[@@zero_alloc ignore]

(** {1 Validation} *)

(** [is_canonical t cell_ids] returns [true] iff [cell_ids] is already a valid covering
    under [t]'s parameters. A canonical covering is sorted, non-overlapping, each cell id
    is valid and has level in [[min_level, true_max_level]] satisfying [level_mod], the
    covering contains no four siblings that could collapse to their parent, and - when it
    exceeds [max_cells] - no two cells share an ancestor at or above [min_level]. *)
val is_canonical : t -> S2_cell_id.t array -> bool

(** [canonicalize_covering t cell_ids] returns a new array that covers the same region as
    [cell_ids] and conforms to the parameters of [t]. The input may be unsorted, may
    contain overlapping cells, and may have arbitrary levels. *)
val canonicalize_covering : t -> S2_cell_id.t array -> S2_cell_id.t array
[@@zero_alloc ignore]
