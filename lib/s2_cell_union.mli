(** A region represented by a set of S2 cells of mixed sizes.

    An {!S2_cell_union.t} is a sorted sequence of pairwise-disjoint {!S2_cell_id.t}
    values, typically used to approximate an arbitrary shape with a hierarchical
    collection of quadrilaterals. There is a tradeoff between covering accuracy and the
    number of cells used; unlike polygons, cells have a fixed hierarchical structure,
    which makes them very amenable to preprocessing-based queries.

    A union is "normalized" when, in addition to being sorted and disjoint, every group of
    four sibling cells has been merged into its common parent. Most constructors produce
    normalized unions; {!from_verbatim} skips normalization and requires the caller to
    guarantee the inputs are already sorted and disjoint. Operations that depend on
    normalization (most notably {!contains_cell_id} when the query matches a parent of
    four stored children) are documented on the individual operation.

    The underlying storage is an unboxed [S2_cell_id.t array] ([int64# array]). *)

open Core

[@@@zero_alloc all]

type t

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [create ids] copies [ids], sorts them, removes duplicates, and collapses four-sibling
    groups into their parents. The caller's array is not modified. *)
val create : S2_cell_id.t array -> t
[@@zero_alloc ignore]

(** [from_verbatim ids] copies [ids] verbatim without sorting or merging. The caller must
    ensure each id is valid, the array is sorted in Hilbert order, and no two ids contain
    each other. Failing to satisfy these preconditions will produce a {!t} for which
    predicates like {!contains_cell_id} and {!intersects_cell_id} return unspecified
    results. *)
val from_verbatim : S2_cell_id.t array -> t
[@@zero_alloc ignore]

(** [of_raw_owned ids] is {!create} but consumes [ids] in place (the array is sorted and
    reused as the internal storage). The caller transfers ownership and must not read or
    mutate [ids] afterwards. *)
val of_raw_owned : S2_cell_id.t array -> t
[@@zero_alloc ignore]

(** [of_verbatim_owned ids] is {!from_verbatim} but stores [ids] directly. The caller
    transfers ownership and must not mutate [ids] afterwards. *)
val of_verbatim_owned : S2_cell_id.t array -> t

(** [from_min_max min_id max_id] returns a normalized union covering every leaf cell with
    id in [[min_id, max_id]]. Both endpoints must be leaf cells and [min_id <= max_id]. *)
val from_min_max : S2_cell_id.t -> S2_cell_id.t -> t
[@@zero_alloc ignore]

(** [from_begin_end begin_id end_id] returns a normalized union covering every leaf cell
    with id in the half-open range [\[begin_id, end_id)]. If the endpoints coincide the
    result is empty. Both must be leaf cells and [begin_id <= end_id]. *)
val from_begin_end : S2_cell_id.t -> S2_cell_id.t -> t
[@@zero_alloc ignore]

(** [whole_sphere ()] returns a union containing the six face cells (which cover the
    sphere exactly). *)
val whole_sphere : unit -> t
[@@zero_alloc ignore]

(** [empty ()] returns a union containing no cells. *)
val empty : unit -> t

(** {1 Accessors} *)

(** [cell_ids_raw t] returns a fresh copy of the underlying sorted cell ids. *)
val cell_ids_raw : t -> S2_cell_id.t array
[@@zero_alloc ignore]

(** [num_cells t] returns the number of cells in [t]. *)
val num_cells : t -> int

(** [is_empty t] is [true] iff [t] contains no cells. *)
val is_empty : t -> bool

(** [cell_id t i] returns the [i]-th cell id in Hilbert order. *)
val cell_id : t -> int -> S2_cell_id.t

(** {1 Validation} *)

(** [is_valid t] is [true] iff every stored id is valid, the array is sorted in Hilbert
    order, and no two ids have overlapping Hilbert ranges (equivalently no id contains
    another). *)
val is_valid : t -> bool

(** [is_normalized t] is [true] iff {!is_valid} holds and no four consecutive ids share a
    common parent (which would normalize to that parent). *)
val is_normalized : t -> bool

(** {1 Normalize / Denormalize} *)

(** [normalize t] returns a union equal in coverage to [t] that satisfies
    {!is_normalized}. A no-op (up to array copy) when [t] is already normalized. *)
val normalize : t -> t
[@@zero_alloc ignore]

(** [denormalize t ~min_level ~level_mod] returns a plain array of ids covering the same
    region as [t], subdividing each stored cell until its level is at least [min_level]
    and [(level - min_level) mod level_mod = 0]. Subdivision stops at [max_level]. Useful
    for generating fixed-resolution tilings from a normalized union. *)
val denormalize : t -> min_level:int -> level_mod:int -> S2_cell_id.t array
[@@zero_alloc ignore]

(** {1 Containment and Intersection} *)

(** [contains_cell_id t id] is [true] iff the region covered by [t] contains the region of
    [id]. Runs in O(log n).

    Caveat: on a non-normalized union produced with {!from_verbatim}, four sibling cells
    in [t] are not considered to contain their parent. Call {!normalize} first if you need
    that equivalence. *)
val contains_cell_id : t -> S2_cell_id.t -> bool

(** [intersects_cell_id t id] is [true] iff the regions covered by [t] and [id] share at
    least one point. Runs in O(log n). *)
val intersects_cell_id : t -> S2_cell_id.t -> bool

(** [contains_union t other] is [true] iff the region covered by [other] is a subset of
    the region covered by [t]. Runs in O(m log n) where [n = num_cells t] and
    [m = num_cells other]. *)
val contains_union : t -> t -> bool

(** [intersects_union t other] is [true] iff the regions covered by [t] and [other] share
    at least one point. Runs in O((n + m) log min(n, m)) in the worst case via double
    binary search. *)
val intersects_union : t -> t -> bool

(** [contains_cell t c] is [contains_cell_id t (S2_cell.id c)]. *)
val contains_cell : t -> S2_cell.t -> bool

(** [intersects_cell t c] is [intersects_cell_id t (S2_cell.id c)]. *)
val intersects_cell : t -> S2_cell.t -> bool

(** [contains_point t p] is [contains_cell_id t (S2_cell_id.from_point p)]: [true] iff [p]
    lies inside one of [t]'s cells. *)
val contains_point : t -> S2_point.t -> bool

(** {1 Set Operations} *)

(** [union t other] returns a normalized union covering all cells of both inputs. Runs in
    O(n + m). *)
val union : t -> t -> t
[@@zero_alloc ignore]

(** [intersection t other] returns a normalized union covering exactly the intersection
    region. *)
val intersection : t -> t -> t
[@@zero_alloc ignore]

(** [intersection_with_cell_id t id] is the intersection of [t] with the single cell [id]. *)
val intersection_with_cell_id : t -> S2_cell_id.t -> t
[@@zero_alloc ignore]

(** [difference t other] returns a union covering [region(t) - region(other)]. Implemented
    by splitting cells of [t] recursively against [other] until each piece is either fully
    inside or fully outside [other]. *)
val difference : t -> t -> t
[@@zero_alloc ignore]

(** {1 Measures} *)

(** [leaf_cells_covered t] returns the number of leaf cells (cells at
    {!S2_cell_id.max_level}) covered by [t]. Sums [4^(max_level - level id)] over stored
    ids, so the result fits in an unsigned 62-bit integer. *)
val leaf_cells_covered : t -> int64#

(** [average_based_area t] approximates the total area by weighting each cell by the
    average area at its level. Cheap but only accurate to within a factor of 1.7. *)
val average_based_area : t -> float#

(** [approx_area t] sums {!S2_cell.approx_area} over every stored cell. Accuracy is within
    3% at any level and within 0.1% for cells at level 5 or deeper. *)
val approx_area : t -> float#

(** [exact_area t] sums {!S2_cell.exact_area} over every stored cell; accurate to roundoff
    but more expensive than {!approx_area}. *)
val exact_area : t -> float#

(** {1 Bounding} *)

(** [cap_bound t] returns a bounding spherical cap. Built by taking the area-weighted
    centroid of [t]'s cells as the cap center and then extending the cap to cover every
    cell's own cap bound. *)
val cap_bound : t -> S2_cap.t

(** [rect_bound t] returns a bounding latitude-longitude rectangle computed by unioning
    each cell's {!S2_latlng_rect.from_cell}. *)
val rect_bound : t -> S2_latlng_rect.t

(** [cell_union_bound t] returns a small cell-id covering of [t] by applying
    {!S2_cap.cell_union_bound} to [cap_bound t]. Generally at most 4 cells, up to 6 for
    very large unions. *)
val cell_union_bound : t -> S2_cell_id.t array
[@@zero_alloc ignore]

(** {1 Comparison} *)

(** [equal t other] is [true] iff [t] and [other] store the identical sequence of cell
    ids. Non-normalized unions that cover the same region but differ in representation
    compare unequal; call {!normalize} first if you want coverage equality. *)
val equal : t -> t -> bool
