(** A two-dimensional region on the unit sphere.

    This is a small polymorphic wrapper over the built-in S2 region types ({!S2_cap.t},
    {!S2_latlng_rect.t}, {!S2_cell.t}, {!S2_cell_union.t}) plus a generic {!Custom}
    constructor that takes a record of callbacks. The goal of the interface is not to
    describe the region exactly but to expose just enough operations for another algorithm
    (typically {!S2_region_coverer}) to approximate it as a union of cells:

    - bounding operations ({!cap_bound}, {!rect_bound}, {!cell_union_bound});
    - cell containment tests ({!contains_cell}, {!intersects_cell});
    - point containment ({!contains_point}).

    Each accessor dispatches on the variant tag. If you already know the concrete type of
    your region it is fine, and often faster, to call the type-specific function directly.

    Wrap polygons, polylines, custom shapes, or other types with {!custom} by supplying a
    {!methods} record. *)

open Core

[@@@zero_alloc all]

(** Callbacks used by {!custom} to implement a user-defined region. Each field corresponds
    to the module-level function of the same name, and must satisfy the same contract:
    bounds may be loose but must actually contain the region, [contains_cell] must be
    conservative (returning [false] if containment cannot be proven), and [contains_point]
    assumes unit-length inputs. *)
type methods =
  #{ cap_bound : unit -> S2_cap.t
   ; rect_bound : unit -> S2_latlng_rect.t
   ; contains_cell : S2_cell.t -> bool
   ; intersects_cell : S2_cell.t -> bool
   ; contains_point : S2_point.t -> bool
   ; cell_union_bound : unit -> S2_cell_id.t array
   }

type t =
  | Cap of S2_cap.t
  | Rect of S2_latlng_rect.t
  | Cell of S2_cell.t
  | Cell_union of S2_cell_union.t
  | Custom of methods

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [of_cap c] wraps a cap as a region. *)
val of_cap : S2_cap.t -> t [@@zero_alloc ignore]

(** [of_rect r] wraps a latitude-longitude rectangle as a region. *)
val of_rect : S2_latlng_rect.t -> t
[@@zero_alloc ignore]

(** [of_cell c] wraps a cell as a region. *)
val of_cell : S2_cell.t -> t
[@@zero_alloc ignore]

(** [of_cell_union u] wraps a cell union as a region. *)
val of_cell_union : S2_cell_union.t -> t
[@@zero_alloc ignore]

(** [custom m] wraps a user-supplied methods record. Use this for regions that are not
    natively covered by the built-in constructors. *)
val custom : methods -> t
[@@zero_alloc ignore]

(** {1 Region methods}

    Each accessor dispatches on the variant tag. Pattern-matching [t] directly is also
    fine, and lets the compiler eliminate the dispatch at known-tag call sites. *)

(** [cap_bound t] returns a spherical cap that contains [t]. The bound is not required to
    be tight. *)
val cap_bound : t -> S2_cap.t
[@@zero_alloc ignore]

(** [rect_bound t] returns a lat/lng rectangle that contains [t]. The bound is not
    required to be tight. *)
val rect_bound : t -> S2_latlng_rect.t
[@@zero_alloc ignore]

(** [contains_cell t cell] returns [true] if [cell] is completely contained in [t]. If the
    relationship cannot be determined, returns [false]. *)
val contains_cell : t -> S2_cell.t -> bool
[@@zero_alloc ignore]

(** [intersects_cell t cell] returns [false] only when [t] provably does not intersect
    [cell]. If [true], [t] either intersects [cell] or intersection could not be
    determined - callers treating it as "may intersect" remain correct. *)
val intersects_cell : t -> S2_cell.t -> bool
[@@zero_alloc ignore]

(** [contains_point t p] returns [true] iff the unit-length point [p] lies in [t]. *)
val contains_point : t -> S2_point.t -> bool
[@@zero_alloc ignore]

(** [cell_union_bound t] returns a small, fast-to-compute collection of {!S2_cell_id.t}
    whose union covers [t]. The output is not normalized: it may be unsorted, may contain
    redundant cells, and may cover substantially more area than necessary. Feed it to
    {!S2_region_coverer.fast_covering} or {!S2_region_coverer.canonicalize_covering}
    before use if you need a clean covering. *)
val cell_union_bound : t -> S2_cell_id.t array
[@@zero_alloc ignore]
