(** S2CellUnion is a region consisting of cells of various sizes.

    An S2CellUnion is represented as a sorted array of non-overlapping S2CellIds. By
    default the array is also "normalized", meaning that groups of 4 child cells have been
    replaced by their parent cell whenever possible. S2CellUnions are not required to be
    normalized, but certain operations will return different results if they are not
    (e.g., [contains_union]).

    Internally, cell ids are stored as unboxed [S2_cell_id.t array] (int64# array). *)

open Core

type t [@@deriving sexp_of]

(** {1 Constructors} *)

(** [create ids] constructs a cell union from the given cell ids, then normalizes it
    (sorts, removes duplicates, merges siblings). *)
val create : S2_cell_id.t array -> t

(** [from_verbatim ids] constructs a cell union from sorted, non-overlapping cell ids
    without normalizing. The caller must ensure the ids satisfy [is_valid]. *)
val from_verbatim : S2_cell_id.t array -> t

(** [from_min_max min_id max_id] constructs a normalized cell union covering the leaf
    cells between [min_id] and [max_id] inclusive. Both must be leaf cells, and
    [min_id <= max_id]. *)
val from_min_max : S2_cell_id.t -> S2_cell_id.t -> t

(** [from_begin_end begin_id end_id] constructs a normalized cell union covering the
    half-open range of leaf cells [\[begin_id, end_id)]. If [begin_id = end_id] the result
    is empty. Both must be leaf cells, and [begin_id <= end_id]. *)
val from_begin_end : S2_cell_id.t -> S2_cell_id.t -> t

(** [whole_sphere ()] returns a cell union covering the entire sphere (6 face cells). *)
val whole_sphere : unit -> t

(** [empty ()] returns an empty cell union. *)
val empty : unit -> t

(** {1 Accessors} *)

(** [cell_ids_raw t] returns a copy of the underlying sorted array of cell ids. *)
val cell_ids_raw : t -> S2_cell_id.t array

(** [num_cells t] returns the number of cells in the union. *)
val num_cells : t -> int

(** [is_empty t] returns true if the union is empty. *)
val is_empty : t -> bool

(** [cell_id t i] returns the i-th cell id. *)
val cell_id : t -> int -> S2_cell_id.t

(** {1 Validation} *)

(** [is_valid t] returns true if the cell ids are valid, non-overlapping, and sorted. *)
val is_valid : t -> bool

(** [is_normalized t] returns true if the union satisfies [is_valid] and no four cells
    have a common parent. *)
val is_normalized : t -> bool

(** {1 Normalize / Denormalize} *)

(** [normalize t] returns a normalized version of the cell union. *)
val normalize : t -> t

(** [denormalize t ~min_level ~level_mod] replaces any cell whose level is less than
    [min_level] or where [(level - min_level)] is not a multiple of [level_mod] with its
    children, until both conditions are satisfied or the maximum level is reached. Returns
    an array of cell ids. *)
val denormalize : t -> min_level:int -> level_mod:int -> S2_cell_id.t array

(** {1 Containment and Intersection} *)

(** [contains_cell_id t id] returns true if the union contains the given cell id.
    Containment is defined with respect to regions, e.g. a cell contains its 4 children.
    This is a fast O(log n) operation.

    CAVEAT: If you have constructed a non-normalized union using [from_verbatim], groups
    of 4 child cells are not considered to contain their parent. Call [normalize] first. *)
val contains_cell_id : t -> S2_cell_id.t -> bool

(** [intersects_cell_id t id] returns true if the union intersects the given cell id. This
    is a fast O(log n) operation. *)
val intersects_cell_id : t -> S2_cell_id.t -> bool

(** [contains_union t other] returns true if this union contains every cell of [other]. *)
val contains_union : t -> t -> bool

(** [intersects_union t other] returns true if this union intersects any cell of [other]. *)
val intersects_union : t -> t -> bool

(** [contains_cell t c] returns true if the union contains the given cell. *)
val contains_cell : t -> S2_cell.t -> bool

(** [intersects_cell t c] returns true if the union intersects the given cell. *)
val intersects_cell : t -> S2_cell.t -> bool

(** [contains_point t p] returns true if the union contains the given point. *)
val contains_point : t -> S2_point.t -> bool

(** {1 Set Operations} *)

(** [union t other] returns the union of two cell unions. *)
val union : t -> t -> t

(** [intersection t other] returns the intersection of two cell unions. *)
val intersection : t -> t -> t

(** [intersection_with_cell_id t id] returns the intersection of the union with a single
    cell id. *)
val intersection_with_cell_id : t -> S2_cell_id.t -> t

(** [difference t other] returns the difference [t - other]. *)
val difference : t -> t -> t

(** {1 Measures} *)

(** [leaf_cells_covered t] returns the number of leaf cells covered by this union. *)
val leaf_cells_covered : t -> int64

(** [average_based_area t] returns the approximate area by summing each cell's average
    area. Accurate to within a factor of 1.7. *)
val average_based_area : t -> float#

(** [approx_area t] returns the approximate area. Accurate to within 3% for all sizes and
    0.1% for cells at level 5 or higher. *)
val approx_area : t -> float#

(** [exact_area t] returns the area as accurately as possible. *)
val exact_area : t -> float#

(** {1 Bounding} *)

(** [cap_bound t] returns a bounding cap for the union. *)
val cap_bound : t -> S2_cap.t

(** [rect_bound t] returns a bounding latitude-longitude rectangle for the union. *)
val rect_bound : t -> S2_latlng_rect.t

(** [cell_union_bound t] returns a small set of cell ids whose union covers [t]. *)
val cell_union_bound : t -> S2_cell_id.t array

(** {1 Comparison} *)

(** [equal t other] returns true if two cell unions contain the same cell ids. *)
val equal : t -> t -> bool
