(** S2_padded_cell represents an S2Cell whose (u, v)-range has been expanded on all sides
    by a given amount of "padding". Unlike {!S2_cell}, its methods and representation are
    optimized for clipping edges against cell boundaries to determine which cells are
    intersected by a given set of edges. *)

open Core

[@@@zero_alloc all]

type t :
  bits64
  & float64
  & ((float64 & float64) & (float64 & float64))
  & ((float64 & float64) & (float64 & float64))
  & immediate
  & immediate
  & immediate
  & immediate

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [create id ~padding] constructs a padded cell with the given cell id and padding. *)
val create : S2_cell_id.t -> padding:float# -> t
[@@zero_alloc ignore]

(** [child_ij parent ~i ~j] constructs the child of [parent] with the given (i, j) index.
    The four child cells have indices (0, 0), (0, 1), (1, 0), (1, 1), where [i] and [j]
    correspond to increasing u- and v-values respectively. *)
val child_ij : t -> i:int -> j:int -> t
[@@zero_alloc ignore]

(** {1 Accessors} *)

(** [id t] returns the cell id this padded cell represents. *)
val id : t -> S2_cell_id.t

(** [padding t] returns the amount of padding on this cell. *)
val padding : t -> float#

(** [level t] returns the level of this cell. *)
val level : t -> int

(** [orientation t] returns the Hilbert curve orientation of this cell. *)
val orientation : t -> int

(** [bound t] returns the bounds for this cell in (u, v)-space including padding. *)
val bound : t -> R2_rect.t

(** [middle t] returns the rectangle in the middle of this cell (in (u, v)-space) that
    belongs to all four of its children. *)
val middle : t -> R2_rect.t

(** [center t] returns the center of this cell on the unit sphere. *)
val center : t -> S2_point.t

(** [entry_vertex t] returns the vertex where the S2 space-filling curve enters this cell. *)
val entry_vertex : t -> S2_point.t

(** [exit_vertex t] returns the vertex where the S2 space-filling curve exits this cell. *)
val exit_vertex : t -> S2_point.t

(** [child_ij_of_pos t ~pos] returns the (i, j) coordinates for the child cell at the
    given traversal position. The traversal position corresponds to the order in which
    child cells are visited by the Hilbert curve. *)
val child_ij_of_pos : t -> pos:int -> #(int * int)

(** {1 Operations} *)

(** [shrink_to_fit t rect] returns the smallest cell id that contains all descendants of
    [t] whose bounds intersect [rect]. For algorithms that use recursive subdivision to
    find the cells that intersect a particular object, this can be used to skip all the
    initial subdivision steps where only one child needs to be expanded.

    Note that this is not the same as returning the smallest cell that contains the
    intersection of [t] with [rect]; because of the padding, even if one child completely
    contains [rect] it is still possible that a neighboring child also intersects [rect].

    Requires that [bound t] intersects [rect]. *)
val shrink_to_fit : t -> R2_rect.t -> S2_cell_id.t
[@@zero_alloc ignore]
