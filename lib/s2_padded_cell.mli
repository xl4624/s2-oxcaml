(** An {!S2_cell.t}-like value whose (u, v)-range has been expanded on all sides by a
    fixed "padding". The representation is specialised for clipping great-circle edges
    against cell boundaries when figuring out which cells an edge crosses, which is the
    main operation performed during {!S2_shape_index.t} construction.

    Compared to {!S2_cell}, a padded cell caches its (u, v) bound inclusive of padding,
    can compute the child rectangles in (u, v)-space directly (via {!child_ij}), and
    exposes the Hilbert-curve entry and exit vertices used by incremental traversal. *)

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

(** [create id ~padding] builds a padded cell for [id] with [padding] added to every side
    of its (u, v) bound. *)
val create : S2_cell_id.t -> padding:float# -> t
[@@zero_alloc ignore]

(** [child_ij parent ~i ~j] builds the child of [parent] at index [(i, j)], where [i]
    selects between low-u (0) and high-u (1), and [j] selects between low-v (0) and high-v
    (1). The child inherits [parent]'s padding. *)
val child_ij : t -> i:int -> j:int -> t
[@@zero_alloc ignore]

(** {1 Accessors} *)

(** [id t] returns the cell id underlying [t]. *)
val id : t -> S2_cell_id.t

(** [padding t] returns the padding applied to each side of the (u, v) bound. *)
val padding : t -> float#

(** [level t] returns the subdivision level of [t]. *)
val level : t -> int

(** [orientation t] returns the Hilbert-curve orientation bits for [t] (used to map
    between (i, j) child indices and curve traversal positions). *)
val orientation : t -> int

(** [bound t] returns the (u, v)-space rectangle covering [t], padding included. *)
val bound : t -> R2_rect.t

(** [middle t] returns the rectangle in the interior of [t] that belongs to all four
    children (i.e. the intersection of the four child bounds). This is the "cross" formed
    around the cell center by the padding. Computed on demand and cached; not thread-safe. *)
val middle : t -> R2_rect.t

(** [center t] returns the point at the center of [t] on the unit sphere. *)
val center : t -> S2_point.t

(** [entry_vertex t] returns the cell corner where the S2 space-filling (Hilbert) curve
    enters [t]. *)
val entry_vertex : t -> S2_point.t

(** [exit_vertex t] returns the cell corner where the Hilbert curve exits [t]. *)
val exit_vertex : t -> S2_point.t

(** [child_ij_of_pos t ~pos] returns the [(i, j)] coordinates of the child visited at
    Hilbert position [pos] in [0..3]. Inverse of the mapping used by {!child_ij}. *)
val child_ij_of_pos : t -> pos:int -> #(int * int)

(** {1 Operations} *)

(** [shrink_to_fit t rect] returns the deepest descendant of [t] whose bound still
    contains every descendant that intersects [rect]. It lets recursive subdivision
    algorithms skip the initial levels where only one child ever overlaps [rect].

    Note that [shrink_to_fit] does not necessarily return the smallest cell that contains
    the intersection of [t] and [rect]: padding means that even if one child completely
    covers [rect], a neighbouring child may also overlap it, in which case the function
    must return the common ancestor.

    Requires [bound t] to intersect [rect]. *)
val shrink_to_fit : t -> R2_rect.t -> S2_cell_id.t
[@@zero_alloc ignore]

(** {1 Limitations}

    Padded cells mirror the C++ class closely; there are no deliberately omitted features
    at this time. *)
