(** S2_cell_id uniquely identifies a cell in the S2 cell decomposition. The most
    significant 3 bits encode the face number (0-5). The remaining 61 bits encode the
    position of the center of this cell along the Hilbert curve on that face.

    Sequentially increasing cell IDs follow a continuous space-filling curve over the
    entire sphere.

    The level of a cell is determined by its lowest-numbered bit that is on. For a cell at
    level k, this position is 2 * (max_level - k). *)

open Core

[@@@zero_alloc all]

type t : bits64

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val hash : t -> int [@@zero_alloc ignore]
val hash_fold_t : Hash.state -> t -> Hash.state [@@zero_alloc ignore]

(** {1 Constants} *)

val face_bits : int
val num_faces : int
val max_level : int
val pos_bits : int
val max_size : int

(** {1 Constructors} *)

(** [none] is an invalid cell ID (0). *)
val none : t

(** [sentinel] is an invalid cell ID ((1 << 64) - 1). *)
val sentinel : t

(** [from_face_exn face] returns the face cell for [face]. Raises if [face] is not in
    [0, 5]. *)
val from_face_exn : int -> t

(** [from_face_pos_level face pos level] returns a cell ID from its face, 61-bit position,
    and level. *)
val from_face_pos_level : int -> int64# -> int -> t

(** [from_face_ij face i j] returns a leaf cell ID from its face and (i, j) coordinates. *)
val from_face_ij : int -> int -> int -> t

(** [from_point p] returns the leaf cell ID containing the given point. *)
val from_point : R3_vector.t -> t

(** [from_latlng ll] returns the leaf cell ID containing the given latlng. *)
val from_latlng : S2_latlng.t -> t

(** {1 Accessors} *)

(** [of_int64 i] creates a cell ID from a raw 64-bit integer. The resulting ID may be
    invalid; see {!is_valid}. *)
val of_int64 : int64# -> t

(** [id t] returns the 64-bit integer representation of the cell ID. *)
val id : t -> int64#

(** [to_int64] is {!id}: raw [int64#] for interop and symmetry with {!of_int64}. *)
val to_int64 : t -> int64#

(** [is_valid t] returns true if the ID represents a valid cell. *)
val is_valid : t -> bool

(** [face t] returns the face (0-5) containing this cell. *)
val face : t -> int

(** [pos t] returns the 61-bit Hilbert curve position of the cell. *)
val pos : t -> int64#

(** [level t] returns the subdivision level (0-30) of the cell. *)
val level : t -> int

(** [is_leaf t] returns true if this is a leaf cell (level 30). *)
val is_leaf : t -> bool

(** [is_face t] returns true if this is a face cell (level 0). *)
val is_face : t -> bool

(** [size_ij level] returns the edge length of cells at the given level in (i,j)-space. *)
val size_ij : int -> int

(** [size_st level] returns the edge length of a cell at the given level in (s,t)-space. *)
val size_st : int -> float#

(** {1 Hierarchy Navigation} *)

(** [parent_exn t] returns the parent of this cell. Raises if [t] is a face cell. *)
val parent_exn : t -> t

(** [parent_level t level] returns the ancestor of this cell at the given level. *)
val parent_level : t -> int -> t

(** [child_begin_at_level t level] is the first leaf-descendant boundary at [level]. *)
val child_begin_at_level : t -> int -> t

(** [child_end_at_level t level] is the corresponding exclusive end. *)
val child_end_at_level : t -> int -> t

(** No-arg [child_begin]: first Hilbert child at one finer level. *)
val child_begin : t -> t

(** No-arg [child_end]: exclusive end for iterating immediate children. *)
val child_end : t -> t

(** [hilbert_begin level] is the first cell at [level] on the global Hilbert curve (face
    0). *)
val hilbert_begin : int -> t

(** [hilbert_end level] is the exclusive end at [level] on the global Hilbert curve (past
    face 5). *)
val hilbert_end : int -> t

(** [child_exn t k] returns the k-th child (k in [0, 3]). Raises if [t] is a leaf cell or
    [k] is out of range. *)
val child_exn : t -> int -> t

(** [child_position t] returns the position of this cell within its parent (0-3). [t] must
    not be a face cell. *)
val child_position : t -> int

(** [child_position_level t level] returns the child position of the ancestor at the given
    level. *)
val child_position_level : t -> int -> int

(** {1 Operations} *)

(** [contains t other] returns true if [t] contains [other]. *)
val contains : t -> t -> bool

(** [intersects t other] returns true if [t] and [other] intersect. *)
val intersects : t -> t -> bool

(** [range_min t] returns the minimum leaf cell ID contained by this cell. *)
val range_min : t -> t

(** [range_max t] returns the maximum leaf cell ID contained by this cell. *)
val range_max : t -> t

(** [next t] returns the next cell ID at the same level in Hilbert curve order. *)
val next : t -> t

(** [prev t] returns the previous cell ID at the same level. *)
val prev : t -> t

(** [next_wrap t] returns the next cell ID, wrapping from face 5 to face 0. *)
val next_wrap : t -> t

(** [prev_wrap t] returns the previous cell ID, wrapping from face 0 to face 5. *)
val prev_wrap : t -> t

(** [advance t steps] advances [t] by the given number of steps at its level. Clamps to
    [begin] and [end] of the level. *)
val advance : t -> int64# -> t

(** [advance_wrap t steps] advances [t] by the given number of steps, wrapping around the
    sphere. *)
val advance_wrap : t -> int64# -> t

(** Hilbert index of [ t ] among all cells at the same level. *)
val distance_from_begin : t -> int64#

(** Level of the most specific common ancestor of two cells, or [-1]. *)
val get_common_ancestor_level : t -> t -> int

(** Covering helper. *)
val maximum_tile : t -> t -> t

(** [vertex_neighbors t level] returns the raw 64-bit ids of the cells at [level] that
    share the closest cell vertex to [t]. Returns 3 cells when the closest vertex is one
    of the 8 cube corners, otherwise 4 cells. [level] must be strictly less than
    [level t]. The result is returned as boxed [Int64.t] because [S2_cell_id.t] has layout
    [bits64] and cannot live in a regular list. *)
val vertex_neighbors : t -> int -> Int64.t list
[@@zero_alloc ignore]

(** {1 Conversion} *)

(** [to_point t] returns the center point of the cell on the unit sphere. *)
val to_point : t -> R3_vector.t

(** [to_point_raw t] returns the center point of the cell on the biunit cube. *)
val to_point_raw : t -> R3_vector.t

val to_center_uv : t -> R2_point.t
val to_face_ij_orientation : t -> #(int * int * int * int)

(** [to_token t] returns a hex string representing the cell ID. *)
val to_token : t -> string
[@@zero_alloc ignore]

(** [to_string t] returns a string representation of the cell ID (face/level/pos). *)
val to_string : t -> string
[@@zero_alloc ignore]

(** [from_token s] parses a hex string into a cell ID. Returns [none] for invalid input,
    empty strings, or ["X"] (the token for [none]). *)
val from_token : string -> t
[@@zero_alloc ignore]

(* {1 Comparison }*)

val compare : t -> t -> int
val equal : t -> t -> bool
