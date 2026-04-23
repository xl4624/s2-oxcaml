(** A 64-bit identifier for a cell in the S2 cell decomposition.

    The id encodes both the cell's face and its subdivision position along a Hilbert
    space-filling curve on that face:

    - the top 3 bits are the face number ([0..5]);
    - the next [61] bits encode the Hilbert position of the cell center on that face.

    The subdivision level is read off the position of the lowest set bit: a cell at level
    [k] has its lowest-set bit at position [2 * (max_level - k)], so the parent cell is
    found by zeroing everything below the next higher "1" bit. Sequentially increasing ids
    trace a continuous space-filling curve over the whole sphere, making [compare]
    equivalent to Hilbert order within each level.

    An {!S2_cell_id.t} is a light-weight handle (an unboxed [int64#]). It is suitable for
    indexing points and ranges on the sphere; when you need efficient point containment or
    geometric queries, convert to {!S2_cell.t}, which caches the cell's
    [(face, i, j, orientation)] and [(u, v)] bounds.

    Two special values are reserved: {!none} (all zeros) and {!sentinel} (all ones).
    Neither represents a valid cell; {!sentinel} compares strictly greater than every
    valid id, which is convenient for defining right-open ranges. *)

open Core

[@@@zero_alloc all]

type t : bits64

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
val hash : t -> int [@@zero_alloc ignore]
val hash_fold_t : Hash.state -> t -> Hash.state [@@zero_alloc ignore]

(** {1 Constants} *)

(** Number of bits reserved for the face index ([3]). *)
val face_bits : int

(** Number of distinct faces ([6]). *)
val num_faces : int

(** Deepest subdivision level ([30]). *)
val max_level : int

(** Total number of Hilbert-position bits ([2 * max_level + 1 = 61]). *)
val pos_bits : int

(** Edge length of a face in leaf-cell units ([2^30]). Also the number of leaf cells along
    one side of a face. *)
val max_size : int

(** {1 Constructors} *)

(** The all-zero id. Not a valid cell; used as a sentinel "missing" value. *)
val none : t

(** The all-ones id. Not a valid cell, but compares strictly greater than every valid id,
    which makes it useful as an exclusive upper bound in ranges. *)
val sentinel : t

(** [from_face_exn face] returns the top-level (level-0) cell for cube [face]. Raises if
    [face] is not in [[0, 5]]. *)
val from_face_exn : int -> t

(** [from_face_pos_level face pos level] reconstructs a cell from [face], its
    Hilbert-curve position within that face ([pos] using the low [pos_bits] bits), and
    [level]. Bits of [pos] below the level are discarded. *)
val from_face_pos_level : int -> int64# -> int -> t

(** [from_face_ij face i j] returns the leaf cell whose integer cube coordinates on [face]
    are [(i, j)], both in [\[0, max_size)]. *)
val from_face_ij : int -> int -> int -> t

(** [from_point p] returns the leaf cell containing [p]. [p] does not need to be unit
    length. For points on a cell boundary the choice is deterministic but unspecified; the
    returned cell always contains [p] (cells are closed sets). *)
val from_point : R3_vector.t -> t

(** [from_latlng ll] returns the leaf cell containing the point corresponding to [ll]. *)
val from_latlng : S2_latlng.t -> t

(** {1 Accessors} *)

(** [of_int64 i] reinterprets the raw 64-bit integer as a cell id. The result may not be
    valid - call {!is_valid} if that matters. *)
val of_int64 : int64# -> t

(** [id t] returns the 64-bit integer representation of [t]. *)
val id : t -> int64#

(** [to_int64] is {!id}; exposed for symmetry with {!of_int64}. *)
val to_int64 : t -> int64#

(** [is_valid t] returns true if [t]'s face is in [[0, 5]] and its level tag is in
    [[0, max_level]]. *)
val is_valid : t -> bool

(** [face t] returns the face ([0..5]) containing [t]. *)
val face : t -> int

(** [pos t] returns the [61]-bit Hilbert curve position of [t] within its face (the low
    [pos_bits] bits of the id). *)
val pos : t -> int64#

(** [level t] returns the subdivision level ([0..max_level]) of [t]. Derived from the
    trailing-zero count of the id so runs in O(1). *)
val level : t -> int

(** [is_leaf t] is [true] iff [t] is at level [max_level]. *)
val is_leaf : t -> bool

(** [is_face t] is [true] iff [t] is at level 0 (a top-level face cell). *)
val is_face : t -> bool

(** [size_ij level] returns the edge length of cells at [level] in leaf-cell units
    ([2^(max_level - level)]). *)
val size_ij : int -> int

(** [size_st level] returns the edge length of a cell at [level] in [(s, t)]-space. *)
val size_st : int -> float#

(** {1 Hierarchy Navigation} *)

(** [parent_exn t] returns [t]'s immediate parent cell. Raises if [t] is already a face
    cell. *)
val parent_exn : t -> t

(** [parent_level t level] returns [t]'s ancestor at [level], which must be in
    [[0, level t]]. Behavior is unspecified when [level] is outside that range. *)
val parent_level : t -> int -> t

(** [child_begin_at_level t level] returns the first cell at [level] that lies in [t]'s
    Hilbert subtree (equivalent to walking [t]'s leftmost chain down to [level]). *)
val child_begin_at_level : t -> int -> t

(** [child_end_at_level t level] returns the exclusive upper bound of [t]'s Hilbert
    subtree at [level], so iterating from {!child_begin_at_level} up to but not including
    this id visits every descendant at [level] exactly once. *)
val child_end_at_level : t -> int -> t

(** [child_begin t] is [child_begin_at_level t (level t + 1)]: the first immediate child
    in Hilbert order. *)
val child_begin : t -> t

(** [child_end t] is [child_end_at_level t (level t + 1)]: the exclusive end for iterating
    [t]'s four children. *)
val child_end : t -> t

(** [hilbert_begin level] is the first cell at [level] in the global Hilbert traversal (on
    face 0). *)
val hilbert_begin : int -> t

(** [hilbert_end level] is the exclusive upper bound at [level]: one step past the last
    cell on face 5. *)
val hilbert_end : int -> t

(** [child_exn t k] returns the [k]-th child of [t] in Hilbert order ([k] in [[0, 3]]).
    Raises if [t] is a leaf cell or [k] is out of range. *)
val child_exn : t -> int -> t

(** [child_position t] returns [t]'s position among its parent's children ([0..3]). [t]
    must not be a face cell. *)
val child_position : t -> int

(** [child_position_level t level] returns the child index (among the four children of its
    parent at [level - 1]) of [t]'s ancestor at [level]. *)
val child_position_level : t -> int -> int

(** {1 Operations} *)

(** [contains t other] is [true] iff every leaf-descendant of [other] is also a
    leaf-descendant of [t]. Equivalent to [range_min t <= other <= range_max t] using
    unsigned comparison. *)
val contains : t -> t -> bool

(** [intersects t other] is [true] iff [t] and [other] share at least one leaf descendant. *)
val intersects : t -> t -> bool

(** [range_min t] is the smallest leaf-cell id contained in [t]'s Hilbert subtree. *)
val range_min : t -> t

(** [range_max t] is the largest leaf-cell id contained in [t]'s Hilbert subtree. *)
val range_max : t -> t

(** [next t] returns the next cell at the same level in Hilbert order. Result is undefined
    past the end of face 5; use {!next_wrap} to wrap. *)
val next : t -> t

(** [prev t] returns the previous cell at the same level. Result is undefined before the
    start of face 0; use {!prev_wrap} to wrap. *)
val prev : t -> t

(** [next_wrap t] is like {!next} but wraps around: the successor of the last cell on face
    5 is the first cell on face 0 at the same level. *)
val next_wrap : t -> t

(** [prev_wrap t] is like {!prev} but wraps from face 0 back to face 5. *)
val prev_wrap : t -> t

(** [advance t steps] advances [t] by [steps] cells at its current level (negative values
    move backward). The result is clamped into
    [[hilbert_begin (level t), hilbert_end (level t)]]; further advances past either end
    produce the clamped boundary id. *)
val advance : t -> int64# -> t

(** [advance_wrap t steps] advances [t] by [steps] cells at its current level, wrapping
    around the sphere. [steps] is reduced modulo the number of cells at the level when it
    would otherwise overshoot. *)
val advance_wrap : t -> int64# -> t

(** [distance_from_begin t] is the index of [t] among all cells at the same level in
    Hilbert order ([0] for [hilbert_begin (level t)]). *)
val distance_from_begin : t -> int64#

(** [get_common_ancestor_level t other] returns the level of the deepest cell that
    contains both [t] and [other], or [-1] if they lie on different faces. *)
val get_common_ancestor_level : t -> t -> int

(** [maximum_tile t limit] returns the largest cell aligned with [t]'s subtree whose
    Hilbert range is wholly contained in [\[range_min t, range_min limit)]. This is the
    step used by {!S2_cell_union.from_begin_end} to tile a Hilbert range with the coarsest
    possible cells. *)
val maximum_tile : t -> t -> t

(** [vertex_neighbors t level] returns the cells at [level] that share [t]'s closest cell
    vertex. The result has 3 entries when that vertex is one of the 8 cube corners and 4
    otherwise. [level] must be strictly less than [level t]. *)
val vertex_neighbors : t -> int -> t array
[@@zero_alloc ignore]

(** {1 Conversion} *)

(** [to_point t] returns the cell center as a unit vector on the sphere. *)
val to_point : t -> R3_vector.t

(** [to_point_raw t] returns the cell center on the biunit cube (not unit length). Cheaper
    than {!to_point} when you only need a direction. *)
val to_point_raw : t -> R3_vector.t

(** [to_center_uv t] returns the center of [t] in cube-face [(u, v)] coordinates. *)
val to_center_uv : t -> R2_point.t

(** [to_face_ij_orientation t] returns [(face, i, j, orientation)] where [(i, j)] are the
    leaf-cell coordinates of [t]'s lower-left corner (for non-leaf cells this is their
    interpretation as the 30-bit quadkey prefix) and [orientation] is the Hilbert
    orientation bits of [t]. *)
val to_face_ij_orientation : t -> #(int * int * int * int)

(** [to_token t] returns a compact hex encoding of [t] with trailing zero nibbles
    stripped. The special id {!none} encodes as ["X"]. Tokens are ordered the same way as
    [compare] and round-trip through {!from_token}. *)
val to_token : t -> string
[@@zero_alloc ignore]

(** [to_string t] renders [t] as [face "/" path], where [path] is a string of digits
    [0..3] describing the Hilbert child taken at each level. Invalid ids are rendered as
    [Invalid: <hex>]. *)
val to_string : t -> string
[@@zero_alloc ignore]

(** [from_token s] parses a hex token produced by {!to_token}. Returns {!none} for empty
    strings, the literal ["X"], tokens longer than 16 hex digits, or tokens containing
    non-hex characters. *)
val from_token : string -> t
[@@zero_alloc ignore]

(* {1 Comparison }*)

(** [compare] is unsigned 64-bit comparison on the raw id, which matches Hilbert order
    within each level. *)
val compare : t -> t -> int

(** Structural equality on the raw id. *)
val equal : t -> t -> bool
