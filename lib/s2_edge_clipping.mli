(** Robust clipping of geodesic edges against the six faces of the S2 biunit cube and of
    planar edges against axis-aligned rectangles.

    S2 projects the unit sphere onto the six faces of the cube [[-1,1]^3]. Each face
    carries its own (u, v) coordinate system. Given a geodesic edge on the sphere, this
    module breaks it into the per-face (u, v) pieces that a cell-indexing algorithm can
    work with, and provides the companion primitives for intersecting 2D edges with 2D
    rectangles.

    Typical uses:

    - {!get_face_segments} walks a spherical edge across cube faces and returns the (u, v)
      pieces in order. This drives algorithms that need to enumerate the S2 cells an edge
      passes through.
    - {!clip_to_face} and {!clip_to_padded_face} answer the narrower question "does this
      edge touch face [F], and if so where?". The face-intersection test is exact.
    - {!clip_edge}, {!clip_edge_bound}, {!get_clipped_edge_bound} and {!intersects_rect}
      perform the planar clipping of a line segment against a rectangle, either returning
      the clipped endpoints or a tight bounding box of the intersection.
    - {!interpolate_double} is the numerically careful linear interpolation used
      internally when computing the above.

    All functions in this module are pure and thread-safe.

    {2 Error bounds}

    Every operation returns coordinates within a small, documented distance of the exact
    result. The bounds are exposed as the [*_error_*] constants below so callers can size
    their own padding. *)

open Core

[@@@zero_alloc all]

(** {1 Error bounds} *)

(** Maximum angle (in radians) between a returned vertex and the nearest point on the
    exact edge AB for {!get_face_segments} and {!clip_to_face}. *)
val face_clip_error_radians : float#

(** Same error as [face_clip_error_radians] expressed as a maximum UV distance. *)
val face_clip_error_uv_dist : float#

(** Same error expressed as the maximum error in a single u- or v-coordinate. *)
val face_clip_error_uv_coord : float#

(** Padding applied on each side of a cell when building a shape index. Matches the
    doubled sum of {!face_clip_error_uv_coord} and {!edge_clip_error_uv_coord}. *)
val shape_index_cell_padding : float#

(** Maximum error in {!intersects_rect}. If some point of AB is inside the rectangle by at
    least this distance, the result is guaranteed to be true; if all points of AB are
    outside the rectangle by at least this distance, the result is guaranteed to be false. *)
val intersects_rect_error_uv_dist : float#

(** Maximum error in a u- or v-coordinate produced by {!clip_edge} and {!clip_edge_bound}. *)
val edge_clip_error_uv_coord : float#

(** Maximum distance from a clipped point to the corresponding exact result. *)
val edge_clip_error_uv_dist : float#

(** {1 Face clipping} *)

(** One piece of a geodesic edge AB after clipping to a single S2 cube face, carrying the
    face index and the two (u, v) endpoints on that face. *)
type face_segment =
  { face : int
  ; a : R2_point.t
  ; b : R2_point.t
  }
[@@deriving sexp_of]

val sexp_of_face_segment : face_segment -> Sexp.t [@@zero_alloc ignore]

(** [get_face_segments a b] subdivides the spherical edge from [a] to [b] at every point
    where it crosses a cube-face boundary and returns the resulting per-face pieces in
    order from [a] toward [b]. The input points must be unit length. The returned segments
    form a continuous path from [a] to [b], every vertex is within
    {!face_clip_error_uv_dist} of the exact line AB, and all vertices lie within the
    [[-1,1]x[-1,1]] face rectangles. The result is consistent with the orientation
    predicate used elsewhere in S2 even when [a] and [b] are antipodal. *)
val get_face_segments : S2_point.t -> S2_point.t -> face_segment list
[@@zero_alloc ignore]

(** A clipped edge in (u, v) coordinates on some cube face. *)
type clipped_uv =
  { a : R2_point.t
  ; b : R2_point.t
  }
[@@deriving sexp_of]

val sexp_of_clipped_uv : clipped_uv -> Sexp.t [@@zero_alloc ignore]

(** [clip_to_face a b face] returns [Some { a; b }] giving the (u, v) coordinates on
    [face] of the portion of the spherical edge [a]->[b] that hits that face, or [None] if
    the edge misses the face entirely. The face-intersection test is exact: a result of
    [None] definitively means no intersection. When [Some] is returned, the clipped
    vertices lie within [[-1,1]x[-1,1]] and within {!face_clip_error_uv_dist} of the exact
    line AB, but may differ slightly from the pieces produced by {!get_face_segments}. *)
val clip_to_face : S2_point.t -> S2_point.t -> int -> clipped_uv option
[@@zero_alloc ignore]

(** [clip_to_padded_face a b face ~padding] is like {!clip_to_face} but clips against the
    enlarged square [[-R,R]x[-R,R]] where [R = 1 + padding]. [padding] must be
    non-negative. Used by shape indexing to pad cell boundaries. *)
val clip_to_padded_face
  :  S2_point.t
  -> S2_point.t
  -> int
  -> padding:float#
  -> clipped_uv option
[@@zero_alloc ignore]

(** {1 Rectangle clipping} *)

(** [intersects_rect a b rect] tests whether the planar line segment [a]->[b] meets the
    closed rectangle [rect]. If a point of AB is inside [rect] by at least
    {!intersects_rect_error_uv_dist}, the result is [true]; if every point of AB is
    outside by at least that distance, the result is [false]. The error bound assumes
    [rect] is within or only slightly outside [[-1,1]x[-1,1]]. *)
val intersects_rect : R2_point.t -> R2_point.t -> R2_rect.t -> bool

(** [clip_edge a b clip] returns [Some { a; b }] giving the two endpoints of the portion
    of the segment [a]->[b] that lies inside [clip], or [None] if the segment does not
    meet the rectangle. Coordinates are accurate to {!edge_clip_error_uv_coord}. *)
val clip_edge : R2_point.t -> R2_point.t -> R2_rect.t -> clipped_uv option
[@@zero_alloc ignore]

(** [get_clipped_edge_bound a b clip] returns a tight axis-aligned bound of the portion of
    [a]->[b] that lies inside [clip]. The result is empty when [a]->[b] misses [clip]. A
    convenience wrapper around {!clip_edge_bound}. *)
val get_clipped_edge_bound : R2_point.t -> R2_point.t -> R2_rect.t -> R2_rect.t

(** [clip_edge_bound a b clip bound] is the incremental form of {!get_clipped_edge_bound}:
    given [bound], a tight bound of some sub-segment A'B' of [a]->[b], it returns a tight
    bound of A'B' intersected with [clip], or [R2_rect.Option.none] if A'B' misses [clip].
    Feeding the result back in lets callers clip one edge against many rectangles in
    sequence without ever materializing intermediate endpoints. *)
val clip_edge_bound
  :  R2_point.t
  -> R2_point.t
  -> R2_rect.t
  -> R2_rect.t
  -> R2_rect.Option.t

(** {1 Linear interpolation} *)

(** [interpolate_double x a b a1 b1] returns the value that bears the same affine
    relationship to [a1] and [b1] as [x] does to [a] and [b]. Guarantees:

    - [x = a] gives exactly [a1].
    - [x = b] gives exactly [b1].
    - If [x] lies between [a] and [b], the result lies between [a1] and [b1] (even when
      [a1 = b1]).
    - When [a = b], [a1] is returned.

    For [x] in the interval [[a, b]] the absolute error is at most [2.25 * epsilon_float];
    extrapolating outside that interval may be substantially less accurate. *)
val interpolate_double : float# -> float# -> float# -> float# -> float# -> float#
