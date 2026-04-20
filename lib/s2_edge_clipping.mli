(** Robust clipping of geodesic edges to the faces of the S2 biunit cube, and of 2D edges
    against 2D rectangles.

    These functions can be used to efficiently find the set of [S2_cell_id]s that are
    intersected by a geodesic edge. *)

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

(** A single geodesic edge AB clipped to one S2 cube face.

    Stored as boxed so segments can be returned in a list; the [a]/[b] points themselves
    use the unboxed [R2_point.t] layout. *)
type face_segment =
  { face : int
  ; a : R2_point.t
  ; b : R2_point.t
  }
[@@deriving sexp_of]

val sexp_of_face_segment : face_segment -> Sexp.t [@@zero_alloc ignore]

(** [get_face_segments a b] subdivides the edge [a]->[b] at every point where it crosses
    the boundary between two S2 cube faces and returns the corresponding face segments in
    order from [a] toward [b]. The input points must be unit length. All returned vertices
    lie within the [[-1,1]x[-1,1]] cube face rectangles. *)
val get_face_segments : S2_point.t -> S2_point.t -> face_segment list
[@@zero_alloc ignore]

(** A clipped edge in (u, v) coordinates on some cube face. *)
type clipped_uv =
  { a : R2_point.t
  ; b : R2_point.t
  }
[@@deriving sexp_of]

val sexp_of_clipped_uv : clipped_uv -> Sexp.t [@@zero_alloc ignore]

(** [clip_to_face a b face] returns [Some { a; b }] giving the (u, v) coordinates of the
    portion of [a]->[b] that intersects the given cube face, or [None] if the edge does
    not intersect the face. The test for face intersection is exact. *)
val clip_to_face : S2_point.t -> S2_point.t -> int -> clipped_uv option
[@@zero_alloc ignore]

(** [clip_to_padded_face a b face ~padding] is like {!clip_to_face} but clips against the
    [[-R,R]x[-R,R]] rectangle where [R = 1 + padding]. [padding] must be non-negative. *)
val clip_to_padded_face
  :  S2_point.t
  -> S2_point.t
  -> int
  -> padding:float#
  -> clipped_uv option
[@@zero_alloc ignore]

(** {1 Rectangle clipping} *)

(** [intersects_rect a b rect] returns true if the 2D edge [a]->[b] intersects the closed
    [rect] to within the error bound [intersects_rect_error_uv_dist]. *)
val intersects_rect : R2_point.t -> R2_point.t -> R2_rect.t -> bool

(** [clip_edge a b clip] returns [Some { a; b }] giving the portion of [a]->[b] contained
    in [clip], or [None] if there is no intersection. *)
val clip_edge : R2_point.t -> R2_point.t -> R2_rect.t -> clipped_uv option
[@@zero_alloc ignore]

(** [get_clipped_edge_bound a b clip] returns the bounding rectangle of the portion of
    [a]->[b] intersected by [clip]. The result may be empty. *)
val get_clipped_edge_bound : R2_point.t -> R2_point.t -> R2_rect.t -> R2_rect.t

(** [clip_edge_bound a b clip bound] returns the tight bounding rectangle for the portion
    of [a]->[b] inside [clip], starting from [bound] which must itself be a tight bounding
    rectangle for some subsegment of [a]->[b]. Returns [R2_rect.Option.none] if the
    subsegment does not intersect [clip]. *)
val clip_edge_bound
  :  R2_point.t
  -> R2_point.t
  -> R2_rect.t
  -> R2_rect.t
  -> R2_rect.Option.t

(** {1 Linear interpolation}

    [interpolate_double x a b a1 b1] returns the value [x1] that is the same linear
    combination of [a1] and [b1] as [x] is of [a] and [b]. Guarantees:

    - If [x = a] then [x1 = a1] exactly.
    - If [x = b] then [x1 = b1] exactly.
    - If [x] lies between [a] and [b], [x1] lies between [a1] and [b1] (even if
      [a1 = b1]).

    When [a = b], [a1] is returned. *)
val interpolate_double : float# -> float# -> float# -> float# -> float# -> float#
