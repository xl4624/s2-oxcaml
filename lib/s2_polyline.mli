(** [S2_polyline] represents a sequence of zero or more vertices connected by straight
    edges (geodesics). Edges of length 0 and 180 degrees are not allowed: adjacent
    vertices should not be identical or antipodal. Use {!S2_lax_polyline} for the more
    permissive variant.

    The type is structurally the array of vertices; constructors take a copy. *)

open Core

[@@@zero_alloc all]

type t [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [of_vertices vs] copies [vs] and returns the resulting polyline. The vertices are
    expected to satisfy {!is_valid}; pass [validate:false] to skip the check. *)
val of_vertices : ?validate:bool -> S2_point.t array -> t
[@@zero_alloc ignore]

(** {1 Accessors} *)

(** [num_vertices t] is the number of stored vertices. *)
val num_vertices : t -> int

(** [vertex t i] is the [i]-th vertex. Requires [0 <= i < num_vertices t]. *)
val vertex : t -> int -> S2_point.t

(** [vertices t] returns a copy of the vertex array. *)
val vertices : t -> S2_point.t array
[@@zero_alloc ignore]

(** {1 Validation} *)

(** [is_valid t] is true when every vertex is unit length and no two adjacent vertices are
    identical or antipodal. *)
val is_valid : t -> bool

(** {1 Geometry} *)

(** [length t] is the total geodesic length. Returns {!S1_angle.zero} for polylines with
    fewer than two vertices. *)
val length : t -> S1_angle.t

(** [centroid t] returns the true centroid of the polyline scaled by its length. The
    result is not unit length. Returns the zero vector for empty / single-vertex
    polylines. *)
val centroid : t -> S2_point.t

(** Result of {!get_suffix}: the interpolated point and the index of the next polyline
    vertex after it. *)
type suffix =
  #{ point : S2_point.t
   ; next_vertex : int
   }

(** [get_suffix t fraction] returns the point whose distance from vertex 0 along the
    polyline is the given fraction of the polyline's total length. Fractions outside
    [[0, 1]] are clamped. The returned [next_vertex] is in [[1, num_vertices t]] and is
    such that the suffix of the polyline after the interpolated point is given by [point]
    followed by [vertex t next_vertex, ..., vertex t (num_vertices t - 1)]. The polyline
    must be non-empty. *)
val get_suffix : t -> float# -> suffix

(** [interpolate t fraction] is [(get_suffix t fraction).#point]. *)
val interpolate : t -> float# -> S2_point.t

(** [un_interpolate t point next_vertex] is the inverse of {!get_suffix}: given a point on
    the polyline and the [next_vertex] index produced by {!get_suffix}, returns the
    fraction in [[0, 1]] of the polyline length up to that point. Returns [0] if the
    polyline has fewer than two vertices. The polyline must be non-empty. *)
val un_interpolate : t -> S2_point.t -> int -> float#

(** Result of {!project}: the closest point on the polyline and the index of the next
    polyline vertex after it. *)
type projection =
  #{ point : S2_point.t
   ; next_vertex : int
   }

(** [project t point] returns the point on the polyline closest to [point], together with
    the index of the next vertex after the projection (always in [[1, num_vertices t]]).
    The polyline must be non-empty. *)
val project : t -> S2_point.t -> projection

(** [is_on_right t point] returns true when [point] is on the right-hand side of the
    polyline. Uses a naive definition based on which polyline edge the point is closest
    to. The polyline must have at least two vertices. *)
val is_on_right : t -> S2_point.t -> bool

(** {1 Operations} *)

(** [reverse t] returns a polyline with the vertices in reverse order. *)
val reverse : t -> t
[@@zero_alloc ignore]

(** [intersects a b] returns true if the two polylines intersect. Sharing a vertex counts
    as intersecting; when an endpoint is the only intersection the result is unspecified.
    Quadratic in [num_vertices a * num_vertices b]. *)
val intersects : t -> t -> bool

(** [equal a b] is true when both polylines have the same vertex sequence. *)
val equal : t -> t -> bool

(** [approx_equal ~max_error a b] is true when both polylines have the same number of
    vertices and corresponding pairs are within [max_error] radians of each other. Pass
    {!Packed_float_option.Unboxed.none} to use the default tolerance of [1e-15] radians. *)
val approx_equal : max_error:Packed_float_option.Unboxed.t -> t -> t -> bool

(** [subsample_vertices t tolerance] returns a subsequence of vertex indices such that the
    polyline connecting them stays within [tolerance] of the original. Provided the first
    and last vertices are distinct, they are always preserved; if they are not, the result
    may contain only a single index. Negative tolerances behave like zero (every vertex is
    preserved up to floating-point coincidence). *)
val subsample_vertices : t -> S1_angle.t -> int array
[@@zero_alloc ignore]

(** {1 Region interface} *)

(** [cap_bound t] returns a bounding cap for the polyline. *)
val cap_bound : t -> S2_cap.t

(** [rect_bound t] returns a bounding latitude-longitude rectangle. *)
val rect_bound : t -> S2_latlng_rect.t

(** [cell_union_bound t] returns a small set of cell ids whose union covers [t]. *)
val cell_union_bound : t -> S2_cell_id.t array
[@@zero_alloc ignore]

(** [contains_cell t _] always returns false: containment is not numerically well-defined
    except at vertices. *)
val contains_cell : t -> S2_cell.t -> bool

(** [may_intersect_cell t cell] returns true if the polyline intersects [cell]. *)
val may_intersect_cell : t -> S2_cell.t -> bool

(** [contains_point _ _] always returns false: polylines are not closed. *)
val contains_point : t -> S2_point.t -> bool

(** [to_region t] exposes [t] through the generic region interface. *)
val to_region : t -> S2_region.t
[@@zero_alloc ignore]

(** {1 Shape interface} *)

(** [num_edges t] is [max 0 (num_vertices t - 1)]. *)
val num_edges : t -> int

(** [edge t e] returns the [e]-th edge. Requires [0 <= e < num_edges t]. *)
val edge : t -> int -> S2_shape.Edge.t

(** [dimension t] is [1]. *)
val dimension : t -> int

(** [num_chains t] is [1] when the polyline has any edges, [0] otherwise. *)
val num_chains : t -> int

(** [chain t i] returns the single chain spanning every edge. Requires [i = 0]. *)
val chain : t -> int -> S2_shape.Chain.t

(** [chain_edge t i j] is [edge t j]. Requires [i = 0]. *)
val chain_edge : t -> int -> int -> S2_shape.Edge.t

(** [chain_position t e] is [{ chain_id = 0; offset = e }]. *)
val chain_position : t -> int -> S2_shape.Chain_position.t

(** [reference_point t] always returns a non-contained reference point. *)
val reference_point : t -> S2_shape.Reference_point.t

(** [type_tag] is the encoded shape tag for polylines ([2]). *)
val type_tag : S2_shape.Type_tag.t

(** [to_shape t] exposes [t] through the generic shape interface. *)
val to_shape : t -> S2_shape.t
[@@zero_alloc ignore]
