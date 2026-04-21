(** [S2_closest_edge_query] finds the edge(s) of geometry stored in an {!S2_shape_index.t}
    that are closest to a given target (a point, an edge, a cell, or another shape index).

    By default all edges are returned; callers should always constrain the search by
    setting {!Options.max_results}, {!Options.max_distance}, or both. Distances are
    measured to the boundary {b and interior} of polygons by default.

    The implementation mirrors C++'s
    {{:https://github.com/google/s2geometry} S2ClosestEdgeQuery}. Both brute-force and
    optimized (priority-queue) algorithms are supported; the latter is used once the index
    exceeds a target-specific edge threshold. *)

open Core

(** Target geometry to which distances are measured. *)
module Target : sig
  type t

  (** [point p] targets a single point. *)
  val point : S2_point.t -> t

  (** [edge a b] targets the edge [a]->[b]. *)
  val edge : S2_point.t -> S2_point.t -> t

  (** [cell c] targets a full cell (including its interior). *)
  val cell : S2_cell.t -> t

  (** [shape_index ?include_interiors ?use_brute_force index] targets the geometry stored
      in [index]. [include_interiors] controls whether polygon interiors are considered
      for the target side (default [true]). The target uses its own internal
      {!S2_closest_edge_query.t}, so [use_brute_force] configures that sub-query.

      When the target index is modified, callers should construct a new target. *)
  val shape_index
    :  ?include_interiors:bool
    -> ?use_brute_force:bool
    -> S2_shape_index.t
    -> t
end

(** Query options. *)
module Options : sig
  type t

  (** Returns a default options record. Defaults: [max_results = max_int],
      [max_distance = infinity], [max_error = 0], [include_interiors = true],
      [use_brute_force = false]. Chain {!with_max_distance}, {!with_max_error}, etc. to
      customize. *)
  val create : ?include_interiors:bool -> ?use_brute_force:bool -> unit -> t

  val max_results : t -> int
  val max_distance : t -> S1_chord_angle.t
  val max_error : t -> S1_chord_angle.t
  val include_interiors : t -> bool
  val use_brute_force : t -> bool

  (** [with_max_results t n] returns [t] with [max_results] set to [n]. [n >= 1]. *)
  val with_max_results : t -> int -> t

  (** [with_max_distance t d] returns [t] with [max_distance] set to [d]. Edges whose
      distance is exactly equal to [d] are not returned. *)
  val with_max_distance : t -> S1_chord_angle.t -> t

  (** Like {!with_max_distance} but also returns edges at exactly [limit]. *)
  val with_inclusive_max_distance : t -> S1_chord_angle.t -> t

  (** Like {!with_inclusive_max_distance} but further expands [limit] by the maximum error
      in distance calculations, so every edge whose {b true} distance is at most [limit]
      is returned (along with some whose true distance is slightly greater). *)
  val with_conservative_max_distance : t -> S1_chord_angle.t -> t

  val with_max_error : t -> S1_chord_angle.t -> t
  val with_include_interiors : t -> bool -> t
  val with_use_brute_force : t -> bool -> t
end

(** A single result of a closest-edge query. *)
module Result : sig
  type t =
    { distance : S1_chord_angle.t
    ; shape_id : int
    ; edge_id : int
    }
  [@@deriving sexp_of]

  (** [is_interior t] is [true] when the result represents the interior of a polygon, in
      which case [edge_id = -1] and [distance = S1_chord_angle.zero]. Only possible when
      {!Options.include_interiors} is set. *)
  val is_interior : t -> bool

  (** [is_empty t] is [true] when no edge satisfied the query (returned by
      {!find_closest_edge} only). *)
  val is_empty : t -> bool

  (** An empty result: [distance = Infinity], [shape_id = edge_id = -1]. *)
  val empty : t
end

type t

(** [create index ?options ()] returns a reusable query object. The [index] must be built
    (or will be built lazily by the query). *)
val create : ?options:Options.t -> S2_shape_index.t -> unit -> t

val index : t -> S2_shape_index.t
val options : t -> Options.t
val set_options : t -> Options.t -> unit

(** [find_closest_edges t target] returns the closest edges to [target] that satisfy the
    current options. Results are sorted by distance, then by [(shape_id, edge_id)]. If
    [Options.include_interiors] is [true], the result may contain entries with
    [edge_id = -1] identifying polygon interiors. *)
val find_closest_edges : t -> Target.t -> Result.t list

(** [find_closest_edge t target] is a convenience returning exactly one result (or
    [Result.empty] if none match). *)
val find_closest_edge : t -> Target.t -> Result.t

(** [get_distance t target] returns the minimum distance to [target]. Returns
    [S1_chord_angle.infinity] if the index or target is empty. *)
val get_distance : t -> Target.t -> S1_chord_angle.t

(** [is_distance_less t target limit] reports whether the distance to [target] is strictly
    less than [limit]. Faster than {!get_distance} when the answer is all that matters. *)
val is_distance_less : t -> Target.t -> S1_chord_angle.t -> bool

(** [is_distance_less_or_equal t target limit] is like {!is_distance_less} but also
    returns [true] at exact equality. *)
val is_distance_less_or_equal : t -> Target.t -> S1_chord_angle.t -> bool

(** [is_conservative_distance_less_or_equal t target limit] is like
    {!is_distance_less_or_equal} but the limit is expanded by the maximum error in the
    distance computation; it returns [true] whenever the {b true} minimum distance is less
    than or equal to [limit]. *)
val is_conservative_distance_less_or_equal : t -> Target.t -> S1_chord_angle.t -> bool

(** [get_edge t result] returns the underlying edge. Raises if [Result.is_interior]. *)
val get_edge : t -> Result.t -> S2_shape.Edge.t

(** [project t p result] returns the point on the result edge closest to [p]. If the
    result represents a polygon interior, [p] itself is returned. *)
val project : t -> S2_point.t -> Result.t -> S2_point.t
