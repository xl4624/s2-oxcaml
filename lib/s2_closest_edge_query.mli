(** Find the edge(s) of geometry stored in an {!S2_shape_index.t} that are closest to a
    given target.

    A target may be a single point, a single edge, an {!S2_cell.t}, or another shape index
    holding an arbitrary collection of points, polylines, and polygons. The query can
    return either the [k] nearest edges, all edges within a given distance, or both.

    By default {i all} edges are returned, which is rarely what callers want. Constrain
    the search by setting {!Options.max_results}, {!Options.max_distance}, or both.

    Polygon interiors count by default: if a target point lies inside an indexed polygon,
    its distance to that polygon is zero and the polygon's shape id is returned as a
    result with [edge_id = -1]. Disable this with
    [Options.with_include_interiors t false].

    For a simple yes/no distance threshold test, {!is_distance_less} and its variants are
    significantly faster than {!get_distance} because they can stop as soon as the answer
    is settled.

    The query caches a spatial covering of the index the first time it is needed, so
    reusing one query object across many calls is far cheaper than building fresh
    instances. The type is not thread-safe.

    Limitations:

    - Visitor-style enumeration APIs ([VisitClosestEdges], [VisitClosestShapes]) and the
      [ShapeFilter] parameter present in the C++ library are not exposed; results are
      always materialized as a list. *)

open Core

(** Target geometry to which distances are measured. *)
module Target : sig
  type t

  (** [point p] targets a single point. *)
  val point : S2_point.t -> t

  (** [edge a b] targets the edge [a]->[b]. *)
  val edge : S2_point.t -> S2_point.t -> t

  (** [cell c] targets a full S2 cell including its interior: the minimum distance is zero
      for any point inside the cell. *)
  val cell : S2_cell.t -> t

  (** [shape_index ?include_interiors ?use_brute_force index] targets the geometry stored
      in [index]. When [include_interiors] is [true] (the default), distances are measured
      to polygon interiors as well as boundaries; when [false], only boundaries. Setting
      [use_brute_force] forces the sub-query against this target to scan every edge
      linearly, bypassing the spatial index; it is mainly useful for testing.

      The target holds an internal cached sub-query. Construct a new target when the
      underlying index is modified. *)
  val shape_index
    :  ?include_interiors:bool
    -> ?use_brute_force:bool
    -> S2_shape_index.t
    -> t
end

(** Query options. Setters return a modified copy; the original is not mutated. *)
module Options : sig
  type t

  (** [create ?include_interiors ?use_brute_force ()] returns a default options record.
      Defaults: [max_results = Int.max_value], [max_distance = S1_chord_angle.infinity],
      [max_error = S1_chord_angle.zero], [include_interiors = true],
      [use_brute_force = false]. *)
  val create : ?include_interiors:bool -> ?use_brute_force:bool -> unit -> t

  val max_results : t -> int
  val max_distance : t -> S1_chord_angle.t
  val max_error : t -> S1_chord_angle.t
  val include_interiors : t -> bool
  val use_brute_force : t -> bool

  (** [with_max_results t n] returns [t] with [max_results] set to [n]. Raises if [n < 1]. *)
  val with_max_results : t -> int -> t

  (** [with_max_distance t d] returns [t] with [max_distance] set to [d]. Edges whose
      distance is exactly equal to [d] are {b not} returned. Use
      {!with_inclusive_max_distance} when equality should count as a hit. *)
  val with_max_distance : t -> S1_chord_angle.t -> t

  (** [with_inclusive_max_distance t limit] is like {!with_max_distance} but edges at
      exactly [limit] are also returned. Equivalent to calling
      [with_max_distance t (S1_chord_angle.successor limit)]. *)
  val with_inclusive_max_distance : t -> S1_chord_angle.t -> t

  (** [with_conservative_max_distance t limit] is like {!with_inclusive_max_distance} but
      [limit] is further expanded by the maximum error of the distance computation. The
      returned set then contains every edge whose {i true} distance is at most [limit],
      plus possibly some edges whose true distance is slightly greater. Intended as a safe
      over-approximation to feed into exact distance predicates. *)
  val with_conservative_max_distance : t -> S1_chord_angle.t -> t

  (** [with_max_error t e] sets the acceptable error in the distance computation. A
      non-zero value allows the algorithm to prune candidates earlier at the cost of
      looser answers; results may be any edge whose distance is within [e] of the minimum. *)
  val with_max_error : t -> S1_chord_angle.t -> t

  (** [with_include_interiors t b] toggles whether polygon interiors in the indexed
      geometry are considered. When [true], polygon interior hits are returned with
      [edge_id = -1]. *)
  val with_include_interiors : t -> bool -> t

  (** [with_use_brute_force t b] forces the search to scan every edge when [b] is [true],
      bypassing the spatial-index optimizations. Mainly useful for testing. *)
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
      which case [edge_id = -1] and [distance = S1_chord_angle.zero]. Can only occur when
      [Options.include_interiors] is [true]. *)
  val is_interior : t -> bool

  (** [is_empty t] is [true] when no edge satisfied the query. Only returned by
      {!find_closest_edge}; list-returning queries omit empty results. *)
  val is_empty : t -> bool

  (** The canonical empty result: [distance = S1_chord_angle.infinity],
      [shape_id = edge_id = -1]. *)
  val empty : t
end

type t

(** [create ?options index ()] returns a reusable query object bound to [index]. If
    [index] has not been built yet, it is built during construction. The options may be
    updated later with {!set_options}. *)
val create : ?options:Options.t -> S2_shape_index.t -> unit -> t

(** [index t] returns the index [t] was built against. *)
val index : t -> S2_shape_index.t

(** [options t] returns the current query options. *)
val options : t -> Options.t

(** [set_options t o] replaces the options in [t] with [o]. Use this to change
    [max_results] or [max_distance] between queries without rebuilding [t]. *)
val set_options : t -> Options.t -> unit

(** [find_closest_edges t target] returns every edge in the index that satisfies the
    current options. Results are sorted by [(distance, shape_id, edge_id)] and
    deduplicated. When [Options.include_interiors] is [true], entries with [edge_id = -1]
    identify polygon interiors that contain (or intersect) the target. *)
val find_closest_edges : t -> Target.t -> Result.t list

(** [find_closest_edge t target] returns a single nearest edge, ignoring the current
    [max_results] setting, or {!Result.empty} if no edge meets the distance bound. *)
val find_closest_edge : t -> Target.t -> Result.t

(** [get_distance t target] returns the minimum distance from [target] to any edge (or
    polygon interior, when enabled) in the index. Returns [S1_chord_angle.infinity] if the
    index is empty or nothing satisfies the current [max_distance]. *)
val get_distance : t -> Target.t -> S1_chord_angle.t

(** [is_distance_less t target limit] reports whether the distance from [target] to the
    nearest indexed edge is strictly less than [limit]. Typically much faster than
    {!get_distance} because it can stop as soon as one qualifying edge is found. *)
val is_distance_less : t -> Target.t -> S1_chord_angle.t -> bool

(** [is_distance_less_or_equal t target limit] is like {!is_distance_less} but also
    returns [true] when the distance equals [limit] exactly. *)
val is_distance_less_or_equal : t -> Target.t -> S1_chord_angle.t -> bool

(** [is_conservative_distance_less_or_equal t target limit] is like
    {!is_distance_less_or_equal} but [limit] is expanded by the maximum error in the
    distance computation. It returns [true] whenever the {i true} minimum distance is less
    than or equal to [limit], and may also return [true] for some distances slightly
    greater than [limit]. Use this when downstream code applies exact distance predicates. *)
val is_conservative_distance_less_or_equal : t -> Target.t -> S1_chord_angle.t -> bool

(** [get_edge t result] returns the endpoints of the edge the result refers to. Raises if
    [Result.is_interior result] is [true]. *)
val get_edge : t -> Result.t -> S2_shape.Edge.t

(** [project t p result] returns the point on the result edge closest to [p]. If the
    result represents a polygon interior, [p] itself is returned unchanged. *)
val project : t -> S2_point.t -> Result.t -> S2_point.t
