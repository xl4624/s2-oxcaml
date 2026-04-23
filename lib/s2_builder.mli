(** Assembles polygonal geometry from a soup of edges by snap-rounding them onto a
    discrete set of "sites" and then handing the result to one or more output layers.

    Use cases include:
    - building {!S2_polygon.t}, polylines, and polygon meshes from unsorted edges;
    - snapping geometry to a coarser representation (e.g. to S2 cell centers) while
      preserving topology;
    - simplifying geometry for indexing or display;
    - implementing higher-level operations like {!S2_boolean_operation}.

    Workflow:
    + {!create} a builder from an {!Options.t}.
    + {!start_layer} with a {!Layer.t} that will consume the snapped graph (for example
      {!S2_polygon_layer}). You may start multiple layers back-to-back; each layer sees
      only the edges added after its [start_layer] call.
    + Feed edges with {!add_point}, {!add_edge}, {!add_polyline}, {!add_loop},
      {!add_shape}, and {!add_intersection}.
    + {!build} to run snap rounding and drive every layer's build hook. The builder is
      reset afterwards and may be reused.

    {2 Snap-rounding guarantees}

    Given a snap radius [r] (from the chosen {!Snap_function.t}), the builder guarantees
    that the snapped output satisfies all of:
    - every output vertex is a location returned by [snap_point] applied to some site;
    - no vertex has moved by more than [r];
    - no point along an edge moves by more than {!Options.max_edge_deviation} (slightly
      greater than the edge snap radius);
    - output vertices are separated by at least [min_vertex_separation];
    - edges and non-incident vertices are separated by at least
      [min_edge_vertex_separation];
    - input topology is preserved: there is a continuous deformation from input to output
      with no vertex crossing an edge, so loops keep their orientation and no new
      self-intersections are created.

    With {!Options.idempotent} ([true] by default) snapping is a no-op when the input
    already satisfies the guarantees. If snapping is not required the default options
    preserve input vertices bit-exactly.

    {2 Output layers and the polygon model}

    A {!Layer.t} is responsible for turning the snapped {!Graph.t} into its target type.
    Because the graph exposes only edges, it cannot distinguish between the empty polygon
    and the full polygon when the output collapses to degeneracies. Callers of polygon
    layers should use {!add_is_full_polygon_predicate} to disambiguate; helpers
    {!is_full_polygon} and {!is_full_polygon_unspecified} cover the common cases.

    {1 Limitations}

    The following features are not yet implemented. Attempting to use them raises:
    - Undirected input edges.
    - Snap functions other than {!Snap_function.identity} / {!identity_with_radius}
      ({!Snap_function.int_lat_lng} and {!Snap_function.s2_cell_id} are placeholders).
    - Forced vertices, per-edge labels, and edge-chain simplification.
    - Layer types other than {!S2_polygon_layer}.
    - Memory tracking and [Sibling_pairs.Require] / [Sibling_pairs.Create].
    - Bit-exact parity with the reference Voronoi-based site selection: for non-zero snap
      radii the port uses a simpler O(n{^ 2}) cluster-merge that preserves topology for
      well-separated inputs but can differ on near-degenerate configurations. *)

open Core

(** Error value reported by the builder or one of its layers. A status code of ["OK"]
    (with an empty [message]) marks success; any other code indicates a failure and stops
    the pipeline. *)
module Error : sig
  type t =
    { code : string
    ; message : string
    }
  [@@deriving sexp_of]

  (** [ok] is the success sentinel: [code = "OK"], empty [message]. *)
  val ok : t

  (** [is_ok t] returns [true] iff [t.code] equals ["OK"]. *)
  val is_ok : t -> bool

  (** [create ~code ~message] wraps the pair into an error record. *)
  val create : code:string -> message:string -> t
end

(** Directed vs undirected input edges. Only [Directed] is implemented; [Undirected]
    raises. *)
module Edge_type : sig
  type t =
    | Directed
    | Undirected
  [@@deriving sexp_of, equal]
end

(** How to treat degenerate edges (edges of the form [(v, v)]). Such edges may come from
    input (e.g. {!add_point}) or may appear after snapping collapses both endpoints. *)
module Degenerate_edges : sig
  type t =
    | Discard (** Drop every degenerate edge. Required by polygon layers. *)
    | Discard_excess
    (** Drop degenerate edges that are incident to a non-degenerate edge; keep (and merge
        duplicates of) the rest. Useful when simplifying polygons so collapsed loops
        survive as a single point. *)
    | Keep (** Keep every degenerate edge. *)
  [@@deriving sexp_of, equal]
end

(** How to treat duplicate edges (multiple edges with the same endpoints). *)
module Duplicate_edges : sig
  type t =
    | Merge (** Collapse duplicates into a single edge with the union of all labels. *)
    | Keep (** Keep each copy, preserving multiplicity. *)
  [@@deriving sexp_of, equal]
end

(** How to treat sibling pairs: an edge [(u, v)] paired with its reverse [(v, u)]. *)
module Sibling_pairs : sig
  type t =
    | Discard (** Drop both edges of every sibling pair. *)
    | Discard_excess
    (** Discard sibling pairs except for one kept when the result would otherwise become
        empty. Useful when simplifying polylines that might collapse to a single
        back-and-forth segment. *)
    | Keep (** Keep sibling pairs. *)
    | Require
    (** Require every edge to have a sibling; return an error if not. Not implemented. *)
    | Create
    (** Ensure every edge has a sibling by synthesising the reverse edge when missing. Not
        implemented. *)
  [@@deriving sexp_of, equal]
end

(** Options a {!Layer.t} supplies to control how the {!Graph.t} it receives is shaped
    (e.g. whether degenerate edges are dropped, whether duplicates are merged, etc.). *)
module Graph_options : sig
  type t =
    { edge_type : Edge_type.t
    ; degenerate_edges : Degenerate_edges.t
    ; duplicate_edges : Duplicate_edges.t
    ; sibling_pairs : Sibling_pairs.t
    ; allow_vertex_filtering : bool
    (** When [true], each per-layer graph is restricted to the vertices actually
        referenced by that layer's edges. Set to [false] if a layer needs to see every
        site. *)
    }
  [@@deriving sexp_of]

  (** The default options keep every edge ([Directed], [Keep], [Keep], [Keep]), producing
      the least surprising output and making it easy to diagnose missing options. *)
  val default : t

  (** [create ~edge_type ~degenerate_edges ~duplicate_edges ~sibling_pairs] builds graph
      options. [allow_vertex_filtering] defaults to [true]. *)
  val create
    :  edge_type:Edge_type.t
    -> degenerate_edges:Degenerate_edges.t
    -> duplicate_edges:Duplicate_edges.t
    -> sibling_pairs:Sibling_pairs.t
    -> t

  val equal : t -> t -> bool
end

(** Sentinel value larger than any valid input edge id. *)
val k_max_input_edge_id : int

(** Sentinel for graph edges created without a corresponding input edge (e.g., siblings
    generated by [Sibling_pairs.Create]). *)
val k_no_input_edge_id : int

(** Interns sorted integer sets (used for sets of input-edge ids or labels) under compact
    integer handles, so many graph edges sharing the same set reuse one array. *)
module Id_set_lexicon : sig
  type t

  (** [create ()] constructs an empty lexicon; the empty set is pre-registered with id
      {!empty_set_id}. *)
  val create : unit -> t

  (** [empty_set_id] is the canonical id of the empty set. It is always [0]. *)
  val empty_set_id : int

  (** [add t ids] inserts [ids] (deduplicated and sorted) into [t] and returns its lexicon
      id. Repeated calls with the same content return the same id. *)
  val add : t -> int array -> int

  (** [add_singleton t i] is equivalent to [add t [|i|]]. *)
  val add_singleton : t -> int -> int

  (** [id_set t id] returns the sorted id set associated with [id]. The returned array
      aliases internal storage and must not be mutated. *)
  val id_set : t -> int -> int array

  (** [clear t] removes every set except the pre-registered empty one. *)
  val clear : t -> unit

  (** [copy t] returns an independent deep copy of [t]. *)
  val copy : t -> t
end

(** The snap-rounded edge graph handed to a {!Layer.t} during {!build}. Vertices are
    numbered sequentially from zero; edges are directed [(src, dst)] pairs sorted
    lexicographically, so all edges outgoing from a given vertex form a contiguous range
    in the edge array. *)
module Graph : sig
  (** A graph edge: a pair of vertex ids. Boxed so that edges can live in ordinary
      [array]s; access the endpoints through {!src} and {!dst}. *)
  module Edge : sig
    type t =
      { src : int
      ; dst : int
      }

    val create : src:int -> dst:int -> t
    val src : t -> int
    val dst : t -> int

    (** [reverse e] returns the edge with [src] and [dst] swapped. *)
    val reverse : t -> t

    val equal : t -> t -> bool

    (** Lexicographic comparison on [(src, dst)]. *)
    val compare : t -> t -> int
  end

  type t =
    { options : Graph_options.t
    ; vertices : S2_point.t array
    ; edges : Edge.t array
    ; input_edge_id_set_ids : int array
    (** One lexicon id per graph edge, identifying the set of input-edge ids that
        collapsed onto that graph edge. *)
    ; input_edge_id_set_lexicon : Id_set_lexicon.t
    ; is_full_polygon_predicate : t -> bool * Error.t
    }

  val options : t -> Graph_options.t
  val num_vertices : t -> int
  val vertex : t -> int -> S2_point.t
  val num_edges : t -> int
  val edge : t -> int -> Edge.t

  (** [get_in_edge_ids g] returns the edge ids sorted lexicographically by [(dst, src)],
      i.e. grouped by destination vertex. *)
  val get_in_edge_ids : t -> int array

  (** [get_sibling_map g] returns, for each edge id, the id of its sibling (reversed)
      edge. Requires that every edge has a sibling - valid only when [g] was built with
      [Sibling_pairs.Require] or [Sibling_pairs.Create]. *)
  val get_sibling_map : t -> int array

  (** [input_edge_ids g e] is the set of input-edge ids that were snapped to graph edge
      [e]. The result aliases lexicon storage and must not be mutated. *)
  val input_edge_ids : t -> int -> int array

  (** [min_input_edge_id g e] is the smallest input-edge id in [input_edge_ids g e], or
      {!k_no_input_edge_id} when the set is empty. *)
  val min_input_edge_id : t -> int -> int

  (** [is_full_polygon g] evaluates the graph's full-polygon predicate. The result
      [(is_full, err)] says whether [g] (assumed to consist only of degenerate edges
      and/or sibling pairs) should be interpreted as the full polygon. [err] is set if the
      predicate could not decide. *)
  val is_full_polygon : t -> bool * Error.t

  (** [process_edges ~options ~edges ~input_ids ~id_set_lexicon] applies [options] to the
      unsorted edge array [!edges], deduplicating, merging, and dropping edges as
      configured, and writes the canonicalised result back into the three refs. The
      [options] ref is mutated if undirected-to-directed conversion applies
      ([Sibling_pairs.Require] or [Sibling_pairs.Create] together with undirected edges). *)
  val process_edges
    :  options:Graph_options.t ref
    -> edges:Edge.t array ref
    -> input_ids:int array ref
    -> id_set_lexicon:Id_set_lexicon.t
    -> Error.t

  (** Controls how {!get_directed_loops} closes off cycles in the left-turn walk. *)
  module Loop_type : sig
    type t =
      | Simple (** Cut off a repeated-vertex cycle. *)
      | Circuit (** Cut off a repeated-edge cycle. Not implemented. *)
  end

  (** [get_directed_loops g loop_type] reconstructs loops from the directed edges of [g]
      by following left turns at each vertex. The result is an array of loops, each loop
      an array of edge ids in traversal order. Requires [g] to contain no degenerate or
      undirected edges. *)
  val get_directed_loops : t -> Loop_type.t -> int array array * Error.t
end

(** A snap function dictates where output vertices may live. It combines a function from
    arbitrary points to "candidate sites" with three angular tolerances. The builder uses
    these tolerances to pick a subset of candidate sites as the actual output vertices, so
    [snap_point p] is only an upper bound on where [p] will end up - final placement is
    also constrained by minimum separations. *)
module Snap_function : sig
  (** Maximum supported snap radius (approximately 7800 km on the Earth, or 70 degrees on
      the unit sphere). Values larger than this are rejected because parts of the
      algorithm assume an angle strictly less than 90 degrees. *)
  val k_max_snap_radius : unit -> S1_angle.t

  (** A snap function bundles four quantities:
      - [snap_radius]: the maximum distance a vertex may move when snapped.
      - [min_vertex_separation]: the guaranteed minimum distance between distinct output
        vertices.
      - [min_edge_vertex_separation]: the guaranteed minimum distance between an output
        edge and any non-incident output vertex.
      - [snap_point]: the site generator, called on each input point to produce a
        candidate output location. [snap_point p] must be within [snap_radius] of [p]. *)
  type t =
    { snap_radius : S1_angle.t
    ; min_vertex_separation : S1_angle.t
    ; min_edge_vertex_separation : S1_angle.t
    ; snap_point : S2_point.t -> S2_point.t
    }

  (** [identity ()] preserves every input point exactly. Equivalent to
      [identity_with_radius S1_angle.zero]. *)
  val identity : unit -> t

  (** [identity_with_radius r] uses the identity [snap_point] but allows output vertices
      within [r] of each other to be merged into a single site. *)
  val identity_with_radius : S1_angle.t -> t

  (** [int_lat_lng ~exponent] is a placeholder for the IntLatLng snap function that maps
      points to lat/lng values truncated at [10^-exponent] degrees. Not implemented: every
      call to [snap_point] raises. *)
  val int_lat_lng : exponent:int -> t

  (** [s2_cell_id ~level] is a placeholder for snapping to S2 cell centers at the given
      level. Not implemented: every call to [snap_point] raises. *)
  val s2_cell_id : level:int -> t
end

(** Top-level options that parameterise snapping. *)
module Options : sig
  type t =
    { snap_function : Snap_function.t
    ; edge_snap_radius : S1_angle.t
    (** [snap_function.snap_radius] plus [intersection_tolerance]: the maximum distance an
        edge interior may move. *)
    ; intersection_tolerance : S1_angle.t
    (** Maximum computed-intersection error. Must be positive to use {!add_intersection};
        automatically bumped to at least [S2_edge_crossings.intersection_error] when
        [split_crossing_edges] is [true]. *)
    ; split_crossing_edges : bool
    (** If [true], every pair of crossing input edges is detected and a vertex is inserted
        at the intersection. *)
    ; idempotent : bool
    (** If [true] (the default), snapping is skipped entirely when the input already meets
        the output guarantees. *)
    }

  (** [default ()] uses {!Snap_function.identity} (no snapping), zero intersection
      tolerance, and idempotent mode. *)
  val default : unit -> t

  (** [create ~intersection_tolerance ()] builds options. [intersection_tolerance] is
      mandatory because {!S1_angle.t} is unboxed and cannot be used as an optional-arg
      default; pass [S1_angle.zero] if you do not need intersections. *)
  val create
    :  ?snap_function:Snap_function.t
    -> ?split_crossing_edges:bool
    -> ?idempotent:bool
    -> intersection_tolerance:S1_angle.t
    -> unit
    -> t

  (** [max_edge_deviation t] is the maximum distance a point along an edge may move when
      snapped. Up to 10 percent larger than [edge_snap_radius] because snapping a geodesic
      moves its midpoint farther than its endpoints. *)
  val max_edge_deviation : t -> S1_angle.t
end

(** A layer transforms the snapped {!Graph.t} produced by {!build} into a native geometry
    value (polygon, polyline, mesh, ...). Concrete layers like {!S2_polygon_layer} close
    over an output reference and populate it in their [build] callback. *)
module Layer : sig
  type t =
    { graph_options : Graph_options.t
    (** Shape options the layer wants the builder to enforce before dispatch. *)
    ; build : Graph.t -> Error.t
    (** Callback invoked by {!build} with the layer's snapped subgraph. Returns
        {!Error.ok} on success or a populated error otherwise. *)
    ; name : string (** Human-readable label used in error messages. *)
    }
end

(** Predicate invoked by a polygon layer when the output has collapsed to a combination of
    degenerate edges and sibling pairs. [true] means the result should be interpreted as
    the full polygon, [false] as the empty polygon. The predicate may return a non-[ok]
    {!Error.t} to signal that it cannot decide (see {!is_full_polygon_unspecified}). *)
type is_full_polygon_predicate = Graph.t -> bool * Error.t

(** Mutable builder handle. All state (input edges, layer list, flags) is retained until
    {!build} is called, which resets it. Not thread-safe. *)
type t

(** [create options] constructs a fresh builder with the given {!Options.t}. *)
val create : Options.t -> t

(** [options t] returns the options [t] was created with. *)
val options : t -> Options.t

(** [start_layer t layer] opens a new output layer. Every subsequent [add_*] call is
    attributed to this layer, until another {!start_layer} is issued or {!build} runs.
    Multiple layers are built against the same set of snap sites, which is how S2Builder
    prevents simplification or snapping from introducing cross-layer intersections. *)
val start_layer : t -> Layer.t -> unit

(** [add_point t p] adds a degenerate edge [(p, p)] to the current layer. Raises if no
    layer is active. *)
val add_point : t -> S2_point.t -> unit

(** [add_edge t v0 v1] adds the directed edge [v0 -> v1] to the current layer. Raises if
    no layer is active. *)
val add_edge : t -> S2_point.t -> S2_point.t -> unit

(** [add_polyline t vertices] adds the edges
    [(vertices.(0), vertices.(1)); ...; (vertices.(n-2), vertices.(n-1))] to the current
    layer. Polylines with [0] or [1] vertex contribute no edges. *)
val add_polyline : t -> S2_point.t array -> unit

(** [add_loop t vertices] adds the closed loop through [vertices] to the current layer,
    including the final edge from [vertices.(n-1)] back to [vertices.(0)]. An empty
    [vertices] array is a no-op. *)
val add_loop : t -> S2_point.t array -> unit

(** [add_shape t shape] adds every edge of [shape] to the current layer, in the order
    [shape] returns them. *)
val add_shape : t -> S2_shape.t -> unit

(** [add_intersection t vertex] ensures that edges passing near [vertex] snap to a common
    output vertex. Useful for selectively splitting crossing edge pairs when
    {!Options.split_crossing_edges} is [false]. Raises if
    [options.intersection_tolerance <= 0]. *)
val add_intersection : t -> S2_point.t -> unit

(** [add_is_full_polygon_predicate t predicate] registers [predicate] as the full-polygon
    disambiguator for the current layer. At most one predicate is kept per layer; later
    calls overwrite earlier ones. The default, when no predicate is set, is
    {!is_full_polygon_unspecified}. *)
val add_is_full_polygon_predicate : t -> is_full_polygon_predicate -> unit

(** [is_full_polygon_unspecified] is the default full-polygon predicate: it reports an
    error indicating that the caller forgot to set one. Use this when you have no way to
    disambiguate and prefer to fail loudly. *)
val is_full_polygon_unspecified : is_full_polygon_predicate

(** [is_full_polygon constant] is the predicate that always returns [constant]. Use
    [is_full_polygon false] for inputs known to describe empty-or-bounded geometry, and
    [is_full_polygon true] for inputs that always cover the sphere. *)
val is_full_polygon : bool -> is_full_polygon_predicate

(** [build t] performs snap rounding on the accumulated input and invokes every layer's
    [build] callback with its subgraph. Returns the first non-[ok] error produced by the
    pipeline, or {!Error.ok} on success. The builder is reset on return - input edges and
    layers are cleared - so [t] can be reused. *)
val build : t -> Error.t
