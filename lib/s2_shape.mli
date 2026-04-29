(** A generic view of polygonal geometry as a flat collection of edges that optionally
    defines an interior.

    Every shape has a fixed {e dimension}:
    - [0] = points (each represented by a degenerate edge [v == v]);
    - [1] = polylines (edges need not be connected, and may be degenerate);
    -

    [2] = polygons (edges are oriented with the interior on the left; a full polygon is
    represented by a chain of zero edges).

    All geometry inside one shape has the same dimension, so a mixed-dimension scene
    requires multiple shapes (typically stored in an {!S2_shape_index.t}).

    Edges are identified by a contiguous integer range [0 .. num_edges - 1] and are
    grouped into {e chains}: each chain is a contiguous slice of that range describing a
    single polyline or polygon loop. Callers can access edges either globally (by edge id)
    or within a chain (by [chain_id], [offset]).

    A shape is a flat record of counts and callbacks, so different backing representations
    (an {!S2_loop.t}, an {!S2_polyline.t}, raw arrays, etc.) can all produce the same
    [S2_shape.t]. Concrete shape modules expose their own [to_shape] converter. *)

open Core

[@@@zero_alloc all]

(** An edge is a pair of vertices [v0] and [v1]. Zero-length edges are allowed and can be
    used to represent points. *)
module Edge : sig
  type t =
    #{ v0 : S2_point.t
     ; v1 : S2_point.t
     }

  val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

  (** [create ~v0 ~v1] builds an edge from its two endpoints. *)
  val create : v0:S2_point.t -> v1:S2_point.t -> t

  (** [reversed e] returns [e] with its two vertices swapped. *)
  val reversed : t -> t

  (** [is_degenerate e] is true when the edge has equal endpoints. *)
  val is_degenerate : t -> bool

  (** [incoming e p] is true when [p] equals [v1] (the edge arrives at [p]). *)
  val incoming : t -> S2_point.t -> bool

  (** [outgoing e p] is true when [p] equals [v0] (the edge leaves [p]). *)
  val outgoing : t -> S2_point.t -> bool

  (** [incident_on e p] is true when [p] is either endpoint of [e]. *)
  val incident_on : t -> S2_point.t -> bool

  (** Lexicographic comparison by [v0] then [v1]. *)
  val compare : t -> t -> int

  val equal : t -> t -> bool
end

(** A contiguous range of edge ids [start, start + length - 1]. *)
module Chain : sig
  type t =
    #{ start : int
     ; length : int
     }

  val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
  val create : start:int -> length:int -> t
  val equal : t -> t -> bool
end

(** The position of an edge within a chain. *)
module Chain_position : sig
  type t =
    #{ chain_id : int
     ; offset : int
     }

  val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]
  val create : chain_id:int -> offset:int -> t
  val equal : t -> t -> bool
end

(** A reference point consists of a point and a boolean indicating whether the point is
    contained by the shape. *)
module Reference_point : sig
  type t =
    #{ point : S2_point.t
     ; contained : bool
     }

  val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

  (** [create ~point ~contained] builds a reference point. *)
  val create : point:S2_point.t -> contained:bool -> t

  (** [contained contained] returns a reference point at {!S2_point.origin} with the given
      containment flag. Use when all points or no points are contained. *)
  val contained : bool -> t

  val equal : t -> t -> bool
end

(** A 32-bit tag identifying an encoded shape type. Encodable types have a non-zero tag;
    tags [0 .. min_user_type_tag - 1] are reserved for the S2 library. *)
module Type_tag : sig
  type t = int

  (** Marks shape types that cannot be encoded. *)
  val none : t

  (** The next unused type tag in the reserved range. Update this whenever a new encoded
      type is added. *)
  val next_available : t

  (** The minimum allowable tag for user-defined shape types. *)
  val min_user : t
end

(** An unboxed record combining four fixed counts with four accessor callbacks:

    - [num_edges]: total number of edges (or points) in the shape.
    - [num_chains]: number of chains.
    - [dimension]: [0], [1], or [2] as described in the module header.
    - [type_tag]: identifies the shape subtype for serialization (see {!Type_tag}).
    - [reference_point]: a point together with its containment in the shape; used to
      compute containment of other points via edge crossings.
    - [edge id]: returns edge [id] for [0 <= id < num_edges].
    - [chain i]: returns chain [i] for [0 <= i < num_chains]; chains are contiguous and
      cover [0 .. num_edges - 1].
    - [chain_edge chain_id offset]: returns the edge at position [offset] within chain
      [chain_id]; equivalent to [edge (chain chain_id).start + offset] but may be faster.
    - [chain_position edge_id]: inverse of [chain_edge]; returns the chain containing
      [edge_id] and the offset of [edge_id] within that chain. *)
type t =
  #{ num_edges : int
   ; num_chains : int
   ; dimension : int
   ; type_tag : Type_tag.t
   ; reference_point : Reference_point.t
   ; edge : int -> Edge.t
   ; chain : int -> Chain.t
   ; chain_edge : int -> int -> Edge.t
   ; chain_position : int -> Chain_position.t
   }

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** [is_empty t] is true when the shape contains no points. Note that a {e full} polygon
    is encoded as a chain with zero edges, so emptiness also requires either
    [dimension < 2] or [num_chains = 0]. *)
val is_empty : t -> bool

(** [is_full t] is true when the shape is the full polygon (every point on the sphere).
    This requires [dimension = 2], [num_edges = 0], and at least one chain. *)
val is_full : t -> bool

(** [get_reference_point ~num_edges ~num_chains ~edge ~chain] computes a
    {!Reference_point.t} for a closed polygonal shape (dimension 2) that may have
    duplicate vertices and sibling edge pairs. An edge cancels its reverse, and a shape
    whose edges all cancel out is empty unless it contains at least one zero-length chain
    (in which case it is full).

    Arguments mirror the corresponding fields of {!t}, so concrete shape modules can call
    this during construction before the full [S2_shape.t] exists (the full record itself
    needs a reference point). The algorithm scans vertices of [edge 0] first, and falls
    back to a sort-based search over balanced vertices. Runs in O([num_edges] log
    [num_edges]) in the fallback path. *)
val get_reference_point
  :  num_edges:int
  -> num_chains:int
  -> edge:(int -> Edge.t)
  -> chain:(int -> Chain.t)
  -> Reference_point.t
[@@zero_alloc ignore]

(** {1 Limitations}

    The following C++ features are not exposed:
    - [Encode] / [user_data]: per-shape serialization and custom payloads.
    - [ChainVertexIterator] / [ChainVertexRange] / [ChainIterator] / [ChainRange]:
      iterator-style access to chains and chain vertices. Use [chain]/[chain_edge]
      directly. *)
