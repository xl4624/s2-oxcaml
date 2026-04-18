(** [S2_shape] represents polygonal geometry in a flexible way, as a collection of edges
    that optionally defines an interior. All geometry represented by a given shape must
    have the same {e dimension}: a shape can be a set of points, a set of polylines, or a
    set of polygons.

    The edges are identified by a contiguous range of integer edge ids starting at [0] and
    are subdivided into {e chains}, where each chain is a sequence of edges connected
    end-to-end.

    An OCaml [S2_shape.t] is a record-of-functions captured at construction time.
    Concrete shape types (loop, polygon, polyline, lax_polyline, ...) provide their own
    [to_shape] conversion. *)

open Core

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

(** Handles for a shape's concrete implementation. Every function is captured at
    construction time by the corresponding [to_shape] of a concrete module. *)
type t =
  #{ num_edges : unit -> int
   ; edge : int -> Edge.t
   ; dimension : unit -> int
   ; num_chains : unit -> int
   ; chain : int -> Chain.t
   ; chain_edge : int -> int -> Edge.t
   ; chain_position : int -> Chain_position.t
   ; reference_point : unit -> Reference_point.t
   ; type_tag : unit -> Type_tag.t
   }

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** [is_empty t] is true when the shape contains no points. (The full polygon is
    represented as a chain with zero edges, so emptiness also requires the shape to either
    be non-polygonal or have no chains.) *)
val is_empty : t -> bool

(** [is_full t] is true when the shape contains all points on the sphere. Only polygonal
    shapes with no edges and at least one chain are full. *)
val is_full : t -> bool
