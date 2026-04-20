(** In-memory index of {!S2_shape.t} values, used to accelerate point containment and
    other spatial queries. The implementation follows the adaptive subdivision strategy
    used in the reference library: each index cell stores the subset of shape edges that
    may intersect that cell, refined until a bounded number of “short” edges remain per
    cell.

    Shapes are identified by dense integer ids starting at [0]. Only addition is supported
    for now (removals are not yet implemented). *)

open Core

(** Relationship between a query cell id and the index cells. *)
module Cell_relation : sig
  type t =
    | Indexed
    | Subdivided
    | Disjoint
end

(** Part of a shape intersecting one index cell. *)
module Clipped_shape : sig
  type t

  val shape_id : t -> int
  val contains_center : t -> bool
  val num_edges : t -> int
  val edge : t -> int -> int
end

(** Payload stored for one [S2_cell_id.t] key in the index. *)
module Index_cell : sig
  type t

  val num_clipped : t -> int
  val clipped : t -> int -> Clipped_shape.t
  val find_clipped : t -> shape_id:int -> Clipped_shape.t option
end

(** Sorted iterator over index cells. *)
module Iterator : sig
  type t

  val begin_at : t -> unit
  val at_end : t -> unit
  val seek : t -> S2_cell_id.t -> unit
  val next : t -> unit
  val prev : t -> bool
  val is_done : t -> bool
  val cell_id : t -> S2_cell_id.t
  val index_cell : t -> Index_cell.t
  val locate_point : t -> S2_point.t -> bool

  (** [locate_cell_id] positions the iterator when the relation is [Indexed] or
      [Subdivided]; for [Disjoint] the iterator position is undefined. *)
  val locate_cell_id : t -> S2_cell_id.t -> Cell_relation.t
end

type t

val create : ?max_edges_per_cell:int -> unit -> t

(** Assigns the next shape id and queues the shape for indexing. *)
val add : t -> S2_shape.t -> int

val num_shape_ids : t -> int

(** [shape t id] raises if [id] is not in [0, num_shape_ids t). *)
val shape : t -> int -> S2_shape.t

(** Applies any pending additions and builds or updates the cell map. Idempotent when the
    index is already fresh. *)
val build : t -> unit

val is_fresh : t -> bool
val iterator : t -> Iterator.t
