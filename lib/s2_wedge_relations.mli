(** Relationships between two angular "wedges" that share a common middle vertex.

    Given an edge chain [(x0, x1, x2)], the wedge at [x1] is the region to the left of the
    edges. More precisely, it is the set of all rays from [x1 -> x0] (inclusive) to
    [x1 -> x2] (exclusive) swept in the clockwise direction.

    The functions in this module compare two non-empty wedges [A = (a0, ab1, a2)] and
    [B = (b0, ab1, b2)] that share the middle vertex [ab1]. *)

open Core

(** Detailed relation from one wedge [A] to another wedge [B]. *)
module Relation : sig
  type t =
    | Equals (** [A] and [B] are equal. *)
    | Properly_contains (** [A] is a strict superset of [B]. *)
    | Is_properly_contained (** [A] is a strict subset of [B]. *)
    | Properly_overlaps (** [A - B], [B - A], and [A inter B] are all non-empty. *)
    | Is_disjoint (** [A] and [B] are disjoint. *)
  [@@deriving sexp_of, compare, equal]
end

(** [get_wedge_relation ~a0 ~ab1 ~a2 ~b0 ~b2] returns the relation from wedge
    [A = (a0, ab1, a2)] to wedge [B = (b0, ab1, b2)]. Requires that both wedges are
    non-empty. *)
val get_wedge_relation
  :  a0:S2_point.t
  -> ab1:S2_point.t
  -> a2:S2_point.t
  -> b0:S2_point.t
  -> b2:S2_point.t
  -> Relation.t

(** [wedge_contains ~a0 ~ab1 ~a2 ~b0 ~b2] reports whether wedge [A = (a0, ab1, a2)]
    contains wedge [B = (b0, ab1, b2)]. Equivalent to but faster than
    [get_wedge_relation = Equals || Properly_contains]. Requires that both wedges are
    non-empty. *)
val wedge_contains
  :  a0:S2_point.t
  -> ab1:S2_point.t
  -> a2:S2_point.t
  -> b0:S2_point.t
  -> b2:S2_point.t
  -> bool

(** [wedge_intersects ~a0 ~ab1 ~a2 ~b0 ~b2] reports whether wedge [A = (a0, ab1, a2)]
    intersects wedge [B = (b0, ab1, b2)]. Equivalent to but faster than
    [get_wedge_relation <> Is_disjoint]. Requires that both wedges are non-empty. *)
val wedge_intersects
  :  a0:S2_point.t
  -> ab1:S2_point.t
  -> a2:S2_point.t
  -> b0:S2_point.t
  -> b2:S2_point.t
  -> bool
