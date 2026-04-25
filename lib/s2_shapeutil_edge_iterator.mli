(** Mutable iterator that advances through every edge in an {!S2_shape_index.t}, in
    [(shape_id, edge_id)] order.

    The iterator visits shapes by ascending shape id; within each shape it visits the
    edges in their natural [0 .. num_edges - 1] order. Empty shapes and shape ids that
    have been removed (reported as [None] by the index) are skipped automatically.

    Typical usage:

    {[
      let it = S2_shapeutil_edge_iterator.create idx in
      while not (S2_shapeutil_edge_iterator.is_done it) do
        let edge = S2_shapeutil_edge_iterator.edge it in
        (* ... *)
        S2_shapeutil_edge_iterator.next it
      done
    ]}

    The iterator holds a reference to the index but does not own it; the caller must keep
    the index alive and unmodified for the iterator's lifetime. *)

open Core

type t

(** [create idx] returns a fresh iterator already positioned at the first edge of [idx],
    or already [is_done] if [idx] has no edges. *)
val create : S2_shape_index.t -> t

(** [shape_id t] is the shape id of the current edge. Equals
    [S2_shape_index.num_shape_ids] when [is_done t]. *)
val shape_id : t -> int

(** [edge_id t] is the local edge id of the current edge within {!shape_id}'s shape.
    Undefined when [is_done t]. *)
val edge_id : t -> int

(** [shape_edge_id t] packages {!shape_id} and {!edge_id} into the canonical pair.
    Undefined when [is_done t]. *)
val shape_edge_id : t -> S2_shapeutil_shape_edge_id.t

(** [edge t] returns the current edge. Raises when [is_done t]. *)
val edge : t -> S2_shape.Edge.t

(** [is_done t] is [true] once [t] has advanced past the last edge of the index. *)
val is_done : t -> bool

(** [next t] advances [t] by one edge. Has no effect once [is_done t]. *)
val next : t -> unit

(** Two iterators are [equal] when they share the same underlying index (by physical
    equality) and point at the same [(shape_id, edge_id)]. *)
val equal : t -> t -> bool
