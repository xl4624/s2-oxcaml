(** A unique identifier for an edge within an {!S2_shape_index.t}.

    Pairs a shape id with an edge id local to that shape. Comparison is lexicographic with
    [shape_id] taking precedence over [edge_id], matching the canonical ordering used
    throughout the index machinery (crossing-edge queries, validation, set-operation
    outputs). *)

open Core

type t =
  { shape_id : int
  ; edge_id : int
  }
[@@deriving compare, equal, sexp_of]

(** [none] is the sentinel pair [(-1, -1)] used to denote "no edge". It compares less than
    every {e valid} id and is the default value of an uninitialised {!t} on the C++ side. *)
val none : t

(** [create ~shape_id ~edge_id] is a convenience constructor that just records the two
    fields. *)
val create : shape_id:int -> edge_id:int -> t
