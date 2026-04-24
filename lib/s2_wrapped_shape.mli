(** An {!S2_shape.t} that borrows another {!S2_shape.t} without owning it.

    Every accessor forwards directly to the underlying shape; the wrapper adds no extra
    state. This is useful for adding an existing shape to a second {!S2_shape_index.t}
    without duplicating the backing data, and for separating shape identity (the backing
    representation) from encoding identity (a wrapped shape advertises no
    {!S2_shape.Type_tag}).

    The wrapper captures the shape by value (OCaml semantics: closures copied, no
    ownership transfer) and imposes no lifetime requirements beyond those of the backing
    closures. Mutations to a mutable backing shape are visible through the wrapper. *)

open Core

type t [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [create shape] wraps [shape] without copying. *)
val create : S2_shape.t -> t
[@@zero_alloc ignore]

(** {1 Accessors}

    Each of these forwards to the corresponding field of the wrapped shape. *)

val num_edges : t -> int
val edge : t -> int -> S2_shape.Edge.t
val dimension : t -> int
val num_chains : t -> int
val chain : t -> int -> S2_shape.Chain.t
val chain_edge : t -> int -> int -> S2_shape.Edge.t
val chain_position : t -> int -> S2_shape.Chain_position.t
val reference_point : t -> S2_shape.Reference_point.t

(** [type_tag] is {!S2_shape.Type_tag.none}: a wrapped shape deliberately declines its
    backing shape's encoding tag so that encoding callers treat it as uncoded. *)
val type_tag : S2_shape.Type_tag.t

(** [to_shape t] exposes [t] through the generic {!S2_shape.t} interface. The returned
    shape reports {!type_tag} (no encoding) but otherwise delegates every callback to the
    wrapped shape. *)
val to_shape : t -> S2_shape.t
[@@zero_alloc ignore]
