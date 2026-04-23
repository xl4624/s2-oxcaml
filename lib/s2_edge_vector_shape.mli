(** An arbitrary set of edges exposed as an {!S2_shape.t}.

    Each added edge becomes its own chain, so [num_chains = num_edges] and
    [chain i = { start = i; length = 1 }]. The reference point is the distinguished "not
    contained" point; [dimension] defaults to [1] but is configurable via
    {!set_dimension}.

    Mainly useful for testing and for ad-hoc inputs where vertex duplication is
    acceptable. For compact polyline or polygon storage prefer {!S2_polyline}, {!S2_loop},
    or the lax variants.

    {2 Mutability}

    Unlike most shapes in this library, [t] is a mutable builder: {!add} appends an edge
    and {!set_dimension} updates the reported dimension. Do not mutate a value after
    adding the shape it backs to an {!S2_shape_index.t}. *)

open Core

type t [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [create ()] is an empty edge-vector shape with dimension [1]. *)
val create : unit -> t
[@@zero_alloc ignore]

(** [of_edges es] copies [es] into a fresh shape. Dimension is [1]. *)
val of_edges : S2_shape.Edge.t array -> t
[@@zero_alloc ignore]

(** [singleton ~v0 ~v1] is the shape holding the single edge [(v0, v1)]. *)
val singleton : v0:S2_point.t -> v1:S2_point.t -> t
[@@zero_alloc ignore]

(** {1 Mutators} *)

(** [add t ~v0 ~v1] appends [(v0, v1)] as a new edge. *)
val add : t -> v0:S2_point.t -> v1:S2_point.t -> unit
[@@zero_alloc ignore]

(** [set_dimension t dim] overrides the dimension reported by [t]. The default is [1];
    valid values are [0], [1], and [2]. *)
val set_dimension : t -> int -> unit

(** {1 Accessors} *)

(** [num_edges t] is the number of edges currently stored in [t]. *)
val num_edges : t -> int

(** [edge t e] returns the [e]-th edge. *)
val edge : t -> int -> S2_shape.Edge.t

(** [dimension t] is the configured dimension. *)
val dimension : t -> int

(** [num_chains t] equals [num_edges t]: each edge is its own chain. *)
val num_chains : t -> int

(** [chain t i] is [{ start = i; length = 1 }]. *)
val chain : t -> int -> S2_shape.Chain.t

(** [chain_edge t i j] is [edge t i] (requires [j = 0]). *)
val chain_edge : t -> int -> int -> S2_shape.Edge.t

(** [chain_position t e] is [{ chain_id = e; offset = 0 }]. *)
val chain_position : t -> int -> S2_shape.Chain_position.t

(** [reference_point t] returns the fixed "not contained" reference point. *)
val reference_point : t -> S2_shape.Reference_point.t

(** [type_tag] identifies this shape for serialization. Edge-vector shapes have no wire
    format, so this is {!S2_shape.Type_tag.none}. *)
val type_tag : S2_shape.Type_tag.t

(** [to_shape t] exposes [t] through the generic {!S2_shape.t} interface. *)
val to_shape : t -> S2_shape.t
[@@zero_alloc ignore]
