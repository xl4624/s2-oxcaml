(** A set of {!S2_point.t} values exposed as an {!S2_shape.t}.

    Each point becomes a degenerate edge [(p, p)] and its own single-edge chain, so
    [num_edges = num_points = num_chains] and [chain i = { start = i; length = 1 }]. The
    dimension is fixed at [0] and the reference point is the distinguished "not contained"
    point.

    Useful for adding a collection of points to an {!S2_shape_index.t} without building
    one-edge shapes by hand. *)

open Core

type t [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [create ()] is an empty point-vector shape. *)
val create : unit -> t
[@@zero_alloc ignore]

(** [of_points ps] copies [ps] into a fresh shape. *)
val of_points : S2_point.t array -> t
[@@zero_alloc ignore]

(** {1 Accessors} *)

(** [num_points t] is the number of points stored in [t]. *)
val num_points : t -> int

(** [point t i] returns the [i]-th point. *)
val point : t -> int -> S2_point.t

(** [num_edges t] equals [num_points t]: each point is a degenerate edge. *)
val num_edges : t -> int

(** [edge t e] returns the [e]-th point as a degenerate edge [(p, p)]. *)
val edge : t -> int -> S2_shape.Edge.t

(** [dimension t] is [0]. *)
val dimension : t -> int

(** [num_chains t] equals [num_points t]: each point is its own chain. *)
val num_chains : t -> int

(** [chain t i] is [{ start = i; length = 1 }]. *)
val chain : t -> int -> S2_shape.Chain.t

(** [chain_edge t i j] is [edge t i] (requires [j = 0]). *)
val chain_edge : t -> int -> int -> S2_shape.Edge.t

(** [chain_position t e] is [{ chain_id = e; offset = 0 }]. *)
val chain_position : t -> int -> S2_shape.Chain_position.t

(** [reference_point t] returns the fixed "not contained" reference point. *)
val reference_point : t -> S2_shape.Reference_point.t

(** [type_tag] identifies this shape for serialization. Encoding is not implemented here,
    so this is {!S2_shape.Type_tag.none}. *)
val type_tag : S2_shape.Type_tag.t

(** [to_shape t] exposes [t] through the generic {!S2_shape.t} interface. *)
val to_shape : t -> S2_shape.t
[@@zero_alloc ignore]
