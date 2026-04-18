(** [S2_lax_loop] represents a closed loop of edges surrounding an interior region.

    Unlike {!S2_loop}, adjacent vertices are allowed to be identical or antipodal and the
    representation is more compact. A loop may have any number of vertices, including 0,
    1, or 2; a one-vertex loop defines a single degenerate edge from the vertex to itself. *)

open Core

type t [@@deriving sexp_of]

(** {1 Constructors} *)

(** [of_vertices vs] constructs a lax loop whose vertex sequence is a copy of [vs]. *)
val of_vertices : S2_point.t array -> t

(** {1 Accessors} *)

(** [num_vertices t] is the number of stored vertices. *)
val num_vertices : t -> int

(** [vertex t i] is the [i]-th vertex. Requires [0 <= i < num_vertices t]. *)
val vertex : t -> int -> S2_point.t

(** {1 Shape interface} *)

(** [num_edges t] equals [num_vertices t]. *)
val num_edges : t -> int

(** [edge t e] is the [e]-th edge [(vertex t e, vertex t ((e + 1) mod num_vertices))]. *)
val edge : t -> int -> S2_shape.Edge.t

(** [dimension t] is [2]. *)
val dimension : t -> int

(** [num_chains t] is [1] if the loop has any vertices and [0] otherwise. *)
val num_chains : t -> int

(** [chain t i] returns the single chain spanning every edge. Requires [i = 0]. *)
val chain : t -> int -> S2_shape.Chain.t

(** [chain_edge t i j] is [edge t j]. Requires [i = 0]. *)
val chain_edge : t -> int -> int -> S2_shape.Edge.t

(** [chain_position t e] is [{ chain_id = 0; offset = e }]. *)
val chain_position : t -> int -> S2_shape.Chain_position.t

(** [reference_point t] returns a point and whether it lies in the loop's interior. The
    result is computed once at construction time. *)
val reference_point : t -> S2_shape.Reference_point.t

(** [type_tag] is the encoded shape tag for lax loops; lax loops are not yet encodable so
    this is {!S2_shape.Type_tag.none}. *)
val type_tag : S2_shape.Type_tag.t

(** [to_shape t] exposes [t] through the generic shape interface. *)
val to_shape : t -> S2_shape.t
