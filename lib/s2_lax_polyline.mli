(** [S2_lax_polyline] represents a polyline as a sequence of vertices connected by edges.

    Unlike {!S2_polyline}, adjacent vertices are allowed to be identical or antipodal, and
    the representation is slightly more compact. A polyline consisting of a single
    degenerate edge can be built by repeating the same vertex twice; polylines with fewer
    than two vertices define no edges. *)

open Core

[@@@zero_alloc all]

type t [@@deriving sexp_of]

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [of_vertices vs] constructs a lax polyline whose vertex sequence is a copy of [vs]. *)
val of_vertices : S2_point.t array -> t
[@@zero_alloc ignore]

(** {1 Accessors} *)

(** [num_vertices t] is the number of stored vertices. *)
val num_vertices : t -> int

(** [vertex t i] is the [i]-th vertex. Requires [0 <= i < num_vertices t]. *)
val vertex : t -> int -> S2_point.t

(** {1 Shape interface} *)

(** [num_edges t] is [max 0 (num_vertices t - 1)]. *)
val num_edges : t -> int

(** [edge t e] returns the [e]-th edge [(vertex t e, vertex t (e + 1))]. *)
val edge : t -> int -> S2_shape.Edge.t

(** [dimension t] is [1]. *)
val dimension : t -> int

(** [num_chains t] is [1] if the polyline has any edges and [0] otherwise. *)
val num_chains : t -> int

(** [chain t i] returns the single chain spanning every edge. Requires [i = 0]. *)
val chain : t -> int -> S2_shape.Chain.t

(** [chain_edge t i j] is [edge t j]. Requires [i = 0]. *)
val chain_edge : t -> int -> int -> S2_shape.Edge.t

(** [chain_position t e] is [{ chain_id = 0; offset = e }]. *)
val chain_position : t -> int -> S2_shape.Chain_position.t

(** [reference_point t] always returns a reference point that is not contained. *)
val reference_point : t -> S2_shape.Reference_point.t

(** [type_tag] is the encoded shape tag for lax polylines ([4]). *)
val type_tag : S2_shape.Type_tag.t

(** [to_shape t] exposes [t] through the generic shape interface. *)
val to_shape : t -> S2_shape.t
[@@zero_alloc ignore]
