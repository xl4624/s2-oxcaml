(** A {!S2_shapeutil_shape_edge_id.t} together with the two endpoints of the referenced
    edge.

    Used as a small, self-contained packet of data that crossing-edge queries,
    incident-edge visitors, and similar APIs hand to their callers without forcing them to
    look up the endpoints from the shape themselves. *)

open Core

[@@@zero_alloc all]

type t =
  #{ id : S2_shapeutil_shape_edge_id.t
   ; edge : S2_shape.Edge.t
   }

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** [create ~shape_id ~edge_id ~edge] packages the [(shape_id, edge_id)] pair with the
    edge endpoints. The caller is responsible for ensuring [edge] really is the edge at
    index [edge_id] of the shape with id [shape_id]; the constructor stores them verbatim
    without consulting any index. *)
val create : shape_id:int -> edge_id:int -> edge:S2_shape.Edge.t -> t

(** [v0 t] is the start vertex of the edge. *)
val v0 : t -> S2_point.t

(** [v1 t] is the end vertex of the edge. *)
val v1 : t -> S2_point.t
