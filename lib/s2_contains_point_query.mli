(** Point-in-shape queries against geometry stored in an {!S2_shape_index.t}.

    For two-dimensional shapes, only {!Vertex_model.Semi_open} is implemented; other
    models raise. *)

open Core

module Vertex_model : sig
  type t =
    | Open
    | Semi_open
    | Closed
end

type t

(** Default vertex model is [Semi_open], matching the reference library. *)
val create : S2_shape_index.t -> ?vertex_model:Vertex_model.t -> unit -> t

val index : t -> S2_shape_index.t

(** [shape_contains t ~shape_id p] reports whether the shape with the given id contains
    [p] under the configured vertex model. *)
val shape_contains : t -> shape_id:int -> S2_point.t -> bool

(** [visit_containing_shapes t p ~f] calls [f shape_id] for each indexed shape that
    contains [p] under the configured vertex model. Iteration stops early if [f] returns
    [false]; the return value reports whether iteration ran to completion. *)
val visit_containing_shapes : t -> S2_point.t -> f:(int -> bool) -> bool
