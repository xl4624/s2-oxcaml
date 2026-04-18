(** In-place sort for arrays of unboxed-product element types.

    Core's [Array.sort] requires elements of kind [value], so [Edge.t array] and other
    unboxed-product arrays cannot be passed directly. The functors below implement a
    kind-aware [sort] on top of the layout-polymorphic [Array.length] / [Array.get] /
    [Array.set] primitives.

    Uses insertion sort for small inputs and an iterative heap sort otherwise. Both
    variants are in-place and allocate nothing on the heap. The sort is not stable.

    [Make] is a [ppx_template] functor covering simple product kinds (pairs and triples of
    [float64]). [Make_3_3] is a separate functor for the nested
    [((float64 & float64 & float64) & (float64 & float64 & float64))] kind that
    [ppx_template] cannot express because it flattens adjacent [&] products. *)

open Core

module%template
  [@kind
    k = ((float64 & float64) mod external_, (float64 & float64 & float64) mod external_)] Make : functor
    (E : sig
       type t : k
     end)
    -> sig
  (** [sort arr ~compare] sorts [arr] in-place in ascending order according to [compare]. *)
  val sort : E.t array -> compare:(E.t -> E.t -> int) -> unit
end

module Make_3_3 : functor
    (E : sig
       type t :
         ((float64 & float64 & float64) & (float64 & float64 & float64)) mod external_
     end)
    -> sig
  val sort : E.t array -> compare:(E.t -> E.t -> int) -> unit
end
