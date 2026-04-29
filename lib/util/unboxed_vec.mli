(** A growable array ("vector") for unboxed-product element types.

    Base / Core's [Array] module supports layout-polymorphic primitives ([create],
    [length], [get], [set]), but [Array.iter], [Array.blit], [Array.of_list], and friends
    are not yet polymorphic over unboxed products like [float64 & float64]. This module
    fills the gap with a tiny growable-buffer abstraction backed by manual [for]-loops
    over the layout-polymorphic primitives.

    [Make] is a functor over element types with kind [float64 & float64] or
    [float64 & float64 & float64] (both [mod external_], i.e. not scanned by the GC). If
    we end up needing more kinds, extend the [@kind] list at the definition site. *)

module%template
  [@kind
    k = ((float64 & float64) mod external_, (float64 & float64 & float64) mod external_)] Make : functor
    (E : sig
       type t : k

       val default : t
     end)
    -> sig
  type t

  (** [create ?initial_capacity ()] allocates an empty buffer with the given initial
      capacity (default 8). *)
  val create : ?initial_capacity:int -> unit -> t

  val length : t -> int
  val get : t -> int -> E.t
  val set : t -> int -> E.t -> unit

  (** [last b] returns the element at index [length b - 1]. Undefined if the buffer is
      empty. *)
  val last : t -> E.t

  (** [push b v] appends [v] to [b], growing the backing array if needed. *)
  val push : t -> E.t -> unit

  (** [clear b] resets [b] to length zero without releasing the backing array. *)
  val clear : t -> unit

  (** [to_array b] returns a fresh array whose length is [length b]. *)
  val to_array : t -> E.t array
end
