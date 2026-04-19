(** Flat array-backed binary heap, specialized per element type via {!Make}.

    Each [add] and [pop_exn] writes an array slot instead of allocating a heap node, so
    after the backing array reaches steady size both operations are allocation-free.

    The functor specializes [Elt.higher_priority] at the application site, so comparisons
    inside [sift_up] / [sift_down] are direct calls rather than indirect calls through a
    closure. *)

module Make (Elt : sig
    type t

    (** [higher_priority a b] is [true] when [a] must sit above [b] at the root. For a
        max-heap on a record field [priority], use [fun a b -> a.priority > b.priority];
        flip the comparison for a min-heap. *)
    val higher_priority : t -> t -> bool
  end) : sig
  type t

  (** [create ()] returns an empty heap. The backing array is allocated lazily on the
      first [add] (using the inserted value as fill for unused slots), so constructing a
      heap that is never used costs nothing. *)
  val create : unit -> t

  val is_empty : t -> bool
  val length : t -> int

  (** [add h x] inserts [x] in O(log n). *)
  val add : t -> Elt.t -> unit

  (** [pop_exn h] removes and returns the root. Raises if empty. *)
  val pop_exn : t -> Elt.t
end
