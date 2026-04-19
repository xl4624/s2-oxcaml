(** [S2_edge_crosser] tests a chain of edges [C0 C1], [C1 C2], ..., for intersection with
    a single fixed edge [AB]. It is significantly faster than calling
    [S2_edge_crossings.crossing_sign] once per edge because it caches the orientation of
    the triangle [ACB] and the outward-facing tangents at [A] and [B] between calls.

    The type [t] is an unboxed record; every mutating operation returns a new [t] and
    callers typically store it in a [let mutable] binding so the underlying storage is
    rewritten in place. Query operations that both advance the chain and produce a result
    return an unboxed record containing both the updated state and the result.

    Typical usage:

    {[
      let mutable crosser = S2_edge_crosser.create ~a ~b in
      crosser <- S2_edge_crosser.restart_at crosser v0;
      let count = ref 0 in
      List.iter chain ~f:(fun v ->
        let #{ state; sign } = S2_edge_crosser.chain_crossing_sign crosser v in
        crosser <- state;
        if sign > 0 then incr count)
    ]} *)

open Core

[@@@zero_alloc all]

(** The full, unboxed crosser state. *)
type t =
  #{ a : R3_vector.t
   ; b : R3_vector.t
   ; a_cross_b : R3_vector.t
   ; have_tangents : int
   ; a_tangent : R3_vector.t
   ; b_tangent : R3_vector.t
   ; c : R3_vector.t
   ; acb : int
   ; bda : int
   }

(** Query result: the updated state paired with an integer sign. *)
type with_sign =
  #{ state : t
   ; sign : int
   }

(** Query result: the updated state paired with a boolean crossing flag. *)
type with_bool =
  #{ state : t
   ; crossing : bool
   }

(** [create ~a ~b] returns a new crosser with fixed edge [AB]. [a] and [b] should be
    unit-length. After [create], the chain vertex is uninitialized; call [restart_at]
    before any [chain_*] operation. *)
val create : a:S2_point.t -> b:S2_point.t -> t

(** [create_with_chain ~a ~b ~c] is equivalent to [restart_at (create ~a ~b) c]. *)
val create_with_chain : a:S2_point.t -> b:S2_point.t -> c:S2_point.t -> t

(** [a t] returns the first vertex of the fixed edge. *)
val a : t -> S2_point.t

(** [b t] returns the second vertex of the fixed edge. *)
val b : t -> S2_point.t

(** [c t] returns the most recent chain vertex. *)
val c : t -> S2_point.t

(** [init t ~a ~b] returns a crosser with a new fixed edge and cleared chain state. *)
val init : t -> a:S2_point.t -> b:S2_point.t -> t

(** [restart_at t c] returns a crosser whose chain vertex is [c]. Use this to jump to a
    new place in the chain. *)
val restart_at : t -> S2_point.t -> t

(** [crossing_sign t c d] reports whether the fixed edge [AB] intersects [CD]:
    - [+1] if [AB] crosses [CD] at a point interior to both edges;
    - [0] if any two vertices from different edges are the same;
    - [-1] otherwise.

    The returned state has its chain vertex advanced to [d]. If [c] matches the existing
    chain vertex, this is equivalent to [chain_crossing_sign t d]. *)
val crossing_sign : t -> S2_point.t -> S2_point.t -> with_sign

(** [chain_crossing_sign t d] is like [crossing_sign t c d] but uses the current chain
    vertex as [c]. *)
val chain_crossing_sign : t -> S2_point.t -> with_sign

(** [edge_or_vertex_crossing t c d] extends [crossing_sign] to handle shared vertices:
    reports [true] if the crossing is positive, or if [AB] and [CD] share a vertex and
    [S2_edge_crossings.vertex_crossing] returns [true]. *)
val edge_or_vertex_crossing : t -> S2_point.t -> S2_point.t -> with_bool

(** Chain variant of [edge_or_vertex_crossing]. *)
val chain_edge_or_vertex_crossing : t -> S2_point.t -> with_bool

(** [signed_edge_or_vertex_crossing t c d] is like [edge_or_vertex_crossing] but returns a
    signed value: [-1] if [AB] crosses [CD] from left to right, [+1] from right to left,
    and [0] otherwise. *)
val signed_edge_or_vertex_crossing : t -> S2_point.t -> S2_point.t -> with_sign

(** Chain variant of [signed_edge_or_vertex_crossing]. *)
val chain_signed_edge_or_vertex_crossing : t -> S2_point.t -> with_sign

(** [last_interior_crossing_sign t] returns the sign of the last interior crossing ([-1]
    for left to right, [+1] for right to left) when the most recent [chain_crossing_sign]
    / [crossing_sign] returned [+1]. Undefined otherwise. *)
val last_interior_crossing_sign : t -> int
