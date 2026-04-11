(** [S2_edge_crosser] tests a chain of edges [C0 C1], [C1 C2], ..., for intersection with
    a single fixed edge [AB]. It is significantly faster than calling
    [S2_edge_crossings.crossing_sign] once per edge because it caches the orientation of
    the triangle [ACB] and the outward-facing tangents at [A] and [B] between calls.

    Typical usage:

    {[
      let crosser = S2_edge_crosser.create ~a ~b in
      S2_edge_crosser.restart_at crosser v0;
      let count = ref 0 in
      List.iter chain ~f:(fun v ->
        if S2_edge_crosser.chain_crossing_sign crosser v > 0 then incr count)
    ]} *)

open Core

(** Abstract stateful crosser. Not thread safe. *)
type t

(** [create ~a ~b] returns a new crosser with fixed edge [AB]. [a] and [b] should be
    unit-length. After calling [create], the chain vertex [c] is uninitialized; call
    [restart_at] before any of the [chain_*] methods. *)
val create : a:S2_point.t -> b:S2_point.t -> t

(** [create_with_chain ~a ~b ~c] returns a new crosser with fixed edge [AB] and the first
    chain vertex set to [c]. Equivalent to calling [create] followed by [restart_at]. *)
val create_with_chain : a:S2_point.t -> b:S2_point.t -> c:S2_point.t -> t

(** [a t] returns the first vertex of the fixed edge. *)
val a : t -> S2_point.t

(** [b t] returns the second vertex of the fixed edge. *)
val b : t -> S2_point.t

(** [c t] returns the last chain vertex passed to any of the crossing methods or to
    [restart_at]. *)
val c : t -> S2_point.t

(** [init t ~a ~b] resets the fixed edge to [AB] and clears the cached chain vertex and
    tangents. *)
val init : t -> a:S2_point.t -> b:S2_point.t -> unit

(** [restart_at t c] sets the current chain vertex to [c]. Call this whenever the chain
    jumps to a new place. *)
val restart_at : t -> S2_point.t -> unit

(** [crossing_sign t c d] reports whether the fixed edge [AB] intersects [CD]:
    - [+1] if [AB] crosses [CD] at a point interior to both edges;
    - [0] if any two vertices from different edges are the same;
    - [-1] otherwise.

    After the call, the chain vertex is set to [d]. If [c] matches the current chain
    vertex this is equivalent to [chain_crossing_sign t d]. *)
val crossing_sign : t -> S2_point.t -> S2_point.t -> int

(** [chain_crossing_sign t d] is like [crossing_sign t c d], but uses the current chain
    vertex as [c]. After the call, the chain vertex is set to [d]. *)
val chain_crossing_sign : t -> S2_point.t -> int

(** [edge_or_vertex_crossing t c d] extends [crossing_sign] to handle shared vertices:
    returns [true] if [crossing_sign t c d > 0], or if [AB] and [CD] share a vertex and
    [S2_edge_crossings.vertex_crossing] returns [true]. *)
val edge_or_vertex_crossing : t -> S2_point.t -> S2_point.t -> bool

(** [chain_edge_or_vertex_crossing t d] is [edge_or_vertex_crossing] using the current
    chain vertex as [c]. *)
val chain_edge_or_vertex_crossing : t -> S2_point.t -> bool

(** [signed_edge_or_vertex_crossing t c d] is like [edge_or_vertex_crossing], but returns
    a signed value indicating the direction of the crossing: [-1] if [AB] crosses [CD]
    from left to right, [+1] from right to left, and [0] otherwise. *)
val signed_edge_or_vertex_crossing : t -> S2_point.t -> S2_point.t -> int

(** [chain_signed_edge_or_vertex_crossing t d] is [signed_edge_or_vertex_crossing] using
    the current chain vertex as [c]. *)
val chain_signed_edge_or_vertex_crossing : t -> S2_point.t -> int

(** [last_interior_crossing_sign t] returns the sign of the last interior crossing ([-1]
    for left to right, [+1] for right to left) if the most recent call to [crossing_sign]
    returned [+1]. Its return value is undefined otherwise. *)
val last_interior_crossing_sign : t -> int
