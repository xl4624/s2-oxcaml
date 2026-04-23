(** Fast intersection tests between a fixed spherical edge [AB] and a chain of edges
    [C0 C1], [C1 C2], [C2 C3], ....

    When you need to test many edges against the same [AB], this module is noticeably
    faster than calling {!S2_edge_crossings.crossing_sign} once per edge. Between chain
    steps it caches orientation information that lets most non-crossing edges be rejected
    with a single floating-point determinant, and only a minority of edges trigger the
    full exact-arithmetic fallback.

    The module implements an exact, consistent perturbation model: three points are never
    treated as exactly collinear, and results are consistent with
    {!S2_edge_crossings.sign} across calls.

    Usage. Operations that conceptually mutate the crosser return a new [t] instead. Store
    the crosser in a [let mutable] binding and reassign it. Operations that both advance
    the chain and produce an answer return an unboxed pair [#{ state; sign }] or
    [#{ state; crossing }]; install [state] before the next call.

    {[
      let mutable crosser = S2_edge_crosser.create ~a ~b in
      crosser <- S2_edge_crosser.restart_at crosser v0;
      let count = ref 0 in
      List.iter chain ~f:(fun v ->
        let #{ state; sign } = S2_edge_crosser.chain_crossing_sign crosser v in
        crosser <- state;
        if sign > 0 then incr count)
    ]}

    All operations are pure with respect to external state. The module is safe to use
    across threads as long as each thread has its own crosser value. *)

open Core

[@@@zero_alloc all]

(** Full crosser state: fixed edge AB, cached outward tangents at A and B, the previous
    chain vertex C, and the cached orientation of triangles ACB and BDA. *)
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

(** Result of a chain step that returns a crossing sign: the new crosser state plus the
    integer result. *)
type with_sign =
  #{ state : t
   ; sign : int
   }

(** Result of a chain step that returns a boolean: the new crosser state plus whether a
    crossing was detected. *)
type with_bool =
  #{ state : t
   ; crossing : bool
   }

(** [create ~a ~b] builds a crosser with fixed edge AB. [a] and [b] must be unit-length.
    The chain vertex is left zero and must be installed with {!restart_at} before any
    [chain_*] call. *)
val create : a:S2_point.t -> b:S2_point.t -> t

(** [create_with_chain ~a ~b ~c] is equivalent to [restart_at (create ~a ~b) c] and is the
    most common way to start a chain walk. *)
val create_with_chain : a:S2_point.t -> b:S2_point.t -> c:S2_point.t -> t

(** [a t] is the first vertex of the fixed edge. *)
val a : t -> S2_point.t

(** [b t] is the second vertex of the fixed edge. *)
val b : t -> S2_point.t

(** [c t] is the most recent chain vertex, i.e. the C that will form the edge CD on the
    next [chain_*] call. *)
val c : t -> S2_point.t

(** [init t ~a ~b] reuses [t]'s storage to hold a new fixed edge AB with a fresh,
    uninitialized chain. Equivalent in effect to [create ~a ~b] but avoids growing the
    value's footprint. *)
val init : t -> a:S2_point.t -> b:S2_point.t -> t

(** [restart_at t c] positions the chain at vertex [c]. Use this when your chain jumps to
    a new place (for example, when iterating over a polygon with multiple loops). *)
val restart_at : t -> S2_point.t -> t

(** [crossing_sign t c d] tests whether AB intersects CD and returns:

    - [+1] if AB crosses CD at a point interior to both edges;
    - [0] if any two vertices from different edges are equal;
    - [-1] otherwise.

    If an edge is degenerate ([a = b] or [c = d]) the result is [0] when two vertices
    coincide and [-1] otherwise. The function obeys:

    - [crossing_sign t a b c d = crossing_sign t b a c d]
    - [crossing_sign t a b c d = crossing_sign t c d a b]

    The returned state advances the chain vertex to [d]. When [c] already equals the
    stored chain vertex, this is equivalent to {!chain_crossing_sign}. *)
val crossing_sign : t -> S2_point.t -> S2_point.t -> with_sign

(** [chain_crossing_sign t d] is the single-argument fast path of {!crossing_sign}: the
    previous chain vertex is reused as C, so the result is the crossing sign of AB vs. CD
    where C is [c t]. *)
val chain_crossing_sign : t -> S2_point.t -> with_sign

(** [edge_or_vertex_crossing t c d] is the containment-counting companion to
    {!crossing_sign}. Returns [true] when AB and CD cross at an interior point, or when
    they share a vertex and {!S2_edge_crossings.vertex_crossing} says the shared vertex
    counts as a crossing. Designed so that point-in-polygon tests can be implemented by
    counting how many edges return [true]. *)
val edge_or_vertex_crossing : t -> S2_point.t -> S2_point.t -> with_bool

(** Chain variant of {!edge_or_vertex_crossing}. *)
val chain_edge_or_vertex_crossing : t -> S2_point.t -> with_bool

(** [signed_edge_or_vertex_crossing t c d] is like {!edge_or_vertex_crossing} but returns
    a signed value: [-1] if AB crosses CD from left to right (AB exits the region bounded
    by CD under the "interior on the left" convention), [+1] from right to left (AB
    enters), and [0] otherwise. Summing this over the edges of a loop gives the change in
    winding number from A to B. *)
val signed_edge_or_vertex_crossing : t -> S2_point.t -> S2_point.t -> with_sign

(** Chain variant of {!signed_edge_or_vertex_crossing}. *)
val chain_signed_edge_or_vertex_crossing : t -> S2_point.t -> with_sign

(** [last_interior_crossing_sign t] returns the sign of the most recent interior crossing:
    [-1] for left-to-right, [+1] for right-to-left. Only defined when the immediately
    preceding {!crossing_sign} or {!chain_crossing_sign} returned [+1]; otherwise the
    return value is unspecified. *)
val last_interior_crossing_sign : t -> int
