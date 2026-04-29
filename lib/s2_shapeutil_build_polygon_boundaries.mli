(** Group connected loop components into polygons (faces) by inferring their nesting
    hierarchy.

    The input is a collection of {e components}, where each component is a list of loop
    shapes that together form a subdivision of the sphere (or a single degenerate loop).
    Loops in different components must have disjoint boundaries.

    The output is a list of {e faces}: each face is a list of shape ids referring back to
    the input shapes. Within each face except the trailing one, the leading ids are the
    component's "outer" loops (loops that do not contain {!S2_pointutil.origin}) and the
    final id is the indexed loop that encloses them. The trailing face contains every
    depth-zero outer loop.

    {b Shape ids} are assigned to input shapes in flattened input order: the first shape
    of the first component receives id [0], the next sibling [1], and so on across
    components. Use {!flat_id_offsets} when you need the mapping back to your own data
    structure.

    {!S2_shape.t} is an unboxed record, so it cannot inhabit a polymorphic container
    directly. {!Loop.t} is a thin value-layout wrapper for that purpose. *)

module Loop : sig
  type t

  (** [of_shape shape] wraps [shape] so it can be stored in a list or array. *)
  val of_shape : S2_shape.t -> t

  (** [shape t] retrieves the underlying shape. *)
  val shape : t -> S2_shape.t
end

(** [build_polygon_boundaries components] returns the list of faces. Each face is a list
    of flat shape ids. The total number of faces equals [(number of indexed loops) + 1];
    an indexed loop is any loop in a multi-loop component that does not contain
    {!S2_pointutil.origin}.

    The empty input returns the empty list.

    Required input invariants (mirroring the upstream contract):
    - The loops in each component subdivide the sphere, except a component may consist of
      a single degenerate loop.
    - The boundaries of different components are disjoint (no crossings, no shared
      vertices).
    - No component is empty and no loop has zero edges. *)
val build_polygon_boundaries : Loop.t list list -> int list list

(** [flat_id_offsets components] returns an array [offsets] of length
    [List.length components + 1] where [offsets.(i)] is the flat id of the first shape in
    component [i] (and [offsets.(n)] is the total shape count). Useful for translating
    face ids back to [(component_id, offset)] pairs. *)
val flat_id_offsets : Loop.t list list -> int array
