(** Robust predicates and intersection computations for geodesic edges on the sphere.

    Given points on the unit sphere, the functions here answer questions such as:

    - Is triangle ABC wound counter-clockwise? ({!sign})
    - Do edges AB and CD cross at an interior point? ({!crossing_sign})
    - Where do they cross? ({!get_intersection})
    - Do three consecutive polygon edges cover their shared vertex?
      ({!angle_contains_vertex})

    Every predicate uses an exact, consistent perturbation model: no three points are ever
    treated as exactly collinear, and the results stay mutually consistent across related
    calls. This is what makes predicates like point-in-polygon containment work without
    special-casing degenerate inputs.

    Performance note: {!sign} uses a three-tier strategy internally - a fast
    floating-point triage, a stable determinant fallback, and finally an exact-arithmetic
    + symbolic-perturbation path. {!triage_sign} and {!expensive_sign} expose the
      individual tiers for callers that want to interleave their own work between them
      (see {!S2_edge_crosser}).

    When testing one fixed edge against many other edges, prefer {!S2_edge_crosser}: it
    caches state across calls and is substantially faster.

    All functions are pure and thread-safe.

    {2 Limitations}

    Robust cross-product utilities live in {!S2_point} (see {!S2_point.robust_cross_prod})
    rather than being re-exported here. *)

open Core

[@@@zero_alloc all]

(** {1 Orientation predicate}

    The three functions below form a cascade: {!sign} calls {!triage_sign} first and falls
    back to {!expensive_sign} on indeterminate triage. Use the tier functions directly
    only when you need to interleave your own work with the fallback. *)

(** [sign a b c] is the robust 3D orientation predicate. Returns [+1] when A, B, C are in
    counter-clockwise order (as seen from outside the sphere), [-1] for clockwise, and [0]
    only when two of the arguments are equal. The result is symmetric under cyclic
    rotation and flips sign when exactly two arguments are swapped. Thanks to the internal
    symbolic perturbation, distinct collinear points still receive a consistent non-zero
    orientation. *)
val sign : S2_point.t -> S2_point.t -> S2_point.t -> int

(** [triage_sign a b c] is the fast floating-point triage used by {!sign}. Returns [+1] or
    [-1] when the signed determinant [(a x b) . c] is definitely outside the rounding
    envelope, and [0] when it is inside it. A caller receiving [0] should fall through to
    {!expensive_sign}. *)
val triage_sign : S2_point.t -> S2_point.t -> S2_point.t -> int

(** [expensive_sign a b c] is the slow path of {!sign}: it runs a stable determinant
    formulation and, on exact zero, falls through to an exact-arithmetic computation
    combined with Edelsbrunner-Muecke simulation-of-simplicity perturbations. Guarantees a
    non-zero result whenever the three points are distinct; returns [0] only when two
    arguments are equal. *)
val expensive_sign : S2_point.t -> S2_point.t -> S2_point.t -> int

(** {1 Ordering} *)

(** [ordered_ccw a b c o] tests whether the directions OA, OB and OC are encountered in
    that order while sweeping counter-clockwise around [o]. Equivalently, B is contained
    in the closed angular sector that starts at A and extends counter-clockwise to C. *)
val ordered_ccw : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> bool

(** {1 Crossing predicates} *)

(** [crossing_sign a b c d] tests whether edge AB intersects edge CD:

    - [+1] when AB and CD cross at a point interior to both edges;
    - [0] when any two vertices from different edges coincide;
    - [-1] otherwise.

    Degenerate edges ([a = b] or [c = d]) return [0] if two vertices coincide across edges
    and [-1] otherwise. Obeys:

    - [crossing_sign b a c d = crossing_sign a b c d]
    - [crossing_sign c d a b = crossing_sign a b c d]

    For repeated tests of one fixed edge against many others, {!S2_edge_crosser} is
    substantially faster. *)
val crossing_sign : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> int

(** [vertex_crossing a b c d] resolves the tie-breaking question that arises when AB and
    CD share a vertex: does that shared-vertex configuration count as a crossing for the
    purpose of counting-based point-in-polygon tests? The "crossing" rule is that AB is
    encountered after CD during a counter-clockwise sweep around the shared vertex
    starting from a fixed reference direction.

    Precondition: AB and CD must share at least one vertex (i.e.
    [crossing_sign a b c d = 0] after accounting for coincident endpoints). It is an error
    to pass four distinct vertices; no guarantees are made in that case. *)
val vertex_crossing : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> bool

(** [edge_or_vertex_crossing a b c d] returns [true] exactly when AB and CD cross at an
    interior point, or when they share a vertex and {!vertex_crossing} considers the
    shared configuration a crossing. Designed so that a point-in-polygon containment test
    can be written as "count how many polygon edges this test edge crosses" without any
    special-case logic for coincident endpoints. *)
val edge_or_vertex_crossing : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> bool

(** [signed_vertex_crossing a b c d] is a signed analogue of {!vertex_crossing} used when
    AB and CD share at least one vertex. Returns [-1] when AB crosses CD from left to
    right, [+1] when AB crosses CD from right to left, and [0] when the shared-vertex
    configuration does not count as a crossing under the perturbation rules. If CD bounds
    a region with the "interior is on the left" convention, the result is [-1] when AB
    exits the region and [+1] when AB enters it - making it suitable for accumulating
    winding-number deltas along a path.

    Useful identities:
    - [signed_vertex_crossing a a c d = 0]
    - [signed_vertex_crossing a b c c = 0]
    - [signed_vertex_crossing a b a b = 1]
    - [signed_vertex_crossing a b b a = -1]
    - [signed_vertex_crossing b a c d = - signed_vertex_crossing a b c d]
    - [signed_vertex_crossing a b d c = - signed_vertex_crossing a b c d]

    Precondition: AB and CD must share at least one vertex; the four-distinct-vertices
    case is undefined and returns [0]. *)
val signed_vertex_crossing : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> int

(** {1 Angle predicates} *)

(** [angle_contains_vertex a b c] tests whether the angle ABC (with vertex B) contains its
    vertex. Containment is defined so that when several polygons tile the neighborhood of
    a vertex, exactly one of them claims it. Returns [false] for degenerate angles of the
    form ABA.

    Properties:

    - [angle_contains_vertex a b a = false]
    - [angle_contains_vertex a b c = not (angle_contains_vertex c b a)] unless [a = c]
    - Given [v1 ... vk] in counter-clockwise order around [b],
      [angle_contains_vertex v(i+1) b vi] is [true] for exactly one [i].

    Does not handle polygons with repeated vertices; use a dedicated vertex-containment
    query for that case.

    Requires: [a <> b] and [b <> c]. *)
val angle_contains_vertex : S2_point.t -> S2_point.t -> S2_point.t -> bool

(** {1 Intersection} *)

(** [get_intersection a0 a1 b0 b1] returns the point where edges A0A1 and B0B1 cross. The
    result lies within {!intersection_error} of the true intersection, even when the edges
    meet at a very small angle. Satisfies
    [get_intersection a0 a1 b0 b1 = get_intersection b0 b1 a0 a1] and is invariant under
    reversing either edge.

    Precondition: the edges must actually cross, i.e. [crossing_sign a0 a1 b0 b1 > 0]. *)
val get_intersection : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t

(** {1 Edge ordering} *)

(** [compare_edges a0 a1 b0 b1] is a strict total order on undirected edges: [true] iff
    the edge
    {[
      A0, A1
    ]}
    sorts strictly before
    {[
      B0, B1
    ]}
    after canonicalizing each endpoint pair so the smaller point comes first. The result
    is invariant under reversing either edge; comparing an undirected edge against itself
    always returns [false]. *)
val compare_edges : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> bool

(** {1 Error bounds} *)

(** Upper bound on the angular distance between the point returned by {!get_intersection}
    and the true mathematical intersection. Equal to [8 * 0.5 * epsilon_float] radians. *)
val intersection_error : S1_angle.t
[@@zero_alloc ignore]

(** Radius that can be used as an [S2Builder] snap radius to re-merge edges that have been
    displaced by up to {!intersection_error} (for example because the same geometry was
    intersected against overlapping tiles and then unioned). Equal to
    [2 * intersection_error] since input edges may have been displaced in opposite
    directions. *)
val intersection_merge_radius : S1_angle.t
[@@zero_alloc ignore]
