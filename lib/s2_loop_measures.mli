(** Low-level angle, area and centroid measures for loops on the unit sphere.

    Each function takes an {!S2_point.t} array [v] and treats it as a closed loop whose
    edges are [(v.(0), v.(1)), (v.(1), v.(2)), ..., (v.(n-1), v.(0))]. No per-loop object
    is constructed, which is why these helpers are convenient building blocks for
    higher-level types like {!S2_loop}, {!S2_polygon} or {!S2_lax_polygon}.

    Conventions:

    - An empty array is the "full loop" containing every point on the sphere.
    - A one-vertex loop is a point (degenerate single-edge loop).
    - Counter-clockwise loops have positive signed area and curvature; clockwise loops
      have negative area and curvature.
    - Areas are reported in steradians.

    {2 Numerical notes}

    [area], [signed_area], [centroid] use an oriented surface-integral decomposition with
    triangle-fan reorigination for stability near anti-podal edges, and a
    Kahan-compensated sum for the scalar accumulators. See {!curvature_max_error} for the
    per-loop error bound. *)

open Core

[@@@zero_alloc all]

(** {1 Perimeter, area, and centroid} *)

(** [perimeter loop] returns the sum of the geodesic lengths of the loop's edges. Returns
    {!S1_angle.zero} for loops with fewer than two vertices. *)
val perimeter : S2_point.t array -> S1_angle.t

(** [area loop] returns the area of the loop interior (the region on the loop's left side)
    in steradians, a value in [0, 4 * pi]. Nearly-degenerate clockwise loops have area
    close to [0]; nearly-degenerate counter-clockwise loops have area close to [4 * pi]. *)
val area : S2_point.t array -> float#
[@@zero_alloc ignore]

(** [approx_area loop] is like {!area} but cheaper and with more rounding error. The
    result is in [0, 4 * pi] steradians. The per-vertex error is about [2.22e-15] and
    {!curvature_max_error} provides the exact bound. *)
val approx_area : S2_point.t array -> float#
[@@zero_alloc ignore]

(** [signed_area loop] returns the signed area of the loop, i.e. either the positive area
    of the region on the left or the negative area of the region on the right, whichever
    has smaller magnitude. The result lies in [-2 * pi, 2 * pi], except:

    - counter-clockwise loops have strictly positive area, clockwise loops strictly
      negative area;
    - degenerate loops (a single vertex or all sibling edge pairs) return exactly [0];
    - the full loop (empty array) returns [-min_positive_normal_value], used as the
      signed-equivalent of [4 * pi]. *)
val signed_area : S2_point.t array -> float#
[@@zero_alloc ignore]

(** [curvature loop] returns the geodesic curvature of the loop: the sum of the turn
    angles at each vertex (see {!S2_measures.turn_angle}). It is positive for
    counter-clockwise loops, negative for clockwise loops, and zero for great circles, and
    satisfies [curvature loop = 2 * pi - area loop] for valid loops.

    Special cases:

    - degenerate loops return exactly [2 * pi];
    - the full loop (empty array) returns exactly [-2 * pi];
    - for every other loop the result lies strictly in [(-2 * pi, 2 * pi)], and reversing
      the vertex order negates the result. *)
val curvature : S2_point.t array -> float#
[@@zero_alloc ignore]

(** [curvature_max_error loop] returns the maximum rounding error in {!curvature} for this
    loop. The same value is an upper bound for the error in {!area}, {!signed_area}, and
    {!approx_area}. *)
val curvature_max_error : S2_point.t array -> float#

(** [centroid loop] returns the true centroid of the loop scaled by its area (see
    {!S2_centroids} for the definition). The result is not unit length, and the centroid
    need not lie inside the loop. Scaling by area makes it possible to combine the
    centroids of disjoint pieces by simple vector addition. *)
val centroid : S2_point.t array -> S2_point.t
[@@zero_alloc ignore]

(** [is_normalized loop] is [true] iff the loop area is at most [2 * pi]. A small
    tolerance is allowed so that hemispheres are always considered normalized. Degenerate
    loops are handled consistently with orientation-level sign tests: a loop that can be
    expressed as a union of nearly-degenerate counter-clockwise triangles returns [true]. *)
val is_normalized : S2_point.t array -> bool
[@@zero_alloc ignore]

(** {1 Canonical loop order and pruning} *)

(** A cyclic ordering of a loop's vertices, starting at index [first] and stepping in
    direction [dir] (either [+1] or [-1]). The sequence of [n] vertices visited is
    [(first, first + dir, ..., first + (n - 1) * dir)], interpreted modulo [n]. *)
type loop_order =
  { first : int
  ; dir : int
  }
[@@deriving sexp_of]

val sexp_of_loop_order : loop_order -> Sexp.t [@@zero_alloc ignore]

(** [canonical_loop_order loop] returns a {!loop_order} whose vertex sequence is invariant
    under rotations and reversals of the input. Two loops that represent the same ordered
    set of vertices (up to cyclic shift and direction) yield identical results, so callers
    can traverse the loop in a canonical order for hashing or equality. *)
val canonical_loop_order : S2_point.t array -> loop_order
[@@zero_alloc ignore]

(** [prune_degeneracies loop] returns a new array obtained by repeatedly removing any
    adjacent pair [AA] (collapsed to [A]) or triple [ABA] (collapsed to [A]) until no such
    patterns remain, including matches that wrap across the loop boundary. A loop that
    collapses to fewer than three vertices is returned as the empty array.

    The pruned result is unique up to cyclic permutation regardless of the pruning order.

    Caveat: functions in this module can return different answers on the pruned loop than
    on the original. In particular, a non-empty but fully degenerate loop has {!curvature}
    equal to [2 * pi] before pruning and [-2 * pi] after pruning, because pruning
    collapses it to the empty array which represents the full sphere. *)
val prune_degeneracies : S2_point.t array -> S2_point.t array
[@@zero_alloc ignore]
