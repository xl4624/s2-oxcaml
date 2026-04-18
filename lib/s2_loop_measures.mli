(** Angle and area measures for loops on the sphere.

    These are low-level functions that work directly with an array of {!S2_point.t}
    vertices. A loop is represented implicitly: the edges are
    [(v.(0), v.(1)), (v.(1), v.(2)), ..., (v.(n - 1), v.(0))]. The empty array represents
    the full loop (containing all points on the sphere). *)

open Core

(** {1 Perimeter, area, and centroid} *)

(** [perimeter loop] returns the sum of the lengths of the loop's edges. *)
val perimeter : S2_point.t array -> S1_angle.t

(** [area loop] returns the area of the loop interior, i.e. the region on the left side of
    the loop. The result is between [0] and [4 * pi] steradians. The implementation
    ensures that nearly-degenerate clockwise loops have areas close to [0], while
    nearly-degenerate counter-clockwise loops have areas close to [4 * pi]. *)
val area : S2_point.t array -> float#

(** [approx_area loop] is like {!area}, except that it is faster and has more error. The
    result is between [0] and [4 * pi] steradians. The maximum error is about [2.22e-15]
    per vertex. See {!curvature_max_error}. *)
val approx_area : S2_point.t array -> float#

(** [signed_area loop] returns either the positive area of the region on the left of the
    loop, or the negative area of the region on its right, whichever is smaller in
    magnitude. The result is in [[-2 * pi, 2 * pi]] (except for the full loop, whose area
    is [-min_positive_normal_value]).

    - Counter-clockwise loops have positive area; clockwise loops have negative area.
    - Degenerate loops (single vertex or all sibling edge pairs) have area exactly [0].
    - The full loop (represented by the empty array) has area
      [-min_positive_normal_value], the signed-equivalent of [4 * pi]. *)
val signed_area : S2_point.t array -> float#

(** [curvature loop] returns the geodesic curvature of the loop, i.e. the sum of the turn
    angles at each vertex. The result is positive for counter-clockwise loops, negative
    for clockwise loops, and zero for great circles. Equals [2 * pi - area].

    - Degenerate loops have a curvature of exactly [2 * pi].
    - The full loop (represented by the empty array) has a curvature of exactly [-2 * pi]. *)
val curvature : S2_point.t array -> float#

(** [curvature_max_error loop] returns the maximum error in {!curvature} for the given
    loop. This value is also an upper bound on the error in {!area}, {!signed_area}, and
    {!approx_area}. *)
val curvature_max_error : S2_point.t array -> float#

(** [centroid loop] returns the true centroid of the loop multiplied by its area (see
    {!S2_centroids.true_centroid} for details on centroids). The result is not unit
    length, and the centroid may not be contained by the loop. *)
val centroid : S2_point.t array -> S2_point.t

(** [is_normalized loop] returns [true] iff the loop area is at most [2 * pi]. A small
    error is allowed so that hemispheres are always considered normalized. *)
val is_normalized : S2_point.t array -> bool

(** {1 Canonical loop order and pruning} *)

(** A cyclic ordering of the loop vertices, starting at index [first] and proceeding in
    direction [dir] (either [+1] or [-1]). *)
type loop_order =
  { first : int
  ; dir : int
  }
[@@deriving sexp_of]

(** [canonical_loop_order loop] returns a {!loop_order} value that is invariant under
    rotations and reversals of the input vertex sequence. This lets callers traverse the
    loop in a canonical order. *)
val canonical_loop_order : S2_point.t array -> loop_order

(** [prune_degeneracies loop] returns a new array obtained by removing all degeneracies
    that can be detected by comparing adjacent vertices and edges for equality (i.e.
    removing subsequences of the form [AA] or [ABA] repeatedly until none remain). A loop
    of length 0, 1, or 2 is collapsed to an empty loop. *)
val prune_degeneracies : S2_point.t array -> S2_point.t array
