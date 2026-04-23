(** Robust geometric predicates on the unit sphere.

    Every predicate in this module returns a correct, self-consistent answer: the result
    does not flicker as the inputs are perturbed near the "decision boundary" in floating
    point. Each call first performs a cheap floating-point triage with an explicit error
    bound. When triage is inconclusive the predicate falls back to a numerically stable
    formula, and finally to arbitrary-precision arithmetic plus symbolic perturbation
    (Simulation of Simplicity - Edelsbrunner and Muecke 1990) if needed. The symbolic
    layer guarantees that each predicate returns a non-zero sign whenever its inputs are
    distinct. Consequently identities such as
    - [robust_sign a b c = -(robust_sign c b a)]
    - [robust_sign a b c = robust_sign b c a] hold for all inputs.

    All functions assume inputs lie on (or very near) the unit sphere. They are
    allocation-free.

    Limitations: the following upstream predicates are not yet ported -
    - [CompareEdgeDistance], [CompareEdgePairDistance], [CompareEdgeDirections]
    - [CircleEdgeIntersectionSign], [CircleEdgeIntersectionOrdering]
    - [EdgeCircumcenterSign], [GetVoronoiSiteExclusion]
    - the lower-level [TriageSign], [ExpensiveSign], [UnperturbedSign] entry points. *)

open Core

[@@@zero_alloc all]

(** {1 Orientation} *)

(** The relative orientation of three points on the sphere. *)
module Direction : sig
  type t =
    | Clockwise (** [-1]: the points are in clockwise order. *)
    | Counter_clockwise (** [+1]: the points are in counter-clockwise order. *)
    | Indeterminate (** [0]: two or more of the points coincide. *)

  (** [to_int d] returns [-1], [0], or [+1] for {!Clockwise}, {!Indeterminate},
      {!Counter_clockwise} respectively. *)
  val to_int : t -> int

  (** [of_int n] maps negative integers to {!Clockwise}, positive to {!Counter_clockwise},
      and zero to {!Indeterminate}. *)
  val of_int : int -> t
end

(** [sign a b c] returns [true] iff [a], [b], [c] are strictly counter-clockwise on the
    unit sphere, evaluated as [(c x a) . b > 0]. This is the fast non-robust test: it can
    disagree with {!robust_sign} when the three points are near-collinear. Prefer
    {!robust_sign} when you need to distinguish CCW, CW, and collinear, or when you need
    self-consistent answers across related predicates. *)
val sign : S2_point.t -> S2_point.t -> S2_point.t -> bool

(** [robust_sign a b c] returns the orientation of [a], [b], [c] as a {!Direction.t}. It
    returns {!Direction.Indeterminate} only when two of the arguments are equal; in every
    other case symbolic perturbations force a non-zero result. The following identities
    hold for all inputs:
    - [robust_sign b c a = robust_sign a b c] (cyclic rotation preserves the sign)
    - [robust_sign c b a = -robust_sign a b c] (swapping two arguments inverts it) *)
val robust_sign : S2_point.t -> S2_point.t -> S2_point.t -> Direction.t

(** [ordered_ccw a b c o] returns [true] iff the directed edges [OA], [OB], [OC] are
    encountered in that order when sweeping counter-clockwise around [o]. Equivalently,
    [b] lies in the closed range of angles starting at [a] and extending CCW to [c].
    Requires [a], [b], [c] all distinct from [o]. Returns [true] when [a = b] or [b = c]
    (the range collapses to a single direction including [b]) and [false] when [a = c] and
    [b] is distinct (the range is empty). *)
val ordered_ccw : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> bool

(** {1 Distance comparisons} *)

(** [compare_distances x a b] returns [-1], [0], or [+1] according to whether
    [d(A, X) < d(B, X)], [A = B], or [d(A, X) > d(B, X)]. Distances are measured as though
    [x], [a], [b] were first reprojected to lie exactly on the unit sphere, and symbolic
    perturbations force a non-zero result whenever [a <> b] (even when [d(A, X) = d(B, X)]
    exactly). Self-consistent: if [AX < BX] and [BX < CX] then [AX < CX]. *)
val compare_distances : S2_point.t -> S2_point.t -> S2_point.t -> int

(** [compare_distance x y r] returns [-1], [0], or [+1] according to whether the chord
    distance between [x] and [y] is less than, equal to, or greater than [r]. Distances
    are measured as though [x] and [y] lay exactly on the unit sphere. *)
val compare_distance : S2_point.t -> S2_point.t -> S1_chord_angle.t -> int

(** {1 Dot products} *)

(** [sign_dot_prod a b] returns [-1], [0], or [+1] - the exact sign of [a . b]. Both
    inputs must satisfy [|a|^2 <= 2] and [|b|^2 <= 2]; this admits unit points and their
    sums but not larger vectors. *)
val sign_dot_prod : S2_point.t -> S2_point.t -> int
