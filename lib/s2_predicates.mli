(** Robust geometric predicates on the unit sphere.

    These predicates compute conservative error bounds using floating-point arithmetic and
    fall back to more stable formulas when triage is inconclusive. When even the stable
    formula is ambiguous, the implementation falls back to arbitrary-precision arithmetic
    (a small internal bignum / dyadic-rational module) plus symbolic perturbations,
    matching the canonical C++ and Go S2 ports. Results are self-consistent: e.g.
    {!robust_sign} always satisfies [robust_sign a b c = -(robust_sign c b a)]. *)

open Core

(** {1 Orientation} *)

(** The relative orientation of three points on the sphere. *)
module Direction : sig
  type t =
    | Clockwise (** -1 *)
    | Counter_clockwise (** +1 *)
    | Indeterminate (** 0 *)

  val to_int : t -> int
  val of_int : int -> t
end

(** [sign a b c] returns [true] iff [a], [b], [c] are strictly counter-clockwise on the
    unit sphere. Computed as [(c x a) . b > 0]. This matches Go's [s2.Sign]; use
    {!robust_sign} when you also need to distinguish clockwise from collinear. *)
val sign : S2_point.t -> S2_point.t -> S2_point.t -> bool

(** [robust_sign a b c] returns the orientation of [a], [b], [c] as a {!Direction.t}. It
    returns {!Direction.Indeterminate} only when two of the points are equal. Otherwise it
    uses symbolic perturbations so that the following identities always hold:
    - [robust_sign b c a = robust_sign a b c]
    - [robust_sign c b a = -robust_sign a b c] *)
val robust_sign : S2_point.t -> S2_point.t -> S2_point.t -> Direction.t

(** [ordered_ccw a b c o] returns [true] iff the edges [OA], [OB], [OC] are encountered in
    that order while sweeping CCW around [o]. Equivalent to testing whether [B] is
    contained in the closed range of angles that starts at [A] and extends
    counter-clockwise to [C]. *)
val ordered_ccw : S2_point.t -> S2_point.t -> S2_point.t -> S2_point.t -> bool

(** {1 Distance comparisons} *)

(** [compare_distances x a b] returns -1, 0, or +1 according to whether
    [d(A, X) < d(B, X)], [A == B], or [d(A, X) > d(B, X)], where distances are measured as
    though [x], [a], [b] lay exactly on the unit sphere. Symbolic perturbations ensure the
    result is non-zero whenever [a <> b]. *)
val compare_distances : S2_point.t -> S2_point.t -> S2_point.t -> int

(** [compare_distance x y r] returns -1, 0, or +1 according to whether [d(X, Y)] is less
    than, equal to, or greater than [r]. *)
val compare_distance : S2_point.t -> S2_point.t -> S1_chord_angle.t -> int

(** {1 Dot products} *)

(** [sign_dot_prod a b] returns the exact sign of the dot product of [a] and [b]. Both
    inputs must satisfy [|a|^2 <= 2] and [|b|^2 <= 2]. *)
val sign_dot_prod : S2_point.t -> S2_point.t -> int
