open Core
module V = R3_vector

(* Shared with [S2_point.robust_cross_prod]; factored into [Exact_arith] so
   the big-integer and dyadic-rational scaffolding is not duplicated. *)
module Bigint = Exact_arith.Bigint
module Dyadic = Exact_arith.Dyadic
module Exact_vec = Exact_arith.Exact_vec

(* Rounding epsilon for IEEE float64: 2^-53. *)
let[@inline] [@zero_alloc] dbl_error () = Float_u.O.(Float_u.epsilon_float / #2.0)

(* Conservative upper bound on the error in evaluating [(A x B) . C] in
   floating point for unit-length inputs. Derivation: see s2predicates.h
   lines 376-392; for vectors of magnitude <= sqrt(2) the bound doubles. *)
let[@inline] [@zero_alloc] max_determinant_error () =
  let open Float_u.O in
  #1.8274 * Float_u.epsilon_float
;;

(* Stable-sign error scaling: [det_error_multiplier * sqrt(|e1|^2 * |e2|^2)]
   bounds the determinant error when the two shortest edges of the triangle
   are used for the cross product. See s2predicates.cc::StableSign. *)
let[@inline] [@zero_alloc] det_error_multiplier () =
  let open Float_u.O in
  #3.2321 * Float_u.epsilon_float
;;

module Direction = struct
  type t =
    | Clockwise
    | Counter_clockwise
    | Indeterminate

  let[@inline] [@zero_alloc] to_int = function
    | Clockwise -> -1
    | Indeterminate -> 0
    | Counter_clockwise -> 1
  ;;

  let[@inline] [@zero_alloc] of_int n =
    if n < 0 then Clockwise else if n > 0 then Counter_clockwise else Indeterminate
  ;;
end

let[@inline] [@zero_alloc] sign a b c =
  let open Float_u.O in
  (* (C x A) . B > 0. *)
  V.dot (V.cross c a) b > #0.0
;;

(* Fast floating-point triage: compute det = (A x B) . C and compare to the
   conservative error bound. Resolves most callsites without any fallback. *)
let[@inline] [@zero_alloc] triage_sign a b c =
  let open Float_u.O in
  let det = V.dot (V.cross a b) c in
  if det > max_determinant_error ()
  then Direction.Counter_clockwise
  else if det < Float_u.neg (max_determinant_error ())
  then Direction.Clockwise
  else Direction.Indeterminate
;;

(* Errors smaller than [min_no_underflow_error] cannot be trusted because the
   intermediate product [e1n2 * e2n2] has already underflowed to zero. Without this
   guard the stable-sign formula returns [Indeterminate] instead of [Clockwise] for
   nearly collinear points whose edge norms are near [DBL_MIN]. *)
let[@inline] [@zero_alloc] min_no_underflow_error () =
  let open Float_u.O in
  (* DBL_MIN = 2.2250738585072014e-308 *)
  det_error_multiplier () * Float_u.sqrt (Float_u.min_positive_normal_value)
;;

(* Second-level stable formula. Uses the two shortest edges of the triangle
   instead of [A x B] so the rounding error scales with the shortest
   magnitudes rather than with [|A x B|]. Handles near-collinear points that
   triage cannot decide. *)
let[@inline] [@zero_alloc] stable_sign a b c =
  let open Float_u.O in
  let ab = V.sub b a in
  let ab2 = V.norm2 ab in
  let bc = V.sub c b in
  let bc2 = V.norm2 bc in
  let ca = V.sub a c in
  let ca2 = V.norm2 ca in
  (* Pick the longest edge and set (e1, e2, op) to the two shorter edges
     plus the point opposite the longest edge. *)
  let #(e1, e2, op) =
    if ab2 >= bc2 && ab2 >= ca2
    then #(ca, bc, c)
    else if bc2 >= ca2
    then #(ab, ca, a)
    else #(bc, ab, b)
  in
  let det = Float_u.neg (V.dot (V.cross e1 e2) op) in
  let max_err = det_error_multiplier () * Float_u.sqrt (V.norm2 e1 * V.norm2 e2) in
  if max_err < min_no_underflow_error ()
  then Direction.Indeterminate
  else if det > max_err
  then Direction.Counter_clockwise
  else if det < Float_u.neg max_err
  then Direction.Clockwise
  else Direction.Indeterminate
;;

(* Final tiebreak when the exact determinant is zero. Assigns each coordinate
   a distinct infinitesimal perturbation and enumerates the resulting
   polynomial coefficients in order of decreasing magnitude (Simulation of
   Simplicity, Edelsbrunner and Muecke 1990). Assumes the arguments have been
   sorted into lex order xa < xb < xc. The cascade of [if s <> 0 then s] tests
   is inlined rather than expressed as a higher-order combinator to stay
   zero-alloc. *)
let[@zero_alloc] symbolically_perturbed_sign
  (a : Exact_vec.t @ local)
  (b : Exact_vec.t @ local)
  (c : Exact_vec.t @ local)
  (b_cross_c : Exact_vec.t @ local)
  =
  let module D = Dyadic in
  (* Cascade of sign tests; first non-zero wins. Written as nested ifs rather
     than a closure-based combinator so the body stays zero-alloc. *)
  let s = D.sign b_cross_c.#z in
  (* da.Z *)
  if s <> 0
  then s
  else (
    let s = D.sign b_cross_c.#y in
    (* da.Y *)
    if s <> 0
    then s
    else (
      let s = D.sign b_cross_c.#x in
      (* da.X *)
      if s <> 0
      then s
      else (
        let s = D.sign (D.sub (D.mul c.#x a.#y) (D.mul c.#y a.#x)) in
        (* db.Z *)
        if s <> 0
        then s
        else (
          let s = D.sign c.#x in
          (* db.Z * da.Y *)
          if s <> 0
          then s
          else (
            let s = -D.sign c.#y in
            (* db.Z * da.X *)
            if s <> 0
            then s
            else (
              let s = D.sign (D.sub (D.mul c.#z a.#x) (D.mul c.#x a.#z)) in
              (* db.Y *)
              if s <> 0
              then s
              else (
                let s = D.sign c.#z in
                (* db.Y * da.X *)
                if s <> 0
                then s
                else (
                  let s = D.sign (D.sub (D.mul a.#x b.#y) (D.mul a.#y b.#x)) in
                  (* dc.Z *)
                  if s <> 0
                  then s
                  else (
                    let s = -D.sign b.#x in
                    (* dc.Z * da.Y *)
                    if s <> 0
                    then s
                    else (
                      let s = D.sign b.#y in
                      (* dc.Z * da.X *)
                      if s <> 0
                      then s
                      else (
                        let s = D.sign a.#x in
                        (* dc.Z * db.Y *)
                        if s <> 0 then s else 1 (* dc.Z * db.Y * da.X *))))))))))))
;;

(* Arbitrary-precision determinant evaluation plus symbolic perturbation.
   Requires pairwise-distinct inputs. Permutation sign tracking is needed
   because the symbolic perturbation step below requires its inputs in lex
   order. *)
let[@zero_alloc] exact_sign a b c ~perturb =
  (* Sort (a, b, c) lexicographically, tracking the sign of the permutation.
     R3_vector.t has an unboxed layout, so we can't stash it in an ordinary
     tuple; unboxed tuples keep the sort allocation-free. *)
  let #(pa, pb, perm) = if V.compare a b > 0 then #(b, a, -1) else #(a, b, 1) in
  let #(pb, pc, perm) = if V.compare pb c > 0 then #(c, pb, -perm) else #(pb, c, perm) in
  let #(pa, pb, perm) =
    if V.compare pa pb > 0 then #(pb, pa, -perm) else #(pa, pb, perm)
  in
  let xa = Exact_vec.of_r3 pa in
  let xb = Exact_vec.of_r3 pb in
  let xc = Exact_vec.of_r3 pc in
  let xb_cross_xc = Exact_vec.cross xb xc in
  let det_sign = Dyadic.sign (Exact_vec.dot xa xb_cross_xc) in
  let result =
    if det_sign <> 0
    then det_sign
    else if perturb
    then symbolically_perturbed_sign xa xb xc xb_cross_xc
    else 0
  in
  perm * result
;;

let[@zero_alloc] robust_sign a b c =
  let s = triage_sign a b c in
  match s with
  | Counter_clockwise | Clockwise -> s
  | Indeterminate ->
    (* Return zero if and only if two of the points are equal. This lets us
       assume distinct inputs in [exact_sign] below. *)
    if V.equal a b || V.equal b c || V.equal c a
    then Direction.Indeterminate
    else (
      match stable_sign a b c with
      | (Counter_clockwise | Clockwise) as s -> s
      | Indeterminate -> Direction.of_int (exact_sign a b c ~perturb:true))
;;

let[@zero_alloc] ordered_ccw a b c o =
  let mutable sum = 0 in
  (match robust_sign b o a with
   | Clockwise -> ()
   | _ -> sum <- sum + 1);
  (match robust_sign c o b with
   | Clockwise -> ()
   | _ -> sum <- sum + 1);
  (match robust_sign a o c with
   | Counter_clockwise -> sum <- sum + 1
   | _ -> ());
  sum >= 2
;;

(* cosDistance returns (cos, err) where cos = x.y and err is the bound. *)
let[@inline] [@zero_alloc] cos_distance_err x y =
  let open Float_u.O in
  let cos = V.dot x y in
  let err = (#9.5 * dbl_error () * Float_u.abs cos) + (#1.5 * dbl_error ()) in
  #(cos, err)
;;

(* sin2Distance returns (sin2, err) via the (x-y) x (x+y) trick. *)
let[@inline] [@zero_alloc] sin2_distance_err x y =
  let open Float_u.O in
  let n = V.cross (V.sub x y) (V.add x y) in
  let sin2 = #0.25 * V.norm2 n in
  let sqrt3 = #1.7320508075688772 in
  let err =
    (#21.0 * dbl_error () * sin2)
    + (#4.0 * sqrt3 * dbl_error () * sin2)
    + (#32.0 * sqrt3 * dbl_error () * dbl_error () * Float_u.sqrt sin2)
    + (#768.0 * dbl_error () * dbl_error () * dbl_error () * dbl_error ())
  in
  #(sin2, err)
;;

let[@inline] [@zero_alloc] triage_compare_cos_distances x a b =
  let open Float_u.O in
  let #(cos_ax, err_ax) = cos_distance_err a x in
  let #(cos_bx, err_bx) = cos_distance_err b x in
  let diff = cos_ax - cos_bx in
  let err = err_ax + err_bx in
  if diff > err then -1 else if diff < Float_u.neg err then 1 else 0
;;

let[@inline] [@zero_alloc] triage_compare_sin2_distances x a b =
  let open Float_u.O in
  let #(sin2_ax, err_ax) = sin2_distance_err a x in
  let #(sin2_bx, err_bx) = sin2_distance_err b x in
  let diff = sin2_ax - sin2_bx in
  let err = err_ax + err_bx in
  if diff > err then 1 else if diff < Float_u.neg err then -1 else 0
;;

(* Symbolic perturbation for a != b with equal projected distance: if A < B
   lexicographically then A's pedestal is higher, so AX > BX, so return -1. *)
let[@inline] [@zero_alloc] symbolic_compare_distances _x a b =
  let c = V.compare a b in
  if c < 0 then 1 else if c > 0 then -1 else 0
;;

(* Arbitrary-precision comparison of the distances from [x] to [a] and from
   [x] to [b]. Reduces to a sign test on a polynomial in the exact
   coordinates (no trig, no square roots). *)
let[@zero_alloc] exact_compare_distances x a b =
  let module D = Dyadic in
  let xv = Exact_vec.of_r3 x in
  let av = Exact_vec.of_r3 a in
  let bv = Exact_vec.of_r3 b in
  let cos_ax = Exact_vec.dot xv av in
  let cos_bx = Exact_vec.dot xv bv in
  let a_sign = D.sign cos_ax in
  let b_sign = D.sign cos_bx in
  if a_sign <> b_sign
  then if a_sign > b_sign then -1 else 1
  else (
    let cos_ax2 = D.mul cos_ax cos_ax in
    let cos_bx2 = D.mul cos_bx cos_bx in
    let an2 = Exact_vec.norm2 av in
    let bn2 = Exact_vec.norm2 bv in
    let cmp = D.sub (D.mul cos_bx2 an2) (D.mul cos_ax2 bn2) in
    a_sign * D.sign cmp)
;;

let[@zero_alloc] compare_distances x a b =
  let s = triage_compare_cos_distances x a b in
  if s <> 0
  then s
  else if V.equal a b
  then 0
  else (
    let cos_ax = V.dot a x in
    let inv_sqrt2 = #0.7071067811865476 in
    let s =
      let open Float_u.O in
      if cos_ax > inv_sqrt2
      then triage_compare_sin2_distances x a b
      else if cos_ax < Float_u.neg inv_sqrt2
      then Int.neg (triage_compare_sin2_distances x a b)
      else 0
    in
    if s <> 0
    then s
    else (
      let s = exact_compare_distances x a b in
      if s <> 0 then s else symbolic_compare_distances x a b))
;;

(* ca45Degrees length^2 = 2 - sqrt(2). *)
let[@inline] [@zero_alloc] ca_45_degrees_length2 () = #0.5857864376269049

let[@inline] [@zero_alloc] triage_compare_cos_distance x y r2 =
  let open Float_u.O in
  let #(cos_xy, err_xy) = cos_distance_err x y in
  let cos_r = #1.0 - (#0.5 * r2) in
  let err_r = #2.0 * dbl_error () * cos_r in
  let diff = cos_xy - cos_r in
  let err = err_xy + err_r in
  if diff > err then -1 else if diff < Float_u.neg err then 1 else 0
;;

let[@inline] [@zero_alloc] triage_compare_sin2_distance x y r2 =
  let open Float_u.O in
  let #(sin2_xy, err_xy) = sin2_distance_err x y in
  let sin2_r = r2 * (#1.0 - (#0.25 * r2)) in
  let err_r = #3.0 * dbl_error () * sin2_r in
  let diff = sin2_xy - sin2_r in
  let err = err_xy + err_r in
  if diff > err then 1 else if diff < Float_u.neg err then -1 else 0
;;

(* Arbitrary-precision comparison of [d(X, Y)] against a chord angle whose
   squared length is [r2]. *)
let[@zero_alloc] exact_compare_distance x y r2 =
  let module D = Dyadic in
  let xv = Exact_vec.of_r3 x in
  let yv = Exact_vec.of_r3 y in
  let cos_xy = Exact_vec.dot xv yv in
  let r2d = D.of_float r2 in
  let one = D.of_float #1.0 in
  let half = D.of_float #0.5 in
  let cos_r = D.sub one (D.mul half r2d) in
  let xy_sign = D.sign cos_xy in
  let r_sign = D.sign cos_r in
  if xy_sign <> r_sign
  then if xy_sign > r_sign then -1 else 1
  else (
    let cos_r2 = D.mul cos_r cos_r in
    let cos_xy2 = D.mul cos_xy cos_xy in
    let xn2 = Exact_vec.norm2 xv in
    let yn2 = Exact_vec.norm2 yv in
    let cmp = D.sub (D.mul cos_r2 (D.mul xn2 yn2)) cos_xy2 in
    xy_sign * D.sign cmp)
;;

let[@zero_alloc] compare_distance x y r =
  let r2 = S1_chord_angle.length2 r in
  let s = triage_compare_cos_distance x y r2 in
  if s <> 0
  then s
  else (
    let s =
      let open Float_u.O in
      if r2 < ca_45_degrees_length2 () then triage_compare_sin2_distance x y r2 else 0
    in
    if s <> 0 then s else exact_compare_distance x y r2)
;;

(* Triage with error bound [3.046875 * DBL_EPSILON]; exact dyadic fallback
   when the magnitude is below that bound. The bound accommodates inputs up
   to [|a|^2 <= 2] and [|b|^2 <= 2]. *)
let[@zero_alloc] sign_dot_prod a b =
  let open Float_u.O in
  let max_error = #3.046875 * Float_u.epsilon_float in
  let dp = V.dot a b in
  if Float_u.abs dp > max_error
  then if dp > #0.0 then 1 else -1
  else (
    let av = Exact_vec.of_r3 a in
    let bv = Exact_vec.of_r3 b in
    Dyadic.sign (Exact_vec.dot av bv) [@nontail])
;;

(* TODO: port CompareEdgeDistance, CompareEdgePairDistance, and
   CompareEdgeDirections from s2predicates.h:134-165 / s2predicates.cc. *)
(* TODO: port CircleEdgeIntersectionSign and CircleEdgeIntersectionOrdering
   from s2predicates.h:205-264 / s2predicates.cc. *)
(* TODO: port EdgeCircumcenterSign and GetVoronoiSiteExclusion from
   s2predicates.h:279-316 / s2predicates.cc. *)
(* Fast triage that takes a precomputed [a x b] (computed via [V.cross], not
   [robust_cross_prod]). The det error bound is the same as [triage_sign] above; the
   only saving here is one cross product. The [a] and [b] arguments are not used at
   runtime, but are kept in the signature so the precondition [a_cross_b = V.cross a b]
   stays visible to callers. *)
let[@inline] [@zero_alloc] triage_sign_with_cross _a _b c a_cross_b =
  let open Float_u.O in
  let det = V.dot a_cross_b c in
  if det > max_determinant_error ()
  then Direction.Counter_clockwise
  else if det < Float_u.neg (max_determinant_error ())
  then Direction.Clockwise
  else Direction.Indeterminate
;;

let[@zero_alloc] expensive_sign a b c ~perturb =
  (* [exact_sign] assumes pairwise-distinct inputs because the symbolic perturbation
     step requires the three points in lexicographic order. Short-circuit when any two
     points coincide so the public contract is "result is non-zero unless two of the
     inputs are the same". *)
  if V.equal a b || V.equal b c || V.equal c a
  then Direction.Indeterminate
  else Direction.of_int (exact_sign a b c ~perturb)
;;

let[@zero_alloc] unperturbed_sign a b c =
  match triage_sign a b c with
  | (Counter_clockwise | Clockwise) as s -> s
  | Indeterminate -> expensive_sign a b c ~perturb:false
;;
