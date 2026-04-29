open Core

[@@@zero_alloc all]

type t = float# [@@deriving unboxed_option { sentinel = true }]

let%template[@alloc a = (heap, stack)] [@inline] [@zero_alloc ignore] sexp_of_t t : Sexp.t
  =
  Sexp.List [ Sexp.List [ Sexp.Atom "length2"; Float.sexp_of_t (Float_u.to_float t) ] ]
  [@exclave_if_stack a]
;;

let relative_sum_error = Float_u.O.(#2.02 * Float_u.epsilon_float)
let max_length2 = #4.0
let zero = #0.0
let right = #2.0
let straight = max_length2
let infinity = Float_u.infinity
let negative = -#1.0

let[@inline] [@zero_alloc] rec of_angle angle =
  let radians = S1_angle.radians angle in
  if Float_u.O.(radians < #0.0)
  then negative
  else if S1_angle.is_inf angle
  then Float_u.infinity
  else if Float_u.O.(radians >= Float_u.pi)
  then straight
  else
    let open Float_u.O in
    let l = #2.0 * Float_u.sin (#0.5 * radians) in
    of_length2 (l * l)

and[@inline] [@zero_alloc] of_length2 length2 =
  if Float_u.O.(length2 > max_length2) then straight else length2
;;

let[@inline] [@zero_alloc] of_radians radians = of_angle (S1_angle.of_radians radians)
let[@inline] [@zero_alloc] of_degrees degrees = of_angle (S1_angle.of_degrees degrees)
let[@inline] [@zero_alloc] of_e5 e5 = of_angle (S1_angle.of_e5 e5)
let[@inline] [@zero_alloc] of_e6 e6 = of_angle (S1_angle.of_e6 e6)
let[@inline] [@zero_alloc] of_e7 e7 = of_angle (S1_angle.of_e7 e7)

let[@inline] [@zero_alloc] fast_upper_bound_from angle =
  (* The geodesic (surface) distance is always at least as large as the chord through
     the sphere's interior, so squaring the radians-valued angle gives a valid
     [length2]. The squared radian is accurate to within about 1% of the true squared
     chord up to surface distances around 3100 km on Earth. *)
  let r = S1_angle.radians angle in
  of_length2 Float_u.O.(r * r)
;;

let[@inline] [@zero_alloc] length2 t = t

let[@zero_alloc] to_angle t =
  let open Float_u.O in
  if t < #0.0
  then S1_angle.of_radians (-#1.0)
  else if Float_u.is_inf t
  then S1_angle.infinity
  else S1_angle.of_radians (#2.0 * Float_u.asin (#0.5 * Float_u.sqrt t))
;;

let[@inline] [@zero_alloc] radians t = S1_angle.radians (to_angle t)
let[@inline] [@zero_alloc] degrees t = S1_angle.degrees (to_angle t)
let[@inline] [@zero_alloc] is_zero t = Float_u.O.(t = #0.0)
let[@inline] [@zero_alloc] is_negative t = Float_u.O.(t < #0.0)
let[@inline] [@zero_alloc] is_infinity t = Float_u.is_inf t
let[@inline] [@zero_alloc] is_special t = is_negative t || is_infinity t

let[@inline] [@zero_alloc] is_valid t =
  (Float_u.O.(t >= #0.0) && Float_u.O.(t <= max_length2)) || is_special t
;;

let[@inline] [@zero_alloc] successor t =
  if Float_u.O.(t >= max_length2)
  then Float_u.infinity
  else if Float_u.O.(t < #0.0)
  then zero
  else
    let open Stdlib_upstream_compatible in
    Float_u.next_after t #10.0
;;

let[@inline] [@zero_alloc] predecessor t =
  if Float_u.O.(t <= #0.0)
  then negative
  else if Float_u.O.(t > max_length2)
  then straight
  else
    let open Stdlib_upstream_compatible in
    Float_u.next_after t (-#10.0)
;;

(* Let a and b (non-squared) be chord lengths with half-angles A and B
   (a = 2 sin A, etc). Starting from c = 2 sin(A + B) and the identities
     sin(A + B) = sin A cos B + cos A sin B
     cos X      = sqrt (1 - sin^2 X)
   the squared sum simplifies to x + y + 2*sqrt(x*y) where x and y are the
   two products below. This needs only one sqrt versus converting to angles.

   Error analysis (u = epsilon/2 = 2^-53 is the unit roundoff): each of x and
   y is computed with (1+u)^2 error, so the full expression bounds at
   (1+u)^4 ~= 4.04 * u = 2.02 * epsilon. Hence [relative_sum_error]. *)
let[@inline] [@zero_alloc] add a b =
  assert (not (is_special a || is_special b));
  let open Float_u.O in
  if b = #0.0
  then a
  else if a + b >= max_length2
  then straight
  else (
    let x = a * (#1.0 - (#0.25 * b)) in
    let y = b * (#1.0 - (#0.25 * a)) in
    let result = x + y + (#2.0 * Float_u.sqrt (x * y)) in
    if result > max_length2 then straight else result)
;;

(* Subtraction uses two sqrts (rather than the single sqrt of [add]) so that
   [sqrt x - sqrt y] does not catastrophically cancel when a and b are nearly
   equal. The max-with-zero guards against tiny negative results from rounding. *)
let[@inline] [@zero_alloc] sub a b =
  assert (not (is_special a || is_special b));
  let open Float_u.O in
  if b = #0.0
  then a
  else if a <= b
  then zero
  else (
    let x = a * (#1.0 - (#0.25 * b)) in
    let y = b * (#1.0 - (#0.25 * a)) in
    let c = Float_util.max_u #0.0 (Float_u.sqrt x - Float_u.sqrt y) in
    c * c)
;;

(* With length2 = 4 sin^2 A, the identities
     sin(2A) = 2 sin A cos A
     cos^2 A = 1 - sin^2 A
   give sin^2(2A) = length2 * (1 - length2/4) and
   cos(2A) = 1 - 2 sin^2 A = 1 - length2/2.
   Much faster than converting to an angle and taking sin/cos. *)
let[@inline] [@zero_alloc] sin2 t =
  let open Float_u.O in
  t * (#1.0 - (#0.25 * t))
;;

let[@inline] [@zero_alloc] sin t = Float_u.sqrt (sin2 t)
let[@inline] [@zero_alloc] cos t = Float_u.O.(#1.0 - (#0.5 * t))
let[@inline] [@zero_alloc] tan t = Float_u.O.(sin t / cos t)

let[@inline] [@zero_alloc] plus_error t error =
  if is_special t
  then t
  else Float_util.max_u #0.0 (Float_util.min_u max_length2 Float_u.O.(t + error))
;;

(* 2.5 * epsilon comes from computing the squared distance; another
   2 * epsilon from the input point lengths being off by up to 2 * epsilon
   each (the maximum error from S2Point normalization). Absolute term
   16 * epsilon^2 accounts for the product of the two relative errors. *)
let[@inline] [@zero_alloc] max_point_error t =
  let eps = Float_u.epsilon_float in
  Float_u.O.((#4.5 * eps * t) + (#16.0 * eps * eps))
;;

(* Assuming an accurate libm, sin() and the multiplication each contribute
   0.5 * epsilon of relative error; the sin error is squared into length2,
   so the coefficient is 1.5 * epsilon. *)
let[@inline] [@zero_alloc] max_angle_error t =
  let open Float_u.O in
  #1.5 * Float_u.epsilon_float * t
;;

let[@inline] [@zero_alloc] compare a b = Float_u.compare a b
let[@inline] [@zero_alloc] equal a b = Float_u.O.(a = b)
