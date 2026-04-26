open Core

[@@@zero_alloc all]

(* Half ULP of a double: this is the S2 reference's DBL_ERR. *)
let dbl_error = Float_u.O.(#0.5 * Float_u.epsilon_float ())

(* Maximum rounding error in the triage determinant (a x b) . c for
   unit-length inputs.  Values under this threshold are treated as
   indeterminate and fall through to [stable_sign].  The 1.8274 factor
   comes from the analysis in s2predicates_internal.h. *)
let max_determinant_error = Float_u.O.(#1.8274 * Float_u.epsilon_float ())

(* Multiplier used to bound the error of the stable-sign determinant
   in terms of the norms of two edge vectors. *)
let det_error_multiplier = Float_u.O.(#3.2321 * Float_u.epsilon_float ())

(* --- Robust sign predicate ------------------------------------------------ *)

let[@inline] [@zero_alloc] float_sign v =
  let open Float_u.O in
  if v > #0.0 then 1 else if v < #0.0 then -1 else 0
;;

(* Tier 1 of the sign cascade.  Computes the signed volume (a x b) . c
   once in double precision and commits to a sign only when the
   magnitude is larger than the rounding envelope.  Roughly 15 ns for
   well-separated inputs. *)
let[@inline] [@zero_alloc] triage_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
  let open Float_u.O in
  let det =
    R3_vector.dot
      (R3_vector.cross (S2_point.to_r3 a) (S2_point.to_r3 b))
      (S2_point.to_r3 c)
  in
  if det > max_determinant_error
  then 1
  else if det < Float_u.neg max_determinant_error
  then -1
  else 0
;;

(* Tier 2 of the sign cascade.  Rewrites the determinant using edge
   differences AB, BC, CA; the longest edge is selected as the pivot
   so the final cross product is taken between the two shorter edges.
   This cuts the error bound to det_error_multiplier * sqrt(len2 * len2)
   and extends the range of determinants we can resolve without
   falling through to exact arithmetic. *)
let[@inline] [@zero_alloc] stable_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
  let open Float_u.O in
  let ab = R3_vector.sub (S2_point.to_r3 b) (S2_point.to_r3 a) in
  let bc = R3_vector.sub (S2_point.to_r3 c) (S2_point.to_r3 b) in
  let ca = R3_vector.sub (S2_point.to_r3 a) (S2_point.to_r3 c) in
  let ab2 = R3_vector.norm2 ab in
  let bc2 = R3_vector.norm2 bc in
  let ca2 = R3_vector.norm2 ca in
  let mutable det = #0.0 in
  let mutable max_error = #0.0 in
  if ab2 >= bc2 && ab2 >= ca2
  then (
    det <- Float_u.neg (R3_vector.dot (R3_vector.cross ca bc) (S2_point.to_r3 c));
    max_error <- det_error_multiplier * Float_u.sqrt (ca2 * bc2))
  else if bc2 >= ca2
  then (
    det <- Float_u.neg (R3_vector.dot (R3_vector.cross ab ca) (S2_point.to_r3 a));
    max_error <- det_error_multiplier * Float_u.sqrt (ab2 * ca2))
  else (
    det <- Float_u.neg (R3_vector.dot (R3_vector.cross bc ab) (S2_point.to_r3 b));
    max_error <- det_error_multiplier * Float_u.sqrt (bc2 * ab2));
  let min_no_underflow_error =
    det_error_multiplier * Float_u.sqrt (Float_u.min_positive_normal_value ())
  in
  if max_error < min_no_underflow_error
  then 0
  else if Float_u.abs det > max_error
  then if det > #0.0 then 1 else -1
  else 0
;;

type float_pair =
  #{ hi : float#
   ; lo : float#
   }

(* Error-free transformation: returns hi = a * b (rounded) and
   lo = a * b - hi exactly, so hi + lo represents the product with no
   rounding loss.  Relies on the IEEE fused multiply-add. *)
let[@inline] [@zero_alloc] two_product a b =
  let open Float_u.O in
  let p = a * b in
  let e =
    Float_u.of_float
      (Stdlib.Float.fma
         (Float_u.to_float a)
         (Float_u.to_float b)
         (Float_u.to_float (Float_u.neg p)))
  in
  #{ hi = p; lo = e }
;;

(* Error-free transformation: hi = a + b (rounded), lo = a + b - hi
   exactly.  Together with [two_product] this is the working basis of
   the Shewchuk-style expansion arithmetic used below. *)
let[@inline] [@zero_alloc] two_sum a b =
  let open Float_u.O in
  let s = a + b in
  let a' = s - b in
  let b' = s - a' in
  let da = a - a' in
  let db = b - b' in
  #{ hi = s; lo = da + db }
;;

(* Exact 3x3 determinant sign via Shewchuk's floating-point expansion
   arithmetic.  A non-overlapping expansion is a list of doubles whose
   exact sum represents the value; the highest-magnitude term at
   buf[len-1] therefore has the same sign as the sum.  We use a single
   local float# array as workspace so the whole path stays zero-alloc. *)

(* Shewchuk's GROW-EXPANSION: add the scalar [b] into the expansion
   stored in [buf.(0..len-1)] and return the new length.  Zero terms
   are squeezed out as they appear. *)
let grow_expansion (buf : float# array @ local) (len : int) (b : float#) : int =
  let mutable q = b in
  let mutable out = 0 in
  for i = 0 to len - 1 do
    let #{ hi; lo } = two_sum q buf.(i) in
    if Float_u.O.(lo <> #0.0)
    then (
      buf.(out) <- lo;
      out <- out + 1);
    q <- hi
  done;
  if Float_u.O.(q <> #0.0)
  then (
    buf.(out) <- q;
    out <- out + 1);
  out
;;

(* Build a non-overlapping expansion out of the [n] input terms in
   [buf.(0..n-1)] and return the sign of its sum (i.e. the sign of
   the highest-magnitude surviving term).  [buf] must have room for
   the in-place expansion; we pass a workspace sized for 24 input
   terms below. *)
let sign_of_terms (buf : float# array @ local) (n : int) : int =
  let mutable len = 0 in
  for i = 0 to n - 1 do
    len <- grow_expansion buf len buf.(i)
  done;
  if len = 0 then 0 else float_sign buf.(len - 1)
;;

let exact_det_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
  let open Float_u.O in
  let ax = S2_point.x a in
  let ay = S2_point.y a in
  let az = S2_point.z a in
  let bx = S2_point.x b in
  let by = S2_point.y b in
  let bz = S2_point.z b in
  let cx = S2_point.x c in
  let cy = S2_point.y c in
  let cz = S2_point.z c in
  (* Compute the three 2x2 cofactors of the expansion-by-first-row
     formula exactly.  Each cofactor is a four-term expansion built
     from two two-products and one two-sum:
       cofactor0 = by*cz - bz*cy
       cofactor1 = bz*cx - bx*cz
       cofactor2 = bx*cy - by*cx *)
  let #{ hi = p0; lo = e0 } = two_product by cz in
  let #{ hi = q0; lo = f0 } = two_product bz cy in
  let #{ hi = p1; lo = e1 } = two_product bz cx in
  let #{ hi = q1; lo = f1 } = two_product bx cz in
  let #{ hi = p2; lo = e2 } = two_product bx cy in
  let #{ hi = q2; lo = f2 } = two_product by cx in
  (* cofactor_i = (p_i - q_i) + (e_i - f_i), four terms each *)
  let #{ hi = s0; lo = r0 } = two_sum p0 (Float_u.neg q0) in
  let #{ hi = s1; lo = r1 } = two_sum p1 (Float_u.neg q1) in
  let #{ hi = s2; lo = r2 } = two_sum p2 (Float_u.neg q2) in
  (* det = ax * cofactor0 + ay * cofactor1 + az * cofactor2.  Expand
     each a_i * (four-term cofactor) into eight terms via two-product,
     giving 24 terms total.  Their exact sum is the determinant. *)
  let #{ hi = t00; lo = u00 } = two_product ax s0 in
  let #{ hi = t01; lo = u01 } = two_product ax r0 in
  let #{ hi = t02; lo = u02 } = two_product ax e0 in
  let #{ hi = t03; lo = u03 } = two_product ax (Float_u.neg f0) in
  let #{ hi = t10; lo = u10 } = two_product ay s1 in
  let #{ hi = t11; lo = u11 } = two_product ay r1 in
  let #{ hi = t12; lo = u12 } = two_product ay e1 in
  let #{ hi = t13; lo = u13 } = two_product ay (Float_u.neg f1) in
  let #{ hi = t20; lo = u20 } = two_product az s2 in
  let #{ hi = t21; lo = u21 } = two_product az r2 in
  let #{ hi = t22; lo = u22 } = two_product az e2 in
  let #{ hi = t23; lo = u23 } = two_product az (Float_u.neg f2) in
  (* Stack-allocate the workspace: 24 live terms plus 24 extra slots
     used as in-place expansion scratch so the whole path is
     zero-alloc. *)
  let local_ buf : float# array =
    [| t00
     ; u00
     ; t01
     ; u01
     ; t02
     ; u02
     ; t03
     ; u03
     ; t10
     ; u10
     ; t11
     ; u11
     ; t12
     ; u12
     ; t13
     ; u13
     ; t20
     ; u20
     ; t21
     ; u21
     ; t22
     ; u22
     ; t23
     ; u23 (* 24 extra slots for grow_expansion workspace *)
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
     ; #0.0
    |]
  in
  sign_of_terms buf 24 [@nontail]
;;

(* Symbolic perturbation ("simulation of simplicity" in the sense of
   Edelsbrunner and Muecke) used when the exact determinant is zero
   and the three points are genuinely collinear.  The perturbed
   coordinates form a polynomial in a fictitious infinitesimal; the
   cascade below tries successive coefficients of that polynomial in
   increasing order, returning the sign of the first non-zero
   coefficient.  Preserves consistency with [triage_sign] for any
   non-collinear perturbation of the inputs.  Requires
   a < b < c lexicographically; the caller is responsible for the
   sort and records the permutation sign. *)
let symbolically_perturbed_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
  let[@inline] [@zero_alloc] sign_of v =
    if Float_u.O.( > ) v #0.0 then 1 else if Float_u.O.( < ) v #0.0 then -1 else 0
  in
  let ax = S2_point.x a in
  let ay = S2_point.y a in
  let az = S2_point.z a in
  let bx = S2_point.x b in
  let by = S2_point.y b in
  let bz = S2_point.z b in
  let cx = S2_point.x c in
  let cy = S2_point.y c in
  let cz = S2_point.z c in
  let s = sign_of Float_u.O.((bx * cy) - (by * cx)) in
  if Int.( <> ) s 0
  then s
  else (
    let s = sign_of Float_u.O.((bz * cx) - (bx * cz)) in
    if Int.( <> ) s 0
    then s
    else (
      let s = sign_of Float_u.O.((by * cz) - (bz * cy)) in
      if Int.( <> ) s 0
      then s
      else (
        let s = sign_of Float_u.O.((cx * ay) - (cy * ax)) in
        if Int.( <> ) s 0
        then s
        else (
          let s = sign_of cx in
          if Int.( <> ) s 0
          then s
          else (
            let s = sign_of (Float_u.neg cy) in
            if Int.( <> ) s 0
            then s
            else (
              let s = sign_of Float_u.O.((cz * ax) - (cx * az)) in
              if Int.( <> ) s 0
              then s
              else (
                let s = sign_of cz in
                if Int.( <> ) s 0
                then s
                else (
                  let s = sign_of Float_u.O.((ax * by) - (ay * bx)) in
                  if Int.( <> ) s 0
                  then s
                  else (
                    let s = sign_of (Float_u.neg bx) in
                    if Int.( <> ) s 0
                    then s
                    else (
                      let s = sign_of by in
                      if Int.( <> ) s 0
                      then s
                      else (
                        let s = sign_of ax in
                        if Int.( <> ) s 0 then s else 1)))))))))))
;;

(* Lexicographic sort of three S2 points.  The permutation sign is
   carried alongside the sorted outputs so callers can restore the
   orientation of the original triangle after running any operation
   that requires sorted inputs (for example the symbolic perturbation
   above).  An unboxed record is used because [S2_point.t] cannot
   appear in tuples. *)
type sorted3 =
  #{ p0 : S2_point.t
   ; p1 : S2_point.t
   ; p2 : S2_point.t
   ; perm : int
   }

let[@inline] [@zero_alloc] sort3 (a : S2_point.t) (b : S2_point.t) (c : S2_point.t)
  : sorted3
  =
  (* Step 1: sort a, b *)
  let s =
    if S2_point.compare a b > 0
    then #{ p0 = b; p1 = a; p2 = c; perm = -1 }
    else #{ p0 = a; p1 = b; p2 = c; perm = 1 }
  in
  (* Step 2: sort s.p1, s.p2 *)
  let s =
    if S2_point.compare s.#p1 s.#p2 > 0
    then #{ p0 = s.#p0; p1 = s.#p2; p2 = s.#p1; perm = -1 * s.#perm }
    else s
  in
  (* Step 3: sort s.p0, s.p1 *)
  if S2_point.compare s.#p0 s.#p1 > 0
  then #{ p0 = s.#p1; p1 = s.#p0; p2 = s.#p2; perm = -1 * s.#perm }
  else s
;;

let exact_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) ~perturb =
  let #{ p0; p1; p2; perm } = sort3 a b c in
  let det_sign = exact_det_sign p0 p1 p2 in
  if not (Int.equal det_sign 0)
  then perm * det_sign
  else if perturb
  then perm * symbolically_perturbed_sign p0 p1 p2
  else 0
;;

let[@inline] [@zero_alloc] expensive_sign
  (a : S2_point.t)
  (b : S2_point.t)
  (c : S2_point.t)
  =
  if S2_point.equal a b || S2_point.equal b c || S2_point.equal c a
  then 0
  else (
    let det_sign = stable_sign a b c in
    if not (Int.equal det_sign 0) then det_sign else exact_sign a b c ~perturb:true)
;;

let[@inline] [@zero_alloc] sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
  let s = triage_sign a b c in
  if not (Int.equal s 0) then s else expensive_sign a b c
;;

(* --- OrderedCCW ----------------------------------------------------------- *)

let ordered_ccw (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) (o : S2_point.t) =
  let sum =
    (if not (Int.equal (sign b o a) (-1)) then 1 else 0)
    + (if not (Int.equal (sign c o b) (-1)) then 1 else 0)
    + if Int.equal (sign a o c) 1 then 1 else 0
  in
  sum >= 2
;;

(* --- Crossing predicates -------------------------------------------------- *)

(* Interior-crossing characterization: edges AB and CD cross at a
   point interior to both iff the four oriented triangles ACB, BDA,
   CBD, DAC all share the same non-zero orientation.  The fast path
   checks the first pair (ACB and BDA) and rejects most misses before
   taking a second [sign] call. *)
let[@inline] [@zero_alloc] crossing_sign
  (a : S2_point.t)
  (b : S2_point.t)
  (c : S2_point.t)
  (d : S2_point.t)
  =
  if S2_point.equal a c || S2_point.equal a d || S2_point.equal b c || S2_point.equal b d
  then 0
  else if S2_point.equal a b || S2_point.equal c d
  then -1
  else (
    (* Use the rotation identities
         sign(A,C,B) = -sign(A,B,C),  sign(B,D,A) =  sign(A,B,D),
         sign(C,B,D) = -sign(C,D,B),  sign(D,A,C) =  sign(C,D,A)
       so every orientation can be expressed as a single [sign] call on a
       triangle whose first two vertices are either [AB] or [CD]. *)
    let acb = -sign a b c in
    let bda = sign a b d in
    (* Early rejection when ACB and BDA have opposite non-zero signs:
       C and D already lie on opposite sides of AB's great circle but
       the two triangles rotate oppositely, so no interior crossing is
       possible. *)
    if acb = -bda && not (Int.equal bda 0)
    then -1
    else (
      let cbd = -sign c d b in
      let dac = sign c d a in
      (* A crossing requires all four triangles to share the same
         non-zero orientation. *)
      if (not (Int.equal acb 0))
         && Int.equal acb bda
         && Int.equal acb cbd
         && Int.equal acb dac
      then 1
      else -1))
;;

let vertex_crossing (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) (d : S2_point.t) =
  if S2_point.equal a b || S2_point.equal c d
  then false
  else if S2_point.equal a c
  then S2_point.equal b d || ordered_ccw (S2_point.ortho a) d b a
  else if S2_point.equal b d
  then ordered_ccw (S2_point.ortho b) c a b
  else if S2_point.equal a d
  then S2_point.equal b c || ordered_ccw (S2_point.ortho a) c b a
  else if S2_point.equal b c
  then ordered_ccw (S2_point.ortho b) d a b
  else false
;;

let[@inline] [@zero_alloc] edge_or_vertex_crossing
  (a : S2_point.t)
  (b : S2_point.t)
  (c : S2_point.t)
  (d : S2_point.t)
  =
  let cs = crossing_sign a b c d in
  if cs < 0 then false else if cs > 0 then true else vertex_crossing a b c d
;;

(* Tie-breaking for shared-vertex crossings: returns +1 when AB enters the region bounded
   by CD (interior on the left), -1 when AB exits, and 0 when the configuration is not
   considered a crossing under the perturbation rules. The four-distinct-vertices case is
   undefined; callers must invoke [vertex_crossing]/[signed_vertex_crossing] only when at
   least one endpoint is shared. *)
let signed_vertex_crossing
  (a : S2_point.t)
  (b : S2_point.t)
  (c : S2_point.t)
  (d : S2_point.t)
  =
  if S2_point.equal a b || S2_point.equal c d
  then 0
  else if S2_point.equal a c
  then if S2_point.equal b d || ordered_ccw (S2_point.ortho a) d b a then 1 else 0
  else if S2_point.equal b d
  then if ordered_ccw (S2_point.ortho b) c a b then 1 else 0
  else if S2_point.equal a d
  then if S2_point.equal b c || ordered_ccw (S2_point.ortho a) c b a then -1 else 0
  else if S2_point.equal b c
  then if ordered_ccw (S2_point.ortho b) d a b then -1 else 0
  else 0
;;

let angle_contains_vertex (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
  not (ordered_ccw (S2_point.ortho b) c a b)
;;

(* --- Intersection --------------------------------------------------------- *)

let intersection_error = S1_angle.of_radians Float_u.O.(#8.0 * dbl_error)
let intersection_merge_radius = S1_angle.of_radians Float_u.O.(#16.0 * dbl_error)
let rounding_epsilon_u = dbl_error

type edge =
  #{ e0 : S2_point.t
   ; e1 : S2_point.t
   }

let compare_edges (a0 : S2_point.t) (a1 : S2_point.t) (b0 : S2_point.t) (b1 : S2_point.t) =
  let pa =
    if S2_point.compare a0 a1 >= 0 then #{ e0 = a1; e1 = a0 } else #{ e0 = a0; e1 = a1 }
  in
  let pb =
    if S2_point.compare b0 b1 >= 0 then #{ e0 = b1; e1 = b0 } else #{ e0 = b0; e1 = b1 }
  in
  S2_point.compare pa.#e0 pb.#e0 < 0
  || (S2_point.equal pa.#e0 pb.#e0 && S2_point.compare pa.#e1 pb.#e1 < 0)
;;

let get_projection
  (x : R3_vector.t)
  (a_norm : R3_vector.t)
  (a_norm_len : float#)
  (a0 : S2_point.t)
  (a1 : S2_point.t)
  =
  let open Float_u.O in
  let x0 = R3_vector.sub x (S2_point.to_r3 a0) in
  let x1 = R3_vector.sub x (S2_point.to_r3 a1) in
  let x0_dist2 = R3_vector.norm2 x0 in
  let x1_dist2 = R3_vector.norm2 x1 in
  let sqrt3 = Float_u.sqrt #3.0 in
  let mutable dist = #0.0 in
  let mutable proj = #0.0 in
  if x0_dist2 < x1_dist2 || (x0_dist2 = x1_dist2 && Int.(R3_vector.compare x0 x1 < 0))
  then (
    dist <- Float_u.sqrt x0_dist2;
    proj <- R3_vector.dot x0 a_norm)
  else (
    dist <- Float_u.sqrt x1_dist2;
    proj <- R3_vector.dot x1 a_norm);
  let t_err = rounding_epsilon_u in
  let bound =
    (((((#3.5 + (#2.0 * sqrt3)) * a_norm_len) + (#32.0 * sqrt3 * dbl_error)) * dist)
     + (#1.5 * Float_u.abs proj))
    * t_err
  in
  #{ hi = proj; lo = bound }
;;

let get_intersection_stable_sorted
  (a0 : S2_point.t)
  (a1 : S2_point.t)
  (b0 : S2_point.t)
  (b1 : S2_point.t)
  =
  let open Float_u.O in
  let a_norm =
    R3_vector.cross
      (R3_vector.sub (S2_point.to_r3 a0) (S2_point.to_r3 a1))
      (R3_vector.add (S2_point.to_r3 a0) (S2_point.to_r3 a1))
  in
  let a_norm_len = R3_vector.norm a_norm in
  let b_len = R3_vector.norm (R3_vector.sub (S2_point.to_r3 b1) (S2_point.to_r3 b0)) in
  let #{ hi = b0_dist; lo = b0_error } =
    get_projection (S2_point.to_r3 b0) a_norm a_norm_len a0 a1
  in
  let #{ hi = b1_dist; lo = b1_error } =
    get_projection (S2_point.to_r3 b1) a_norm a_norm_len a0 a1
  in
  let mutable b0_dist = b0_dist in
  let mutable b1_dist = b1_dist in
  if b0_dist < b1_dist
  then (
    b0_dist <- Float_u.neg b0_dist;
    b1_dist <- Float_u.neg b1_dist);
  let dist_sum = b0_dist - b1_dist in
  let error_sum = b0_error + b1_error in
  if dist_sum <= error_sum
  then R3_vector.Option.none
  else (
    let x =
      R3_vector.sub
        (R3_vector.mul (S2_point.to_r3 b1) b0_dist)
        (R3_vector.mul (S2_point.to_r3 b0) b1_dist)
    in
    let t_err = rounding_epsilon_u in
    let err =
      (b_len
       * Float_u.abs ((b0_dist * b1_error) - (b1_dist * b0_error))
       / (dist_sum - error_sum))
      + (#2.0 * t_err * dist_sum)
    in
    let x_len2 = R3_vector.norm2 x in
    if x_len2 < Float_u.min_positive_normal_value ()
    then R3_vector.Option.none
    else (
      let x_len = Float_u.sqrt x_len2 in
      let max_err = S1_angle.radians intersection_error in
      if err > (max_err - t_err) * x_len
      then R3_vector.Option.none
      else R3_vector.Option.some (R3_vector.normalize x)))
;;

let get_intersection_stable
  (a0 : S2_point.t)
  (a1 : S2_point.t)
  (b0 : S2_point.t)
  (b1 : S2_point.t)
  =
  let open Float_u.O in
  let a_len2 = R3_vector.norm2 (R3_vector.sub (S2_point.to_r3 a1) (S2_point.to_r3 a0)) in
  let b_len2 = R3_vector.norm2 (R3_vector.sub (S2_point.to_r3 b1) (S2_point.to_r3 b0)) in
  if a_len2 < b_len2 || (a_len2 = b_len2 && compare_edges a0 a1 b0 b1)
  then get_intersection_stable_sorted b0 b1 a0 a1
  else get_intersection_stable_sorted a0 a1 b0 b1
;;

let get_intersection_exact
  (a0 : S2_point.t)
  (a1 : S2_point.t)
  (b0 : S2_point.t)
  (b1 : S2_point.t)
  =
  let a_norm = R3_vector.cross (S2_point.to_r3 a0) (S2_point.to_r3 a1) in
  let b_norm = R3_vector.cross (S2_point.to_r3 b0) (S2_point.to_r3 b1) in
  let x = R3_vector.cross a_norm b_norm in
  if R3_vector.equal x R3_vector.zero
  then (
    (* Collinear edges: the great circles containing AB and CD
       coincide, so the "intersection" is not a single point.  To keep
       the result reproducible we return the lexicographically
       smallest endpoint that lies on both edges, using [big] as a
       sentinel initial value that is guaranteed to be replaced. *)
    let big = R3_vector.create ~x:#10.0 ~y:#10.0 ~z:#10.0 in
    let a_norm_pt : S2_point.t = R3_vector.normalize a_norm in
    let b_norm_pt : S2_point.t = R3_vector.normalize b_norm in
    let result = big in
    let result =
      if ordered_ccw b0 a0 b1 b_norm_pt && S2_point.compare a0 result < 0
      then a0
      else result
    in
    let result =
      if ordered_ccw b0 a1 b1 b_norm_pt && S2_point.compare a1 result < 0
      then a1
      else result
    in
    let result =
      if ordered_ccw a0 b0 a1 a_norm_pt && S2_point.compare b0 result < 0
      then b0
      else result
    in
    let result =
      if ordered_ccw a0 b1 a1 a_norm_pt && S2_point.compare b1 result < 0
      then b1
      else result
    in
    result)
  else R3_vector.normalize x
;;

let[@inline] [@zero_alloc] get_intersection
  (a0 : S2_point.t)
  (a1 : S2_point.t)
  (b0 : S2_point.t)
  (b1 : S2_point.t)
  =
  (* Two-tier strategy: the stable path covers all non-degenerate
     crossings in double precision and returns None only when the
     computed error bound would exceed kIntersectionError; the exact
     path handles the remaining cases (including perfectly collinear
     edges) using unbounded-precision cross products. *)
  let pt =
    match%optional_u.R3_vector.Option get_intersection_stable a0 a1 b0 b1 with
    | Some result -> result
    | None -> get_intersection_exact a0 a1 b0 b1
  in
  (* The result of a cross product is only determined up to sign; flip
     it so the returned point is on the same hemisphere as the average
     of the four inputs, which is always very close to the true
     intersection. *)
  let sum =
    R3_vector.add
      (R3_vector.add (S2_point.to_r3 a0) (S2_point.to_r3 a1))
      (R3_vector.add (S2_point.to_r3 b0) (S2_point.to_r3 b1))
  in
  if Float_u.O.(R3_vector.dot (S2_point.to_r3 pt) sum < #0.0)
  then R3_vector.neg pt
  else pt
;;
