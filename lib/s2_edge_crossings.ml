open Core

(* Layout-polymorphic array primitives for float# arrays. *)
external arr_get : ('a : any mod separable). 'a array -> int -> 'a = "%array_safe_get"
[@@layout_poly]

external arr_set
  : ('a : any mod separable).
  'a array -> int -> 'a -> unit
  = "%array_safe_set"
[@@layout_poly]

(* Precision constants, matching C++ s2predicates. *)
let dbl_epsilon = Float_u.of_float Float.epsilon_float
let dbl_error = Float_u.O.(#0.5 * dbl_epsilon)

(* Maximum error in the determinant (AxB).C for unit-length vectors. *)
let max_determinant_error = Float_u.O.(#1.8274 * dbl_epsilon)

(* Multiplier for the stable-sign error bound. *)
let det_error_multiplier = Float_u.O.(#3.2321 * dbl_epsilon)

(* --- Robust sign predicate ------------------------------------------------ *)

let[@inline] float_sign v =
  let open Float_u.O in
  if v > #0.0 then 1 else if v < #0.0 then -1 else 0
;;

(* Fast triage: returns +1, -1, or 0 if uncertain. *)
let triage_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
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

(* More careful determinant sign using longest-edge pivot. *)
let stable_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
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
    det_error_multiplier * Float_u.sqrt (Float_u.of_float Float.min_value)
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

(* Error-free two-product: a*b = hi + lo exactly. Uses FMA. *)
let[@inline] two_product a b =
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

(* Error-free two-sum: a+b = hi + lo exactly. *)
let[@inline] two_sum a b =
  let open Float_u.O in
  let s = a + b in
  let a' = s - b in
  let b' = s - a' in
  let da = a - a' in
  let db = b - b' in
  #{ hi = s; lo = da + db }
;;

(* Exact sign of 3x3 determinant using Shewchuk-style expansions.
   We avoid heap allocation by using a fixed-size float array as a
   workspace for the expansion arithmetic. *)

(* Grow-expansion: add a single value e to a non-overlapping expansion
   stored in buf[0..len-1]. Returns new length. Shewchuk's GROW-EXPANSION. *)
let grow_expansion (buf : float# array) (len : int) (b : float#) : int =
  let mutable q = b in
  let mutable out = 0 in
  for i = 0 to len - 1 do
    let #{ hi; lo } = two_sum q (arr_get buf i) in
    if Float_u.O.(lo <> #0.0)
    then (
      arr_set buf out lo;
      out <- out + 1);
    q <- hi
  done;
  if Float_u.O.(q <> #0.0)
  then (
    arr_set buf out q;
    out <- out + 1);
  out
;;

(* Return the sign of a set of terms by building a proper expansion.
   buf is a workspace of at least n+24 entries. terms[0..n-1] are the input. *)
let sign_of_terms (buf : float# array) (n : int) : int =
  let mutable len = 0 in
  for i = 0 to n - 1 do
    len <- grow_expansion buf len (arr_get buf i)
  done;
  if len = 0 then 0 else float_sign (arr_get buf (len - 1))
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
  (* Compute 2x2 cofactors exactly: each is a two-term expansion [hi, lo].
     cofactor0 = by*cz - bz*cy, cofactor1 = bz*cx - bx*cz, cofactor2 = bx*cy - by*cx *)
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
  (* det = ax * cofactor0 + ay * cofactor1 + az * cofactor2
     We expand each product ax * (s0 + r0 + e0 - f0) into terms. *)
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
  (* All 24 terms summed give the exact determinant. *)
  let buf : float# array =
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
  sign_of_terms buf 24
;;

(* Symbolic perturbation for exactly collinear points. Follows the
   Edelsbrunner-Muecke simulation of simplicity.
   Requires a < b < c in lexicographic order. *)
let symbolically_perturbed_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
  (* Edelsbrunner-Muecke SOS perturbation cascade. Each term is tried in order;
     the first nonzero sign wins. The sequence comes from the C++ implementation. *)
  let sign_of v =
    if Float_u.O.( > ) v #0.0 then 1 else if Float_u.O.( < ) v #0.0 then -1 else 0
  in
  let terms =
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
    [| sign_of ((bx * cy) - (by * cx))
     ; sign_of ((bz * cx) - (bx * cz))
     ; sign_of ((by * cz) - (bz * cy))
     ; sign_of ((cx * ay) - (cy * ax))
     ; sign_of cx
     ; sign_of (Float_u.neg cy)
     ; sign_of ((cz * ax) - (cx * az))
     ; sign_of cz
     ; sign_of ((ax * by) - (ay * bx))
     ; sign_of (Float_u.neg bx)
     ; sign_of by
     ; sign_of ax
    |]
  in
  let n = Array.length terms in
  let mutable i = 0 in
  while i < n && terms.(i) = 0 do
    i <- i + 1
  done;
  if i < n then terms.(i) else 1
;;

(* Sort three S2Points lexicographically and return sorted + permutation sign.
   Uses an unboxed record since S2_point.t can't appear in tuples. *)
type sorted3 =
  #{ p0 : S2_point.t
   ; p1 : S2_point.t
   ; p2 : S2_point.t
   ; perm : int
   }

let[@inline] sort3 (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) : sorted3 =
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

let expensive_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
  if S2_point.equal a b || S2_point.equal b c || S2_point.equal c a
  then 0
  else (
    let det_sign = stable_sign a b c in
    if not (Int.equal det_sign 0) then det_sign else exact_sign a b c ~perturb:true)
;;

let sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
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

(* Crossing sign implementation following the C++/Go EdgeCrosser logic.
   For edges AB and CD to cross at an interior point, all four oriented
   triangles ACB, BDA, CBD, DAC must have the same non-zero orientation. *)
let crossing_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) (d : S2_point.t) =
  if S2_point.equal a c || S2_point.equal a d || S2_point.equal b c || S2_point.equal b d
  then 0
  else if S2_point.equal a b || S2_point.equal c d
  then -1
  else (
    (* Compute the four triangle orientations. The crossing condition is that
       all four have the same sign. We use the identities:
         ACB = -sign(A,B,C), BDA = sign(A,B,D),
         CBD = -sign(C,D,B), DAC = sign(C,D,A) *)
    let acb = -sign a b c in
    let bda = sign a b d in
    (* Fast rejection: if ACB and BDA have opposite orientations. *)
    if acb = -bda && not (Int.equal bda 0)
    then -1
    else (
      let cbd = -sign c d b in
      let dac = sign c d a in
      (* All four must be the same non-zero value for a crossing. *)
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

let edge_or_vertex_crossing
  (a : S2_point.t)
  (b : S2_point.t)
  (c : S2_point.t)
  (d : S2_point.t)
  =
  let cs = crossing_sign a b c d in
  if cs < 0 then false else if cs > 0 then true else vertex_crossing a b c d
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
    if x_len2 < Float_u.of_float Float.min_value
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
    (* Collinear: pick the lexicographically smallest interior endpoint. *)
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

let get_intersection
  (a0 : S2_point.t)
  (a1 : S2_point.t)
  (b0 : S2_point.t)
  (b1 : S2_point.t)
  =
  let pt =
    match%optional_u.R3_vector.Option get_intersection_stable a0 a1 b0 b1 with
    | Some result -> result
    | None -> get_intersection_exact a0 a1 b0 b1
  in
  let sum =
    R3_vector.add
      (R3_vector.add (S2_point.to_r3 a0) (S2_point.to_r3 a1))
      (R3_vector.add (S2_point.to_r3 b0) (S2_point.to_r3 b1))
  in
  if Float_u.O.(R3_vector.dot (S2_point.to_r3 pt) sum < #0.0)
  then R3_vector.neg pt
  else pt
;;
