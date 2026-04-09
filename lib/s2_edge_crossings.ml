open Core

(* Precision constants, matching C++ s2predicates. *)
let dbl_epsilon = Float.epsilon_float
let dbl_error = 0.5 *. dbl_epsilon

(* Maximum error in the determinant (AxB).C for unit-length vectors. *)
let max_determinant_error = 1.8274 *. dbl_epsilon

(* Multiplier for the stable-sign error bound. *)
let det_error_multiplier = 3.2321 *. dbl_epsilon

(* --- Robust sign predicate ------------------------------------------------ *)

let[@inline] float_sign v =
  if Float.( > ) v 0.0 then 1 else if Float.( < ) v 0.0 then -1 else 0
;;

(* Fast triage: returns +1, -1, or 0 if uncertain. *)
let triage_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
  let det =
    Float_u.to_float
      (R3_vector.dot
         (R3_vector.cross (S2_point.to_r3 a) (S2_point.to_r3 b))
         (S2_point.to_r3 c))
  in
  if Float.( > ) det max_determinant_error
  then 1
  else if Float.( < ) det (Float.neg max_determinant_error)
  then -1
  else 0
;;

(* More careful determinant sign using longest-edge pivot. *)
let stable_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
  let ab = R3_vector.sub (S2_point.to_r3 b) (S2_point.to_r3 a) in
  let bc = R3_vector.sub (S2_point.to_r3 c) (S2_point.to_r3 b) in
  let ca = R3_vector.sub (S2_point.to_r3 a) (S2_point.to_r3 c) in
  let ab2 = Float_u.to_float (R3_vector.norm2 ab) in
  let bc2 = Float_u.to_float (R3_vector.norm2 bc) in
  let ca2 = Float_u.to_float (R3_vector.norm2 ca) in
  let det, max_error =
    if Float.( >= ) ab2 bc2 && Float.( >= ) ab2 ca2
    then (
      let d =
        Float.neg
          (Float_u.to_float (R3_vector.dot (R3_vector.cross ca bc) (S2_point.to_r3 c)))
      in
      d, det_error_multiplier *. Float.sqrt (ca2 *. bc2))
    else if Float.( >= ) bc2 ca2
    then (
      let d =
        Float.neg
          (Float_u.to_float (R3_vector.dot (R3_vector.cross ab ca) (S2_point.to_r3 a)))
      in
      d, det_error_multiplier *. Float.sqrt (ab2 *. ca2))
    else (
      let d =
        Float.neg
          (Float_u.to_float (R3_vector.dot (R3_vector.cross bc ab) (S2_point.to_r3 b)))
      in
      d, det_error_multiplier *. Float.sqrt (bc2 *. ab2))
  in
  let min_no_underflow_error = det_error_multiplier *. Float.sqrt Float.min_value in
  if Float.( < ) max_error min_no_underflow_error
  then 0
  else if Float.( > ) (Float.abs det) max_error
  then if Float.( > ) det 0.0 then 1 else -1
  else 0
;;

(* Error-free two-product: a*b = p + e exactly. Uses FMA. *)
let[@inline] two_product a b =
  let p = a *. b in
  let e = Stdlib.Float.fma a b (Float.neg p) in
  p, e
;;

(* Error-free two-sum: a+b = s + e exactly. *)
let[@inline] two_sum a b =
  let s = a +. b in
  let a' = s -. b in
  let b' = s -. a' in
  let da = a -. a' in
  let db = b -. b' in
  s, da +. db
;;

(* Exact sign of 3x3 determinant.
   We compute det = (CxA).B using the expansion:
     det = (cy*az - cz*ay)*bx + (cz*ax - cx*az)*by + (cx*ay - cy*ax)*bz
   Each 2x2 sub-determinant is computed exactly as a 4-term expansion
   using two_product, and the final combination is accumulated into a
   multi-term expansion whose sign we determine. *)

(* Grow-expansion: add a single value e to a non-overlapping expansion.
   Returns a new non-overlapping expansion. Shewchuk's GROW-EXPANSION. *)
let grow_expansion (expansion : float list) (b : float) : float list =
  let q = ref b in
  let result = ref [] in
  List.iter expansion ~f:(fun ei ->
    let sum, err = two_sum !q ei in
    if not (Float.( = ) err 0.0) then result := err :: !result;
    q := sum);
  if not (Float.( = ) !q 0.0) then result := !q :: !result;
  List.rev !result
;;

(* Return the sign of an expansion (list of non-overlapping terms).
   The sign is determined by the most significant (largest magnitude) term. *)
let sign_of_expansion terms =
  (* The terms may not be sorted. Build a proper expansion by growing. *)
  let exp = List.fold terms ~init:[] ~f:(fun acc t -> grow_expansion acc t) in
  (* The last element of the expansion has the largest magnitude and
     determines the sign. *)
  match List.last exp with
  | None -> 0
  | Some v -> float_sign v
;;

let exact_det_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
  let ax = Float_u.to_float (S2_point.x a) in
  let ay = Float_u.to_float (S2_point.y a) in
  let az = Float_u.to_float (S2_point.z a) in
  let bx = Float_u.to_float (S2_point.x b) in
  let by = Float_u.to_float (S2_point.y b) in
  let bz = Float_u.to_float (S2_point.z b) in
  let cx = Float_u.to_float (S2_point.x c) in
  let cy = Float_u.to_float (S2_point.y c) in
  let cz = Float_u.to_float (S2_point.z c) in
  (* Compute 2x2 cofactors exactly: each is a two-term expansion [hi, lo].
     cofactor0 = by*cz - bz*cy, cofactor1 = bz*cx - bx*cz, cofactor2 = bx*cy - by*cx *)
  let p0, e0 = two_product by cz in
  let q0, f0 = two_product bz cy in
  let p1, e1 = two_product bz cx in
  let q1, f1 = two_product bx cz in
  let p2, e2 = two_product bx cy in
  let q2, f2 = two_product by cx in
  (* cofactor_i = (p_i - q_i) + (e_i - f_i), four terms each *)
  let s0, r0 = two_sum p0 (Float.neg q0) in
  let s1, r1 = two_sum p1 (Float.neg q1) in
  let s2, r2 = two_sum p2 (Float.neg q2) in
  (* det = ax * cofactor0 + ay * cofactor1 + az * cofactor2
     We expand each product ax * (s0 + r0 + e0 - f0) into terms. *)
  let t00, u00 = two_product ax s0 in
  let t01, u01 = two_product ax r0 in
  let t02, u02 = two_product ax e0 in
  let t03, u03 = two_product ax (Float.neg f0) in
  let t10, u10 = two_product ay s1 in
  let t11, u11 = two_product ay r1 in
  let t12, u12 = two_product ay e1 in
  let t13, u13 = two_product ay (Float.neg f1) in
  let t20, u20 = two_product az s2 in
  let t21, u21 = two_product az r2 in
  let t22, u22 = two_product az e2 in
  let t23, u23 = two_product az (Float.neg f2) in
  (* All 24 terms summed give the exact determinant. *)
  sign_of_expansion
    [ t00
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
    ; u23
    ]
;;

(* Symbolic perturbation for exactly collinear points. Follows the
   Edelsbrunner-Muecke simulation of simplicity.
   Requires a < b < c in lexicographic order. *)
let symbolically_perturbed_sign (a : S2_point.t) (b : S2_point.t) (c : S2_point.t) =
  let ax = Float_u.to_float (S2_point.x a) in
  let ay = Float_u.to_float (S2_point.y a) in
  let _az = Float_u.to_float (S2_point.z a) in
  let bx = Float_u.to_float (S2_point.x b) in
  let by = Float_u.to_float (S2_point.y b) in
  let bz = Float_u.to_float (S2_point.z b) in
  let cx = Float_u.to_float (S2_point.x c) in
  let cy = Float_u.to_float (S2_point.y c) in
  let cz = Float_u.to_float (S2_point.z c) in
  let bxc_z = (bx *. cy) -. (by *. cx) in
  let bxc_y = (bz *. cx) -. (bx *. cz) in
  let bxc_x = (by *. cz) -. (bz *. cy) in
  let s = float_sign bxc_z in
  if not (Int.equal s 0)
  then s
  else (
    let s = float_sign bxc_y in
    if not (Int.equal s 0)
    then s
    else (
      let s = float_sign bxc_x in
      if not (Int.equal s 0)
      then s
      else (
        let s = float_sign ((cx *. ay) -. (cy *. ax)) in
        if not (Int.equal s 0)
        then s
        else (
          let s = float_sign cx in
          if not (Int.equal s 0)
          then s
          else (
            let s = float_sign (Float.neg cy) in
            if not (Int.equal s 0)
            then s
            else (
              let s = float_sign ((cz *. ax) -. (cx *. _az)) in
              if not (Int.equal s 0)
              then s
              else (
                let s = float_sign cz in
                if not (Int.equal s 0)
                then s
                else (
                  let s = float_sign ((ax *. by) -. (ay *. bx)) in
                  if not (Int.equal s 0)
                  then s
                  else (
                    let s = float_sign (Float.neg bx) in
                    if not (Int.equal s 0)
                    then s
                    else (
                      let s = float_sign by in
                      if not (Int.equal s 0)
                      then s
                      else (
                        let s = float_sign ax in
                        if not (Int.equal s 0) then s else 1)))))))))))
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
  let sum = ref 0 in
  if not (Int.equal (sign b o a) (-1)) then sum := !sum + 1;
  if not (Int.equal (sign c o b) (-1)) then sum := !sum + 1;
  if Int.equal (sign a o c) 1 then sum := !sum + 1;
  !sum >= 2
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

let dbl_error_u = Float_u.of_float dbl_error
let intersection_error = S1_angle.of_radians Float_u.O.(#8.0 * dbl_error_u)
let intersection_merge_radius = S1_angle.of_radians Float_u.O.(#16.0 * dbl_error_u)
let rounding_epsilon_f64 = dbl_error

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
  (a_norm_len : float)
  (a0 : S2_point.t)
  (a1 : S2_point.t)
  =
  let x0 = R3_vector.sub x (S2_point.to_r3 a0) in
  let x1 = R3_vector.sub x (S2_point.to_r3 a1) in
  let x0_dist2 = Float_u.to_float (R3_vector.norm2 x0) in
  let x1_dist2 = Float_u.to_float (R3_vector.norm2 x1) in
  let sqrt3 = Float.sqrt 3.0 in
  let dist, proj =
    if Float.( < ) x0_dist2 x1_dist2
       || (Float.( = ) x0_dist2 x1_dist2 && R3_vector.compare x0 x1 < 0)
    then Float.sqrt x0_dist2, Float_u.to_float (R3_vector.dot x0 a_norm)
    else Float.sqrt x1_dist2, Float_u.to_float (R3_vector.dot x1 a_norm)
  in
  let t_err = rounding_epsilon_f64 in
  let bound =
    (((((3.5 +. (2.0 *. sqrt3)) *. a_norm_len) +. (32.0 *. sqrt3 *. dbl_error)) *. dist)
     +. (1.5 *. Float.abs proj))
    *. t_err
  in
  proj, bound
;;

let get_intersection_stable_sorted
  (a0 : S2_point.t)
  (a1 : S2_point.t)
  (b0 : S2_point.t)
  (b1 : S2_point.t)
  =
  let a_norm =
    R3_vector.cross
      (R3_vector.sub (S2_point.to_r3 a0) (S2_point.to_r3 a1))
      (R3_vector.add (S2_point.to_r3 a0) (S2_point.to_r3 a1))
  in
  let a_norm_len = Float_u.to_float (R3_vector.norm a_norm) in
  let b_len =
    Float_u.to_float
      (R3_vector.norm (R3_vector.sub (S2_point.to_r3 b1) (S2_point.to_r3 b0)))
  in
  let b0_dist, b0_error = get_projection (S2_point.to_r3 b0) a_norm a_norm_len a0 a1 in
  let b1_dist, b1_error = get_projection (S2_point.to_r3 b1) a_norm a_norm_len a0 a1 in
  let b0_dist, b1_dist =
    if Float.( < ) b0_dist b1_dist
    then Float.neg b0_dist, Float.neg b1_dist
    else b0_dist, b1_dist
  in
  let dist_sum = b0_dist -. b1_dist in
  let error_sum = b0_error +. b1_error in
  if Float.( <= ) dist_sum error_sum
  then R3_vector.Option.none
  else (
    let x =
      R3_vector.sub
        (R3_vector.mul (S2_point.to_r3 b1) (Float_u.of_float b0_dist))
        (R3_vector.mul (S2_point.to_r3 b0) (Float_u.of_float b1_dist))
    in
    let t_err = rounding_epsilon_f64 in
    let err =
      (b_len
       *. Float.abs ((b0_dist *. b1_error) -. (b1_dist *. b0_error))
       /. (dist_sum -. error_sum))
      +. (2.0 *. t_err *. dist_sum)
    in
    let x_len2 = Float_u.to_float (R3_vector.norm2 x) in
    if Float.( < ) x_len2 Float.min_value
    then R3_vector.Option.none
    else (
      let x_len = Float.sqrt x_len2 in
      let max_err = Float_u.to_float (S1_angle.radians intersection_error) in
      if Float.( > ) err ((max_err -. t_err) *. x_len)
      then R3_vector.Option.none
      else R3_vector.Option.some (R3_vector.normalize x)))
;;

let get_intersection_stable
  (a0 : S2_point.t)
  (a1 : S2_point.t)
  (b0 : S2_point.t)
  (b1 : S2_point.t)
  =
  let a_len2 =
    Float_u.to_float
      (R3_vector.norm2 (R3_vector.sub (S2_point.to_r3 a1) (S2_point.to_r3 a0)))
  in
  let b_len2 =
    Float_u.to_float
      (R3_vector.norm2 (R3_vector.sub (S2_point.to_r3 b1) (S2_point.to_r3 b0)))
  in
  if Float.( < ) a_len2 b_len2 || (Float.( = ) a_len2 b_len2 && compare_edges a0 a1 b0 b1)
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
  if Float.( < ) (Float_u.to_float (R3_vector.dot (S2_point.to_r3 pt) sum)) 0.0
  then R3_vector.neg pt
  else pt
;;
