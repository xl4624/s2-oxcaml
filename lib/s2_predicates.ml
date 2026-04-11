open Core
module V = R3_vector

(* ============================================================ *)
(* Minimal arbitrary-precision arithmetic used for the exact     *)
(* fallback of the geometric predicates below. We avoid pulling  *)
(* in a general bignum dependency by implementing just enough    *)
(* signed bigints and dyadic rationals to evaluate the sign of   *)
(* bounded-degree polynomials over IEEE-754 double inputs.       *)
(* ============================================================ *)

module Bigint = struct
  (* Signed-magnitude arbitrary-precision integers in little-endian
     base 2^30. Storing 30 bits per limb keeps the product of two
     limbs within OCaml's 63-bit native int. *)
  type t =
    { sign : int (* -1, 0, or +1 *)
    ; digits : int array (* least-significant first, no trailing zeros *)
    }

  let base_bits = 30
  let base = 1 lsl base_bits
  let mask = base - 1
  let zero = { sign = 0; digits = [||] }

  let normalize ~sign digits =
    let n = ref (Array.length digits) in
    while !n > 0 && digits.(!n - 1) = 0 do
      decr n
    done;
    if !n = 0
    then zero
    else if !n = Array.length digits
    then { sign; digits }
    else { sign; digits = Array.sub digits ~pos:0 ~len:!n }
  ;;

  let sign t = t.sign
  let neg t = if t.sign = 0 then t else { t with sign = -t.sign }

  let cmp_abs a b =
    let la = Array.length a in
    let lb = Array.length b in
    if la <> lb
    then Int.compare la lb
    else (
      let rec loop i =
        if i < 0
        then 0
        else (
          let d = Int.compare a.(i) b.(i) in
          if d <> 0 then d else loop (i - 1))
      in
      loop (la - 1))
  ;;

  let add_abs ad bd =
    let la = Array.length ad in
    let lb = Array.length bd in
    let n = Int.max la lb + 1 in
    let r = Array.create ~len:n 0 in
    let carry = ref 0 in
    for i = 0 to n - 1 do
      let x = if i < la then ad.(i) else 0 in
      let y = if i < lb then bd.(i) else 0 in
      let s = x + y + !carry in
      r.(i) <- s land mask;
      carry := s lsr base_bits
    done;
    r
  ;;

  (* Assumes |ad| >= |bd|. *)
  let sub_abs ad bd =
    let la = Array.length ad in
    let lb = Array.length bd in
    let r = Array.create ~len:la 0 in
    let borrow = ref 0 in
    for i = 0 to la - 1 do
      let x = ad.(i) in
      let y = if i < lb then bd.(i) else 0 in
      let d = x - y - !borrow in
      if d < 0
      then (
        r.(i) <- d + base;
        borrow := 1)
      else (
        r.(i) <- d;
        borrow := 0)
    done;
    assert (!borrow = 0);
    r
  ;;

  let add a b =
    if a.sign = 0
    then b
    else if b.sign = 0
    then a
    else if a.sign = b.sign
    then normalize ~sign:a.sign (add_abs a.digits b.digits)
    else (
      let c = cmp_abs a.digits b.digits in
      if c = 0
      then zero
      else if c > 0
      then normalize ~sign:a.sign (sub_abs a.digits b.digits)
      else normalize ~sign:b.sign (sub_abs b.digits a.digits))
  ;;

  let sub a b = add a (neg b)

  let mul_abs ad bd =
    let la = Array.length ad in
    let lb = Array.length bd in
    if la = 0 || lb = 0
    then [||]
    else (
      let r = Array.create ~len:(la + lb) 0 in
      for i = 0 to la - 1 do
        let carry = ref 0 in
        let ai = ad.(i) in
        for j = 0 to lb - 1 do
          (* ai, bd.(j) < 2^30 so ai*bd.(j) < 2^60. Adding r.(i+j) < 2^30
             and carry < 2^31 keeps s < 2^61, within OCaml's 63-bit int. *)
          let s = r.(i + j) + (ai * bd.(j)) + !carry in
          r.(i + j) <- s land mask;
          carry := s lsr base_bits
        done;
        r.(i + lb) <- r.(i + lb) + !carry
      done;
      r)
  ;;

  let mul a b =
    if a.sign = 0 || b.sign = 0
    then zero
    else normalize ~sign:(a.sign * b.sign) (mul_abs a.digits b.digits)
  ;;

  let shift_left a k =
    assert (k >= 0);
    if a.sign = 0 || k = 0
    then a
    else (
      let nd = k / base_bits in
      let nr = k mod base_bits in
      let la = Array.length a.digits in
      if nr = 0
      then (
        let r = Array.create ~len:(la + nd) 0 in
        Array.blit ~src:a.digits ~src_pos:0 ~dst:r ~dst_pos:nd ~len:la;
        normalize ~sign:a.sign r)
      else (
        let r = Array.create ~len:(la + nd + 1) 0 in
        let carry = ref 0 in
        for i = 0 to la - 1 do
          let v = a.digits.(i) in
          r.(i + nd) <- (v lsl nr) lor !carry land mask;
          carry := v lsr (base_bits - nr)
        done;
        r.(la + nd) <- !carry;
        normalize ~sign:a.sign r))
  ;;

  let of_int n =
    if n = 0
    then zero
    else (
      let sign = if n < 0 then -1 else 1 in
      let m = abs n in
      let d0 = m land mask in
      let d1 = (m lsr base_bits) land mask in
      let d2 = m lsr (2 * base_bits) in
      let digits =
        if d2 <> 0 then [| d0; d1; d2 |] else if d1 <> 0 then [| d0; d1 |] else [| d0 |]
      in
      normalize ~sign digits)
  ;;
end

module Dyadic = struct
  (* Dyadic rational m * 2^exp where m is an arbitrary-precision integer.
     Closed under +, -, * on IEEE-754 double inputs, which is enough for
     the polynomial sign tests used by the exact predicates. *)
  type t =
    { m : Bigint.t
    ; exp : int
    }

  let zero = { m = Bigint.zero; exp = 0 }
  let sign t = Bigint.sign t.m
  let neg t = { t with m = Bigint.neg t.m }

  let of_float x =
    if Float.equal x 0.0
    then zero
    else (
      assert (Float.is_finite x);
      let mf, e = Float.frexp x in
      (* |mf| in [0.5, 1); mf * 2^53 is an exact integer in (-2^53, 2^53). *)
      let im = Int.of_float (mf *. Float.ldexp 1.0 53) in
      { m = Bigint.of_int im; exp = e - 53 })
  ;;

  let mul a b = { m = Bigint.mul a.m b.m; exp = a.exp + b.exp }

  let add a b =
    if Bigint.sign a.m = 0
    then b
    else if Bigint.sign b.m = 0
    then a
    else if a.exp = b.exp
    then { m = Bigint.add a.m b.m; exp = a.exp }
    else if a.exp > b.exp
    then { m = Bigint.add (Bigint.shift_left a.m (a.exp - b.exp)) b.m; exp = b.exp }
    else { m = Bigint.add a.m (Bigint.shift_left b.m (b.exp - a.exp)); exp = a.exp }
  ;;

  let sub a b = add a (neg b)
end

module Exact_vec = struct
  type t =
    { x : Dyadic.t
    ; y : Dyadic.t
    ; z : Dyadic.t
    }

  let of_r3 v =
    { x = Dyadic.of_float (Float_u.to_float (V.x v))
    ; y = Dyadic.of_float (Float_u.to_float (V.y v))
    ; z = Dyadic.of_float (Float_u.to_float (V.z v))
    }
  ;;

  let dot a b =
    Dyadic.add (Dyadic.add (Dyadic.mul a.x b.x) (Dyadic.mul a.y b.y)) (Dyadic.mul a.z b.z)
  ;;

  let cross a b =
    { x = Dyadic.sub (Dyadic.mul a.y b.z) (Dyadic.mul a.z b.y)
    ; y = Dyadic.sub (Dyadic.mul a.z b.x) (Dyadic.mul a.x b.z)
    ; z = Dyadic.sub (Dyadic.mul a.x b.y) (Dyadic.mul a.y b.x)
    }
  ;;

  let norm2 a = dot a a
end

(* ============================================================ *)
(* Constants (from predicates.go).                               *)
(* ============================================================ *)

(* dblEpsilon = 2^-52. *)
let[@inline] dbl_epsilon () = Float_u.of_float 2.220446049250313e-16

(* roundingEpsilon for float64 = 2^-53. *)
let[@inline] dbl_error () = Float_u.of_float 1.110223024625156e-16

(* Maximum error in triageSign's determinant. *)
let[@inline] max_determinant_error () =
  let open Float_u.O in
  #1.8274 * dbl_epsilon ()
;;

(* Scaling factor for stableSign. *)
let[@inline] det_error_multiplier () =
  let open Float_u.O in
  #3.2321 * dbl_epsilon ()
;;

module Direction = struct
  type t =
    | Clockwise
    | Counter_clockwise
    | Indeterminate

  let to_int = function
    | Clockwise -> -1
    | Indeterminate -> 0
    | Counter_clockwise -> 1
  ;;

  let of_int n =
    if n < 0 then Clockwise else if n > 0 then Counter_clockwise else Indeterminate
  ;;
end

let[@inline] sign a b c =
  let open Float_u.O in
  (* (C x A) . B > 0. *)
  V.dot (V.cross c a) b > #0.0
;;

(* triageSign: compute det = (A x B) . C and compare to error bound. *)
let[@inline] triage_sign a b c =
  let open Float_u.O in
  let det = V.dot (V.cross a b) c in
  if det > max_determinant_error ()
  then Direction.Counter_clockwise
  else if det < Float_u.neg (max_determinant_error ())
  then Direction.Clockwise
  else Direction.Indeterminate
;;

(* Errors smaller than [min_no_underflow_error] cannot be trusted because the
   intermediate product [e1n2 * e2n2] has already underflowed to zero. This
   guard mirrors kMinNoUnderflowError in the C++ port; the Go port omits it,
   which is why the Go-flavored version mishandles cases like Sign.StableSignUnderflow. *)
let[@inline] min_no_underflow_error () =
  let open Float_u.O in
  (* DBL_MIN = 2.2250738585072014e-308 *)
  det_error_multiplier () * Float_u.sqrt (Float_u.of_float 2.2250738585072014e-308)
;;

(* stableSign: numerically stable formula that handles near-collinear points. *)
let stable_sign a b c =
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

(* Symbolic perturbation for exact_sign: assumes xa < xb < xc in lex order and
   the exact determinant is zero. Enumerates perturbation coefficients in order
   of decreasing magnitude (Simulation of Simplicity, Edelsbrunner and Muecke
   1990), mirroring the table in predicates.go / s2predicates.cc. *)
let symbolically_perturbed_sign
  (a : Exact_vec.t)
  (b : Exact_vec.t)
  (c : Exact_vec.t)
  (b_cross_c : Exact_vec.t)
  =
  let module D = Dyadic in
  (* da.Z *)
  let s = D.sign b_cross_c.z in
  if s <> 0
  then s
  else (
    (* da.Y *)
    let s = D.sign b_cross_c.y in
    if s <> 0
    then s
    else (
      (* da.X *)
      let s = D.sign b_cross_c.x in
      if s <> 0
      then s
      else (
        (* db.Z: c.X * a.Y - c.Y * a.X *)
        let s = D.sign (D.sub (D.mul c.x a.y) (D.mul c.y a.x)) in
        if s <> 0
        then s
        else (
          (* db.Z * da.Y *)
          let s = D.sign c.x in
          if s <> 0
          then s
          else (
            (* db.Z * da.X *)
            let s = -D.sign c.y in
            if s <> 0
            then s
            else (
              (* db.Y: c.Z * a.X - c.X * a.Z *)
              let s = D.sign (D.sub (D.mul c.z a.x) (D.mul c.x a.z)) in
              if s <> 0
              then s
              else (
                (* db.Y * da.X *)
                let s = D.sign c.z in
                if s <> 0
                then s
                else (
                  (* dc.Z: a.X * b.Y - a.Y * b.X *)
                  let s = D.sign (D.sub (D.mul a.x b.y) (D.mul a.y b.x)) in
                  if s <> 0
                  then s
                  else (
                    (* dc.Z * da.Y *)
                    let s = -D.sign b.x in
                    if s <> 0
                    then s
                    else (
                      (* dc.Z * da.X *)
                      let s = D.sign b.y in
                      if s <> 0
                      then s
                      else (
                        (* dc.Z * db.Y *)
                        let s = D.sign a.x in
                        if s <> 0 then s else (* dc.Z * db.Y * da.X: always +1 *)
                                           1)))))))))))
;;

(* exactSign: arbitrary-precision determinant evaluation plus symbolic
   perturbation. Mirrors exactSign in predicates.go. Requires pairwise-distinct
   inputs. *)
let exact_sign a b c ~perturb =
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

let robust_sign a b c =
  let s = triage_sign a b c in
  match s with
  | Counter_clockwise | Clockwise -> s
  | Indeterminate ->
    (* Return zero if and only if two of the points are equal. This matches
       property 1 of RobustSign and lets us assume distinct inputs in
       exact_sign below. *)
    if V.equal a b || V.equal b c || V.equal c a
    then Direction.Indeterminate
    else (
      match stable_sign a b c with
      | (Counter_clockwise | Clockwise) as s -> s
      | Indeterminate -> Direction.of_int (exact_sign a b c ~perturb:true))
;;

let ordered_ccw a b c o =
  let sum = ref 0 in
  (match robust_sign b o a with
   | Clockwise -> ()
   | _ -> incr sum);
  (match robust_sign c o b with
   | Clockwise -> ()
   | _ -> incr sum);
  (match robust_sign a o c with
   | Counter_clockwise -> incr sum
   | _ -> ());
  !sum >= 2
;;

(* cosDistance returns (cos, err) where cos = x.y and err is the bound. *)
let[@inline] cos_distance_err x y =
  let open Float_u.O in
  let cos = V.dot x y in
  let err = (#9.5 * dbl_error () * Float_u.abs cos) + (#1.5 * dbl_error ()) in
  #(cos, err)
;;

(* sin2Distance returns (sin2, err) via the (x-y) x (x+y) trick. *)
let[@inline] sin2_distance_err x y =
  let open Float_u.O in
  let n = V.cross (V.sub x y) (V.add x y) in
  let sin2 = #0.25 * V.norm2 n in
  let sqrt3 = Float_u.of_float 1.7320508075688772 in
  let err =
    (#21.0 * dbl_error () * sin2)
    + (#4.0 * sqrt3 * dbl_error () * sin2)
    + (#32.0 * sqrt3 * dbl_error () * dbl_error () * Float_u.sqrt sin2)
    + (#768.0 * dbl_error () * dbl_error () * dbl_error () * dbl_error ())
  in
  #(sin2, err)
;;

let[@inline] triage_compare_cos_distances x a b =
  let open Float_u.O in
  let #(cos_ax, err_ax) = cos_distance_err a x in
  let #(cos_bx, err_bx) = cos_distance_err b x in
  let diff = cos_ax - cos_bx in
  let err = err_ax + err_bx in
  if diff > err then -1 else if diff < Float_u.neg err then 1 else 0
;;

let[@inline] triage_compare_sin2_distances x a b =
  let open Float_u.O in
  let #(sin2_ax, err_ax) = sin2_distance_err a x in
  let #(sin2_bx, err_bx) = sin2_distance_err b x in
  let diff = sin2_ax - sin2_bx in
  let err = err_ax + err_bx in
  if diff > err then 1 else if diff < Float_u.neg err then -1 else 0
;;

(* Symbolic perturbation for a != b with equal projected distance: if A < B
   lexicographically then A's pedestal is higher, so AX > BX, so return -1. *)
let[@inline] symbolic_compare_distances _x a b =
  let c = V.compare a b in
  if c < 0 then 1 else if c > 0 then -1 else 0
;;

(* exactCompareDistances: mirrors Go's version. Compares AX vs BX by reducing
   to a sign test on a polynomial in the exact coordinates. *)
let exact_compare_distances x a b =
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

let compare_distances x a b =
  let s = triage_compare_cos_distances x a b in
  if s <> 0
  then s
  else if V.equal a b
  then 0
  else (
    let cos_ax = V.dot a x in
    let inv_sqrt2 = Float_u.of_float 0.7071067811865476 in
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
let[@inline] ca_45_degrees_length2 () = Float_u.of_float (2.0 -. 1.4142135623730951)

let[@inline] triage_compare_cos_distance x y r2 =
  let open Float_u.O in
  let #(cos_xy, err_xy) = cos_distance_err x y in
  let cos_r = #1.0 - (#0.5 * r2) in
  let err_r = #2.0 * dbl_error () * cos_r in
  let diff = cos_xy - cos_r in
  let err = err_xy + err_r in
  if diff > err then -1 else if diff < Float_u.neg err then 1 else 0
;;

let[@inline] triage_compare_sin2_distance x y r2 =
  let open Float_u.O in
  let #(sin2_xy, err_xy) = sin2_distance_err x y in
  let sin2_r = r2 * (#1.0 - (#0.25 * r2)) in
  let err_r = #3.0 * dbl_error () * sin2_r in
  let diff = sin2_xy - sin2_r in
  let err = err_xy + err_r in
  if diff > err then 1 else if diff < Float_u.neg err then -1 else 0
;;

(* exactCompareDistance: mirrors Go's version. Compares d(X,Y) against a chord
   angle whose squared length is [r2]. *)
let exact_compare_distance x y r2 =
  let module D = Dyadic in
  let xv = Exact_vec.of_r3 x in
  let yv = Exact_vec.of_r3 y in
  let cos_xy = Exact_vec.dot xv yv in
  let r2d = D.of_float (Float_u.to_float r2) in
  let one = D.of_float 1.0 in
  let half = D.of_float 0.5 in
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

let compare_distance x y r =
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

(* sign_dot_prod: triageSignDotProd with exact fallback. Error bound =
   3.046875 * dblEpsilon. *)
let sign_dot_prod a b =
  let open Float_u.O in
  let max_error = #3.046875 * dbl_epsilon () in
  let dp = V.dot a b in
  if Float_u.abs dp > max_error
  then if dp > #0.0 then 1 else -1
  else (
    (* Exact dot product in dyadic arithmetic. *)
    let av = Exact_vec.of_r3 a in
    let bv = Exact_vec.of_r3 b in
    Dyadic.sign (Exact_vec.dot av bv))
;;
