open Core
module V = R3_vector

(* ============================================================ *)
(* Minimal arbitrary-precision arithmetic used for the exact    *)
(* fallback of the geometric predicates below. Bigint, Dyadic,  *)
(* and Exact_vec are unboxed records (#{}): their fields are    *)
(* passed inline/in registers with no record-header allocation. *)
(* ============================================================ *)

module Bigint = struct
  (* Signed-magnitude arbitrary-precision integers in little-endian
     base 2^30. Storing 30 bits per limb keeps the product of two
     limbs within OCaml's 63-bit native int.

     Each [t] owns a fixed-capacity local [int array] of length [capacity].
     Only the first [len] limbs are meaningful; the rest are zero.

     [capacity = 64] gives 1920 bits of headroom. The worst offender is
     [exact_compare_distances]'s degree-6 polynomial: when one input
     coordinate is very small (e.g. 7e-101 in the fixture), the
     [Dyadic.add] inside [Exact_vec.norm2] must shift the larger summand
     left by ~560 bits (~19 limbs) to align exponents, and subsequent
     squaring and cross-multiplication push the largest intermediate to
     ~35 limbs. 64 leaves comfortable margin for the IEEE-corner case
     where coordinates approach the smallest normal double. *)
  let base_bits = 30
  let base = 1 lsl base_bits
  let mask = base - 1

  type t =
    #{ sign : int (* -1, 0, or +1 *)
     ; len : int (* number of meaningful limbs *)
     ; digits : int array (* least-significant first; digits[len..] are zero *)
     }

  let[@inline] [@zero_alloc] sign (t @ local) = t.#sign
  let[@zero_alloc assume] make_limbs () = exclave_ Array.create_local ~len:64 0
  let[@zero_alloc] zero () = exclave_ #{ sign = 0; len = 0; digits = make_limbs () }

  let[@zero_alloc] neg (t @ local) = exclave_
    #{ sign = -t.#sign; len = t.#len; digits = t.#digits }
  ;;

  let[@zero_alloc] of_int n = exclave_
    let r = make_limbs () in
    if n = 0
    then #{ sign = 0; len = 0; digits = r }
    else (
      let sign = if n < 0 then -1 else 1 in
      let m = abs n in
      r.(0) <- m land mask;
      r.(1) <- (m lsr base_bits) land mask;
      r.(2) <- m lsr (2 * base_bits);
      let mutable len = 3 in
      while len > 0 && r.(len - 1) = 0 do
        len <- len - 1
      done;
      #{ sign; len; digits = r })
  ;;

  let[@zero_alloc] cmp_abs (ad : int array) la (bd : int array) lb =
    if la <> lb
    then Int.compare la lb
    else (
      let mutable i = la - 1 in
      let mutable result = 0 in
      while i >= 0 && result = 0 do
        let d = Int.compare ad.(i) bd.(i) in
        if d <> 0 then result <- d;
        i <- i - 1
      done;
      result)
  ;;

  let[@zero_alloc] add (a @ local) (b @ local) = exclave_
    if a.#sign = 0
    then b
    else if b.#sign = 0
    then a
    else (
      let r = make_limbs () in
      let la = a.#len in
      let lb = b.#len in
      let ad = a.#digits in
      let bd = b.#digits in
      if a.#sign = b.#sign
      then (
        let n = Int.max la lb + 1 in
        let mutable carry = 0 in
        for i = 0 to n - 1 do
          let x = if i < la then ad.(i) else 0 in
          let y = if i < lb then bd.(i) else 0 in
          let s = x + y + carry in
          r.(i) <- s land mask;
          carry <- s lsr base_bits
        done;
        let mutable len = n in
        while len > 0 && r.(len - 1) = 0 do
          len <- len - 1
        done;
        #{ sign = a.#sign; len; digits = r })
      else (
        let c = cmp_abs ad la bd lb in
        if c = 0
        then #{ sign = 0; len = 0; digits = r }
        else if c > 0
        then (
          let mutable borrow = 0 in
          for i = 0 to la - 1 do
            let x = ad.(i) in
            let y = if i < lb then bd.(i) else 0 in
            let d = x - y - borrow in
            if d < 0
            then (
              r.(i) <- d + base;
              borrow <- 1)
            else (
              r.(i) <- d;
              borrow <- 0)
          done;
          assert (borrow = 0);
          let mutable len = la in
          while len > 0 && r.(len - 1) = 0 do
            len <- len - 1
          done;
          #{ sign = a.#sign; len; digits = r })
        else (
          let mutable borrow = 0 in
          for i = 0 to lb - 1 do
            let x = bd.(i) in
            let y = if i < la then ad.(i) else 0 in
            let d = x - y - borrow in
            if d < 0
            then (
              r.(i) <- d + base;
              borrow <- 1)
            else (
              r.(i) <- d;
              borrow <- 0)
          done;
          assert (borrow = 0);
          let mutable len = lb in
          while len > 0 && r.(len - 1) = 0 do
            len <- len - 1
          done;
          #{ sign = b.#sign; len; digits = r })))
  ;;

  let[@zero_alloc] sub (a @ local) (b @ local) = exclave_ add a (neg b)

  let[@zero_alloc] mul (a @ local) (b @ local) = exclave_
    if a.#sign = 0 || b.#sign = 0
    then zero ()
    else (
      let r = make_limbs () in
      let la = a.#len in
      let lb = b.#len in
      let ad = a.#digits in
      let bd = b.#digits in
      for i = 0 to la - 1 do
        let mutable carry = 0 in
        let ai = ad.(i) in
        for j = 0 to lb - 1 do
          (* ai, bd.(j) < 2^30 so ai*bd.(j) < 2^60. Adding r.(i+j) < 2^30
               and carry < 2^31 keeps s < 2^61, within OCaml's 63-bit int. *)
          let s = r.(i + j) + (ai * bd.(j)) + carry in
          r.(i + j) <- s land mask;
          carry <- s lsr base_bits
        done;
        r.(i + lb) <- r.(i + lb) + carry
      done;
      let mutable len = la + lb in
      while len > 0 && r.(len - 1) = 0 do
        len <- len - 1
      done;
      #{ sign = a.#sign * b.#sign; len; digits = r })
  ;;

  let[@zero_alloc] shift_left (a @ local) k = exclave_
    assert (k >= 0);
    if a.#sign = 0 || k = 0
    then a
    else (
      let r = make_limbs () in
      let nd = k / base_bits in
      let nr = k mod base_bits in
      let la = a.#len in
      let ad = a.#digits in
      if nr = 0
      then (
        for i = 0 to la - 1 do
          r.(i + nd) <- ad.(i)
        done;
        let mutable len = la + nd in
        while len > 0 && r.(len - 1) = 0 do
          len <- len - 1
        done;
        #{ sign = a.#sign; len; digits = r })
      else (
        let mutable carry = 0 in
        for i = 0 to la - 1 do
          let v = ad.(i) in
          r.(i + nd) <- (v lsl nr) lor carry land mask;
          carry <- v lsr (base_bits - nr)
        done;
        r.(la + nd) <- carry;
        let mutable len = la + nd + 1 in
        while len > 0 && r.(len - 1) = 0 do
          len <- len - 1
        done;
        #{ sign = a.#sign; len; digits = r }))
  ;;
end

module Dyadic = struct
  (* Dyadic rational m * 2^exp where m is a stack-local Bigint. Closed under
     +, -, * on IEEE-754 double inputs, which is enough for the polynomial
     sign tests used by the exact predicates. *)
  type t =
    #{ m : Bigint.t
     ; exp : int
     }

  let[@zero_alloc] zero () = exclave_ #{ m = Bigint.zero (); exp = 0 }
  let[@inline] [@zero_alloc] sign (t @ local) = Bigint.sign t.#m
  let[@zero_alloc] neg (t @ local) = exclave_ #{ m = Bigint.neg t.#m; exp = t.#exp }

  (* Decode the IEEE-754 bit pattern of [x] into a signed integer mantissa
     and an exponent so that [x = im * 2^exp] exactly. *)
  let[@zero_alloc] of_float (x : float#) = exclave_
    if Float_u.O.(x = #0.0)
    then zero ()
    else (
      let bits = Int64_u.bits_of_float x in
      let sign_is_neg = Int64_u.O.(bits < #0L) in
      let biased_exp = Int64_u.to_int_trunc Int64_u.O.((bits lsr 52) land #0x7ffL) in
      let mantissa = Int64_u.to_int_trunc Int64_u.O.(bits land #0xfffffffffffffL) in
      let mutable im_pos = 0 in
      let mutable exp = 0 in
      if biased_exp = 0
      then (
        (* Subnormal: value = mantissa * 2^(-1074). *)
        im_pos <- mantissa;
        exp <- -1074)
      else (
        (* Normal: value = (2^52 + mantissa) * 2^(biased_exp - 1023 - 52). *)
        im_pos <- mantissa lor (1 lsl 52);
        exp <- biased_exp - 1023 - 52);
      let im = if sign_is_neg then -im_pos else im_pos in
      #{ m = Bigint.of_int im; exp })
  ;;

  let[@zero_alloc] mul (a @ local) (b @ local) = exclave_
    #{ m = Bigint.mul a.#m b.#m; exp = a.#exp + b.#exp }
  ;;

  let[@zero_alloc] add (a @ local) (b @ local) = exclave_
    if Bigint.sign a.#m = 0
    then b
    else if Bigint.sign b.#m = 0
    then a
    else if a.#exp = b.#exp
    then #{ m = Bigint.add a.#m b.#m; exp = a.#exp }
    else if a.#exp > b.#exp
    then #{ m = Bigint.add (Bigint.shift_left a.#m (a.#exp - b.#exp)) b.#m; exp = b.#exp }
    else #{ m = Bigint.add a.#m (Bigint.shift_left b.#m (b.#exp - a.#exp)); exp = a.#exp }
  ;;

  let[@zero_alloc] sub (a @ local) (b @ local) = exclave_ add a (neg b)
end

module Exact_vec = struct
  type t =
    #{ x : Dyadic.t
     ; y : Dyadic.t
     ; z : Dyadic.t
     }

  let[@zero_alloc] of_r3 v = exclave_
    #{ x = Dyadic.of_float (V.x v)
     ; y = Dyadic.of_float (V.y v)
     ; z = Dyadic.of_float (V.z v)
     }
  ;;

  let[@zero_alloc] dot (a @ local) (b @ local) = exclave_
    Dyadic.add
      (Dyadic.add (Dyadic.mul a.#x b.#x) (Dyadic.mul a.#y b.#y))
      (Dyadic.mul a.#z b.#z)
  ;;

  let[@zero_alloc] cross (a @ local) (b @ local) = exclave_
    #{ x = Dyadic.sub (Dyadic.mul a.#y b.#z) (Dyadic.mul a.#z b.#y)
     ; y = Dyadic.sub (Dyadic.mul a.#z b.#x) (Dyadic.mul a.#x b.#z)
     ; z = Dyadic.sub (Dyadic.mul a.#x b.#y) (Dyadic.mul a.#y b.#x)
     }
  ;;

  let[@zero_alloc] norm2 (a @ local) = exclave_ dot a a
end

(* ============================================================ *)
(* Orientation/determinant constants.                            *)
(* ============================================================ *)

(* roundingEpsilon for float64 = 2^-53. *)
let[@inline] [@zero_alloc] dbl_error () = Float_u.O.(Float_u.epsilon_float () / #2.0)

(* Maximum error in triageSign's determinant. *)
let[@inline] [@zero_alloc] max_determinant_error () =
  let open Float_u.O in
  #1.8274 * Float_u.epsilon_float ()
;;

(* Scaling factor for stableSign. *)
let[@inline] [@zero_alloc] det_error_multiplier () =
  let open Float_u.O in
  #3.2321 * Float_u.epsilon_float ()
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

(* triageSign: compute det = (A x B) . C and compare to error bound. *)
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
  det_error_multiplier () * Float_u.sqrt (Float_u.min_positive_normal_value ())
;;

(* stableSign: numerically stable formula that handles near-collinear points. *)
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

(* Symbolic perturbation for exact_sign: assumes xa < xb < xc in lex order and
   the exact determinant is zero. Enumerates perturbation coefficients in order
   of decreasing magnitude (Simulation of Simplicity, Edelsbrunner and Muecke
   1990). *)
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

(* exactSign: arbitrary-precision determinant evaluation plus symbolic perturbation.
   Requires pairwise-distinct inputs. *)
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

(* exactCompareDistances: mirrors Go's version. Compares AX vs BX by reducing
   to a sign test on a polynomial in the exact coordinates. *)
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

(* exactCompareDistance: mirrors Go's version. Compares d(X,Y) against a chord
   angle whose squared length is [r2]. *)
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

(* sign_dot_prod: triageSignDotProd with exact fallback. Error bound =
   3.046875 * dblEpsilon. *)
let[@zero_alloc] sign_dot_prod a b =
  let open Float_u.O in
  let max_error = #3.046875 * Float_u.epsilon_float () in
  let dp = V.dot a b in
  if Float_u.abs dp > max_error
  then if dp > #0.0 then 1 else -1
  else (
    (* Exact dot product in dyadic arithmetic. *)
    let av = Exact_vec.of_r3 a in
    let bv = Exact_vec.of_r3 b in
    Dyadic.sign (Exact_vec.dot av bv) [@nontail])
;;
