open Core
module V = R3_vector

(* Minimal arbitrary-precision arithmetic used for the exact fallback of the
   geometric predicates and [S2_point.robust_cross_prod]. Bigint, Dyadic, and
   Exact_vec are unboxed records (#{}): their fields are passed inline/in
   registers with no record-header allocation. *)

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

  (* Number of bits in the binary representation of |m|. Returns 0 when m = 0,
     otherwise [floor(log2 |m|) + 1]. *)
  let[@zero_alloc] bit_length (t @ local) =
    if t.#sign = 0
    then 0
    else (
      let top = t.#digits.(t.#len - 1) in
      (* [top] is nonzero because trailing zero limbs were stripped. *)
      Int.( + )
        (Int.( * ) (Int.( - ) t.#len 1) base_bits)
        (Int.( + ) (Int.floor_log2 top) 1))
  ;;

  (* Convert to a double. If |m| has more than 53 bits of precision, low-order
     bits are lost (the high 53 bits are preserved). If the magnitude exceeds
     the double range, returns +/-inf. Zero-alloc but only approximate for
     large magnitudes; this is fine for direction-preserving uses like
     [S2_point.robust_cross_prod] where the caller rescales via [ldexp]. *)
  let[@zero_alloc] to_float_approx (t @ local) =
    if t.#sign = 0
    then #0.0
    else (
      (* Horner evaluation of [sum_i digits.(i) * 2^(30 * i)] from top down.
         Each step multiplies by [2^30] and adds the next limb. Intermediate
         values only overflow for inputs whose magnitude already exceeds
         [~2^1023], at which point the result is correctly [+inf]. *)
      let mutable acc = #0.0 in
      for i = Int.( - ) t.#len 1 downto 0 do
        acc <- Float_u.O.((acc * #0x1p30) + Float_u.of_int t.#digits.(i))
      done;
      if Int.( < ) t.#sign 0 then Float_u.neg acc else acc)
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

  (* Position of the most significant bit of the exact value, i.e. [e] such
     that [2^(e-1) <= |value| < 2^e]. Returns [Int.min_value] when the value
     is exactly zero (sentinel). *)
  let[@zero_alloc] true_exp (t @ local) =
    if Bigint.sign t.#m = 0
    then Int.min_value
    else Int.( + ) (Bigint.bit_length t.#m) t.#exp
  ;;

  (* Convert to a double scaled by [2^(-exp_offset)]. That is, returns the
     IEEE double closest to [m * 2^(exp - exp_offset)]. Overflows to +/-inf
     and underflows to 0 at the IEEE limits. Precision loss for [|m|] wider
     than 53 bits is absorbed via [Bigint.to_float_approx]; for cross products
     of double inputs this loses bits well below the result's top 53 and has
     no effect on the direction returned by [normalizable_from_exact]. *)
  let[@zero_alloc] to_float_scaled (t @ local) ~exp_offset =
    if Bigint.sign t.#m = 0
    then #0.0
    else Float_u.ldexp (Bigint.to_float_approx t.#m) (Int.( - ) t.#exp exp_offset)
  ;;
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

  let[@inline] [@zero_alloc] is_zero (t @ local) =
    Dyadic.sign t.#x = 0 && Dyadic.sign t.#y = 0 && Dyadic.sign t.#z = 0
  ;;
end

(* Convert an exact vector back to an R3_vector, scaling so the result is
   normalizable (max component magnitude in [2^(-242), 1)). Returns the zero
   vector when the exact value is exactly zero. *)
let[@zero_alloc] normalizable_from_exact (v : Exact_vec.t @ local) =
  let ex = Dyadic.true_exp v.#x in
  let ey = Dyadic.true_exp v.#y in
  let ez = Dyadic.true_exp v.#z in
  let max_exp = Int.max ex (Int.max ey ez) in
  if max_exp = Int.min_value
  then V.zero
  else
    (* Scaling by [2^(-max_exp)] puts the largest component magnitude in
       [0.5, 1). Offsetting by [max_exp] is always safe: the largest [Dyadic]
       component has true_exp [max_exp], so [Dyadic.to_float_scaled] returns a
       normal-range double for it, and the other components are at most that
       size. *)
    V.create
      ~x:(Dyadic.to_float_scaled v.#x ~exp_offset:max_exp)
      ~y:(Dyadic.to_float_scaled v.#y ~exp_offset:max_exp)
      ~z:(Dyadic.to_float_scaled v.#z ~exp_offset:max_exp)
;;
