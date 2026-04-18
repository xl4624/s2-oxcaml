open Core

type t = Float_u.t

let%template[@alloc a = (heap, stack)] [@inline] sexp_of_t t : Sexp.t =
  Sexp.List [ Sexp.List [ Sexp.Atom "radians"; Float.sexp_of_t (Float_u.to_float t) ] ]
  [@exclave_if_stack a]
;;

let[@zero_alloc ignore] pp ppf t =
  let x = Float_u.to_float t in
  Format.fprintf ppf "%a" Float.pp x
;;

let[@zero_alloc ignore] to_string t = Float_u.to_string t
let zero = #0.0
let infinity = Float_u.infinity ()
let[@inline] [@zero_alloc] of_radians r = r
let[@inline] [@zero_alloc] of_degrees d = Float_u.O.(Float_u.pi () / #180.0 * d)
let[@inline] [@zero_alloc] of_e5 e = of_degrees Float_u.O.(#1e-5 * Float_u.of_int e)
let[@inline] [@zero_alloc] of_e6 e = of_degrees Float_u.O.(#1e-6 * Float_u.of_int e)
let[@inline] [@zero_alloc] of_e7 e = of_degrees Float_u.O.(#1e-7 * Float_u.of_int e)

(* [u] is taken modulo [2^32] then reinterpreted as a signed int32: keep the low 32 bits
   and subtract [2^32] if the result exceeds [INT32_MAX]. *)
let[@inline] [@zero_alloc] of_unsigned_e6 (u : int) =
  let u = u land 0xFFFF_FFFF in
  let bits = if u > 0x7FFF_FFFF then u - 0x1_0000_0000 else u in
  of_e6 bits
;;

let[@inline] [@zero_alloc] of_unsigned_e7 (u : int) =
  let u = u land 0xFFFF_FFFF in
  let bits = if u > 0x7FFF_FFFF then u - 0x1_0000_0000 else u in
  of_e7 bits
;;

let[@inline] [@zero_alloc] radians t = t
let[@inline] [@zero_alloc] degrees t = Float_u.O.(#180.0 / Float_u.pi () * t)

module Int_option = struct
  type value = int
  type t = int

  let none = Int.min_value
  let some x = x
  let is_none x = x = none
  let is_some x = x <> none

  let value_exn x =
    if is_none x
    then (
      match raise_s [%message "S1_angle.Int_option.value_exn: none"] with
      | (_ : Nothing.t) -> .)
    else x
  ;;

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none x = is_none x
      let unsafe_value x = x
    end
  end
end

let[@inline] [@zero_alloc] iround_nearest_sentinel v =
  match Float_u.iround_nearest v with
  | None -> Int_option.none
  | Some i -> i
;;

let[@inline] [@zero_alloc] e5 t = iround_nearest_sentinel Float_u.O.(#1e5 * degrees t)

let[@inline] [@zero_alloc] e5_exn t =
  let res = e5 t in
  if Int_option.is_none res
  then (
    let d = Float_u.to_float (degrees t) in
    match
      raise_s [%message "S1Angle.e5_exn: angle overflows int" ~degrees:(d : float)]
    with
    | (_ : Nothing.t) -> .)
  else res
;;

let[@inline] [@zero_alloc] e6 t = iround_nearest_sentinel Float_u.O.(#1e6 * degrees t)

let[@inline] [@zero_alloc] e6_exn t =
  let res = e6 t in
  if Int_option.is_none res
  then (
    let d = Float_u.to_float (degrees t) in
    match
      raise_s [%message "S1Angle.e6_exn: angle overflows int" ~degrees:(d : float)]
    with
    | (_ : Nothing.t) -> .)
  else res
;;

let[@inline] [@zero_alloc] e7 t = iround_nearest_sentinel Float_u.O.(#1e7 * degrees t)

let[@inline] [@zero_alloc] e7_exn t =
  let res = e7 t in
  if Int_option.is_none res
  then (
    let d = Float_u.to_float (degrees t) in
    match
      raise_s [%message "S1Angle.e7_exn: angle overflows int" ~degrees:(d : float)]
    with
    | (_ : Nothing.t) -> .)
  else res
;;

let[@inline] [@zero_alloc] is_inf t = Float_u.is_inf (radians t)
let[@inline] [@zero_alloc] is_zero t = Float_u.O.(radians t = #0.0)
let[@inline] [@zero_alloc] abs t = Float_u.abs t
let[@inline] [@zero_alloc] neg t = Float_u.neg t
let[@inline] [@zero_alloc] add a b = Float_u.O.(a + b)
let[@inline] [@zero_alloc] sub a b = Float_u.O.(a - b)
let[@inline] [@zero_alloc] mul t m = Float_u.O.(t * m)
let[@inline] [@zero_alloc] div t m = Float_u.O.(t / m)
let[@inline] [@zero_alloc] ratio a b = Float_u.O.(a / b)
let[@inline] [@zero_alloc] sin t = Float_u.sin (radians t)
let[@inline] [@zero_alloc] cos t = Float_u.cos (radians t)
let[@inline] [@zero_alloc] tan t = Float_u.tan (radians t)

let[@inline] [@zero_alloc] sin_cos t =
  let r = radians t in
  #(Float_u.sin r, Float_u.cos r)
;;

let[@inline] [@zero_alloc] normalized t =
  let open Float_u.O in
  let two_pi = #2.0 * Float_u.pi () in
  let n = Float_u.round_nearest_half_to_even (t / two_pi) in
  let r = t - (n * two_pi) in
  if r <= Float_u.neg (Float_u.pi ()) then Float_u.pi () else r
;;

let[@inline] [@zero_alloc] compare a b = Float_u.compare (radians a) (radians b)
let[@inline] [@zero_alloc] equal a b = Float_u.O.(radians a = radians b)
