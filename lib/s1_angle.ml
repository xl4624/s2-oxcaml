open Core

type t = Float_u.t

let[@zero_alloc ignore] sexp_of_t t =
  Sexp.List [ Sexp.List [ Sexp.Atom "radians"; Float.sexp_of_t (Float_u.to_float t) ] ]
;;

let[@zero_alloc ignore] pp ppf t =
  let x = Float_u.to_float t in
  Format.fprintf ppf "%a" Float.pp x
;;

let[@zero_alloc ignore] to_string t = Float_u.to_string t
let zero = #0.0
let infinity = Float_u.infinity ()
let[@inline] [@zero_alloc] of_radians r = r

(* The conversion factor (pi / 180) is chosen so that Degrees(180) == Radians(pi)
   exactly. *)
let[@inline] [@zero_alloc] of_degrees d =
  let open Float_u.O in
  Float_u.of_float Float.pi / #180.0 * d
;;

let[@inline] [@zero_alloc] of_e5 e =
  let open Float_u.O in
  of_degrees (#1e-5 * Float_u.of_float (Float.of_int e))
;;

let[@inline] [@zero_alloc] of_e6 e =
  let open Float_u.O in
  of_degrees (#1e-6 * Float_u.of_float (Float.of_int e))
;;

let[@inline] [@zero_alloc] of_e7 e =
  let open Float_u.O in
  of_degrees (#1e-7 * Float_u.of_float (Float.of_int e))
;;

(* [u] is taken modulo [2^32] then cast to signed int32, like C++ [static_cast<int32_t>(u)]. *)
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

let[@inline] [@zero_alloc] degrees t =
  let open Float_u.O in
  #180.0 / Float_u.pi () * t
;;

let[@inline] [@zero_alloc ignore] e5 t = Float_u.iround_nearest Float_u.(#1e5 * degrees t)

let[@zero_alloc ignore] e5_exn t =
  match e5 t with
  | Some e -> e
  | None ->
    let d = Float_u.to_float (degrees t) in
    raise_s [%message "S1Angle.e5_exn: angle overflows int" ~degrees:(d : float)]
;;

let[@inline] [@zero_alloc ignore] e6 t = Float_u.iround_nearest Float_u.(#1e6 * degrees t)

let[@zero_alloc ignore] e6_exn t =
  match e6 t with
  | Some e -> e
  | None ->
    let d = Float_u.to_float (degrees t) in
    raise_s [%message "S1Angle.e6_exn: angle overflows int" ~degrees:(d : float)]
;;

let[@inline] [@zero_alloc ignore] e7 t = Float_u.iround_nearest Float_u.(#1e7 * degrees t)

let[@zero_alloc ignore] e7_exn t =
  match e7 t with
  | Some e -> e
  | None ->
    let d = Float_u.to_float (degrees t) in
    raise_s [%message "S1Angle.e7_exn: angle overflows int" ~degrees:(d : float)]
;;

let[@inline] [@zero_alloc] is_inf t = Float_u.is_inf (radians t)
let[@inline] [@zero_alloc] is_zero t = Float_u.(radians t = #0.0)
let[@inline] [@zero_alloc] abs t = Float_u.abs t
let[@inline] [@zero_alloc] neg t = Float_u.neg t
let[@inline] [@zero_alloc] add a b = Float_u.(a + b)
let[@inline] [@zero_alloc] sub a b = Float_u.(a - b)
let[@inline] [@zero_alloc] mul t m = Float_u.(t * m)
let[@inline] [@zero_alloc] div t m = Float_u.(t / m)
let[@inline] [@zero_alloc] ratio a b = Float_u.(a / b)
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
let[@inline] [@zero_alloc] equal a b = Float_u.( = ) (radians a) (radians b)
