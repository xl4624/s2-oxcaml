open Core

[@@@zero_alloc all]

type t = float#

let%template[@alloc a = (heap, stack)] [@inline] [@zero_alloc ignore] sexp_of_t t : Sexp.t
  =
  Sexp.List [ Sexp.List [ Sexp.Atom "length2"; Float.sexp_of_t (Float_u.to_float t) ] ]
  [@exclave_if_stack a]
;;

let relative_sum_error = Float_u.O.(#2.02 * Float_u.epsilon_float ())
let max_length2 = #4.0
let zero = #0.0
let right = #2.0
let straight = max_length2
let infinity = Float_u.of_float Float.infinity
let negative = -#1.0

let[@inline] [@zero_alloc] rec of_angle angle =
  let radians = S1_angle.radians angle in
  if Float_u.O.(radians < #0.0)
  then negative
  else if S1_angle.is_inf angle
  then infinity
  else if Float_u.O.(radians >= Float_u.pi ())
  then straight
  else
    let open Float_u.O in
    let l = #2.0 * Float_u.sin (#0.5 * radians) in
    of_length2 (l * l)

and[@inline] [@zero_alloc] of_length2 length2 =
  if Float_u.O.(length2 > max_length2) then straight else length2
;;

let[@inline] [@zero_alloc] of_radians radians =
  of_angle (S1_angle.of_radians (Float_u.of_float radians))
;;

let[@inline] [@zero_alloc] of_degrees degrees =
  of_angle (S1_angle.of_degrees (Float_u.of_float degrees))
;;

let[@inline] [@zero_alloc] of_e5 e5 = of_angle (S1_angle.of_e5 e5)
let[@inline] [@zero_alloc] of_e6 e6 = of_angle (S1_angle.of_e6 e6)
let[@inline] [@zero_alloc] of_e7 e7 = of_angle (S1_angle.of_e7 e7)
let[@inline] [@zero_alloc] length2 t = t

let[@zero_alloc] to_angle t =
  if Float_u.O.(t < #0.0)
  then S1_angle.of_radians (-#1.0)
  else if Float_u.is_inf t
  then S1_angle.infinity
  else
    let open Float_u.O in
    S1_angle.of_radians (#2.0 * Float_u.asin (#0.5 * Float_u.sqrt t))
;;

let[@inline] [@zero_alloc ignore] radians t =
  Float_u.to_float (S1_angle.radians (to_angle t))
;;

let[@inline] [@zero_alloc ignore] degrees t =
  Float_u.to_float (S1_angle.degrees (to_angle t))
;;

let[@inline] [@zero_alloc] is_zero t = Float_u.O.(t = #0.0)
let[@inline] [@zero_alloc] is_negative t = Float_u.O.(t < #0.0)
let[@inline] [@zero_alloc] is_infinity t = Float_u.is_inf t
let[@inline] [@zero_alloc] is_special t = is_negative t || is_infinity t

let[@inline] [@zero_alloc] is_valid t =
  (Float_u.O.(t >= #0.0) && Float_u.O.(t <= max_length2)) || is_special t
;;

let[@inline] [@zero_alloc] successor t =
  if Float_u.O.(t >= max_length2)
  then infinity
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
    let c = Float_u.max #0.0 (Float_u.sqrt x - Float_u.sqrt y) in
    c * c)
;;

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
  else Float_u.max #0.0 (Float_u.min max_length2 Float_u.O.(t + error))
;;

let[@inline] [@zero_alloc ignore] max_point_error t =
  let eps = Float_u.epsilon_float () in
  Float_u.O.((#4.5 * eps * t) + (#16.0 * eps * eps))
;;

let[@inline] [@zero_alloc ignore] max_angle_error t =
  let open Float_u.O in
  #1.5 * Float_u.epsilon_float () * t
;;

let[@inline] [@zero_alloc] compare a b = Float_u.compare a b
let[@inline] [@zero_alloc] equal a b = Float_u.O.(a = b)

module Option = struct
  type t = float#

  let%template[@alloc a = (heap, stack)] [@inline] [@zero_alloc ignore] sexp_of_t t
    : Sexp.t
    =
    if Float_u.is_nan t
    then Sexp.Atom "None"
    else (sexp_of_t [@alloc a]) t [@exclave_if_stack a]
  ;;

  let none = Float_u.of_float Float.nan
  let some v = v
  let is_none t = Float_u.is_nan t
  let is_some t = not (Float_u.is_nan t)
  let value t ~default = if is_none t then default else t

  let value_exn t =
    if is_none t
    then (
      match raise_s [%message "S1_chord_angle.Option.value_exn: none"] with
      | (_ : Nothing.t) -> .)
    else t
  ;;

  let unchecked_value t = t

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none t = is_none t
      let unsafe_value t = unchecked_value t
    end
  end
end
