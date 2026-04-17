open Core

let[@inline] ieee_remainder a b =
  let q = Float.round_nearest_half_to_even (a /. b) in
  a -. (q *. b)
;;

let[@inline] [@zero_alloc] ieee_remainder_u a b =
  let open Float_u.O in
  let q = Float_u.round_nearest_half_to_even (a / b) in
  a - (q * b)
;;

let[@inline] [@zero_alloc] min_u a b = Ocaml_intrinsics_kernel.Float.Unboxed.min a b
let[@inline] [@zero_alloc] max_u a b = Ocaml_intrinsics_kernel.Float.Unboxed.max a b
