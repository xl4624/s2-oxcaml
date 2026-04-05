open Core
open Float_u.O

type t =
  #{ lo : Float_u.t
   ; hi : Float_u.t
   }
[@@deriving sexp_of]

let[@zero_alloc ignore] pp ppf t =
  Format.fprintf ppf "[%s, %s]" (Float_u.to_string t.#lo) (Float_u.to_string t.#hi)
;;

let[@zero_alloc ignore] to_string t =
  sprintf "[%s, %s]" (Float_u.to_string t.#lo) (Float_u.to_string t.#hi)
;;

let[@inline] [@zero_alloc] create ~lo ~hi = #{ lo; hi }
let empty = #{ lo = #1.0; hi = #0.0 }
let[@inline] [@zero_alloc] from_point p = #{ lo = p; hi = p }

let[@inline] [@zero_alloc] from_point_pair p1 p2 =
  if Float_u.O.(p1 <= p2) then #{ lo = p1; hi = p2 } else #{ lo = p2; hi = p1 }
;;

let[@inline] [@zero_alloc] lo t = t.#lo
let[@inline] [@zero_alloc] hi t = t.#hi
let[@inline] [@zero_alloc] is_empty t = Float_u.O.(t.#lo > t.#hi)
let[@inline] [@zero_alloc] center t = Float_u.O.(#0.5 * (t.#lo + t.#hi))
let[@inline] [@zero_alloc] length t = Float_u.O.(t.#hi - t.#lo)
let[@inline] [@zero_alloc] contains t p = Float_u.O.(p >= t.#lo && p <= t.#hi)
let[@inline] [@zero_alloc] interior_contains t p = Float_u.O.(t.#lo < p && p < t.#hi)

let[@inline] [@zero_alloc] contains_interval t y =
  if is_empty y then true else y.#lo >= t.#lo && y.#hi <= t.#hi
;;

let[@inline] [@zero_alloc] interior_contains_interval t y =
  if is_empty y then true else y.#lo > t.#lo && y.#hi < t.#hi
;;

let[@inline] [@zero_alloc] intersects t y =
  if t.#lo <= y.#lo
  then y.#lo <= t.#hi && y.#lo <= y.#hi
  else t.#lo <= y.#hi && t.#lo <= t.#hi
;;

let[@inline] [@zero_alloc] interior_intersects t y =
  y.#lo < t.#hi && t.#lo < y.#hi && t.#lo < t.#hi && y.#lo <= y.#hi
;;

let[@inline] [@zero_alloc] intersection t y : t =
  #{ lo = Float_u.max t.#lo y.#lo; hi = Float_u.min t.#hi y.#hi }
;;

let[@inline] [@zero_alloc] union t y : t =
  if is_empty t
  then #{ lo = y.#lo; hi = y.#hi }
  else if is_empty y
  then #{ lo = t.#lo; hi = t.#hi }
  else #{ lo = Float_u.min t.#lo y.#lo; hi = Float_u.max t.#hi y.#hi }
;;

let[@inline] [@zero_alloc] add_point t p : t =
  if is_empty t
  then #{ lo = p; hi = p }
  else if p < t.#lo
  then #{ lo = p; hi = t.#hi }
  else if p > t.#hi
  then #{ lo = t.#lo; hi = p }
  else #{ lo = t.#lo; hi = t.#hi }
;;

let[@inline] [@zero_alloc] project t p : Packed_float_option.Unboxed.t =
  if is_empty t
  then Packed_float_option.Unboxed.none ()
  else Packed_float_option.Unboxed.some (Float_u.clamp_exn p ~min:t.#lo ~max:t.#hi)
;;

let[@inline] [@zero_alloc] project_exn t p = Float_u.clamp_exn p ~min:t.#lo ~max:t.#hi

let[@inline] [@zero_alloc] expanded t margin : t =
  if is_empty t then t else #{ lo = t.#lo - margin; hi = t.#hi + margin }
;;

let[@inline] [@zero_alloc] directed_hausdorff_distance t y =
  if is_empty t
  then #0.0
  else if is_empty y
  then Float_u.infinity ()
  else Float_u.max #0.0 (Float_u.max (t.#hi - y.#hi) (y.#lo - t.#lo))
;;

let[@inline] [@zero_alloc] approx_equal ~(max_error : Packed_float_option.Unboxed.t) t y =
  let max_error = Packed_float_option.Unboxed.value max_error ~default:#1e-15 in
  if is_empty t
  then length y <= #2.0 * max_error
  else if is_empty y
  then length t <= #2.0 * max_error
  else abs (y.#lo - t.#lo) <= max_error && abs (y.#hi - t.#hi) <= max_error
;;

let[@inline] [@zero_alloc] equal t y =
  (t.#lo = y.#lo && t.#hi = y.#hi) || (is_empty t && is_empty y)
;;
