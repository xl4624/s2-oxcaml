open Core
open Float_u.O

type t =
  #{ lo : Float_u.t
   ; hi : Float_u.t
   }
[@@deriving sexp_of]

let[@zero_alloc ignore] pp ppf t =
  let lo = Float_u.to_float t.#lo in
  let hi = Float_u.to_float t.#hi in
  Format.fprintf ppf "[%a, %a]" Float.pp lo Float.pp hi
;;

let[@zero_alloc ignore] to_string t =
  sprintf "[%s, %s]" (Float_u.to_string t.#lo) (Float_u.to_string t.#hi)
;;

let empty = #{ lo = Float_u.pi (); hi = Float_u.neg (Float_u.pi ()) }
let full = #{ lo = Float_u.neg (Float_u.pi ()); hi = Float_u.pi () }

let[@inline] [@zero_alloc] create ~lo:lo_orig ~hi:hi_orig =
  let lo =
    if lo_orig = Float_u.neg (Float_u.pi ()) && not (hi_orig = Float_u.pi ())
    then Float_u.pi ()
    else lo_orig
  in
  let hi =
    if hi_orig = Float_u.neg (Float_u.pi ()) && not (lo_orig = Float_u.pi ())
    then Float_u.pi ()
    else hi_orig
  in
  #{ lo; hi }
;;

let[@inline] [@zero_alloc] from_point p =
  let p = if p = Float_u.neg (Float_u.pi ()) then Float_u.pi () else p in
  #{ lo = p; hi = p }
;;

let[@inline] [@zero_alloc] positive_distance a b =
  let d = b - a in
  if d >= #0.0 then d else b + Float_u.pi () - (a - Float_u.pi ())
;;

let[@inline] [@zero_alloc] from_point_pair p1 p2 =
  let p1 = if p1 = Float_u.neg (Float_u.pi ()) then Float_u.pi () else p1 in
  let p2 = if p2 = Float_u.neg (Float_u.pi ()) then Float_u.pi () else p2 in
  if positive_distance p1 p2 <= Float_u.pi ()
  then #{ lo = p1; hi = p2 }
  else #{ lo = p2; hi = p1 }
;;

let[@inline] [@zero_alloc] is_valid_point p = abs p <= Float_u.pi ()
let[@inline] [@zero_alloc] lo t = t.#lo
let[@inline] [@zero_alloc] hi t = t.#hi

let[@inline] [@zero_alloc] is_valid t =
  abs t.#lo <= Float_u.pi ()
  && abs t.#hi <= Float_u.pi ()
  && (not (t.#lo = Float_u.neg (Float_u.pi ()) && not (t.#hi = Float_u.pi ())))
  && not (t.#hi = Float_u.neg (Float_u.pi ()) && not (t.#lo = Float_u.pi ()))
;;

let[@inline] [@zero_alloc] is_full t =
  t.#lo = Float_u.neg (Float_u.pi ()) && t.#hi = Float_u.pi ()
;;

let[@inline] [@zero_alloc] is_empty t =
  t.#lo = Float_u.pi () && t.#hi = Float_u.neg (Float_u.pi ())
;;

let[@inline] [@zero_alloc] is_inverted t = t.#lo > t.#hi

let[@inline] [@zero_alloc] center t =
  let c = #0.5 * (t.#lo + t.#hi) in
  if not (t.#lo > t.#hi)
  then c
  else if c <= #0.0
  then c + Float_u.pi ()
  else c - Float_u.pi ()
;;

let[@inline] [@zero_alloc] length t =
  let l = t.#hi - t.#lo in
  if l >= #0.0
  then l
  else (
    let l = l + (#2.0 * Float_u.pi ()) in
    if l > #0.0 then l else -#1.0)
;;

let[@inline] [@zero_alloc] complement t : t =
  if t.#lo = t.#hi
  then #{ lo = Float_u.neg (Float_u.pi ()); hi = Float_u.pi () }
  else #{ lo = t.#hi; hi = t.#lo }
;;

let[@inline] [@zero_alloc] complement_center t =
  if not (t.#lo = t.#hi)
  then center (complement t)
  else if t.#hi <= #0.0
  then t.#hi + Float_u.pi ()
  else t.#hi - Float_u.pi ()
;;

let[@inline] [@zero_alloc] fast_contains t p =
  if is_inverted t
  then (p >= t.#lo || p <= t.#hi) && not (is_empty t)
  else p >= t.#lo && p <= t.#hi
;;

let[@inline] [@zero_alloc] contains t p =
  let p = if p = Float_u.neg (Float_u.pi ()) then Float_u.pi () else p in
  fast_contains t p
;;

let[@inline] [@zero_alloc] interior_contains t p =
  let p = if p = Float_u.neg (Float_u.pi ()) then Float_u.pi () else p in
  if is_inverted t then p > t.#lo || p < t.#hi else (p > t.#lo && p < t.#hi) || is_full t
;;

let[@inline] [@zero_alloc] contains_interval t y =
  if is_inverted t
  then
    if is_inverted y
    then y.#lo >= t.#lo && y.#hi <= t.#hi
    else (y.#lo >= t.#lo || y.#hi <= t.#hi) && not (is_empty t)
  else if is_inverted y
  then is_full t || is_empty y
  else y.#lo >= t.#lo && y.#hi <= t.#hi
;;

let[@inline] [@zero_alloc] interior_contains_interval t y =
  if is_inverted t
  then
    if not (is_inverted y)
    then y.#lo > t.#lo || y.#hi < t.#hi
    else (y.#lo > t.#lo && y.#hi < t.#hi) || is_empty y
  else if is_inverted y
  then is_full t || is_empty y
  else (y.#lo > t.#lo && y.#hi < t.#hi) || is_full t
;;

let[@inline] [@zero_alloc] intersects t y =
  if is_empty t || is_empty y
  then false
  else if is_inverted t
  then is_inverted y || y.#lo <= t.#hi || y.#hi >= t.#lo
  else if is_inverted y
  then y.#lo <= t.#hi || y.#hi >= t.#lo
  else y.#lo <= t.#hi && y.#hi >= t.#lo
;;

let[@inline] [@zero_alloc] interior_intersects t y =
  if is_empty t || is_empty y || t.#lo = t.#hi
  then false
  else if is_inverted t
  then is_inverted y || y.#lo < t.#hi || y.#hi > t.#lo
  else if is_inverted y
  then y.#lo < t.#hi || y.#hi > t.#lo
  else (y.#lo < t.#hi && y.#hi > t.#lo) || is_full t
;;

let[@inline] [@zero_alloc] union t y : t =
  if is_empty y
  then #{ lo = t.#lo; hi = t.#hi }
  else if fast_contains t y.#lo
  then
    if fast_contains t y.#hi
    then
      if contains_interval t y
      then #{ lo = t.#lo; hi = t.#hi }
      else #{ lo = Float_u.neg (Float_u.pi ()); hi = Float_u.pi () }
    else #{ lo = t.#lo; hi = y.#hi }
  else if fast_contains t y.#hi
  then #{ lo = y.#lo; hi = t.#hi }
  else if is_empty t || fast_contains y t.#lo
  then #{ lo = y.#lo; hi = y.#hi }
  else (
    let dlo = positive_distance y.#hi t.#lo in
    let dhi = positive_distance t.#hi y.#lo in
    if dlo < dhi then #{ lo = y.#lo; hi = t.#hi } else #{ lo = t.#lo; hi = y.#hi })
;;

let[@inline] [@zero_alloc] intersection t y : t =
  if is_empty y
  then #{ lo = Float_u.pi (); hi = Float_u.neg (Float_u.pi ()) }
  else if fast_contains t y.#lo
  then
    if fast_contains t y.#hi
    then
      if length y < length t
      then #{ lo = y.#lo; hi = y.#hi }
      else #{ lo = t.#lo; hi = t.#hi }
    else #{ lo = y.#lo; hi = t.#hi }
  else if fast_contains t y.#hi
  then #{ lo = t.#lo; hi = y.#hi }
  else if fast_contains y t.#lo
  then #{ lo = t.#lo; hi = t.#hi }
  else #{ lo = Float_u.pi (); hi = Float_u.neg (Float_u.pi ()) }
;;

let[@inline] [@zero_alloc] add_point t p : t =
  if abs p > Float_u.pi ()
  then #{ lo = t.#lo; hi = t.#hi }
  else (
    let p = if p = Float_u.neg (Float_u.pi ()) then Float_u.pi () else p in
    if fast_contains t p
    then #{ lo = t.#lo; hi = t.#hi }
    else if is_empty t
    then #{ lo = p; hi = p }
    else (
      let dlo = positive_distance p t.#lo in
      let dhi = positive_distance t.#hi p in
      if dlo < dhi then #{ lo = p; hi = t.#hi } else #{ lo = t.#lo; hi = p }))
;;

let[@inline] [@zero_alloc] project t p =
  let p = if p = Float_u.neg (Float_u.pi ()) then Float_u.pi () else p in
  if fast_contains t p
  then p
  else (
    let dlo = positive_distance p t.#lo in
    let dhi = positive_distance t.#hi p in
    if dlo < dhi then t.#lo else t.#hi)
;;

let[@inline] [@zero_alloc] expanded t margin : t =
  if margin >= #0.0
  then
    if is_empty t
    then #{ lo = t.#lo; hi = t.#hi }
    else if length t + (#2.0 * margin) + (#2.0 * Float_u.epsilon_float ())
            >= #2.0 * Float_u.pi ()
    then #{ lo = Float_u.neg (Float_u.pi ()); hi = Float_u.pi () }
    else (
      let lo = Float_util.ieee_remainder_u (t.#lo - margin) (#2.0 * Float_u.pi ()) in
      let hi = Float_util.ieee_remainder_u (t.#hi + margin) (#2.0 * Float_u.pi ()) in
      let lo = if lo <= Float_u.neg (Float_u.pi ()) then Float_u.pi () else lo in
      let g = create ~lo ~hi in
      #{ lo = g.#lo; hi = g.#hi })
  else if is_full t
  then #{ lo = t.#lo; hi = t.#hi }
  else if length t + (#2.0 * margin) - (#2.0 * Float_u.epsilon_float ()) <= #0.0
  then #{ lo = Float_u.pi (); hi = Float_u.neg (Float_u.pi ()) }
  else (
    let lo = Float_util.ieee_remainder_u (t.#lo - margin) (#2.0 * Float_u.pi ()) in
    let hi = Float_util.ieee_remainder_u (t.#hi + margin) (#2.0 * Float_u.pi ()) in
    let lo = if lo <= Float_u.neg (Float_u.pi ()) then Float_u.pi () else lo in
    let g = create ~lo ~hi in
    #{ lo = g.#lo; hi = g.#hi })
;;

let[@inline] [@zero_alloc] directed_hausdorff_distance t y =
  if contains_interval y t
  then #0.0
  else if is_empty y
  then Float_u.pi ()
  else (
    let y_complement_center = complement_center y in
    if contains t y_complement_center
    then positive_distance y.#hi y_complement_center
    else (
      let hi_hi =
        if fast_contains (create ~lo:y.#hi ~hi:y_complement_center) t.#hi
        then positive_distance y.#hi t.#hi
        else #0.0
      in
      let lo_lo =
        if fast_contains (create ~lo:y_complement_center ~hi:y.#lo) t.#lo
        then positive_distance t.#lo y.#lo
        else #0.0
      in
      Float_util.max_u hi_hi lo_lo))
;;

let[@inline] [@zero_alloc] approx_equal ~(max_error : Packed_float_option.Unboxed.t) t y =
  let max_error = Packed_float_option.Unboxed.value max_error ~default:#1e-15 in
  if is_empty t
  then length y <= #2.0 * max_error
  else if is_empty y
  then length t <= #2.0 * max_error
  else if is_full t
  then length y >= #2.0 * (Float_u.pi () - max_error)
  else if is_full y
  then length t >= #2.0 * (Float_u.pi () - max_error)
  else
    abs (Float_u.mod_float (y.#lo - t.#lo) (#2.0 * Float_u.pi ())) <= max_error
    && abs (Float_u.mod_float (y.#hi - t.#hi) (#2.0 * Float_u.pi ())) <= max_error
    && abs (length t - length y) <= #2.0 * max_error
;;

let[@inline] [@zero_alloc] equal t y =
  (t.#lo = y.#lo && t.#hi = y.#hi) || (is_empty t && is_empty y)
;;
