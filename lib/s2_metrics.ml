open Core

type t =
  #{ dim : int
   ; deriv : Float_u.t
   }

let max_cell_level = 30
let[@inline] dim t = t.#dim
let[@inline] deriv t = t.#deriv

(* Each cell at level [l] has s-size [2^-l] (linear) or [2^-2l] (area), so the metric
   value at level [l] is [deriv * 2^(-dim * l)]. *)
let[@inline] get_value t level = Float_u.ldexp t.#deriv (-t.#dim * level)

(* [ilogb x] returns the integer [e] such that [1 <= |x| * 2^-e < 2] for normal non-zero
   [x]. Implemented via the IEEE exponent field on [x]. Only correct for positive finite
   normal inputs, which is all we need: callers guard against non-positive values. *)
let[@inline] ilogb x =
  let e = Float_u.ieee_exponent x in
  if e = 0
  then Int.min_value (* zero or subnormal *)
  else if e = 2047
  then Int.max_value (* infinity or NaN *)
  else e - 1023
;;

(* Equivalent to computing a floating-point "level" value and rounding up. *)
let get_level_for_max_value t value =
  if not Float_u.O.(value > #0.0)
  then max_cell_level
  else (
    let level = ilogb Float_u.O.(value / t.#deriv) in
    let shifted = -(level asr (t.#dim - 1)) in
    Int.clamp_exn shifted ~min:0 ~max:max_cell_level)
;;

(* Equivalent to computing a floating-point "level" value and rounding down. *)
let get_level_for_min_value t value =
  if not Float_u.O.(value > #0.0)
  then max_cell_level
  else (
    let level = ilogb Float_u.O.(t.#deriv / value) in
    let shifted = level asr (t.#dim - 1) in
    Int.clamp_exn shifted ~min:0 ~max:max_cell_level)
;;

let get_closest_level t value =
  let x = if t.#dim = 1 then Float_u.sqrt #2.0 else #2.0 in
  get_level_for_max_value t Float_u.O.(x * value)
;;

(* Metric constants for the quadratic projection, the only projection supported at
   runtime. *)

let min_angle_span = #{ dim = 1; deriv = Float_u.O.(#4.0 / #3.0) }
let max_angle_span = #{ dim = 1; deriv = #1.704897179199218452 }
let avg_angle_span = #{ dim = 1; deriv = Float_u.O.(Float_u.pi () / #2.0) }
let min_width = #{ dim = 1; deriv = Float_u.O.(#2.0 * Float_u.sqrt #2.0 / #3.0) }
let max_width = #{ dim = 1; deriv = max_angle_span.#deriv }
let avg_width = #{ dim = 1; deriv = #1.434523672886099389 }
let min_edge = #{ dim = 1; deriv = Float_u.O.(#2.0 * Float_u.sqrt #2.0 / #3.0) }
let max_edge = #{ dim = 1; deriv = max_angle_span.#deriv }
let avg_edge = #{ dim = 1; deriv = #1.459213746386106062 }
let max_edge_aspect = #1.442615274452682920
let min_diag = #{ dim = 1; deriv = Float_u.O.(#8.0 * Float_u.sqrt #2.0 / #9.0) }
let max_diag = #{ dim = 1; deriv = #2.438654594434021032 }
let avg_diag = #{ dim = 1; deriv = #2.060422738998471683 }
let max_diag_aspect = Float_u.sqrt #3.0
let min_area = #{ dim = 2; deriv = Float_u.O.(#8.0 * Float_u.sqrt #2.0 / #9.0) }
let max_area = #{ dim = 2; deriv = #2.635799256963161491 }
let avg_area = #{ dim = 2; deriv = Float_u.O.(#4.0 * Float_u.pi () / #6.0) }
