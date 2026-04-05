open Core
open Float_u.O

type t =
  #{ x : Float_u.t
   ; y : Float_u.t
   ; z : Float_u.t
   }
[@@deriving sexp_of]

let[@zero_alloc ignore] pp ppf t =
  let x = Float_u.to_float t.#x in
  let y = Float_u.to_float t.#y in
  let z = Float_u.to_float t.#z in
  Format.fprintf ppf "(%a, %a, %a)" Float.pp x Float.pp y Float.pp z
;;

let[@zero_alloc ignore] to_string t =
  sprintf
    "(%s, %s, %s)"
    (Float_u.to_string t.#x)
    (Float_u.to_string t.#y)
    (Float_u.to_string t.#z)
;;

let[@inline] [@zero_alloc] create ~x ~y ~z = #{ x; y; z }
let zero = #{ x = #0.0; y = #0.0; z = #0.0 }
let[@inline] [@zero_alloc] x t = t.#x
let[@inline] [@zero_alloc] y t = t.#y
let[@inline] [@zero_alloc] z t = t.#z

let[@inline] [@zero_alloc] add a b =
  #{ x = a.#x + b.#x; y = a.#y + b.#y; z = a.#z + b.#z }
;;

let[@inline] [@zero_alloc] sub a b =
  #{ x = a.#x - b.#x; y = a.#y - b.#y; z = a.#z - b.#z }
;;

let[@inline] [@zero_alloc] mul t k = #{ x = t.#x * k; y = t.#y * k; z = t.#z * k }

let[@inline] [@zero_alloc] neg t =
  #{ x = Float_u.neg t.#x; y = Float_u.neg t.#y; z = Float_u.neg t.#z }
;;

let[@inline] [@zero_alloc] dot a b = (a.#x * b.#x) + (a.#y * b.#y) + (a.#z * b.#z)

let[@inline] [@zero_alloc] abs t =
  #{ x = Float_u.abs t.#x; y = Float_u.abs t.#y; z = Float_u.abs t.#z }
;;

let[@inline] [@zero_alloc] cross a b =
  #{ x = (a.#y * b.#z) - (a.#z * b.#y)
   ; y = (a.#z * b.#x) - (a.#x * b.#z)
   ; z = (a.#x * b.#y) - (a.#y * b.#x)
   }
;;

let[@inline] [@zero_alloc] mul_components a b =
  #{ x = a.#x * b.#x; y = a.#y * b.#y; z = a.#z * b.#z }
;;

let[@inline] [@zero_alloc] div_components a b =
  #{ x = a.#x / b.#x; y = a.#y / b.#y; z = a.#z / b.#z }
;;

let[@inline] [@zero_alloc] max a b =
  #{ x = Float_u.max a.#x b.#x; y = Float_u.max a.#y b.#y; z = Float_u.max a.#z b.#z }
;;

let[@inline] [@zero_alloc] min a b =
  #{ x = Float_u.min a.#x b.#x; y = Float_u.min a.#y b.#y; z = Float_u.min a.#z b.#z }
;;

let[@inline] [@zero_alloc] norm2 t = dot t t
let[@inline] [@zero_alloc] norm t = Float_u.sqrt (norm2 t)

let[@inline] [@zero_alloc] normalize t =
  let n2 = norm2 t in
  if n2 = #0.0 then zero else mul t (#1.0 / Float_u.sqrt n2)
;;

let[@inline] [@zero_alloc] distance a b = norm (sub a b)

let[@inline] [@zero_alloc] angle a b =
  let cross_norm = norm (cross a b) in
  let dot_prod = dot a b in
  S1_angle.of_radians (Float_u.atan2 cross_norm dot_prod)
;;

let[@inline] [@zero_alloc] largest_abs_component t =
  let ax = Float_u.abs t.#x in
  let ay = Float_u.abs t.#y in
  let az = Float_u.abs t.#z in
  if ax > ay then if ax > az then 0 else 2 else if ay > az then 1 else 2
;;

let[@inline] [@zero_alloc] smallest_component t =
  let ax = Float_u.abs t.#x in
  let ay = Float_u.abs t.#y in
  let az = Float_u.abs t.#z in
  if ax < ay then if ax < az then 0 else 2 else if ay < az then 1 else 2
;;

let[@inline] [@zero_alloc] ortho t =
  let ov =
    match largest_abs_component t with
    | 0 -> #{ x = #0.0; y = #0.0; z = #1.0 }
    | 1 -> #{ x = #1.0; y = #0.0; z = #0.0 }
    | 2 -> #{ x = #0.0; y = #1.0; z = #0.0 }
    | _ -> assert false
  in
  normalize (cross t ov)
;;

let[@inline] [@zero_alloc] compare a b =
  match Float_u.compare a.#x b.#x with
  | 0 ->
    (match Float_u.compare a.#y b.#y with
     | 0 -> Float_u.compare a.#z b.#z
     | n -> n)
  | n -> n
;;

let[@inline] [@zero_alloc] equal a b =
  Float_u.O.(a.#x = b.#x && a.#y = b.#y && a.#z = b.#z)
;;

(* TODO: to unbox this we can make this take a Packed_float_option instead *)
let[@zero_alloc ignore] approx_equal ?(max_error = 1e-16) a b =
  let max_error = Float_u.of_float max_error in
  Float_u.abs (a.#x - b.#x) <= max_error
  && Float_u.abs (a.#y - b.#y) <= max_error
  && Float_u.abs (a.#z - b.#z) <= max_error
;;
