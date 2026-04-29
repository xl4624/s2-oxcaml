open Core
open Float_u.O

type t =
  #{ x : Float_u.t
   ; y : Float_u.t
   ; z : Float_u.t
   }
[@@deriving sexp_of, unboxed_option { sentinel = true }]

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
  #{ x = Float_util.max_u a.#x b.#x
   ; y = Float_util.max_u a.#y b.#y
   ; z = Float_util.max_u a.#z b.#z
   }
;;

let[@inline] [@zero_alloc] min a b =
  #{ x = Float_util.min_u a.#x b.#x
   ; y = Float_util.min_u a.#y b.#y
   ; z = Float_util.min_u a.#z b.#z
   }
;;

let[@inline] [@zero_alloc] norm2 t = dot t t
let[@inline] [@zero_alloc] norm t = Float_u.sqrt (norm2 t)

let[@inline] [@zero_alloc] normalize t =
  let n2 = norm2 t in
  if n2 = #0.0 then zero else mul t (#1.0 / Float_u.sqrt n2)
;;

(* Components whose magnitude is at least [2^(-242)] can be squared (~2^(-484)) and still
   fit in the normal-range doubles the rest of the pipeline expects. Anything smaller
   underflows once squared, which breaks [norm2], [atan2(|cross|, dot)], and any later
   scaled arithmetic. *)
let[@inline] [@zero_alloc] max_abs_component t =
  Float_util.max_u
    (Float_u.abs t.#x)
    (Float_util.max_u (Float_u.abs t.#y) (Float_u.abs t.#z))
;;

let[@inline] [@zero_alloc] is_normalizable t =
  let m = max_abs_component t in
  m >= #0x1p-242 || m = #0.0
;;

let[@inline] [@zero_alloc] ensure_normalizable t =
  let m = max_abs_component t in
  if m >= #0x1p-242 || m = #0.0
  then t
  else (
    (* [ieee_exponent m] is the biased exponent field. For a normal finite [m > 0], the
       unbiased exponent is [e = ieee_exponent m - 1023] and [m] lies in [2^e, 2^(e+1)).
       Scaling by [2^(-e)] puts [m] in [1, 2). *)
    let e = Int.( - ) (Float_u.ieee_exponent m) 1023 in
    let scale = Float_u.ldexp #1.0 (Int.neg e) in
    mul t scale)
;;

let[@inline] [@zero_alloc] distance a b = norm (sub a b)

(* [atan2(|a x b|, a . b)] is well-conditioned for every angle between 0 and pi, unlike
   [acos(a . b / (|a| |b|))] which loses precision as the vectors become parallel or
   antiparallel. Inputs do not have to be unit-length. *)
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

(* Pick an auxiliary axis rotated away from [t]'s dominant component so the cross product
   has large magnitude and the normalization step is well-conditioned. Choosing the axis
   opposite the largest component instead of a fixed one avoids the near-zero cross
   product when [t] happens to align with a coordinate axis. *)
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

let[@inline] [@zero_alloc] approx_equal ~(max_error : Packed_float_option.Unboxed.t) a b =
  let max_error = Packed_float_option.Unboxed.value max_error ~default:#1e-16 in
  Float_u.abs (a.#x - b.#x) <= max_error
  && Float_u.abs (a.#y - b.#y) <= max_error
  && Float_u.abs (a.#z - b.#z) <= max_error
;;
