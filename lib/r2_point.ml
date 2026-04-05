open Core

type t =
  #{ x : Float_u.t
   ; y : Float_u.t
   }
[@@deriving sexp_of]

let[@zero_alloc ignore] pp ppf t =
  let x = Float_u.to_float t.#x in
  let y = Float_u.to_float t.#y in
  Format.fprintf ppf "(%a, %a)" Float.pp x Float.pp y
;;

let[@zero_alloc ignore] to_string t =
  sprintf "(%s, %s)" (Float_u.to_string t.#x) (Float_u.to_string t.#y)
;;

let[@inline] [@zero_alloc] create ~x ~y = #{ x; y }
let zero = #{ x = #0.0; y = #0.0 }
let[@inline] [@zero_alloc] x t = t.#x
let[@inline] [@zero_alloc] y t = t.#y

let[@inline] [@zero_alloc] add a b =
  let open Float_u.O in
  #{ x = a.#x + b.#x; y = a.#y + b.#y }
;;

let[@inline] [@zero_alloc] sub a b =
  let open Float_u.O in
  #{ x = a.#x - b.#x; y = a.#y - b.#y }
;;

let[@inline] [@zero_alloc] mul t k =
  let open Float_u.O in
  #{ x = t.#x * k; y = t.#y * k }
;;

let[@inline] [@zero_alloc] div t k =
  let open Float_u.O in
  #{ x = t.#x / k; y = t.#y / k }
;;

let[@inline] [@zero_alloc] neg t = #{ x = Float_u.neg t.#x; y = Float_u.neg t.#y }
let[@inline] [@zero_alloc] ortho t = #{ x = Float_u.neg t.#y; y = t.#x }

let[@inline] [@zero_alloc] dot a b =
  let open Float_u.O in
  (a.#x * b.#x) + (a.#y * b.#y)
;;

let[@inline] [@zero_alloc] cross a b =
  let open Float_u.O in
  (a.#x * b.#y) - (a.#y * b.#x)
;;

let[@inline] [@zero_alloc] angle a b = Float_u.atan2 (cross a b) (dot a b)
let[@inline] [@zero_alloc] fabs t = #{ x = Float_u.abs t.#x; y = Float_u.abs t.#y }

let[@inline] [@zero_alloc] norm2 t =
  let open Float_u.O in
  (t.#x * t.#x) + (t.#y * t.#y)
;;

let[@inline] [@zero_alloc] norm t = Float_u.sqrt (norm2 t)

let[@inline] [@zero_alloc] normalize t =
  let n = norm t in
  let k = if Float_u.(n = #0.0) then #0.0 else Float_u.(#1.0 / n) in
  mul t k
;;

let[@inline] [@zero_alloc] equal a b = Float_u.(a.#x = b.#x && a.#y = b.#y)

module Option = struct
  type value = t
  type t = value

  let none = #{ x = Float_u.nan (); y = Float_u.nan () }

  let[@inline] [@zero_alloc] some (v : value) =
    if Float_u.is_nan v.#x || Float_u.is_nan v.#y then none else v
  ;;

  let[@inline] [@zero_alloc] is_none t = Float_u.is_nan t.#x || Float_u.is_nan t.#y
  let[@inline] [@zero_alloc] is_some t = not (is_none t)
  let[@inline] [@zero_alloc] unchecked_value t = t

  let[@zero_alloc ignore] sexp_of_t t =
    if is_none t
    then Sexp.List []
    else
      Sexp.List
        [ Sexp.List
            [ Sexp.List [ Sexp.Atom "x"; Float.sexp_of_t (Float_u.to_float t.#x) ]
            ; Sexp.List [ Sexp.Atom "y"; Float.sexp_of_t (Float_u.to_float t.#y) ]
            ]
        ]
  ;;

  let[@inline] [@zero_alloc] value t ~default = if is_some t then t else default

  let[@inline] [@zero_alloc] value_exn t =
    if is_none t
    then (
      match raise_s [%message "R2_point.Option.value_exn: none"] with
      | (_ : Nothing.t) -> .)
    else t
  ;;

  module Optional_syntax = struct
    module Optional_syntax = struct
      let[@zero_alloc] is_none t = is_none t
      let[@zero_alloc] unsafe_value t = unchecked_value t
    end
  end
end
