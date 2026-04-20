open Core

[@@@zero_alloc all]

type kind =
  | Plate_carree
  | Mercator
[@@deriving sexp_of]

(* The two built-in projections have identical parameterization (a single
   scale), so we collapse them into one unboxed record with a kind tag and
   dispatch on that. This keeps the type immediate/float64-only (no heap box)
   while preserving the common [Projection] interface. *)
type t =
  #{ kind_tag : int
   ; x_wrap : Float_u.t
   ; to_radians : Float_u.t
   ; from_radians : Float_u.t
   }

let plate_carree_tag = 0
let mercator_tag = 1
let[@inline] [@zero_alloc] is_plate_carree t = Int.(t.#kind_tag = plate_carree_tag)

let[@zero_alloc ignore] sexp_of_t t : Sexp.t =
  let kind = if is_plate_carree t then "Plate_carree" else "Mercator" in
  Sexp.List
    [ Sexp.List [ Sexp.Atom "kind"; Sexp.Atom kind ]
    ; Sexp.List [ Sexp.Atom "x_wrap"; Float.sexp_of_t (Float_u.to_float t.#x_wrap) ]
    ; Sexp.List
        [ Sexp.Atom "to_radians"; Float.sexp_of_t (Float_u.to_float t.#to_radians) ]
    ; Sexp.List
        [ Sexp.Atom "from_radians"; Float.sexp_of_t (Float_u.to_float t.#from_radians) ]
    ]
;;

let[@inline] [@zero_alloc] plate_carree ~x_scale =
  let open Float_u.O in
  #{ kind_tag = plate_carree_tag
   ; x_wrap = #2.0 * x_scale
   ; to_radians = Float_u.pi () / x_scale
   ; from_radians = x_scale / Float_u.pi ()
   }
;;

let[@inline] [@zero_alloc] mercator ~max_x =
  let open Float_u.O in
  #{ kind_tag = mercator_tag
   ; x_wrap = #2.0 * max_x
   ; to_radians = Float_u.pi () / max_x
   ; from_radians = max_x / Float_u.pi ()
   }
;;

let[@inline] [@zero_alloc] kind t = if is_plate_carree t then Plate_carree else Mercator

let[@inline] [@zero_alloc] from_latlng t ll =
  let open Float_u.O in
  let lat = S1_angle.radians (S2_latlng.lat ll) in
  let lng = S1_angle.radians (S2_latlng.lng ll) in
  if is_plate_carree t
  then R2_point.create ~x:(t.#from_radians * lng) ~y:(t.#from_radians * lat)
  else (
    (* Mercator: y = 0.5 * log((1 + sin phi) / (1 - sin phi)). This formula
       is more accurate near zero than log(tan(...)); lat = +/-pi/2 yields
       y = +/-inf. *)
    let sin_phi = Float_u.sin lat in
    let y = #0.5 * Float_u.log ((#1.0 + sin_phi) / (#1.0 - sin_phi)) in
    R2_point.create ~x:(t.#from_radians * lng) ~y:(t.#from_radians * y))
;;

let[@inline] [@zero_alloc] to_latlng t p =
  let open Float_u.O in
  let x = t.#to_radians * Float_util.ieee_remainder_u (R2_point.x p) t.#x_wrap in
  if is_plate_carree t
  then S2_latlng.of_radians ~lat:(t.#to_radians * R2_point.y p) ~lng:x
  else (
    (* Mercator inverse: this form is more accurate near zero than
       atan(exp(...)). When the intermediate k overflows to infinity the
       latitude saturates at +/-pi/2. *)
    let two_ty = #2.0 * t.#to_radians * R2_point.y p in
    let k = Float_u.exp two_ty in
    let lat =
      if Float_u.is_inf k
      then
        (* exp overflows only for large positive argument; signum from y. *)
        if R2_point.y p > #0.0 then Float_u.pi () / #2.0 else -(Float_u.pi () / #2.0)
      else Float_u.asin ((k - #1.0) / (k + #1.0))
    in
    S2_latlng.of_radians ~lat ~lng:x)
;;

let[@inline] [@zero_alloc] project t p = from_latlng t (S2_latlng.of_point p)
let[@inline] [@zero_alloc] unproject t p = S2_latlng.to_point (to_latlng t p)

let[@inline] [@zero_alloc] interpolate _t ~f a b =
  let open Float_u.O in
  let one_minus_f = #1.0 - f in
  R2_point.add (R2_point.mul a one_minus_f) (R2_point.mul b f)
;;

let[@inline] [@zero_alloc] wrap_distance t = R2_point.create ~x:t.#x_wrap ~y:#0.0

let[@inline] [@zero_alloc] wrap_destination t ~a ~b =
  let open Float_u.O in
  let wrap_x = t.#x_wrap in
  let ax = R2_point.x a in
  let bx = R2_point.x b in
  let by = R2_point.y b in
  (* Only the x axis wraps for both built-in projections; y is passed
     through unchanged. *)
  let x =
    if wrap_x > #0.0 && Float_u.abs (bx - ax) > #0.5 * wrap_x
    then bx - (Float_u.round_nearest_half_to_even ((bx - ax) / wrap_x) * wrap_x)
    else bx
  in
  R2_point.create ~x ~y:by
;;
