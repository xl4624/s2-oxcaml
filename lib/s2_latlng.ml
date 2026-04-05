open Core

type t =
  #{ lat : Float_u.t
   ; lng : Float_u.t
   }
[@@deriving sexp_of]

let[@zero_alloc ignore] pp ppf t =
  let lat_val = Float_u.to_float t.#lat in
  let lng_val = Float_u.to_float t.#lng in
  Format.fprintf ppf "{lat = %a; lng = %a}" Float.pp lat_val Float.pp lng_val
;;

let[@zero_alloc ignore] to_string t =
  let buffer = Buffer.create 32 in
  let ppf = Format.formatter_of_buffer buffer in
  pp ppf t;
  Format.pp_print_flush ppf ();
  Buffer.contents buffer
;;

module Option = struct
  type value = t
  type t = value

  let none = #{ lat = Float_u.nan (); lng = Float_u.nan () }

  let[@inline] [@zero_alloc] some (v : value) =
    if Float_u.is_nan v.#lat || Float_u.is_nan v.#lng then none else v
  ;;

  let[@inline] [@zero_alloc] is_none t = Float_u.is_nan t.#lat || Float_u.is_nan t.#lng
  let[@inline] [@zero_alloc] is_some t = not (is_none t)
  let[@inline] [@zero_alloc] unchecked_value t = t

  let[@zero_alloc ignore] sexp_of_t t =
    if is_none t
    then Sexp.List []
    else
      Sexp.List
        [ Sexp.List
            [ Sexp.List [ Sexp.Atom "lat"; Float.sexp_of_t (Float_u.to_float t.#lat) ]
            ; Sexp.List [ Sexp.Atom "lng"; Float.sexp_of_t (Float_u.to_float t.#lng) ]
            ]
        ]
  ;;

  let[@inline] [@zero_alloc] value t ~default = if is_some t then t else default

  let[@inline] [@zero_alloc] value_exn t =
    if is_none t
    then (
      match raise_s [%message "S2_latlng.Option.value_exn: none"] with
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

let[@inline] [@zero_alloc] create ~lat ~lng =
  #{ lat = S1_angle.radians lat; lng = S1_angle.radians lng }
;;

let zero = #{ lat = #0.0; lng = #0.0 }
let invalid = #{ lat = Float_u.pi (); lng = Float_u.O.(#2.0 * Float_u.pi ()) }
let[@inline] [@zero_alloc] of_radians ~lat ~lng = #{ lat; lng }

let[@inline] [@zero_alloc] of_degrees ~lat ~lng =
  create ~lat:(S1_angle.of_degrees lat) ~lng:(S1_angle.of_degrees lng)
;;

let[@inline] [@zero_alloc] of_e5 ~lat ~lng : Option.t =
  let lat_a = S1_angle.of_e5 lat in
  let lng_a = S1_angle.of_e5 lng in
  let deg = S1_angle.degrees lat_a in
  if Float_u.O.(deg < -#90.0 || deg > #90.0)
  then Option.none
  else Option.some (create ~lat:lat_a ~lng:lng_a)
;;

let[@inline] [@zero_alloc] of_e5_exn ~lat ~lng =
  let lat_a = S1_angle.of_e5 lat in
  let lng_a = S1_angle.of_e5 lng in
  let deg = S1_angle.degrees lat_a in
  if Float_u.O.(deg < -#90.0 || deg > #90.0)
  then (
    match
      raise_s
        [%message "S2LatLng.of_e5_exn: latitude out of range" (lat : int) (lng : int)]
    with
    | (_ : Nothing.t) -> .)
  else create ~lat:lat_a ~lng:lng_a
;;

let[@inline] [@zero_alloc] of_e6 ~lat ~lng : Option.t =
  let lat_a = S1_angle.of_e6 lat in
  let lng_a = S1_angle.of_e6 lng in
  let deg = S1_angle.degrees lat_a in
  if Float_u.O.(deg < -#90.0 || deg > #90.0)
  then Option.none
  else Option.some (create ~lat:lat_a ~lng:lng_a)
;;

let[@inline] [@zero_alloc] of_e6_exn ~lat ~lng =
  let lat_a = S1_angle.of_e6 lat in
  let lng_a = S1_angle.of_e6 lng in
  let deg = S1_angle.degrees lat_a in
  if Float_u.O.(deg < -#90.0 || deg > #90.0)
  then (
    match
      raise_s
        [%message "S2LatLng.of_e6_exn: latitude out of range" (lat : int) (lng : int)]
    with
    | (_ : Nothing.t) -> .)
  else create ~lat:lat_a ~lng:lng_a
;;

let[@inline] [@zero_alloc] of_e7 ~lat ~lng : Option.t =
  let lat_a = S1_angle.of_e7 lat in
  let lng_a = S1_angle.of_e7 lng in
  let deg = S1_angle.degrees lat_a in
  if Float_u.O.(deg < -#90.0 || deg > #90.0)
  then Option.none
  else Option.some (create ~lat:lat_a ~lng:lng_a)
;;

let[@inline] [@zero_alloc] of_e7_exn ~lat ~lng =
  let lat_a = S1_angle.of_e7 lat in
  let lng_a = S1_angle.of_e7 lng in
  let deg = S1_angle.degrees lat_a in
  if Float_u.O.(deg < -#90.0 || deg > #90.0)
  then (
    match
      raise_s
        [%message "S2LatLng.of_e7_exn: latitude out of range" (lat : int) (lng : int)]
    with
    | (_ : Nothing.t) -> .)
  else create ~lat:lat_a ~lng:lng_a
;;

let[@inline] [@zero_alloc] lat t = S1_angle.of_radians t.#lat
let[@inline] [@zero_alloc] lng t = S1_angle.of_radians t.#lng

let[@inline] [@zero_alloc] latitude p =
  let z = Float_u.O.(S2_point.z p + #0.0) in
  let x = S2_point.x p in
  let y = S2_point.y p in
  S1_angle.of_radians (Float_u.atan2 z (Float_u.sqrt Float_u.O.((x * x) + (y * y))))
;;

let[@inline] [@zero_alloc] longitude p =
  let x = Float_u.O.(S2_point.x p + #0.0) in
  let y = Float_u.O.(S2_point.y p + #0.0) in
  S1_angle.of_radians (Float_u.atan2 y x)
;;

let[@inline] [@zero_alloc] of_point p =
  #{ lat = S1_angle.radians (latitude p); lng = S1_angle.radians (longitude p) }
;;

let[@inline] [@zero_alloc] is_valid t =
  let open Float_u.O in
  Float_u.abs t.#lat <= Float_u.pi () / #2.0
  && Float_u.abs t.#lng <= Float_u.pi ()
  && (not (Float_u.is_nan t.#lat || Float_u.is_inf t.#lat))
  && not (Float_u.is_nan t.#lng || Float_u.is_inf t.#lng)
;;

let[@inline] [@zero_alloc] normalized t : t =
  if Float_u.is_nan t.#lat
     || Float_u.is_inf t.#lat
     || Float_u.is_nan t.#lng
     || Float_u.is_inf t.#lng
  then invalid
  else
    let open Float_u.O in
    let lat =
      Float_u.clamp_exn
        t.#lat
        ~min:(Float_u.neg (Float_u.pi ()) / #2.0)
        ~max:(Float_u.pi () / #2.0)
    in
    let lng = Float_util.ieee_remainder_u t.#lng (#2.0 * Float_u.pi ()) in
    #{ lat; lng }
;;

let[@inline] [@zero_alloc] to_point t =
  let phi = t.#lat in
  let theta = t.#lng in
  let cos_phi = Float_u.cos phi in
  let open Float_u.O in
  R3_vector.create
    ~x:(Float_u.cos theta * cos_phi)
    ~y:(Float_u.sin theta * cos_phi)
    ~z:(Float_u.sin phi)
;;

let[@inline] [@zero_alloc] distance a b =
  let lat1 = a.#lat in
  let lat2 = b.#lat in
  let lng1 = a.#lng in
  let lng2 = b.#lng in
  let open Float_u.O in
  let dlat = Float_u.sin (#0.5 * (lat2 - lat1)) in
  let dlng = Float_u.sin (#0.5 * (lng2 - lng1)) in
  let x = (dlat * dlat) + (dlng * dlng * Float_u.cos lat1 * Float_u.cos lat2) in
  S1_angle.of_radians (#2.0 * Float_u.asin (Float_u.sqrt (Float_u.min #1.0 x)))
;;

let[@inline] [@zero_alloc] add a b : t =
  let open Float_u.O in
  #{ lat = a.#lat + b.#lat; lng = a.#lng + b.#lng }
;;

let[@inline] [@zero_alloc] sub a b : t =
  let open Float_u.O in
  #{ lat = a.#lat - b.#lat; lng = a.#lng - b.#lng }
;;

let[@inline] [@zero_alloc] mul t k : t =
  let open Float_u.O in
  #{ lat = t.#lat * k; lng = t.#lng * k }
;;

let[@zero_alloc ignore] approx_equal ?(max_error = 1e-15) a b =
  let max_error = Float_u.of_float max_error in
  let open Float_u.O in
  Float_u.abs (a.#lat - b.#lat) <= max_error && Float_u.abs (a.#lng - b.#lng) <= max_error
;;

let[@inline] [@zero_alloc] equal a b = Float_u.(a.#lat = b.#lat && a.#lng = b.#lng)
