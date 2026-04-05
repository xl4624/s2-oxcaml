open Core

type lat_lng_rect =
  { lat : R1_interval.t
  ; lng : S1_interval.t
  }
[@@deriving sexp_of]

type t =
  #{ center : S2_point.t
   ; radius : S1_chord_angle.t
   }
[@@deriving sexp_of]

module Option = struct
  type value = t
  type nonrec t = t

  let none =
    #{ center =
         S2_point.of_coords ~x:(Float_u.nan ()) ~y:(Float_u.nan ()) ~z:(Float_u.nan ())
     ; radius = S1_chord_angle.Option.none
     }
  ;;

  let[@inline] [@zero_alloc] is_none t = S1_chord_angle.Option.is_none t.#radius
  let[@inline] [@zero_alloc] is_some t = not (is_none t)
  let[@inline] [@zero_alloc] some v = v
  let[@inline] [@zero_alloc] value t ~default = if is_none t then default else t

  let value_exn t =
    if is_none t
    then (
      match failwith "S2_cap.Option: value_exn called on none" with
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

  let%template[@alloc a = (heap, stack)] [@inline] [@zero_alloc ignore] sexp_of_t t
    : Sexp.t
    =
    if is_none t then sexp_of_string "None" else sexp_of_t t [@exclave_if_stack a]
  ;;
end

let empty =
  #{ center = S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0
   ; radius = S1_chord_angle.negative
   }
;;

let full =
  #{ center = S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0
   ; radius = S1_chord_angle.straight
   }
;;

let[@inline] [@zero_alloc] of_point center = #{ center; radius = S1_chord_angle.zero }

let[@inline] [@zero_alloc] of_center_angle center angle =
  let open Float_u.O in
  let angle =
    if S1_angle.is_inf angle
    then S1_angle.of_radians (Float_u.pi ())
    else if S1_angle.radians angle > Float_u.pi ()
    then S1_angle.of_radians (Float_u.pi ())
    else angle
  in
  #{ center; radius = S1_chord_angle.of_angle angle }
;;

let[@inline] [@zero_alloc] of_center_chord_angle center radius = #{ center; radius }

let[@inline] [@zero_alloc] of_center_height center height =
  #{ center; radius = S1_chord_angle.of_length2 Float_u.O.(#2.0 * height) }
;;

let[@inline] [@zero_alloc] of_center_area center area =
  #{ center; radius = S1_chord_angle.of_length2 Float_u.O.(area / Float_u.pi ()) }
;;

let[@inline] [@zero_alloc] center t = t.#center
let[@inline] [@zero_alloc] radius_chord t = t.#radius
let[@inline] [@zero_alloc] height t = Float_u.O.(#0.5 * S1_chord_angle.length2 t.#radius)
let radius_angle t = S1_chord_angle.to_angle t.#radius

let[@inline] [@zero_alloc] area t =
  let open Float_u.O in
  #2.0 * Float_u.pi () * Float_u.max #0.0 (height t)
;;

let[@inline] [@zero_alloc] is_empty t = S1_chord_angle.is_negative t.#radius
let is_full t = Float_u.O.(S1_chord_angle.length2 t.#radius = #4.0)

let is_valid t =
  S2_point.is_unit_length t.#center
  && S1_chord_angle.is_valid t.#radius
  && Float_u.O.(S1_chord_angle.length2 t.#radius <= #4.0)
;;

let[@zero_alloc ignore] centroid t =
  let open Float_u.O in
  if is_empty t
  then R3_vector.zero
  else (
    let r = #1.0 - (#0.5 * height t) in
    let a = area t in
    R3_vector.mul t.#center (r * a))
;;

let[@inline] [@zero_alloc] complement t =
  if is_full t
  then empty
  else if is_empty t
  then full
  else (
    let l2 = S1_chord_angle.length2 t.#radius in
    #{ center = R3_vector.neg t.#center
     ; radius = S1_chord_angle.of_length2 Float_u.O.(#4.0 - l2)
     })
;;

let[@inline] [@zero_alloc] contains_cap t other =
  if is_full t || is_empty other
  then true
  else (
    let dist = S2_point.chord_angle_between t.#center other.#center in
    let sum = S1_chord_angle.add dist other.#radius in
    S1_chord_angle.compare t.#radius sum >= 0)
;;

let[@inline] [@zero_alloc] intersects t other =
  if is_empty t || is_empty other
  then false
  else (
    let dist = S2_point.chord_angle_between t.#center other.#center in
    let sum = S1_chord_angle.add t.#radius other.#radius in
    S1_chord_angle.compare sum dist >= 0)
;;

let[@inline] [@zero_alloc] interior_intersects t other =
  if Float_u.O.(S1_chord_angle.length2 t.#radius <= #0.0) || is_empty other
  then false
  else (
    let dist = S2_point.chord_angle_between t.#center other.#center in
    let sum = S1_chord_angle.add t.#radius other.#radius in
    S1_chord_angle.compare sum dist > 0)
;;

let[@inline] [@zero_alloc] contains_point t p =
  if is_empty t
  then false
  else if is_full t
  then true
  else (
    let d = S2_point.chord_angle_between t.#center p in
    S1_chord_angle.compare d t.#radius <= 0)
;;

let[@inline] [@zero_alloc] interior_contains_point t p =
  if is_full t
  then true
  else if is_empty t || Float_u.O.(S1_chord_angle.length2 t.#radius <= #0.0)
  then false
  else (
    let d = S2_point.chord_angle_between t.#center p in
    S1_chord_angle.compare d t.#radius < 0)
;;

let[@inline] [@zero_alloc] add_point t p =
  if is_empty t
  then #{ center = p; radius = S1_chord_angle.zero }
  else (
    let d = S2_point.chord_angle_between t.#center p in
    if S1_chord_angle.compare d t.#radius > 0
    then #{ center = t.#center; radius = d }
    else t)
;;

let[@inline] [@zero_alloc] add_cap t other =
  if is_empty t
  then other
  else if is_empty other
  then t
  else (
    let dist =
      S1_chord_angle.add
        (S2_point.chord_angle_between t.#center other.#center)
        other.#radius
    in
    let err =
      Float_u.(
        ((#2.0 * Float_u.epsilon_float ()) + S1_chord_angle.relative_sum_error)
        * S1_chord_angle.length2 dist)
    in
    let dist = S1_chord_angle.plus_error dist err in
    if S1_chord_angle.compare dist t.#radius > 0
    then #{ center = t.#center; radius = dist }
    else t)
;;

let[@inline] [@zero_alloc] expanded t distance =
  let open Float_u.O in
  if S1_angle.radians distance < #0.0
  then Option.none
  else if is_empty t
  then Option.some empty
  else
    Option.some
      #{ center = t.#center
       ; radius = S1_chord_angle.add t.#radius (S1_chord_angle.of_angle distance)
       }
;;

let expanded_exn t distance =
  let opt = expanded t distance in
  if Option.is_none opt
  then (
    match
      Core.raise_s
        [%message
          "S2_cap.expanded: distance must be non-negative" (distance : S1_angle.t)]
    with
    | (_ : Nothing.t) -> .)
  else Option.value_exn opt
;;

let get_point_on_ray origin dir r =
  let c = Float_u.cos (S1_angle.radians r) in
  let s = Float_u.sin (S1_angle.radians r) in
  let t1 = R3_vector.mul origin c in
  let t2 = R3_vector.mul dir s in
  let p = R3_vector.add t1 t2 in
  R3_vector.normalize p [@nontail]
;;

let get_point_on_line a b r =
  let xp = S2_point.robust_cross_prod a b in
  let dir = R3_vector.normalize (R3_vector.cross xp a) in
  get_point_on_ray a dir r [@nontail]
;;

let[@zero_alloc ignore] rec union t other =
  if S1_chord_angle.compare t.#radius other.#radius < 0
  then union other t
  else if is_full t || is_empty other
  then t
  else (
    let this_r = S1_chord_angle.to_angle t.#radius in
    let other_r = S1_chord_angle.to_angle other.#radius in
    let dist = S2_point.distance t.#center other.#center in
    if S1_angle.compare this_r (S1_angle.add dist other_r) >= 0
    then t
    else (
      let result_r =
        S1_angle.mul (S1_angle.add (S1_angle.add dist this_r) other_r) #0.5
      in
      let f = S1_angle.mul (S1_angle.add (S1_angle.sub dist this_r) other_r) #0.5 in
      let result_center = get_point_on_line t.#center other.#center f in
      of_center_angle result_center result_r))
;;

let[@zero_alloc ignore] rect_bound t =
  if is_empty t
  then { lat = R1_interval.empty; lng = S1_interval.empty }
  else (
    let center_ll = S2_latlng.of_point t.#center in
    let cap_angle = Float_u.to_float (S1_angle.radians (radius_angle t)) in
    let mutable all_longitudes = false in
    let mutable lat_lo =
      Float_u.to_float (S1_angle.radians (S2_latlng.lat center_ll)) -. cap_angle
    in
    let mutable lat_hi =
      Float_u.to_float (S1_angle.radians (S2_latlng.lat center_ll)) +. cap_angle
    in
    let mutable lng_lo = Float.neg Float.pi in
    let mutable lng_hi = Float.pi in
    if Float.( <= ) lat_lo (Float.neg (Float.pi /. 2.0))
    then (
      lat_lo <- Float.neg (Float.pi /. 2.0);
      all_longitudes <- true);
    if Float.( >= ) lat_hi (Float.pi /. 2.0)
    then (
      lat_hi <- Float.pi /. 2.0;
      all_longitudes <- true);
    if not all_longitudes
    then (
      let sin_a = S1_chord_angle.sin t.#radius in
      let sin_c =
        Float_u.of_float
          (Float.cos (Float_u.to_float (S1_angle.radians (S2_latlng.lat center_ll))))
      in
      if Float_u.O.(sin_a <= sin_c)
      then (
        let angle_a = Float.asin (Float_u.to_float Float_u.O.(sin_a / sin_c)) in
        let lng = Float_u.to_float (S1_angle.radians (S2_latlng.lng center_ll)) in
        lng_lo <- Float_util.ieee_remainder (lng -. angle_a) (2.0 *. Float.pi);
        lng_hi <- Float_util.ieee_remainder (lng +. angle_a) (2.0 *. Float.pi)));
    { lat = R1_interval.create ~lo:(Float_u.of_float lat_lo) ~hi:(Float_u.of_float lat_hi)
    ; lng = S1_interval.create ~lo:(Float_u.of_float lng_lo) ~hi:(Float_u.of_float lng_hi)
    })
;;

let[@inline] [@zero_alloc] equal t other =
  (S2_point.equal t.#center other.#center && S1_chord_angle.equal t.#radius other.#radius)
  || (is_empty t && is_empty other)
  || (is_full t && is_full other)
;;

let approx_equal ?(max_error = 1e-14) t other =
  let r2 = S1_chord_angle.length2 t.#radius in
  let o2 = S1_chord_angle.length2 other.#radius in
  (S2_point.approx_equal ~max_error t.#center other.#center
   && Float.( <= ) (Float.abs (Float_u.to_float Float_u.O.(r2 - o2))) max_error)
  || (is_empty t && Float.( <= ) (Float_u.to_float o2) max_error)
  || (is_empty other && Float.( <= ) (Float_u.to_float r2) max_error)
  || (is_full t && Float.( >= ) (Float_u.to_float o2) (2.0 -. max_error))
  || (is_full other && Float.( >= ) (Float_u.to_float r2) (2.0 -. max_error))
;;

let[@inline] set_le_f64_into buf off f =
  let u = Stdlib.Int64.bits_of_float f in
  for i = 0 to 7 do
    Bytes.unsafe_set
      buf
      (off + i)
      (Stdlib.Char.chr
         (Stdlib.Int64.to_int
            (Stdlib.Int64.logand 0xFFL (Stdlib.Int64.shift_right_logical u (8 * i)))))
  done
;;

let[@inline] get_le_f64 s off =
  let rec loop i acc =
    if Int.( = ) i 8
    then Stdlib.Int64.float_of_bits acc
    else (
      let byte = Stdlib.Char.code (String.unsafe_get s (off + i)) in
      loop
        (i + 1)
        (Stdlib.Int64.logor
           acc
           (Stdlib.Int64.shift_left (Stdlib.Int64.of_int byte) (8 * i))))
  in
  loop 0 0L
;;

let encode t =
  let b = Bytes.create 32 in
  let c = t.#center in
  set_le_f64_into b 0 (Float_u.to_float (R3_vector.x c));
  set_le_f64_into b 8 (Float_u.to_float (R3_vector.y c));
  set_le_f64_into b 16 (Float_u.to_float (R3_vector.z c));
  set_le_f64_into b 24 (Float_u.to_float (S1_chord_angle.length2 t.#radius));
  Bytes.to_string b
;;

let decode s =
  if String.length s <> 32
  then Option.none
  else (
    let x = get_le_f64 s 0 in
    let y = get_le_f64 s 8 in
    let z = get_le_f64 s 16 in
    let l2 = get_le_f64 s 24 in
    let center =
      S2_point.of_coords
        ~x:(Float_u.of_float x)
        ~y:(Float_u.of_float y)
        ~z:(Float_u.of_float z)
    in
    let radius = S1_chord_angle.of_length2 (Float_u.of_float l2) in
    let t = #{ center; radius } in
    if not (is_valid t) then Option.none else Option.some t)
;;

let decode_exn s = Option.value_exn (decode s)
