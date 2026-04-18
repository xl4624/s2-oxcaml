open Core

(* Mean radius: 6371.01 km == 6371010 meters. *)
let radius_meters = #6_371_010.0
let radius_km = #6_371.01

(* Altitude range: Dead Sea (-10898 m) to Mount Everest (8848 m). *)
let lowest_altitude_meters = -#10_898.0
let highest_altitude_meters = #8_848.0

let[@inline] [@zero_alloc] angle_from_length meters =
  S1_angle.of_radians Float_u.O.(meters / radius_meters)
;;

let[@inline] [@zero_alloc] length_from_angle angle =
  Float_u.O.(S1_angle.radians angle * radius_meters)
;;

let[@inline] [@zero_alloc] length_from_points a b =
  length_from_angle (S2_point.distance a b)
;;

let[@inline] [@zero_alloc] length_from_latlngs a b =
  length_from_angle (S2_latlng.distance a b)
;;

let[@inline] [@zero_alloc] area_from_steradians s =
  Float_u.O.(s * radius_meters * radius_meters)
;;

let[@inline] [@zero_alloc] steradians_from_area m2 =
  Float_u.O.(m2 / (radius_meters * radius_meters))
;;

let[@inline] [@zero_alloc] initial_bearing_from_latlngs a b =
  let open Float_u.O in
  let lat1 = S1_angle.radians (S2_latlng.lat a) in
  let lat2 = S1_angle.radians (S2_latlng.lat b) in
  let lng1 = S1_angle.radians (S2_latlng.lng a) in
  let lng2 = S1_angle.radians (S2_latlng.lng b) in
  let cos_lat2 = Float_u.cos lat2 in
  let lat_diff = lat2 - lat1 in
  let lng_diff = lng2 - lng1 in
  let sin_half_lng = Float_u.sin (lng_diff / #2.0) in
  let hav_lng = sin_half_lng * sin_half_lng in
  let x = Float_u.sin lat_diff + (Float_u.sin lat1 * cos_lat2 * #2.0 * hav_lng) in
  let y = Float_u.sin lng_diff * cos_lat2 in
  S1_angle.of_radians (Float_u.atan2 y x)
;;
