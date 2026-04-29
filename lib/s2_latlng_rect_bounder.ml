open Core

type t =
  #{ a : S2_point.t
   ; a_latlng : S2_latlng.t
   ; bound : S2_latlng_rect.t
   }

(* [a] / [a_latlng] are sentinels until the first [add_internal] call. They
   are only inspected once [bound] is non-empty, so the initial values never
   leak into the result. *)
let[@inline] [@zero_alloc] create () =
  #{ a = S2_point.origin; a_latlng = S2_latlng.zero; bound = S2_latlng_rect.empty }
;;

(* Core update. Accumulates the geodesic edge (a, b) into [bound]. The
   interesting work is bounding the latitude range: a great circle can reach
   above or below both endpoints, so we check whether the edge's maximum
   latitude lies in its interior and adjust accordingly. Magic numbers like
   1.91346e-15 and 6.06638e-16 come from the error analysis in the upstream
   source; see s2latlng_rect_bounder.cc:AddInternal. *)
let add_internal (t : t) (b : S2_point.t) (b_latlng : S2_latlng.t) : t =
  if S2_latlng_rect.is_empty t.#bound
  then #{ a = b; a_latlng = b_latlng; bound = S2_latlng_rect.add_point t.#bound b_latlng }
  else
    let open Float_u.O in
    let a = t.#a in
    let a_latlng = t.#a_latlng in
    (* N = (a - b) x (a + b) = 2 * (a x b); computing it this way retains
       precision when [a] and [b] are close to each other, where a direct
       cross product would subtract nearly equal quantities. *)
    let n = R3_vector.cross (R3_vector.sub a b) (R3_vector.add a b) in
    let n_norm = R3_vector.norm n in
    let new_bound =
      if n_norm < #1.91346e-15
      then
        (* A and B are either nearly identical or nearly antipodal. *)
        if R3_vector.dot a b < #0.0
        then S2_latlng_rect.full
        else
          S2_latlng_rect.union t.#bound (S2_latlng_rect.of_point_pair a_latlng b_latlng)
      else (
        let a_lat_rad = S1_angle.radians (S2_latlng.lat a_latlng) in
        let a_lng_rad = S1_angle.radians (S2_latlng.lng a_latlng) in
        let b_lat_rad = S1_angle.radians (S2_latlng.lat b_latlng) in
        let b_lng_rad = S1_angle.radians (S2_latlng.lng b_latlng) in
        let lng_ab = S1_interval.from_point_pair a_lng_rad b_lng_rad in
        let lng_ab =
          if S1_interval.length lng_ab >= Float_u.pi - (#2.0 * Float_u.epsilon_float)
          then S1_interval.full
          else lng_ab
        in
        let lat_ab = R1_interval.from_point_pair a_lat_rad b_lat_rad in
        (* [m] is perpendicular to the plane through [n] and the Z axis. The
           sign of dot(m, a) vs dot(m, b) tells us whether the latitude
           extremum lies in the interior of the edge (signs differ) or at an
           endpoint (signs agree). *)
        let m = R3_vector.cross n (R3_vector.create ~x:#0.0 ~y:#0.0 ~z:#1.0) in
        let m_a = R3_vector.dot m a in
        let m_b = R3_vector.dot m b in
        let m_error = (#6.06638e-16 * n_norm) + #6.83174e-31 in
        let lat_ab =
          if m_a * m_b < #0.0 || Float_u.abs m_a <= m_error || Float_u.abs m_b <= m_error
          then (
            (* The latitude extremum may lie in the interior of the edge.
               [max_lat] is the theoretical maximum latitude on the great
               circle; the "latitude budget" then bounds how much farther the
               actual edge can reach beyond its endpoints. *)
            let nx = R3_vector.x n in
            let ny = R3_vector.y n in
            let nz = R3_vector.z n in
            let max_lat =
              Float_u.min
                (Float_u.atan2 (Float_u.sqrt ((nx * nx) + (ny * ny))) (Float_u.abs nz)
                 + (#3.0 * Float_u.epsilon_float))
                (Float_u.pi / #2.0)
            in
            (* Tight bound relative to the endpoints using a latitude budget. *)
            let ab_norm = R3_vector.norm (R3_vector.sub a b) in
            let lat_budget_z = #0.5 * ab_norm * Float_u.sin max_lat in
            let lat_budget =
              #2.0
              * Float_u.asin
                  (Float_u.min
                     ((#1.0 + (#4.0 * Float_u.epsilon_float)) * lat_budget_z)
                     #1.0)
            in
            let max_delta =
              (#0.5 * (lat_budget - R1_interval.length lat_ab)) + Float_u.epsilon_float
            in
            let hi =
              if m_a <= m_error && m_b >= Float_u.neg m_error
              then Float_util.min_u max_lat (R1_interval.hi lat_ab + max_delta)
              else R1_interval.hi lat_ab
            in
            let lo =
              if m_b <= m_error && m_a >= Float_u.neg m_error
              then
                Float_util.max_u (Float_u.neg max_lat) (R1_interval.lo lat_ab - max_delta)
              else R1_interval.lo lat_ab
            in
            R1_interval.create ~lo ~hi)
          else lat_ab
        in
        S2_latlng_rect.union t.#bound (S2_latlng_rect.create ~lat:lat_ab ~lng:lng_ab))
    in
    #{ a = b; a_latlng = b_latlng; bound = new_bound }
;;

let[@inline] [@zero_alloc] add_point t b = add_internal t b (S2_latlng.of_point b)

let[@inline] [@zero_alloc] add_latlng t b_latlng =
  add_internal t (S2_latlng.to_point b_latlng) b_latlng
;;

let[@zero_alloc] get_bound t =
  (* Pad latitude by 2*epsilon to absorb the worst-case rounding when a caller
     re-projects an S2Point back to S2LatLng, then take polar_closure so that
     a bound that touches a pole covers every longitude at that pole. *)
  let open Float_u.O in
  let expansion = S2_latlng.of_radians ~lat:(#2.0 * Float_u.epsilon_float) ~lng:#0.0 in
  S2_latlng_rect.polar_closure (S2_latlng_rect.expanded t.#bound expansion)
;;

let[@zero_alloc] expand_for_subregions bound =
  if S2_latlng_rect.is_empty bound
  then bound
  else
    let open Float_u.O in
    let lat = S2_latlng_rect.lat bound in
    let lng = S2_latlng_rect.lng bound in
    let lng_gap =
      Float_u.max
        #0.0
        (Float_u.pi - S1_interval.length lng - (#2.5 * Float_u.epsilon_float))
    in
    let min_abs_lat =
      Float_util.max_u (R1_interval.lo lat) (Float_u.neg (R1_interval.hi lat))
    in
    let lat_gap1 = (Float_u.pi / #2.0) + R1_interval.lo lat in
    let lat_gap2 = (Float_u.pi / #2.0) - R1_interval.hi lat in
    let returns_full =
      if min_abs_lat >= #0.0
      then (#2.0 * min_abs_lat) + lng_gap < #1.354e-15
      else if lng_gap >= Float_u.pi / #2.0
      then lat_gap1 + lat_gap2 < #1.687e-15
      else Float_util.max_u lat_gap1 lat_gap2 * lng_gap < #1.765e-15
    in
    if returns_full
    then S2_latlng_rect.full
    else (
      let lat_expansion = #9.0 * Float_u.epsilon_float in
      let lng_expansion = if lng_gap <= #0.0 then Float_u.pi else #0.0 in
      S2_latlng_rect.polar_closure
        (S2_latlng_rect.expanded
           bound
           (S2_latlng.of_radians ~lat:lat_expansion ~lng:lng_expansion)))
;;

let max_error_for_tests () =
  let open Float_u.O in
  S2_latlng.of_radians
    ~lat:(#10.0 * Float_u.epsilon_float)
    ~lng:(#1.0 * Float_u.epsilon_float)
;;
