open Core

type t =
  #{ lat : R1_interval.t
   ; lng : S1_interval.t
   }
[@@deriving sexp_of]

let full_lat =
  let open Float_u.O in
  R1_interval.create ~lo:(Float_u.neg (Float_u.pi () / #2.0)) ~hi:(Float_u.pi () / #2.0)
;;

let empty = #{ lat = R1_interval.empty; lng = S1_interval.empty }
let full = #{ lat = full_lat; lng = S1_interval.full }
let[@inline] [@zero_alloc] create ~lat ~lng = #{ lat; lng }
let[@inline] [@zero_alloc] lat t = t.#lat
let[@inline] [@zero_alloc] lng t = t.#lng

let[@inline] [@zero_alloc] of_lo_hi ~lo ~hi =
  #{ lat =
       R1_interval.create
         ~lo:(S1_angle.radians (S2_latlng.lat lo))
         ~hi:(S1_angle.radians (S2_latlng.lat hi))
   ; lng =
       S1_interval.create
         ~lo:(S1_angle.radians (S2_latlng.lng lo))
         ~hi:(S1_angle.radians (S2_latlng.lng hi))
   }
;;

let[@inline] [@zero_alloc] of_point ll =
  let lat_rad = S1_angle.radians (S2_latlng.lat ll) in
  let lng_rad = S1_angle.radians (S2_latlng.lng ll) in
  #{ lat = R1_interval.create ~lo:lat_rad ~hi:lat_rad
   ; lng = S1_interval.create ~lo:lng_rad ~hi:lng_rad
   }
;;

let[@inline] [@zero_alloc] of_point_pair p1 p2 =
  #{ lat =
       R1_interval.from_point_pair
         (S1_angle.radians (S2_latlng.lat p1))
         (S1_angle.radians (S2_latlng.lat p2))
   ; lng =
       S1_interval.from_point_pair
         (S1_angle.radians (S2_latlng.lng p1))
         (S1_angle.radians (S2_latlng.lng p2))
   }
;;

let[@inline] [@zero_alloc] lo t =
  S2_latlng.of_radians ~lat:(R1_interval.lo t.#lat) ~lng:(S1_interval.lo t.#lng)
;;

let[@inline] [@zero_alloc] hi t =
  S2_latlng.of_radians ~lat:(R1_interval.hi t.#lat) ~lng:(S1_interval.hi t.#lng)
;;

let[@inline] [@zero_alloc] center t =
  S2_latlng.of_radians ~lat:(R1_interval.center t.#lat) ~lng:(S1_interval.center t.#lng)
;;

let[@inline] [@zero_alloc] size t =
  S2_latlng.of_radians ~lat:(R1_interval.length t.#lat) ~lng:(S1_interval.length t.#lng)
;;

let[@inline] [@zero_alloc] vertex t k =
  (* Twiddle bits to return CCW: lower-left, lower-right, upper-right, upper-left *)
  let i = (k lsr 1) land 1 in
  let j = i lxor (k land 1) in
  let lat_val = if i = 0 then R1_interval.lo t.#lat else R1_interval.hi t.#lat in
  let lng_val = if j = 0 then S1_interval.lo t.#lng else S1_interval.hi t.#lng in
  S2_latlng.of_radians ~lat:lat_val ~lng:lng_val
;;

let[@inline] [@zero_alloc] is_empty t = R1_interval.is_empty t.#lat

let[@inline] [@zero_alloc] is_full t =
  R1_interval.equal t.#lat full_lat && S1_interval.is_full t.#lng
;;

let[@inline] [@zero_alloc] is_point t =
  let open Float_u.O in
  R1_interval.lo t.#lat = R1_interval.hi t.#lat
  && S1_interval.lo t.#lng = S1_interval.hi t.#lng
;;

let[@inline] [@zero_alloc] is_inverted t = S1_interval.is_inverted t.#lng

let[@inline] [@zero_alloc] is_valid t =
  let open Float_u.O in
  Float_u.abs (R1_interval.lo t.#lat) <= Float_u.pi () / #2.0
  && Float_u.abs (R1_interval.hi t.#lat) <= Float_u.pi () / #2.0
  && S1_interval.is_valid t.#lng
  && Bool.equal (R1_interval.is_empty t.#lat) (S1_interval.is_empty t.#lng)
;;

let[@zero_alloc ignore] area t =
  if is_empty t
  then #0.0
  else
    let open Float_u.O in
    S1_interval.length t.#lng
    * Float_u.abs
        (Float_u.sin (R1_interval.hi t.#lat) - Float_u.sin (R1_interval.lo t.#lat))
;;

let[@zero_alloc ignore] centroid t =
  if is_empty t
  then R3_vector.zero
  else
    let open Float_u.O in
    let z1 = Float_u.sin (R1_interval.lo t.#lat) in
    let z2 = Float_u.sin (R1_interval.hi t.#lat) in
    let r1 = Float_u.cos (R1_interval.lo t.#lat) in
    let r2 = Float_u.cos (R1_interval.hi t.#lat) in
    let alpha = #0.5 * S1_interval.length t.#lng in
    let r0 = Float_u.sin alpha * ((r2 * z2) - (r1 * z1) + R1_interval.length t.#lat) in
    let lng_center = S1_interval.center t.#lng in
    let z = alpha * (z2 + z1) * (z2 - z1) in
    R3_vector.create ~x:(r0 * Float_u.cos lng_center) ~y:(r0 * Float_u.sin lng_center) ~z
;;

let[@inline] [@zero_alloc] contains_latlng t ll =
  R1_interval.contains t.#lat (S1_angle.radians (S2_latlng.lat ll))
  && S1_interval.contains t.#lng (S1_angle.radians (S2_latlng.lng ll))
;;

let[@inline] [@zero_alloc] interior_contains_latlng t ll =
  R1_interval.interior_contains t.#lat (S1_angle.radians (S2_latlng.lat ll))
  && S1_interval.interior_contains t.#lng (S1_angle.radians (S2_latlng.lng ll))
;;

let[@inline] [@zero_alloc] contains_point t p = contains_latlng t (S2_latlng.of_point p)

let[@inline] [@zero_alloc] interior_contains_point t p =
  interior_contains_latlng t (S2_latlng.of_point p)
;;

let[@inline] [@zero_alloc] contains t other =
  R1_interval.contains_interval t.#lat other.#lat
  && S1_interval.contains_interval t.#lng other.#lng
;;

let[@inline] [@zero_alloc] interior_contains t other =
  R1_interval.interior_contains_interval t.#lat other.#lat
  && S1_interval.interior_contains_interval t.#lng other.#lng
;;

let[@inline] [@zero_alloc] intersects t other =
  R1_interval.intersects t.#lat other.#lat && S1_interval.intersects t.#lng other.#lng
;;

let[@inline] [@zero_alloc] interior_intersects t other =
  R1_interval.interior_intersects t.#lat other.#lat
  && S1_interval.interior_intersects t.#lng other.#lng
;;

let[@inline] [@zero_alloc] union t other =
  #{ lat = R1_interval.union t.#lat other.#lat
   ; lng = S1_interval.union t.#lng other.#lng
   }
;;

let[@inline] [@zero_alloc] intersection t other =
  let lat = R1_interval.intersection t.#lat other.#lat in
  let lng = S1_interval.intersection t.#lng other.#lng in
  if R1_interval.is_empty lat || S1_interval.is_empty lng then empty else #{ lat; lng }
;;

let[@inline] [@zero_alloc] add_point t ll =
  #{ lat = R1_interval.add_point t.#lat (S1_angle.radians (S2_latlng.lat ll))
   ; lng = S1_interval.add_point t.#lng (S1_angle.radians (S2_latlng.lng ll))
   }
;;

let[@inline] [@zero_alloc] expanded t margin =
  let lat = R1_interval.expanded t.#lat (S1_angle.radians (S2_latlng.lat margin)) in
  let lng = S1_interval.expanded t.#lng (S1_angle.radians (S2_latlng.lng margin)) in
  if R1_interval.is_empty lat || S1_interval.is_empty lng
  then empty
  else #{ lat = R1_interval.intersection lat full_lat; lng }
;;

let[@inline] [@zero_alloc] polar_closure t =
  let open Float_u.O in
  if R1_interval.lo t.#lat = Float_u.neg (Float_u.pi () / #2.0)
     || R1_interval.hi t.#lat = Float_u.pi () / #2.0
  then #{ lat = t.#lat; lng = S1_interval.full }
  else t
;;

let of_center_size ~center ~size =
  let half = S2_latlng.mul size #0.5 in
  expanded (of_point center) half
;;

let[@zero_alloc ignore] cap_bound t =
  if is_empty t
  then S2_cap.empty
  else
    let open Float_u.O in
    let mutable pole_z = #1.0 in
    let mutable pole_angle = (Float_u.pi () / #2.0) - R1_interval.lo t.#lat in
    if R1_interval.lo t.#lat + R1_interval.hi t.#lat < #0.0
    then (
      (* South pole axis yields smaller cap *)
      pole_z <- -#1.0;
      pole_angle <- (Float_u.pi () / #2.0) + R1_interval.hi t.#lat);
    let pole_angle_with_err = (#1.0 + (#2.0 * Float_u.epsilon_float ())) * pole_angle in
    let pole_cap =
      S2_cap.of_center_angle
        (S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:pole_z)
        (S1_angle.of_radians pole_angle_with_err)
    in
    (* For bounding rectangles that span <= 180 degrees in longitude, the max
       cap size is achieved at one of the rectangle vertices. *)
    if S1_interval.length t.#lng <= Float_u.pi ()
    then (
      let mutable mid_cap = S2_cap.of_point (S2_latlng.to_point (center t)) in
      for k = 0 to 3 do
        mid_cap <- S2_cap.add_point mid_cap (S2_latlng.to_point (vertex t k))
      done;
      if S2_cap.height mid_cap < S2_cap.height pole_cap then mid_cap else pole_cap)
    else pole_cap
;;

let[@inline] [@zero_alloc] rect_bound t = t

(* Bounding rectangle of a cap. *)
let[@zero_alloc ignore] from_cap c =
  let open Float_u.O in
  if S2_cap.is_empty c
  then empty
  else (
    let center_ll = S2_latlng.of_point (S2_cap.center c) in
    let cap_angle = S1_angle.radians (S2_cap.radius_angle c) in
    let mutable all_longitudes = false in
    let mutable lat_lo = S1_angle.radians (S2_latlng.lat center_ll) - cap_angle in
    let mutable lat_hi = S1_angle.radians (S2_latlng.lat center_ll) + cap_angle in
    let mutable lng_lo = Float_u.neg (Float_u.pi ()) in
    let mutable lng_hi = Float_u.pi () in
    if lat_lo <= Float_u.neg (Float_u.pi () / #2.0)
    then (
      lat_lo <- Float_u.neg (Float_u.pi () / #2.0);
      all_longitudes <- true);
    if lat_hi >= Float_u.pi () / #2.0
    then (
      lat_hi <- Float_u.pi () / #2.0;
      all_longitudes <- true);
    if not all_longitudes
    then (
      let sin_a = S1_chord_angle.sin (S2_cap.radius_chord c) in
      let sin_c = Float_u.cos (S1_angle.radians (S2_latlng.lat center_ll)) in
      if Float_u.O.(sin_a <= sin_c)
      then (
        let angle_a = Float_u.asin (sin_a / sin_c) in
        let lng = S1_angle.radians (S2_latlng.lng center_ll) in
        lng_lo <- Float_util.ieee_remainder_u (lng - angle_a) (#2.0 * Float_u.pi ());
        lng_hi <- Float_util.ieee_remainder_u (lng + angle_a) (#2.0 * Float_u.pi ())));
    #{ lat = R1_interval.create ~lo:lat_lo ~hi:lat_hi
     ; lng = S1_interval.create ~lo:lng_lo ~hi:lng_hi
     })
;;

let pole_min_lat =
  let open Float_u.O in
  Float_u.asin (Float_u.sqrt (#1.0 / #3.0)) - (#0.5 * Float_u.epsilon_float ())
;;

(* Bounding rectangle of an S2 cell. *)
let[@zero_alloc ignore] from_cell cell =
  let level = S2_cell.level cell in
  let face = S2_cell.face cell in
  let uv = S2_cell.bound_uv cell in
  let two_eps = Float_u.O.(#2.0 * Float_u.epsilon_float ()) in
  let two_eps_ll = S2_latlng.of_radians ~lat:two_eps ~lng:two_eps in
  if Stdlib.( > ) level 0
  then
    let open Float_u.O in
    let u0 = R1_interval.lo (R2_rect.x uv) in
    let u1 = R1_interval.hi (R2_rect.x uv) in
    let v0 = R1_interval.lo (R2_rect.y uv) in
    let v1 = R1_interval.hi (R2_rect.y uv) in
    let u_axis = S2_coords.get_u_axis face in
    let v_axis = S2_coords.get_v_axis face in
    let u_sum = u0 + u1 in
    let v_sum = v0 + v1 in
    let i =
      if Float_u.O.(R3_vector.z u_axis = #0.0)
      then if Float_u.O.(u_sum < #0.0) then 1 else 0
      else if Float_u.O.(u_sum > #0.0)
      then 1
      else 0
    in
    let j =
      if Float_u.O.(R3_vector.z v_axis = #0.0)
      then if Float_u.O.(v_sum < #0.0) then 1 else 0
      else if Float_u.O.(v_sum > #0.0)
      then 1
      else 0
    in
    let pick_u idx = if Stdlib.( = ) idx 0 then u0 else u1 in
    let pick_v idx = if Stdlib.( = ) idx 0 then v0 else v1 in
    let lat_at i j =
      S1_angle.radians
        (S2_latlng.latitude (S2_coords.face_uv_to_xyz face (pick_u i) (pick_v j)))
    in
    let lng_at i j =
      S1_angle.radians
        (S2_latlng.longitude (S2_coords.face_uv_to_xyz face (pick_u i) (pick_v j)))
    in
    let lat =
      R1_interval.from_point_pair
        (lat_at i j)
        (lat_at (Stdlib.( - ) 1 i) (Stdlib.( - ) 1 j))
    in
    let lng =
      S1_interval.from_point_pair
        (lng_at i (Stdlib.( - ) 1 j))
        (lng_at (Stdlib.( - ) 1 i) j)
    in
    polar_closure (expanded #{ lat; lng } two_eps_ll)
  else
    let open Float_u.O in
    let pi4 = Float_u.pi () / #4.0 in
    let pi2 = Float_u.pi () / #2.0 in
    let pi3_4 = #3.0 * Float_u.pi () / #4.0 in
    let bound =
      match face with
      | 0 ->
        #{ lat = R1_interval.create ~lo:(Float_u.neg pi4) ~hi:pi4
         ; lng = S1_interval.create ~lo:(Float_u.neg pi4) ~hi:pi4
         }
      | 1 ->
        #{ lat = R1_interval.create ~lo:(Float_u.neg pi4) ~hi:pi4
         ; lng = S1_interval.create ~lo:pi4 ~hi:pi3_4
         }
      | 2 ->
        #{ lat = R1_interval.create ~lo:pole_min_lat ~hi:pi2; lng = S1_interval.full }
      | 3 ->
        #{ lat = R1_interval.create ~lo:(Float_u.neg pi4) ~hi:pi4
         ; lng = S1_interval.create ~lo:pi3_4 ~hi:(Float_u.neg pi3_4)
         }
      | 4 ->
        #{ lat = R1_interval.create ~lo:(Float_u.neg pi4) ~hi:pi4
         ; lng = S1_interval.create ~lo:(Float_u.neg pi3_4) ~hi:(Float_u.neg pi4)
         }
      | _ ->
        #{ lat = R1_interval.create ~lo:(Float_u.neg pi2) ~hi:(Float_u.neg pole_min_lat)
         ; lng = S1_interval.full
         }
    in
    let one_eps_ll = S2_latlng.of_radians ~lat:(Float_u.epsilon_float ()) ~lng:#0.0 in
    expanded bound one_eps_ll
;;

let[@zero_alloc ignore] contains_cell t cell = contains t (from_cell cell)

(* Mirrors C++ S2LatLngRect::MayIntersect: a cheap, conservative check that
   reduces to "does this rectangle intersect the cell's bounding rectangle?".
   The Go port goes further with edge-crossing tests, but the C++ reference
   library uses this conservative form. *)
let[@zero_alloc ignore] intersects_cell t cell = intersects t (from_cell cell)
let[@zero_alloc ignore] cell_union_bound t = S2_cap.cell_union_bound (cap_bound t)

(* Distance helpers -- these mirror s2edge_distances functions. *)

(* Minimum distance from point p to great-circle segment (a, b). *)
let[@zero_alloc ignore] point_to_segment_distance p a b =
  let open Float_u.O in
  (* Cross product n = a x b (normal to the great circle). *)
  let n = R3_vector.cross a b in
  (* If the great circle degenerates, return the angle to a. *)
  let n2 = R3_vector.norm2 n in
  if n2 = #0.0
  then R3_vector.angle a p
  else (
    (* Project p onto the great circle plane. *)
    let d = R3_vector.dot p n / Float_u.sqrt n2 in
    (* The closest point on the great circle is the projection, but we need to
       check whether it lies within the arc from a to b. *)
    let proj = R3_vector.sub p (R3_vector.mul n (d / n2)) in
    (* Check whether proj is in the interior of the arc.
       The point is in the arc if (a x proj) . n >= 0 and (proj x b) . n >= 0. *)
    let a_cross_proj = R3_vector.cross a proj in
    let proj_cross_b = R3_vector.cross proj b in
    if R3_vector.dot a_cross_proj n >= #0.0 && R3_vector.dot proj_cross_b n >= #0.0
    then (
      (* Closest point is on the arc. Distance = |asin(d / |n|)| *)
      let r = Float_u.abs (Float_u.asin (Float_u.clamp_exn d ~min:(-#1.0) ~max:#1.0)) in
      S1_angle.of_radians r)
    else (
      (* Closest point is one of the endpoints. *)
      let da = S1_angle.radians (R3_vector.angle a p) in
      let db = S1_angle.radians (R3_vector.angle b p) in
      S1_angle.of_radians (Float_u.min da db)))
;;

let[@zero_alloc ignore] distance_to_latlng t ll =
  let open Float_u.O in
  let lng_rad = S1_angle.radians (S2_latlng.lng ll) in
  if S1_interval.contains t.#lng lng_rad
  then (
    let lat_rad = S1_angle.radians (S2_latlng.lat ll) in
    let d =
      Float_u.max
        #0.0
        (Float_u.max (lat_rad - R1_interval.hi t.#lat) (R1_interval.lo t.#lat - lat_rad))
    in
    S1_angle.of_radians d)
  else (
    let i =
      S1_interval.create
        ~lo:(S1_interval.hi t.#lng)
        ~hi:(S1_interval.complement_center t.#lng)
    in
    let rect_lng =
      if S1_interval.contains i lng_rad
      then S1_interval.hi t.#lng
      else S1_interval.lo t.#lng
    in
    let lo_pt =
      S2_latlng.to_point (S2_latlng.of_radians ~lat:(R1_interval.lo t.#lat) ~lng:rect_lng)
    in
    let hi_pt =
      S2_latlng.to_point (S2_latlng.of_radians ~lat:(R1_interval.hi t.#lat) ~lng:rect_lng)
    in
    let p_pt = S2_latlng.to_point ll in
    point_to_segment_distance p_pt lo_pt hi_pt)
;;

let[@zero_alloc ignore] distance t other =
  let open Float_u.O in
  if S1_interval.intersects t.#lng other.#lng
  then
    if R1_interval.intersects t.#lat other.#lat
    then S1_angle.of_radians #0.0
    else (
      let mutable lo_angle = R1_interval.hi t.#lat in
      let mutable hi_angle = R1_interval.lo other.#lat in
      if R1_interval.lo t.#lat > R1_interval.hi other.#lat
      then (
        lo_angle <- R1_interval.hi other.#lat;
        hi_angle <- R1_interval.lo t.#lat);
      S1_angle.of_radians (hi_angle - lo_angle))
  else (
    let lo_hi =
      S1_interval.from_point_pair (S1_interval.lo t.#lng) (S1_interval.hi other.#lng)
    in
    let hi_lo =
      S1_interval.from_point_pair (S1_interval.hi t.#lng) (S1_interval.lo other.#lng)
    in
    let mutable a_lng = S1_interval.hi t.#lng in
    let mutable b_lng = S1_interval.lo other.#lng in
    if S1_interval.length lo_hi < S1_interval.length hi_lo
    then (
      a_lng <- S1_interval.lo t.#lng;
      b_lng <- S1_interval.hi other.#lng);
    let a_lo =
      S2_latlng.to_point (S2_latlng.of_radians ~lat:(R1_interval.lo t.#lat) ~lng:a_lng)
    in
    let a_hi =
      S2_latlng.to_point (S2_latlng.of_radians ~lat:(R1_interval.hi t.#lat) ~lng:a_lng)
    in
    let b_lo =
      S2_latlng.to_point
        (S2_latlng.of_radians ~lat:(R1_interval.lo other.#lat) ~lng:b_lng)
    in
    let b_hi =
      S2_latlng.to_point
        (S2_latlng.of_radians ~lat:(R1_interval.hi other.#lat) ~lng:b_lng)
    in
    let d1 = point_to_segment_distance a_lo b_lo b_hi in
    let d2 = point_to_segment_distance a_hi b_lo b_hi in
    let d3 = point_to_segment_distance b_lo a_lo a_hi in
    let d4 = point_to_segment_distance b_hi a_lo a_hi in
    let min12 = if S1_angle.radians d1 < S1_angle.radians d2 then d1 else d2 in
    let min34 = if S1_angle.radians d3 < S1_angle.radians d4 then d3 else d4 in
    if S1_angle.radians min12 < S1_angle.radians min34 then min12 else min34)
;;

(* Directed Hausdorff distance helpers. *)

(* Max distance from a point b to the segment spanning latitude range a_lat on
   longitude 0, if the max occurs in the interior of a_lat. Otherwise returns
   a negative value. *)
let[@zero_alloc ignore] interior_max_distance a_lat b =
  let open Float_u.O in
  if R1_interval.is_empty a_lat || R3_vector.x b >= #0.0
  then S1_angle.of_radians (-#1.0)
  else (
    let intersection_point =
      R3_vector.normalize
        (R3_vector.create
           ~x:(Float_u.neg (R3_vector.x b))
           ~y:#0.0
           ~z:(Float_u.neg (R3_vector.z b)))
    in
    let int_lat = S1_angle.radians (S2_latlng.latitude intersection_point) in
    if R1_interval.interior_contains a_lat int_lat
    then R3_vector.angle b intersection_point
    else S1_angle.of_radians (-#1.0))
;;

(* Intersection of longitude 0 with the bisector of an edge on longitude lng
   spanning latitude range lat. *)
let[@zero_alloc ignore] bisector_intersection lat_interval lng_val =
  let open Float_u.O in
  let lng_abs = Float_u.abs lng_val in
  let lat_center = R1_interval.center lat_interval in
  let ortho_bisector =
    if lat_center >= #0.0
    then S2_latlng.of_radians ~lat:(lat_center - (Float_u.pi () / #2.0)) ~lng:lng_abs
    else
      S2_latlng.of_radians
        ~lat:(Float_u.neg lat_center - (Float_u.pi () / #2.0))
        ~lng:(lng_abs - Float_u.pi ())
  in
  let ortho_lng = S2_point.of_coords ~x:#0.0 ~y:(-#1.0) ~z:#0.0 in
  S2_point.robust_cross_prod ortho_lng (S2_latlng.to_point ortho_bisector)
;;

let[@zero_alloc ignore] directed_hausdorff_distance_helper lng_diff a b =
  let open Float_u.O in
  if lng_diff = #0.0
  then S1_angle.of_radians (R1_interval.directed_hausdorff_distance a b)
  else (
    let b_lng = lng_diff in
    let b_lo =
      S2_latlng.to_point (S2_latlng.of_radians ~lat:(R1_interval.lo b) ~lng:b_lng)
    in
    let b_hi =
      S2_latlng.to_point (S2_latlng.of_radians ~lat:(R1_interval.hi b) ~lng:b_lng)
    in
    let a_lo =
      S2_latlng.to_point (S2_latlng.of_radians ~lat:(R1_interval.lo a) ~lng:#0.0)
    in
    let a_hi =
      S2_latlng.to_point (S2_latlng.of_radians ~lat:(R1_interval.hi a) ~lng:#0.0)
    in
    let mutable max_distance =
      let d1 = point_to_segment_distance a_lo b_lo b_hi in
      let d2 = point_to_segment_distance a_hi b_lo b_hi in
      if S1_angle.radians d1 > S1_angle.radians d2 then d1 else d2
    in
    if lng_diff <= Float_u.pi () / #2.0
    then (
      (* Case A2 *)
      if R1_interval.contains a #0.0 && R1_interval.contains b #0.0
      then (
        let d = S1_angle.of_radians lng_diff in
        if S1_angle.radians d > S1_angle.radians max_distance then max_distance <- d);
      max_distance)
    else (
      (* Case B2 *)
      let p = bisector_intersection b b_lng in
      let p_lat = S1_angle.radians (S2_latlng.latitude p) in
      if R1_interval.contains a p_lat
      then (
        let d = R3_vector.angle p b_lo in
        if S1_angle.radians d > S1_angle.radians max_distance then max_distance <- d);
      (* Case B3 *)
      if p_lat > R1_interval.lo a
      then (
        let a_clipped =
          R1_interval.create
            ~lo:(R1_interval.lo a)
            ~hi:(Float_u.min p_lat (R1_interval.hi a))
        in
        let d = interior_max_distance a_clipped b_lo in
        if S1_angle.radians d > S1_angle.radians max_distance then max_distance <- d);
      if p_lat < R1_interval.hi a
      then (
        let a_clipped =
          R1_interval.create
            ~lo:(Float_u.max p_lat (R1_interval.lo a))
            ~hi:(R1_interval.hi a)
        in
        let d = interior_max_distance a_clipped b_hi in
        if S1_angle.radians d > S1_angle.radians max_distance then max_distance <- d);
      max_distance))
;;

let[@zero_alloc ignore] directed_hausdorff_distance t other =
  if is_empty t
  then S1_angle.of_radians #0.0
  else if is_empty other
  then S1_angle.of_radians (Float_u.pi ())
  else (
    let lng_diff = S1_interval.directed_hausdorff_distance t.#lng other.#lng in
    directed_hausdorff_distance_helper lng_diff t.#lat other.#lat)
;;

let[@zero_alloc ignore] hausdorff_distance t other =
  let d1 = directed_hausdorff_distance t other in
  let d2 = directed_hausdorff_distance other t in
  if Float_u.O.(S1_angle.radians d1 >= S1_angle.radians d2) then d1 else d2
;;

let[@inline] [@zero_alloc] equal t other =
  R1_interval.equal t.#lat other.#lat && S1_interval.equal t.#lng other.#lng
;;

let[@inline] [@zero_alloc] approx_equal
  ~(max_error : Packed_float_option.Unboxed.t)
  t
  other
  =
  R1_interval.approx_equal ~max_error t.#lat other.#lat
  && S1_interval.approx_equal ~max_error t.#lng other.#lng
;;

let[@inline] [@zero_alloc] approx_equal_latlng ~max_error t other =
  let lat_err = S1_angle.radians (S2_latlng.lat max_error) in
  let lng_err = S1_angle.radians (S2_latlng.lng max_error) in
  R1_interval.approx_equal
    ~max_error:(Packed_float_option.Unboxed.some lat_err)
    t.#lat
    other.#lat
  && S1_interval.approx_equal
       ~max_error:(Packed_float_option.Unboxed.some lng_err)
       t.#lng
       other.#lng
;;
