open Core

type t =
  #{ lat : R1_interval.t
   ; lng : S1_interval.t
   }
[@@deriving sexp_of, unboxed_option { sentinel = true }]

let full_lat =
  let open Float_u.O in
  R1_interval.create ~lo:(Float_u.neg (Float_u.pi / #2.0)) ~hi:(Float_u.pi / #2.0)
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
  (* Select CCW vertex (lower-left, lower-right, upper-right, upper-left) by deriving the
     (i, j) bits from [k]: i = (k >> 1) ^ 1, j = i ^ (k & 1). *)
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
  Float_u.abs (R1_interval.lo t.#lat) <= Float_u.pi / #2.0
  && Float_u.abs (R1_interval.hi t.#lat) <= Float_u.pi / #2.0
  && S1_interval.is_valid t.#lng
  && Bool.equal (R1_interval.is_empty t.#lat) (S1_interval.is_empty t.#lng)
;;

let[@zero_alloc] area t =
  if is_empty t
  then #0.0
  else
    let open Float_u.O in
    S1_interval.length t.#lng
    * Float_u.abs
        (Float_u.sin (R1_interval.hi t.#lat) - Float_u.sin (R1_interval.lo t.#lat))
;;

let[@zero_alloc] centroid t =
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
  if R1_interval.lo t.#lat = Float_u.neg (Float_u.pi / #2.0)
     || R1_interval.hi t.#lat = Float_u.pi / #2.0
  then #{ lat = t.#lat; lng = S1_interval.full }
  else t
;;

let of_center_size ~center ~size =
  let half = S2_latlng.mul size #0.5 in
  expanded (of_point center) half
;;

let[@inline] [@zero_alloc] cap_bound t =
  if is_empty t
  then S2_cap.empty
  else
    let open Float_u.O in
    let mutable pole_z = #1.0 in
    let mutable pole_angle = (Float_u.pi / #2.0) - R1_interval.lo t.#lat in
    if R1_interval.lo t.#lat + R1_interval.hi t.#lat < #0.0
    then (
      (* The latitudes are skewed south; centering at the south pole yields the smaller
         cap. *)
      pole_z <- -#1.0;
      pole_angle <- (Float_u.pi / #2.0) + R1_interval.hi t.#lat);
    let pole_angle_with_err = (#1.0 + (#2.0 * Float_u.epsilon_float)) * pole_angle in
    let pole_cap =
      S2_cap.of_center_angle
        (S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:pole_z)
        (S1_angle.of_radians pole_angle_with_err)
    in
    (* For longitude spans <= 180 degrees the tightest "mid-cap" is attained at one of the
       four rectangle vertices, so we seed with the center and add each vertex. *)
    if S1_interval.length t.#lng <= Float_u.pi
    then (
      let mutable mid_cap = S2_cap.of_point (S2_latlng.to_point (center t)) in
      for k = 0 to 3 do
        mid_cap <- S2_cap.add_point mid_cap (S2_latlng.to_point (vertex t k))
      done;
      if S2_cap.height mid_cap < S2_cap.height pole_cap then mid_cap else pole_cap)
    else pole_cap
;;

let[@inline] [@zero_alloc] rect_bound t = t

let[@inline] [@zero_alloc] from_cap c =
  let open Float_u.O in
  if S2_cap.is_empty c
  then empty
  else (
    let center_ll = S2_latlng.of_point (S2_cap.center c) in
    let cap_angle = S1_angle.radians (S2_cap.radius_angle c) in
    let mutable all_longitudes = false in
    let mutable lat_lo = S1_angle.radians (S2_latlng.lat center_ll) - cap_angle in
    let mutable lat_hi = S1_angle.radians (S2_latlng.lat center_ll) + cap_angle in
    let mutable lng_lo = Float_u.neg Float_u.pi in
    let mutable lng_hi = Float_u.pi in
    if lat_lo <= Float_u.neg (Float_u.pi / #2.0)
    then (
      lat_lo <- Float_u.neg (Float_u.pi / #2.0);
      all_longitudes <- true);
    if lat_hi >= Float_u.pi / #2.0
    then (
      lat_hi <- Float_u.pi / #2.0;
      all_longitudes <- true);
    if not all_longitudes
    then (
      let sin_a = S1_chord_angle.sin (S2_cap.radius_chord c) in
      let sin_c = Float_u.cos (S1_angle.radians (S2_latlng.lat center_ll)) in
      if Float_u.O.(sin_a <= sin_c)
      then (
        let angle_a = Float_u.asin (sin_a / sin_c) in
        let lng = S1_angle.radians (S2_latlng.lng center_ll) in
        lng_lo <- Float_util.ieee_remainder_u (lng - angle_a) (#2.0 * Float_u.pi);
        lng_hi <- Float_util.ieee_remainder_u (lng + angle_a) (#2.0 * Float_u.pi)));
    #{ lat = R1_interval.create ~lo:lat_lo ~hi:lat_hi
     ; lng = S1_interval.create ~lo:lng_lo ~hi:lng_hi
     })
;;

(* For a non-negative [distance], builds the smallest cap centered at each rectangle
   vertex with that spherical radius, takes the lat/lng bound of each, and unions them
   with the original rectangle. Including the original is necessary for very large
   rectangles, where the four vertex caps may not yet cover the interior.

   For a negative [distance], shrinks the latitude interval by [|distance|] on each side
   unless the corresponding side is on the pole and the longitude interval is full (in
   which case the rectangle has no boundary at that pole). The longitude interval is then
   shrunk by the maximum longitude margin spanned by a cap of radius [|distance|] at the
   shrunk latitude where it is widest. Either reduction may collapse the rectangle to
   empty. *)
let[@zero_alloc] expanded_by_distance t distance =
  if Stdlib.( >= ) (S1_angle.compare distance S1_angle.zero) 0
  then (
    let radius = S1_chord_angle.of_angle distance in
    let mutable r = t in
    for k = 0 to 3 do
      let cap = S2_cap.of_center_chord_angle (S2_latlng.to_point (vertex t k)) radius in
      r <- union r (from_cap cap)
    done;
    r)
  else
    let open Float_u.O in
    let abs_dist = Float_u.neg (S1_angle.radians distance) in
    let lat_full_lo = R1_interval.lo full_lat in
    let lat_full_hi = R1_interval.hi full_lat in
    let lng_full = S1_interval.is_full t.#lng in
    let lat_lo_in = R1_interval.lo t.#lat in
    let lat_hi_in = R1_interval.hi t.#lat in
    let new_lat_lo =
      if lat_lo_in <= lat_full_lo && lng_full then lat_full_lo else lat_lo_in + abs_dist
    in
    let new_lat_hi =
      if lat_hi_in >= lat_full_hi && lng_full then lat_full_hi else lat_hi_in - abs_dist
    in
    let lat_result = R1_interval.create ~lo:new_lat_lo ~hi:new_lat_hi in
    if R1_interval.is_empty lat_result
    then empty
    else (
      let max_abs_lat = Float_u.max (Float_u.neg new_lat_lo) new_lat_hi in
      let sin_a = Float_u.sin abs_dist in
      let sin_c = Float_u.cos max_abs_lat in
      let max_lng_margin =
        if sin_a < sin_c then Float_u.asin (sin_a / sin_c) else Float_u.pi / #2.0
      in
      let lng_result = S1_interval.expanded t.#lng (Float_u.neg max_lng_margin) in
      if S1_interval.is_empty lng_result
      then empty
      else #{ lat = lat_result; lng = lng_result })
;;

let pole_min_lat =
  let open Float_u.O in
  Float_u.asin (Float_u.sqrt (#1.0 / #3.0)) - (#0.5 * Float_u.epsilon_float)
;;

(* For levels > 0 we build the bound from the four corners of the cell's (u, v) rectangle,
   picking the pair of corners whose lat/lng extrema are extremal. For level 0 each face
   has a canonical hard-coded bound, extended to near-polar faces using [pole_min_lat]
   (which accounts for rounding in [uv_to_xyz]). The result is expanded by two units of
   epsilon to swallow projection rounding and then run through [polar_closure]. *)
let[@zero_alloc] from_cell cell =
  let level = S2_cell.level cell in
  let face = S2_cell.face cell in
  let uv = S2_cell.bound_uv cell in
  let two_eps = Float_u.O.(#2.0 * Float_u.epsilon_float) in
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
    let u_i = if Int.equal i 0 then u0 else u1 in
    let u_ni = if Int.equal i 0 then u1 else u0 in
    let v_j = if Int.equal j 0 then v0 else v1 in
    let v_nj = if Int.equal j 0 then v1 else v0 in
    let lat =
      R1_interval.from_point_pair
        (S1_angle.radians (S2_latlng.latitude (S2_coords.face_uv_to_xyz face u_i v_j)))
        (S1_angle.radians (S2_latlng.latitude (S2_coords.face_uv_to_xyz face u_ni v_nj)))
    in
    let lng =
      S1_interval.from_point_pair
        (S1_angle.radians (S2_latlng.longitude (S2_coords.face_uv_to_xyz face u_i v_nj)))
        (S1_angle.radians (S2_latlng.longitude (S2_coords.face_uv_to_xyz face u_ni v_j)))
    in
    polar_closure (expanded #{ lat; lng } two_eps_ll)
  else
    let open Float_u.O in
    let pi4 = Float_u.pi / #4.0 in
    let pi2 = Float_u.pi / #2.0 in
    let pi3_4 = #3.0 * Float_u.pi / #4.0 in
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
    let one_eps_ll = S2_latlng.of_radians ~lat:Float_u.epsilon_float ~lng:#0.0 in
    expanded bound one_eps_ll
;;

let[@zero_alloc] contains_cell t cell = contains t (from_cell cell)

(* Edges of constant longitude are great-circle arcs (geodesics), so a single
   [crossing_sign] against the segment from [(lat.lo, lng)] to [(lat.hi, lng)] is
   sufficient. *)
let[@zero_alloc] intersects_lng_edge a b ~lat ~lng =
  let lo_pt = S2_latlng.to_point (S2_latlng.of_radians ~lat:(R1_interval.lo lat) ~lng) in
  let hi_pt = S2_latlng.to_point (S2_latlng.of_radians ~lat:(R1_interval.hi lat) ~lng) in
  Stdlib.( > ) (S2_edge_crossings.crossing_sign a b lo_pt hi_pt) 0
;;

(* Edges of constant latitude are not great circles, so we work in a frame where the great
   circle through (a, b) is the x-y plane. The two intersection candidates with the
   latitude line lie at angles +/- theta from the x axis, where cos theta = sin(lat) /
   x.z. For each candidate we check that it falls within both the AB edge's parametric
   range and the longitude interval. *)
let[@zero_alloc] intersects_lat_edge a b ~lat ~lng =
  let open Float_u.O in
  let z0 = R3_vector.normalize (S2_point.robust_cross_prod a b) in
  let z = if R3_vector.z z0 < #0.0 then R3_vector.neg z0 else z0 in
  let north = S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0 in
  let y = R3_vector.normalize (S2_point.robust_cross_prod z north) in
  let x = R3_vector.cross y z in
  let xz = R3_vector.z x in
  let sin_lat = Float_u.sin lat in
  if Float_u.abs sin_lat >= xz
  then false
  else (
    let cos_theta = sin_lat / xz in
    let sin_theta = Float_u.sqrt (#1.0 - (cos_theta * cos_theta)) in
    let theta = Float_u.atan2 sin_theta cos_theta in
    let ab_theta =
      S1_interval.from_point_pair
        (Float_u.atan2 (R3_vector.dot a y) (R3_vector.dot a x))
        (Float_u.atan2 (R3_vector.dot b y) (R3_vector.dot b x))
    in
    let mutable result = false in
    if S1_interval.contains ab_theta theta
    then (
      let isect = R3_vector.add (R3_vector.mul x cos_theta) (R3_vector.mul y sin_theta) in
      if S1_interval.contains lng (Float_u.atan2 (R3_vector.y isect) (R3_vector.x isect))
      then result <- true);
    if (not result) && S1_interval.contains ab_theta (Float_u.neg theta)
    then (
      let isect = R3_vector.sub (R3_vector.mul x cos_theta) (R3_vector.mul y sin_theta) in
      if S1_interval.contains lng (Float_u.atan2 (R3_vector.y isect) (R3_vector.x isect))
      then result <- true);
    result)
;;

let[@zero_alloc] boundary_intersects t v0 v1 =
  let open Float_u.O in
  if is_empty t
  then false
  else (
    let mutable hit = false in
    if not (S1_interval.is_full t.#lng)
    then
      if intersects_lng_edge v0 v1 ~lat:t.#lat ~lng:(S1_interval.lo t.#lng)
      then hit <- true
      else if intersects_lng_edge v0 v1 ~lat:t.#lat ~lng:(S1_interval.hi t.#lng)
      then hit <- true;
    if (not hit)
       && R1_interval.lo t.#lat <> Float_u.neg (Float_u.pi / #2.0)
       && intersects_lat_edge v0 v1 ~lat:(R1_interval.lo t.#lat) ~lng:t.#lng
    then hit <- true;
    if (not hit)
       && R1_interval.hi t.#lat <> Float_u.pi / #2.0
       && intersects_lat_edge v0 v1 ~lat:(R1_interval.hi t.#lat) ~lng:t.#lng
    then hit <- true;
    hit)
;;

(* Conservative rect-vs-cell test used by region operations: returns [true] whenever the
   rectangle could possibly intersect [cell]. False positives are allowed (cells whose
   spherical geometry is disjoint from [t] but whose lat/lng bound overlaps [t]); false
   negatives are not. Kept cheap because region coverers subdivide on every [true]. *)
let[@zero_alloc] intersects_cell t cell = intersects t (from_cell cell)

(* Exact rect-vs-cell intersection. Disposes first of containment in either direction
   (which would otherwise be missed by the boundary-only edge tests below); then runs the
   cheap [intersects_cell] rejection on the cell's lat/lng bound; then handles vertex
   containment in either direction; then tests each cell edge against each non-degenerate
   rectangle boundary edge.

   Latitude-longitude rectangles do not have straight edges -- the latitude edges are
   curves, and at least one of them is concave -- so we use [intersects_lat_edge] /
   [intersects_lng_edge] rather than a generic geodesic crossing for those boundaries. *)
let[@zero_alloc ignore] intersects_s2_cell t cell =
  if is_empty t
  then false
  else if contains_point t (S2_cell.center_raw cell)
  then true
  else if S2_cell.contains_point cell (S2_latlng.to_point (center t))
  then true
  else if not (intersects t (from_cell cell))
  then false
  else (
    let cell_v0 = S2_cell.vertex cell 0 in
    let cell_v1 = S2_cell.vertex cell 1 in
    let cell_v2 = S2_cell.vertex cell 2 in
    let cell_v3 = S2_cell.vertex cell 3 in
    let cell_ll0 = S2_latlng.of_point cell_v0 in
    let cell_ll1 = S2_latlng.of_point cell_v1 in
    let cell_ll2 = S2_latlng.of_point cell_v2 in
    let cell_ll3 = S2_latlng.of_point cell_v3 in
    let corner_hit i ll =
      contains_latlng t ll
      || S2_cell.contains_point cell (S2_latlng.to_point (vertex t i))
    in
    if corner_hit 0 cell_ll0
       || corner_hit 1 cell_ll1
       || corner_hit 2 cell_ll2
       || corner_hit 3 cell_ll3
    then true
    else (
      let edge_hit a b ll_a ll_b =
        let edge_lng =
          S1_interval.from_point_pair
            (S1_angle.radians (S2_latlng.lng ll_a))
            (S1_angle.radians (S2_latlng.lng ll_b))
        in
        if not (S1_interval.intersects t.#lng edge_lng)
        then false
        else if S1_interval.contains edge_lng (S1_interval.lo t.#lng)
                && intersects_lng_edge a b ~lat:t.#lat ~lng:(S1_interval.lo t.#lng)
        then true
        else if S1_interval.contains edge_lng (S1_interval.hi t.#lng)
                && intersects_lng_edge a b ~lat:t.#lat ~lng:(S1_interval.hi t.#lng)
        then true
        else if intersects_lat_edge a b ~lat:(R1_interval.lo t.#lat) ~lng:t.#lng
        then true
        else intersects_lat_edge a b ~lat:(R1_interval.hi t.#lat) ~lng:t.#lng
      in
      edge_hit cell_v0 cell_v1 cell_ll0 cell_ll1
      || edge_hit cell_v1 cell_v2 cell_ll1 cell_ll2
      || edge_hit cell_v2 cell_v3 cell_ll2 cell_ll3
      || edge_hit cell_v3 cell_v0 cell_ll3 cell_ll0))
;;

let[@zero_alloc ignore] cell_union_bound t = S2_cap.cell_union_bound (cap_bound t)

(* Distance helpers used by [distance] and [hausdorff_distance] below. *)

(* Minimum distance from a point to the great-circle arc (a, b). If the foot of the
   perpendicular is inside the arc we return |asin(d/|n|)|; otherwise the closer endpoint
   wins. *)
let[@zero_alloc] point_to_segment_distance p a b =
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
    (* The closest point on the great circle is the projection, but we need to check
       whether it lies within the arc from a to b. *)
    let proj = R3_vector.sub p (R3_vector.mul n (d / n2)) in
    (* Check whether proj is in the interior of the arc. The point is in the arc if (a x
       proj) . n >= 0 and (proj x b) . n >= 0. *)
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
      S1_angle.of_radians (Float_util.min_u da db)))
;;

let[@zero_alloc] distance_to_latlng t ll =
  let open Float_u.O in
  let lng_rad = S1_angle.radians (S2_latlng.lng ll) in
  if S1_interval.contains t.#lng lng_rad
  then (
    let lat_rad = S1_angle.radians (S2_latlng.lat ll) in
    let d =
      Float_u.max
        #0.0
        (Float_util.max_u
           (lat_rad - R1_interval.hi t.#lat)
           (R1_interval.lo t.#lat - lat_rad))
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

let[@zero_alloc] distance t other =
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

(* Max distance from a point b to the segment spanning latitude range a_lat on longitude
   0, if the max occurs in the interior of a_lat. Otherwise returns a negative value. *)
let[@zero_alloc] interior_max_distance a_lat b =
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

(* Intersection of longitude 0 with the bisector of an edge on longitude lng spanning
   latitude range lat. *)
let[@zero_alloc] bisector_intersection lat_interval lng_val =
  let open Float_u.O in
  let lng_abs = Float_u.abs lng_val in
  let lat_center = R1_interval.center lat_interval in
  let ortho_bisector =
    if lat_center >= #0.0
    then S2_latlng.of_radians ~lat:(lat_center - (Float_u.pi / #2.0)) ~lng:lng_abs
    else
      S2_latlng.of_radians
        ~lat:(Float_u.neg lat_center - (Float_u.pi / #2.0))
        ~lng:(lng_abs - Float_u.pi)
  in
  let ortho_lng = S2_point.of_coords ~x:#0.0 ~y:(-#1.0) ~z:#0.0 in
  S2_point.robust_cross_prod ortho_lng (S2_latlng.to_point ortho_bisector)
;;

let[@zero_alloc] directed_hausdorff_distance_helper lng_diff a b =
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
    if lng_diff <= Float_u.pi / #2.0
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
            ~hi:(Float_util.min_u p_lat (R1_interval.hi a))
        in
        let d = interior_max_distance a_clipped b_lo in
        if S1_angle.radians d > S1_angle.radians max_distance then max_distance <- d);
      if p_lat < R1_interval.hi a
      then (
        let a_clipped =
          R1_interval.create
            ~lo:(Float_util.max_u p_lat (R1_interval.lo a))
            ~hi:(R1_interval.hi a)
        in
        let d = interior_max_distance a_clipped b_hi in
        if S1_angle.radians d > S1_angle.radians max_distance then max_distance <- d);
      max_distance))
;;

let[@zero_alloc] directed_hausdorff_distance t other =
  if is_empty t
  then S1_angle.of_radians #0.0
  else if is_empty other
  then S1_angle.of_radians Float_u.pi
  else (
    let lng_diff = S1_interval.directed_hausdorff_distance t.#lng other.#lng in
    directed_hausdorff_distance_helper lng_diff t.#lat other.#lat)
;;

let[@zero_alloc] hausdorff_distance t other =
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

(* TODO: port Encode and Decode from s2latlng_rect.cc. *)
