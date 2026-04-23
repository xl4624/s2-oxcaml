open Core

let[@inline] angle a b c =
  (* Cross products are stable when the two input points are nearly
     (anti-)parallel; the naive cross product loses precision there. *)
  R3_vector.angle (S2_point.robust_cross_prod a b) (S2_point.robust_cross_prod c b)
;;

let[@inline] turn_angle a b c =
  (* Sign must come from the orientation predicate rather than [sign * angle]
     because [a = c] is legal and produces either [-pi] or [pi]. *)
  let ang =
    R3_vector.angle (S2_point.robust_cross_prod a b) (S2_point.robust_cross_prod b c)
  in
  match S2_predicates.robust_sign a b c with
  | Counter_clockwise -> ang
  | Clockwise | Indeterminate -> S1_angle.of_radians (Float_u.neg (S1_angle.radians ang))
;;

let[@inline] girard_area a b c =
  let ab = S2_point.robust_cross_prod a b in
  let bc = S2_point.robust_cross_prod b c in
  let ac = S2_point.robust_cross_prod a c in
  let open Float_u.O in
  let area =
    S1_angle.radians (R3_vector.angle ab ac)
    - S1_angle.radians (R3_vector.angle ab bc)
    + S1_angle.radians (R3_vector.angle bc ac)
  in
  Float_util.max_u #0.0 area
;;

let area a b c =
  (* l'Huilier's theorem:
       tan(E/4) = sqrt(tan(s/2) tan((s-a)/2) tan((s-b)/2) tan((s-c)/2))
     where E is the spherical excess (the triangle area), a/b/c are the side
     lengths, and s is the semiperimeter. Its relative error is roughly
     1e-16 * s / min(s-a, s-b, s-c), which is tiny for non-skinny triangles.
     For long thin triangles the cancellation in (s-a), (s-b), (s-c) dominates
     and we prefer Girard's formula despite its weaker accuracy on small
     triangles. The threshold [dmin < s * 0.1 * (g + 5e-15)] keeps the choice
     conservative by inflating the measured Girard area by its approximate
     error bound. See s2measures.cc:87-147 for the derivation. *)
  let open Float_u.O in
  let sa = S1_angle.radians (S2_point.stable_angle b c) in
  let sb = S1_angle.radians (S2_point.stable_angle c a) in
  let sc = S1_angle.radians (S2_point.stable_angle a b) in
  let s = #0.5 * (sa + sb + sc) in
  let l_huilier () =
    let t =
      Float_u.tan (#0.5 * s)
      * Float_u.tan (#0.5 * (s - sa))
      * Float_u.tan (#0.5 * (s - sb))
      * Float_u.tan (#0.5 * (s - sc))
    in
    #4.0 * Float_u.atan (Float_u.sqrt (Float_util.max_u #0.0 t))
  in
  if s >= #3e-4
  then (
    let s2 = s * s in
    let dmin = s - Float_util.max_u sa (Float_util.max_u sb sc) in
    if dmin < #1e-2 * s * s2 * s2
    then (
      let g = girard_area a b c in
      if dmin < s * #0.1 * (g + #5e-15) then g else l_huilier ())
    else l_huilier ())
  else l_huilier ()
;;

let signed_area a b c =
  let open Float_u.O in
  let sign = of_int (S2_predicates.Direction.to_int (S2_predicates.robust_sign a b c)) in
  sign * area a b c
;;
