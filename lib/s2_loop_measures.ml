open Core

type loop_order =
  { first : int
  ; dir : int
  }
[@@deriving sexp_of]

let perimeter (loop : S2_point.t array) =
  let n = Array.length loop in
  if n <= 1
  then S1_angle.zero
  else (
    let mutable sum = #0.0 in
    for i = 0 to n - 1 do
      let j = if i + 1 = n then 0 else i + 1 in
      let angle = S1_angle.radians (S2_point.distance loop.(i) loop.(j)) in
      sum <- Float_u.add sum angle
    done;
    S1_angle.of_radians sum)
;;

(* [loop_at loop i] returns the vertex at cyclic position [i], accepting
   indices used by the canonical-order comparator (which may be negative by
   one step or up to [2*n - 1]). *)
let[@inline] loop_at (loop : S2_point.t array) i =
  let n = Array.length loop in
  let j = i mod n in
  let j = if j < 0 then j + n else j in
  loop.(j)
;;

(* Maximum edge length (in radians) considered numerically stable for the
   triangle-fan decomposition. Matches C++ s2loop_measures.cc. *)
let k_max_length = Float.(pi - 1e-5)

(* Compute the oriented surface integral of [f_tri] over the loop interior,
   accumulating into an R3 vector via [add]. Mirrors
   [internal::GetSurfaceIntegral] from the C++ header. Specialised to an
   unboxed R3 accumulator so we can hold the running sum in [let mutable]. *)
let surface_integral_r3 (loop : S2_point.t array) ~f_tri =
  let n = Array.length loop in
  if n < 3
  then R3_vector.zero
  else (
    let mutable sum = R3_vector.zero in
    let mutable origin = loop.(0) in
    let mutable i = 1 in
    while i + 1 < n do
      let vi = loop.(i) in
      let vi1 = loop.(i + 1) in
      if Float.( > )
           (Float_u.to_float (S1_angle.radians (S2_point.distance vi1 origin)))
           k_max_length
      then (
        let old_origin = origin in
        if S2_point.equal origin loop.(0)
        then origin <- R3_vector.normalize (S2_point.robust_cross_prod loop.(0) vi)
        else if Float.( < )
                  (Float_u.to_float (S1_angle.radians (S2_point.distance vi loop.(0))))
                  k_max_length
        then origin <- loop.(0)
        else (
          origin <- R3_vector.cross loop.(0) old_origin;
          sum <- R3_vector.add sum (f_tri loop.(0) old_origin origin));
        sum <- R3_vector.add sum (f_tri old_origin vi origin));
      sum <- R3_vector.add sum (f_tri origin vi vi1);
      i <- i + 1
    done;
    if not (S2_point.equal origin loop.(0))
    then sum <- R3_vector.add sum (f_tri origin loop.(n - 1) loop.(0));
    sum)
;;

(* Kahan-compensated surface integral specialised to [float#] accumulators,
   matching [GetSurfaceIntegralKahan]. *)
let surface_integral_kahan_f (loop : S2_point.t array) ~f_tri =
  let n = Array.length loop in
  if n < 3
  then #0.0
  else (
    let mutable sum = #0.0 in
    let mutable err = #0.0 in
    let mutable origin = loop.(0) in
    let mutable i = 1 in
    while i + 1 < n do
      let vi = loop.(i) in
      let vi1 = loop.(i + 1) in
      if Float.( > )
           (Float_u.to_float (S1_angle.radians (S2_point.distance vi1 origin)))
           k_max_length
      then (
        let old_origin = origin in
        if S2_point.equal origin loop.(0)
        then origin <- R3_vector.normalize (S2_point.robust_cross_prod loop.(0) vi)
        else if Float.( < )
                  (Float_u.to_float (S1_angle.radians (S2_point.distance vi loop.(0))))
                  k_max_length
        then origin <- loop.(0)
        else (
          origin <- R3_vector.cross loop.(0) old_origin;
          let value = f_tri loop.(0) old_origin origin in
          let tmp1 = Float_u.sub value err in
          let tmp2 = Float_u.add sum tmp1 in
          err <- Float_u.sub (Float_u.sub tmp2 sum) tmp1;
          sum <- tmp2);
        let value = f_tri old_origin vi origin in
        let tmp1 = Float_u.sub value err in
        let tmp2 = Float_u.add sum tmp1 in
        err <- Float_u.sub (Float_u.sub tmp2 sum) tmp1;
        sum <- tmp2);
      let value = f_tri origin vi vi1 in
      let tmp1 = Float_u.sub value err in
      let tmp2 = Float_u.add sum tmp1 in
      err <- Float_u.sub (Float_u.sub tmp2 sum) tmp1;
      sum <- tmp2;
      i <- i + 1
    done;
    if not (S2_point.equal origin loop.(0))
    then (
      let value = f_tri origin loop.(n - 1) loop.(0) in
      let tmp1 = Float_u.sub value err in
      let tmp2 = Float_u.add sum tmp1 in
      err <- Float_u.sub (Float_u.sub tmp2 sum) tmp1;
      sum <- tmp2);
    sum)
;;

let prune_degeneracies (loop : S2_point.t array) =
  let n = Array.length loop in
  if n = 0
  then [||]
  else (
    let buf = Array.create ~len:n R3_vector.zero in
    let mutable size = 0 in
    for i = 0 to n - 1 do
      let v = loop.(i) in
      let mutable keep = true in
      if size > 0
      then
        if S2_point.equal v buf.(size - 1)
        then (* AA -> A *)
          keep <- false
        else if size >= 2 && S2_point.equal v buf.(size - 2)
        then (
          (* ABA -> A *)
          size <- size - 1;
          keep <- false);
      if keep
      then (
        buf.(size) <- v;
        size <- size + 1)
    done;
    if size >= 2 && S2_point.equal buf.(0) buf.(size - 1) then size <- size - 1;
    if size < 3
    then [||]
    else (
      (* Strip any ABA that wraps the boundary: if the loop begins with BA
         and ends with A, or begins with A and ends with AB, remove both
         ends. Repeat as long as possible. *)
      let mutable k = 0 in
      let mutable continue = true in
      while continue do
        let lhs1 = buf.(k + 1) in
        let rhs1 = buf.(size - 1 - k) in
        let lhs2 = buf.(k) in
        let rhs2 = buf.(size - 2 - k) in
        if S2_point.equal lhs1 rhs1 || S2_point.equal lhs2 rhs2
        then k <- k + 1
        else continue <- false
      done;
      let len = size - (2 * k) in
      let out = Array.create ~len R3_vector.zero in
      for j = 0 to len - 1 do
        out.(j) <- buf.(k + j)
      done;
      out))
;;

let curvature_max_error (loop : S2_point.t array) =
  let open Float_u.O in
  #11.25 * Float_u.epsilon_float () * Float_u.of_int (Array.length loop)
;;

(* Returns [true] iff [order1] is strictly less than [order2] under the
   lexicographic vertex-sequence comparison used by [canonical_loop_order]. *)
let is_order_less (loop : S2_point.t array) ~order1 ~order2 =
  if order1.first = order2.first && order1.dir = order2.dir
  then false
  else (
    let n = Array.length loop in
    let mutable i1 = order1.first in
    let mutable i2 = order2.first in
    let mutable result = false in
    let mutable decided = false in
    let mutable remaining = n - 1 in
    while (not decided) && remaining > 0 do
      i1 <- i1 + order1.dir;
      i2 <- i2 + order2.dir;
      let c = S2_point.compare (loop_at loop i1) (loop_at loop i2) in
      if c < 0
      then (
        result <- true;
        decided <- true)
      else if c > 0
      then (
        result <- false;
        decided <- true);
      remaining <- remaining - 1
    done;
    result)
;;

let rec canonical_loop_order (loop : S2_point.t array) =
  let n = Array.length loop in
  if n = 0
  then { first = 0; dir = 1 }
  else (
    (* Collect all indices where the minimum vertex occurs. *)
    let mutable min_indices = [ 0 ] in
    let mutable current_min = loop.(0) in
    for i = 1 to n - 1 do
      let v = loop.(i) in
      let c = S2_point.compare v current_min in
      if c < 0
      then (
        min_indices <- [ i ];
        current_min <- v)
      else if c = 0
      then min_indices <- i :: min_indices
    done;
    let min_indices = List.rev min_indices in
    let mutable min_order = { first = List.hd_exn min_indices; dir = 1 } in
    let mutable rest = min_indices in
    while not (List.is_empty rest) do
      let idx = List.hd_exn rest in
      rest <- List.tl_exn rest;
      let order1 = { first = idx; dir = 1 } in
      let order2 = { first = idx + n; dir = -1 } in
      if is_order_less loop ~order1 ~order2:min_order then min_order <- order1;
      if is_order_less loop ~order1:order2 ~order2:min_order then min_order <- order2
    done;
    min_order)

and curvature (loop : S2_point.t array) =
  (* By convention, an empty loop is the full sphere. *)
  if Array.length loop = 0
  then Float_u.O.(-#2.0 * Float_u.pi ())
  else (
    let loop = prune_degeneracies loop in
    let n = Array.length loop in
    if n = 0
    then (* Completely degenerate. *)
      Float_u.O.(#2.0 * Float_u.pi ())
    else (
      let order = canonical_loop_order loop in
      let dir = order.dir in
      let first = order.first in
      let turn i =
        let prev = loop_at loop (i - dir) in
        let here = loop_at loop i in
        let next = loop_at loop (i + dir) in
        S1_angle.radians (S2_measures.turn_angle prev here next)
      in
      let mutable sum = turn first in
      let mutable compensation = #0.0 in
      let mutable i = first in
      let mutable remaining = n - 1 in
      while Int.( > ) remaining 0 do
        i <- i + dir;
        let angle = Float_u.add (turn i) compensation in
        let old_sum = sum in
        sum <- Float_u.add sum angle;
        compensation <- Float_u.add (Float_u.sub old_sum sum) angle;
        remaining <- remaining - 1
      done;
      let open Float_u.O in
      let k_max_curvature = (#2.0 * Float_u.pi ()) - (#4.0 * Float_u.epsilon_float ()) in
      let total = sum + compensation in
      let dir_f = Float_u.of_int dir in
      Float_u.max (-k_max_curvature) (Float_u.min k_max_curvature (dir_f * total))))
;;

let is_normalized loop =
  let open Float_u.O in
  curvature loop >= -curvature_max_error loop
;;

let signed_area loop =
  let open Float_u.O in
  (* Compute the signed sum over spherical triangles (Kahan-compensated),
     then double-check tiny magnitudes against [curvature]. *)
  let area =
    surface_integral_kahan_f loop ~f_tri:(fun a b c -> S2_measures.signed_area a b c)
  in
  let max_error = curvature_max_error loop in
  let area = Float_util.ieee_remainder_u area (#4.0 * Float_u.pi ()) in
  let area = if area = -#2.0 * Float_u.pi () then #2.0 * Float_u.pi () else area in
  if Float_u.abs area <= max_error
  then (
    let c = curvature loop in
    if c = #2.0 * Float_u.pi ()
    then #0.0
    else if area <= #0.0 && c > #0.0
    then Float_u.min_positive_normal_value ()
    else if area >= #0.0 && c < #0.0
    then -Float_u.min_positive_normal_value ()
    else area)
  else area
;;

let area loop =
  let a = signed_area loop in
  let open Float_u.O in
  if a < #0.0 then a + (#4.0 * Float_u.pi ()) else a
;;

let approx_area loop =
  let open Float_u.O in
  (#2.0 * Float_u.pi ()) - curvature loop
;;

let centroid loop =
  (* Non-Kahan variant because R3_vector does not fit naturally into scalar
     Kahan summation. The C++ [GetCentroid] also uses the uncompensated
     variant. *)
  surface_integral_r3 loop ~f_tri:(fun a b c -> S2_centroids.true_centroid a b c)
;;
