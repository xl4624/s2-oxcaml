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
   indices used by the canonical-order comparator which can step outside
   [0, n-1] by a single direction unit (as low as [-1] and as high as
   [2*n - 1]). *)
let[@inline] loop_at (loop : S2_point.t array) i =
  let n = Array.length loop in
  let j = i mod n in
  let j = if j < 0 then j + n else j in
  loop.(j)
;;

(* Upper bound on the edge length (in radians) that we treat as numerically
   stable when decomposing the loop interior into a triangle fan. The
   exact value is fairly arbitrary; it matches s2loop_measures.h:294 and is
   conservative enough for every [f_tri] used in this file. *)
let k_max_length = Float.(pi - 1e-5)

(* Compute the oriented surface integral of [f_tri] over the loop interior,
   accumulating into an R3 vector. The running sum is held in three [float#]
   components so no intermediate [R3_vector.t] is allocated. Re-origination
   rules match s2loop_measures.h:258-362: when the leading-edge length would
   exceed [k_max_length], the fan origin is moved to [RobustCrossProd(v0, vi)]
   (or [v0 x old_origin] when [old_origin] was already displaced) and extra
   triangles are emitted to keep the running sum consistent. *)
let surface_integral_r3 (loop : S2_point.t array) ~f_tri =
  let n = Array.length loop in
  if n < 3
  then R3_vector.zero
  else (
    let mutable sum_x = #0.0 in
    let mutable sum_y = #0.0 in
    let mutable sum_z = #0.0 in
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
          let v = f_tri loop.(0) old_origin origin in
          sum_x <- Float_u.add sum_x (R3_vector.x v);
          sum_y <- Float_u.add sum_y (R3_vector.y v);
          sum_z <- Float_u.add sum_z (R3_vector.z v));
        let v = f_tri old_origin vi origin in
        sum_x <- Float_u.add sum_x (R3_vector.x v);
        sum_y <- Float_u.add sum_y (R3_vector.y v);
        sum_z <- Float_u.add sum_z (R3_vector.z v));
      let v = f_tri origin vi vi1 in
      sum_x <- Float_u.add sum_x (R3_vector.x v);
      sum_y <- Float_u.add sum_y (R3_vector.y v);
      sum_z <- Float_u.add sum_z (R3_vector.z v);
      i <- i + 1
    done;
    if not (S2_point.equal origin loop.(0))
    then (
      let v = f_tri origin loop.(n - 1) loop.(0) in
      sum_x <- Float_u.add sum_x (R3_vector.x v);
      sum_y <- Float_u.add sum_y (R3_vector.y v);
      sum_z <- Float_u.add sum_z (R3_vector.z v));
    R3_vector.create ~x:sum_x ~y:sum_y ~z:sum_z)
;;

(* Kahan-compensated surface integral specialised to [float#] accumulators.
   The control flow mirrors [surface_integral_r3] exactly; it is duplicated
   rather than parameterised because the scalar accumulator lets us hold
   [sum] and [err] in unboxed registers without a shared boxed state. *)
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
    (* Two-pass pruning. The first pass runs a stack-like scan that collapses
       any adjacent [AA] or [ABA] pattern as soon as it appears: [AA -> A]
       drops the duplicate tail, while [ABA -> A] pops the middle vertex and
       skips the new one. Post-pass we still need to handle degeneracies
       that wrap across the boundary between the end and start of the buffer,
       which is what the second [while] loop does. *)
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
      (* Strip any [ABA] that wraps the boundary: if the loop begins with
         [BA...] and ends with [...A], or begins with [A...] and ends with
         [...AB], removing both ends is equivalent to the non-wrapping
         [ABA -> A] rule. Keep peeling as long as it applies. *)
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

(* Returns [true] iff the vertex sequence generated by [order1] is
   lexicographically strictly less than the one generated by [order2]. Used
   by [canonical_loop_order] to pick a tie-breaker among loop orders that
   start at the same minimum vertex. *)
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
    (* Collect every index at which the minimum vertex occurs, then for each
       candidate compare the forward order [{ first = idx; dir = 1 }] and
       the reverse order [{ first = idx + n; dir = -1 }] against the best
       known order. This yields the lexicographically smallest traversal
       among all rotations and reversals. *)
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
  (* By convention the empty array means the full sphere, whose curvature
     is [-2 * pi]. A fully-degenerate non-empty loop prunes to the empty
     array, and the contract is that it should still report [+2 * pi];
     we special-case this before pruning. *)
  if Array.length loop = 0
  then Float_u.O.(-#2.0 * Float_u.pi ())
  else (
    let loop = prune_degeneracies loop in
    let n = Array.length loop in
    if n = 0
    then (* Completely degenerate. *)
      Float_u.O.(#2.0 * Float_u.pi ())
    else (
      (* Traversal in canonical order makes the result invariant under
         vertex rotation and direction, which is the property used by the
         wider S2 API (e.g. equality testing of loops). The direction [dir]
         also flips the sign of the sum, which [signed_area] relies on to
         keep CCW loops positive. *)
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
      (* Clamp into the open interval [(-2*pi, 2*pi)] so that rounding noise
         cannot produce the sentinel values [+/- 2*pi] that are reserved
         for fully-degenerate and full-sphere loops respectively. *)
      let k_max_curvature = (#2.0 * Float_u.pi ()) - (#4.0 * Float_u.epsilon_float ()) in
      let total = sum + compensation in
      let dir_f = Float_u.of_int dir in
      Float_util.max_u
        (-k_max_curvature)
        (Float_util.min_u k_max_curvature (dir_f * total))))
;;

let is_normalized loop =
  let open Float_u.O in
  curvature loop >= -curvature_max_error loop
;;

let signed_area loop =
  let open Float_u.O in
  (* Raw signed-area accumulation via the Kahan variant of the triangle-fan
     surface integral. The [ieee_remainder] step reduces the value into
     [(-2*pi, 2*pi]], and the flip at [-2*pi] pins an otherwise degenerate
     case onto the positive boundary. When the result is smaller than the
     error bound, we fall back on [curvature] to pick the correct sign for
     nearly-degenerate loops: the full loop returns the minimum positive
     normal value (negated appropriately), and a completely degenerate
     loop returns [0]. *)
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
  (* Fold the negative half of [signed_area] back into [[0, 4*pi]]: a
     clockwise loop simply represents the complement of its CCW counterpart
     on the sphere. *)
  let a = signed_area loop in
  let open Float_u.O in
  if a < #0.0 then a + (#4.0 * Float_u.pi ()) else a
;;

let approx_area loop =
  let open Float_u.O in
  (#2.0 * Float_u.pi ()) - curvature loop
;;

let centroid loop =
  (* Non-Kahan variant: the scalar Kahan loop accumulates a single
     [float#] register, while a vector centroid needs three independent
     accumulators and the per-component compensation would triple the
     arithmetic cost without a meaningful accuracy win here. *)
  surface_integral_r3 loop ~f_tri:(fun a b c -> S2_centroids.true_centroid a b c)
;;
