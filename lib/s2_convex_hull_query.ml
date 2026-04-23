open Core

type t =
  { mutable bound : S2_latlng_rect.t
  ; mutable points : S2_point.t array
  ; mutable n : int
  }

let initial_capacity = 8

let create () =
  { bound = S2_latlng_rect.empty
  ; points = Array.create ~len:initial_capacity S2_point.origin
  ; n = 0
  }
;;

let copy_points ~(src : S2_point.t array) ~(dst : S2_point.t array) ~len =
  for i = 0 to len - 1 do
    dst.(i) <- src.(i)
  done
;;

let ensure_capacity t ~needed =
  let cap = Array.length t.points in
  if needed > cap
  then (
    let new_cap = ref (max initial_capacity cap) in
    while !new_cap < needed do
      new_cap := 2 * !new_cap
    done;
    let new_points = Array.create ~len:!new_cap S2_point.origin in
    copy_points ~src:t.points ~dst:new_points ~len:t.n;
    t.points <- new_points)
;;

let push_point t p =
  ensure_capacity t ~needed:(t.n + 1);
  t.points.(t.n) <- p;
  t.n <- t.n + 1
;;

let add_point t p =
  t.bound <- S2_latlng_rect.add_point t.bound (S2_latlng.of_point p);
  push_point t p
;;

let add_polyline t pl =
  t.bound <- S2_latlng_rect.union t.bound (S2_polyline.rect_bound pl);
  let n = S2_polyline.num_vertices pl in
  for i = 0 to n - 1 do
    push_point t (S2_polyline.vertex pl i)
  done
;;

let add_loop t l =
  t.bound <- S2_latlng_rect.union t.bound (S2_loop.rect_bound l);
  if S2_loop.is_empty_or_full l
  then ()
  else (
    let n = S2_loop.num_vertices l in
    for i = 0 to n - 1 do
      push_point t (S2_loop.vertex l i)
    done)
;;

let add_polygon t p =
  t.bound <- S2_latlng_rect.union t.bound (S2_polygon.rect_bound p);
  let n = S2_polygon.num_loops p in
  for i = 0 to n - 1 do
    let l = S2_polygon.loop p i in
    if S2_loop.depth l = 0 then add_loop t l
  done
;;

let cap_bound t = S2_latlng_rect.cap_bound t.bound

(* Remove duplicate entries from [t.points.[0 .. t.n - 1]] using structural
   equality. The dedup is O(n^2) against the already-written prefix - good
   enough for the workloads this module is built for. *)
let dedupe t =
  let pts = t.points in
  let mutable w = 0 in
  for r = 0 to t.n - 1 do
    let p = pts.(r) in
    let mutable duplicate = false in
    let mutable k = 0 in
    while (not duplicate) && k < w do
      if S2_point.equal pts.(k) p then duplicate <- true;
      k <- k + 1
    done;
    if not duplicate
    then (
      pts.(w) <- p;
      w <- w + 1)
  done;
  t.n <- w
;;

(* Sort the first [t.n] slots of [t.points] into CCW order around [origin].
   Uses a basic quicksort adapted from [Array.sort] but avoiding the boxed
   comparator, since [S2_point.t] has a non-value layout. *)
let sort_around t ~origin =
  let less a b =
    match S2_predicates.robust_sign origin a b with
    | S2_predicates.Direction.Counter_clockwise -> true
    | S2_predicates.Direction.Clockwise | S2_predicates.Direction.Indeterminate -> false
  in
  let pts = t.points in
  let rec qsort lo hi =
    if lo < hi
    then (
      let pivot = pts.(hi) in
      let mutable i = lo - 1 in
      for j = lo to hi - 1 do
        if less pts.(j) pivot
        then (
          i <- i + 1;
          let tmp = pts.(i) in
          pts.(i) <- pts.(j);
          pts.(j) <- tmp)
      done;
      let tmp = pts.(i + 1) in
      pts.(i + 1) <- pts.(hi);
      pts.(hi) <- tmp;
      let p = i + 1 in
      qsort lo (p - 1);
      qsort (p + 1) hi)
  in
  qsort 0 (t.n - 1)
;;

(* Reverse the first [t.n] slots. *)
let reverse_points t =
  let pts = t.points in
  let mutable l = 0 in
  let mutable r = t.n - 1 in
  while l < r do
    let tmp = pts.(l) in
    pts.(l) <- pts.(r);
    pts.(r) <- tmp;
    l <- l + 1;
    r <- r - 1
  done
;;

(* Andrew's monotone-chain inner loop: walk the current point array and
   maintain a stack of points that make only CCW turns. Each new point pops
   the stack until the top two stack entries plus the new point form a
   left turn. Producing one chain of the hull takes O(n) after the O(n log
   n) sort; [convex_hull] runs it twice (once forward, once reversed) and
   glues the results. Writes into a caller-provided [output] buffer and
   returns the number of points written. *)
let monotone_chain t ~output =
  let mutable out_n = 0 in
  for i = 0 to t.n - 1 do
    let p = t.points.(i) in
    let mutable done_popping = false in
    while (not done_popping) && out_n >= 2 do
      let a = output.(out_n - 2) in
      let b = output.(out_n - 1) in
      match S2_predicates.robust_sign a b p with
      | S2_predicates.Direction.Counter_clockwise -> done_popping <- true
      | S2_predicates.Direction.Clockwise | S2_predicates.Direction.Indeterminate ->
        out_n <- out_n - 1
    done;
    output.(out_n) <- p;
    out_n <- out_n + 1
  done;
  out_n
;;

(* Three-vertex loop containing a single point. Useful when the hull has
   fewer than three distinct input points. *)
let single_point_loop p =
  let offset = #1e-15 in
  let d0 = S2_pointutil.ortho p in
  let d1 = R3_vector.cross p d0 in
  let v1 = R3_vector.normalize (R3_vector.add p (R3_vector.mul d0 offset)) in
  let v2 = R3_vector.normalize (R3_vector.add p (R3_vector.mul d1 offset)) in
  S2_loop.of_vertices ~validate:false [| p; v1; v2 |]
;;

(* Three-vertex loop containing the edge [a]->[b]. Antipodal endpoints turn
   into the full loop so we do not have to deal with the degenerate
   antipodal-adjacent-vertex case. *)
let single_edge_loop a b =
  if R3_vector.equal (R3_vector.add a b) R3_vector.zero
  then S2_loop.full ()
  else (
    let mid = S2_edge_distances.interpolate a b #0.5 in
    let l = S2_loop.of_vertices ~validate:false [| a; b; mid |] in
    S2_loop.normalize l)
;;

let convex_hull t =
  let c = cap_bound t in
  if Float_u.O.(S2_cap.height c >= #1.0)
  then S2_loop.full ()
  else (
    dedupe t;
    match t.n with
    | 0 -> S2_loop.empty ()
    | 1 -> single_point_loop t.points.(0)
    | 2 -> single_edge_loop t.points.(0) t.points.(1)
    | _ ->
      let origin = S2_pointutil.ortho (S2_cap.center c) in
      sort_around t ~origin;
      (* Buffer big enough for both chains. *)
      let buf = Array.create ~len:t.n S2_point.origin in
      let lower_n = monotone_chain t ~output:buf in
      let lower = Array.create ~len:lower_n S2_point.origin in
      copy_points ~src:buf ~dst:lower ~len:lower_n;
      reverse_points t;
      let upper_n = monotone_chain t ~output:buf in
      (* Drop the last vertex of each chain (it is shared with the other
         chain's first vertex) and concatenate. *)
      let total = lower_n - 1 + (upper_n - 1) in
      let combined = Array.create ~len:total S2_point.origin in
      for i = 0 to lower_n - 2 do
        combined.(i) <- lower.(i)
      done;
      for i = 0 to upper_n - 2 do
        combined.(lower_n - 1 + i) <- buf.(i)
      done;
      S2_loop.of_vertices ~validate:false combined)
;;
