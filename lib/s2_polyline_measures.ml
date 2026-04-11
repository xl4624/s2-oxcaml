open Core

(* Layout-polymorphic array access so we can work with arrays of unboxed
   [S2_point.t] values without boxing each vertex on lookup. *)
external arr_get : ('a : any mod separable). 'a array -> int -> 'a = "%array_safe_get"
[@@layout_poly]

let[@inline] [@zero_alloc] length (polyline : S2_point.t array) =
  let n = Array.length polyline in
  if n < 2
  then S1_angle.zero
  else (
    let mutable acc = #0.0 in
    let mutable prev = arr_get polyline 0 in
    for i = 1 to n - 1 do
      let cur = arr_get polyline i in
      acc <- Float_u.O.(acc + S1_angle.radians (S2_point.distance prev cur));
      prev <- cur
    done;
    S1_angle.of_radians acc)
;;

let[@inline] [@zero_alloc] centroid (polyline : S2_point.t array) =
  let n = Array.length polyline in
  if n < 2
  then R3_vector.zero
  else (
    let mutable acc = R3_vector.zero in
    let mutable prev = arr_get polyline 0 in
    for i = 1 to n - 1 do
      let cur = arr_get polyline i in
      acc <- R3_vector.add acc (S2_centroids.edge_true_centroid prev cur);
      prev <- cur
    done;
    acc)
;;
