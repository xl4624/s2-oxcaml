(* Miscellaneous micro-benchmarks. Each benchmark mirrors a Go counterpart in geo/s2 so
   [compare_go.py] can line them up directly. Inputs come from the same constants as Go
   where possible. *)

open! Core
open! Core_bench
open S2

(* Mirrors BenchmarkSign in geo/s2/predicates_test.go. Core_bench dispatches the closure
   body through a function pointer per iteration, so the call site is already opaque to
   the compiler -- no constant-fold defeating tricks needed here. The Go side relies on a
   [//go:noinline] wrapper for the same reason. *)
let bench_sign =
  let p1 = S2_point.of_coords ~x:(-#3.0) ~y:(-#1.0) ~z:#4.0 in
  let p2 = S2_point.of_coords ~x:#2.0 ~y:(-#1.0) ~z:(-#3.0) in
  let p3 = S2_point.of_coords ~x:#1.0 ~y:(-#2.0) ~z:#0.0 in
  Bench.Test.create ~name:"Sign" (fun () ->
    let _ : bool = S2_predicates.sign p1 p2 p3 in
    ())
;;

(* Mirrors BenchmarkRobustSignSimple in geo/s2/predicates_test.go: same constants as
   [bench_sign], but routes through the [robust_sign] cascade (the cheap triage path is
   taken). *)
let bench_robust_sign_simple =
  let p1 = S2_point.of_coords ~x:(-#3.0) ~y:(-#1.0) ~z:#4.0 in
  let p2 = S2_point.of_coords ~x:#2.0 ~y:(-#1.0) ~z:(-#3.0) in
  let p3 = S2_point.of_coords ~x:#1.0 ~y:(-#2.0) ~z:#0.0 in
  Bench.Test.create ~name:"RobustSignSimple" (fun () ->
    let _ : S2_predicates.Direction.t = S2_predicates.robust_sign p1 p2 p3 in
    ())
;;

(* Mirrors BenchmarkPointArea in geo/s2/point_measures_test.go: triangle area on the three
   canonical axis points (1,0,0), (0,1,0), (0,0,1). *)
let bench_point_area =
  let p000 = S2_point.of_coords ~x:#1.0 ~y:#0.0 ~z:#0.0 in
  let p090 = S2_point.of_coords ~x:#0.0 ~y:#1.0 ~z:#0.0 in
  let pz = S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0 in
  Bench.Test.create ~name:"PointArea" (fun () ->
    let _ : float# = S2_measures.area p000 p090 pz in
    ())
;;

(* Mirrors BenchmarkPointFromLatLng in geo/s2/latlng_test.go: a single fixed E7-ish
   lat/lng (2 hex constants from Go) converted to a unit-vector point. *)
let bench_point_from_latlng =
  (* s1.E7: 1 / (180 * 1e7) * pi, the "1e-7 degree" angle in radians. *)
  let e7 = #1.745329251994329547e-09 in
  let lat_idx = Float_u.of_float (Float.of_int 0x150bc888) in
  let lng_idx = Float_u.of_float (Float.of_int 0x5099d63f) in
  let lat = Float_u.O.(e7 * lat_idx) in
  let lng = Float_u.O.(e7 * lng_idx) in
  let ll = S2_latlng.of_radians ~lat ~lng in
  Bench.Test.create ~name:"PointFromLatLng" (fun () ->
    let _ : S2_point.t = S2_latlng.to_point ll in
    ())
;;

(* Mirrors BenchmarkLatLngGetDistance in geo/s2/latlng_test.go: distance from a fixed (25
   deg, -78 deg) anchor to a stream of (i*E7, 56 deg) points. The varying [lat] keeps the
   JIT/branch predictor honest; the index [i] is a plain ref counter just like Go's loop
   variable. *)
let bench_latlng_get_distance =
  let x = S2_latlng.of_degrees ~lat:#25.0 ~lng:(-#78.0) in
  let e7 = #1.745329251994329547e-09 in
  let lng56 = S1_angle.of_degrees #56.0 in
  let i = ref 0 in
  Bench.Test.create ~name:"LatLngGetDistance" (fun () ->
    let lat = Float_u.O.(e7 * Float_u.of_int !i) in
    let y = S2_latlng.create ~lat:(S1_angle.of_radians lat) ~lng:lng56 in
    incr i;
    let _ : S1_angle.t = S2_latlng.distance x y in
    ())
;;

let () =
  Command_unix.run
    (Bench.make_command
       [ bench_sign
       ; bench_robust_sign_simple
       ; bench_point_area
       ; bench_point_from_latlng
       ; bench_latlng_get_distance
       ])
;;
