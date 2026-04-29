(* Minimal hot-loop driver for profiling Cap coverage with perf. Runs the same workload as
   the Cap sub-benchmark but without core_bench's noise model, so perf samples are
   attributed to S2 code rather than the measurement harness. *)

open! Core
open S2

let num_regions = 1000
let iterations = 200_000
let rand_state = Random.State.make [| 0x13375eed |]
let uniform_float ~lo ~hi = lo +. (Random.State.float rand_state 1.0 *. (hi -. lo))

let () =
  let avg_cell_area_at_max_level =
    Float_u.to_float (S2_metrics.get_value S2_metrics.avg_area S2_cell_id.max_level)
  in
  let min_a = 0.1 *. avg_cell_area_at_max_level in
  let max_a = 4.0 *. Float.pi in
  let data = Array.create_float_uninitialized ~len:(4 * num_regions) in
  for i = 0 to num_regions - 1 do
    let r = Random.State.float rand_state 1.0 in
    let area = max_a *. Float.( ** ) (min_a /. max_a) r in
    data.(4 * i) <- uniform_float ~lo:(-1.0) ~hi:1.0;
    data.((4 * i) + 1) <- uniform_float ~lo:(-1.0) ~hi:1.0;
    data.((4 * i) + 2) <- uniform_float ~lo:(-1.0) ~hi:1.0;
    data.((4 * i) + 3) <- area
  done;
  let coverer =
    S2_region_coverer.create ~min_level:0 ~max_level:30 ~level_mod:1 ~max_cells:8 ()
  in
  let mutable total_cells = 0 in
  for i = 0 to iterations - 1 do
    let base = 4 * (i mod num_regions) in
    let center =
      S2_point.of_coords
        ~x:(Float_u.of_float data.(base))
        ~y:(Float_u.of_float data.(base + 1))
        ~z:(Float_u.of_float data.(base + 2))
    in
    let cap = S2_cap.of_center_area center (Float_u.of_float data.(base + 3)) in
    let covering = S2_region_coverer.covering coverer (S2_region.of_cap cap) in
    total_cells <- total_cells + S2_cell_union.num_cells covering
  done;
  Printf.printf "iterations=%d total_cells=%d\n" iterations total_cells
;;
