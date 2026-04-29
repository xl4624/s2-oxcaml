(* Region coverer benchmarks. *)

open! Core
open! Core_bench
open S2

let rand_state = Random.State.make [| 0x13375eed |]
let num_regions = 1000
let uniform_float ~lo ~hi = lo +. (Random.State.float rand_state 1.0 *. (hi -. lo))

let random_cell_id_for_level level =
  let face = Random.State.int rand_state S2_cell_id.num_faces in
  let pos_mask = Int64.( - ) (Int64.shift_left 1L S2_cell_id.pos_bits) 1L in
  let pos = Int64.bit_and (Random.State.int64 rand_state Int64.max_value) pos_mask in
  S2_cell_id.from_face_pos_level face (Int64_u.of_int64 pos) level
;;

(* Random cell union with all cells at [level]. Using a fixed level avoids pathological
   collapse: random multi-level cells often contain each other and normalize down to a
   trivial union, which makes the "size = N" label misleading. Same-level cells are
   disjoint by construction so a union of N of them stays size N (modulo the very rare
   duplicate at fine levels). *)
let random_cell_union_at_level ~level n =
  let ids = Array.create ~len:n S2_cell_id.none in
  for i = 0 to n - 1 do
    ids.(i) <- random_cell_id_for_level level
  done;
  S2_cell_union.create ids
;;

(* Matches Go's 0.1*AvgAreaMetric.Value(MaxLevel). *)
let avg_cell_area_at_max_level =
  Float_u.to_float (S2_metrics.get_value S2_metrics.avg_area S2_cell_id.max_level)
;;

let default_coverer =
  S2_region_coverer.create ~min_level:0 ~max_level:30 ~level_mod:1 ~max_cells:8 ()
;;

(* Precompute [num_regions] regions, then cycle through them per benchmarked call. Mirrors
   Go's StopTimer/StartTimer pattern to exclude setup from the measurement.

   The region arrays are unboxed, so we fill with a for-loop instead of [Array.init]. *)

(* S2_cap.t, S2_cell.t, S2_cell_id.t are all unboxed-product types, and OxCaml arrays of
   such types are restricted. We pre-generate the raw inputs (floats, cell ids) into
   primitive arrays and reconstruct the region on each call. The reconstruction cost is
   small (~10-30ns for [of_coords] + [of_center_area] or [of_cell_id]) compared to the
   coverage work being measured. *)

let bench_cap =
  Bench.Test.create_indexed
    ~name:"RegionCovererCoveringCap"
    ~args:(List.range 2 17)
    (fun _n ->
       (* Store 4 floats per region: center xyz + area. *)
       let data = Array.create_float_uninitialized ~len:(4 * num_regions) in
       let min_a = 0.1 *. avg_cell_area_at_max_level in
       let max_a = 4.0 *. Float.pi in
       for i = 0 to num_regions - 1 do
         let r = Random.State.float rand_state 1.0 in
         let area = max_a *. Float.( ** ) (min_a /. max_a) r in
         data.(4 * i) <- uniform_float ~lo:(-1.0) ~hi:1.0;
         data.((4 * i) + 1) <- uniform_float ~lo:(-1.0) ~hi:1.0;
         data.((4 * i) + 2) <- uniform_float ~lo:(-1.0) ~hi:1.0;
         data.((4 * i) + 3) <- area
       done;
       let i = ref 0 in
       Staged.stage (fun () ->
         let base = 4 * (!i mod num_regions) in
         incr i;
         let center =
           S2_point.of_coords
             ~x:(Float_u.of_float data.(base))
             ~y:(Float_u.of_float data.(base + 1))
             ~z:(Float_u.of_float data.(base + 2))
         in
         let cap = S2_cap.of_center_area center (Float_u.of_float data.(base + 3)) in
         let _ : S2_cell_union.t =
           S2_region_coverer.covering default_coverer (S2_region.of_cap cap)
         in
         ()))
;;

let bench_cell =
  Bench.Test.create_indexed
    ~name:"RegionCovererCoveringCell"
    ~args:(List.range 2 17)
    (fun n ->
       let ids = Array.create ~len:num_regions S2_cell_id.none in
       for i = 0 to num_regions - 1 do
         let level = S2_cell_id.max_level - Random.State.int rand_state n in
         ids.(i) <- random_cell_id_for_level level
       done;
       let i = ref 0 in
       Staged.stage (fun () ->
         let id = ids.(!i mod num_regions) in
         incr i;
         let cell = S2_cell.of_cell_id id in
         let _ : S2_cell_union.t =
           S2_region_coverer.covering default_coverer (S2_region.of_cell cell)
         in
         ()))
;;

(* Cells generated at level 22 - a medium-fine level where random cells don't collide
   (4^22 possible cells per face) and don't collapse under normalization. *)
let bench_cell_union =
  Bench.Test.create_indexed
    ~name:"RegionCovererCoveringCellUnion"
    ~args:(List.range 2 11)
    (fun n ->
       let size = Int.( ** ) 2 n in
       let regions =
         Array.init num_regions ~f:(fun _ -> random_cell_union_at_level ~level:22 size)
       in
       let i = ref 0 in
       Staged.stage (fun () ->
         let cu = regions.(!i mod num_regions) in
         incr i;
         let _ : S2_cell_union.t =
           S2_region_coverer.covering default_coverer (S2_region.of_cell_union cu)
         in
         ()))
;;

let () = Command_unix.run (Bench.make_command [ bench_cap; bench_cell; bench_cell_union ])
