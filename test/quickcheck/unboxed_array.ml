(* Quickcheck property tests for Unboxed_array. *)
open Core

module R2_sort = S2.Unboxed_array.Make [@kind (float64 & float64) mod external_] (struct
    type t = S2.R2_point.t
  end)

(* Instantiate the 3-float Make as a build-time sanity check that the kind
   annotation works. *)
module _ =
S2.Unboxed_array.Make [@kind (float64 & float64 & float64) mod external_] (struct
    type t = S2.R3_vector.t
  end)

let r2_compare a b =
  let open Float_u.O in
  let ax = S2.R2_point.x a
  and bx = S2.R2_point.x b in
  if ax < bx
  then -1
  else if ax > bx
  then 1
  else (
    let ay = S2.R2_point.y a
    and by = S2.R2_point.y b in
    if ay < by then -1 else if ay > by then 1 else 0)
;;

module Float_pairs = struct
  type t = (float * float) list [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let finite = filter float ~f:Float.is_finite in
    list (both finite finite)
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 400; shrink_count = 100 }
;;

let build_array pairs =
  match pairs with
  | [] -> [||]
  | (x0, y0) :: _ ->
    let n = List.length pairs in
    let first = S2.R2_point.create ~x:(Float_u.of_float x0) ~y:(Float_u.of_float y0) in
    let arr = Array.create ~len:n first in
    List.iteri pairs ~f:(fun i (x, y) ->
      arr.(i) <- S2.R2_point.create ~x:(Float_u.of_float x) ~y:(Float_u.of_float y));
    arr
;;

let is_sorted arr =
  let n = Array.length arr in
  let mutable ok = true in
  let mutable i = 1 in
  while ok && i < n do
    if r2_compare arr.(i - 1) arr.(i) > 0 then ok <- false;
    i <- i + 1
  done;
  ok
;;

let%test_unit "sort_is_sorted" =
  Base_quickcheck.Test.run_exn (module Float_pairs) ~config:qc_config ~f:(fun pairs ->
    let arr = build_array pairs in
    R2_sort.sort arr ~compare:r2_compare;
    assert (is_sorted arr))
;;

let%test_unit "sort_is_permutation" =
  Base_quickcheck.Test.run_exn (module Float_pairs) ~config:qc_config ~f:(fun pairs ->
    let before = build_array pairs in
    let after = build_array pairs in
    R2_sort.sort after ~compare:r2_compare;
    let n = Array.length before in
    (* Independently sort [before] via a value-typed index permutation so we
         can compare multisets without relying on [Unboxed_array] itself. *)
    let idx = Array.init n ~f:Fn.id in
    Array.sort idx ~compare:(fun a b -> r2_compare before.(a) before.(b));
    for i = 0 to n - 1 do
      assert (S2.R2_point.equal before.(idx.(i)) after.(i))
    done)
;;

let%test_unit "sort_cross_threshold_sizes" =
  (* Exercise both insertion-sort and heap-sort branches by spanning the
     threshold (16) with a range of sizes. *)
  Base_quickcheck.Test.run_exn
    (module struct
      type t = int * Float_pairs.t [@@deriving sexp_of]

      let quickcheck_generator =
        let open Base_quickcheck.Generator in
        let finite = filter float ~f:Float.is_finite in
        let size_gen = int_inclusive 0 64 in
        create (fun ~size:_ ~random:rnd ->
          let size = generate size_gen ~size:30 ~random:rnd in
          let pairs =
            List.init size ~f:(fun _ ->
              let x = generate finite ~size:30 ~random:rnd in
              let y = generate finite ~size:30 ~random:rnd in
              x, y)
          in
          size, pairs)
      ;;

      let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
    end)
    ~config:qc_config
    ~f:(fun (_size, pairs) ->
      let arr = build_array pairs in
      R2_sort.sort arr ~compare:r2_compare;
      assert (is_sorted arr))
;;

(* Edge arrays use the nested kind handled by [Make_3_3]. Property-test it
   end-to-end on random S2_point pairs. *)
module Edge_sort = S2.Unboxed_array.Make_3_3 (struct
    type t = S2.S2_shape.Edge.t
  end)

module Float_sextuples = struct
  type t = (float * float * float * float * float * float) list [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let finite = filter float ~f:Float.is_finite in
    list
      (create (fun ~size:_ ~random:rnd ->
         let g () = generate finite ~size:30 ~random:rnd in
         g (), g (), g (), g (), g (), g ()))
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let build_edge_array sextuples =
  match sextuples with
  | [] -> [||]
  | (a, b, c, d, e, f) :: _ ->
    let n = List.length sextuples in
    let p0 =
      S2.R3_vector.create
        ~x:(Float_u.of_float a)
        ~y:(Float_u.of_float b)
        ~z:(Float_u.of_float c)
    in
    let p1 =
      S2.R3_vector.create
        ~x:(Float_u.of_float d)
        ~y:(Float_u.of_float e)
        ~z:(Float_u.of_float f)
    in
    let first = S2.S2_shape.Edge.create ~v0:p0 ~v1:p1 in
    let arr = Array.create ~len:n first in
    List.iteri sextuples ~f:(fun i (a, b, c, d, e, f) ->
      let p0 =
        S2.R3_vector.create
          ~x:(Float_u.of_float a)
          ~y:(Float_u.of_float b)
          ~z:(Float_u.of_float c)
      in
      let p1 =
        S2.R3_vector.create
          ~x:(Float_u.of_float d)
          ~y:(Float_u.of_float e)
          ~z:(Float_u.of_float f)
      in
      arr.(i) <- S2.S2_shape.Edge.create ~v0:p0 ~v1:p1);
    arr
;;

let is_edge_sorted arr =
  let n = Array.length arr in
  let mutable ok = true in
  let mutable i = 1 in
  while ok && i < n do
    if S2.S2_shape.Edge.compare arr.(i - 1) arr.(i) > 0 then ok <- false;
    i <- i + 1
  done;
  ok
;;

let%test_unit "edge_sort_is_sorted" =
  Base_quickcheck.Test.run_exn
    (module Float_sextuples)
    ~config:qc_config
    ~f:(fun sextuples ->
      let arr = build_edge_array sextuples in
      Edge_sort.sort arr ~compare:S2.S2_shape.Edge.compare;
      assert (is_edge_sorted arr))
;;
