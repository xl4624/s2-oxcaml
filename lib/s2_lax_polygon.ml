open Core

type t =
  { num_loops : int
  ; num_vertices : int
  ; vertices : S2_point.t array
      (* loop_starts has size [num_loops + 1] when [num_loops > 0]. Entry [i] is the
       starting offset of loop [i] within [vertices]; entry [num_loops] is the total
       number of vertices. Empty when [num_loops = 0]. *)
  ; loop_starts : int array
  ; reference_point : S2_shape.Reference_point.t
      (* Spatial-locality cache for [edge] / [chain_position]: the last loop
         index returned by a lookup. Consecutive queries tend to land in the
         same loop, so checking this first avoids a scan. *)
  ; mutable prev_loop : int
  }

let sexp_of_t
  { num_loops
  ; num_vertices = _
  ; vertices
  ; loop_starts
  ; reference_point = _
  ; prev_loop = _
  }
  =
  let loops_sexp = ref [] in
  for i = num_loops - 1 downto 0 do
    let start = loop_starts.(i) in
    let stop = loop_starts.(i + 1) in
    let pts = ref [] in
    for j = stop - 1 downto start do
      pts := S2_point.sexp_of_t vertices.(j) :: !pts
    done;
    loops_sexp := Sexp.List !pts :: !loops_sexp
  done;
  Sexp.List [ Sexp.Atom "S2_lax_polygon"; Sexp.List !loops_sexp ]
;;

let[@inline] [@zero_alloc] num_loop_vertices_raw ~loop_starts i =
  loop_starts.(i + 1) - loop_starts.(i)
;;

(* Below this threshold a linear scan over [loop_starts] outperforms binary
   search; matches the upstream S2 benchmark constant. *)
let k_max_linear_search_loops = 12

(* Find the loop whose range of vertices contains edge/vertex [e]. [prev] is a
   spatial-locality hint: most consecutive queries land in the same loop or the
   loop immediately following it. The returned index is what the caller should
   store as the next hint. Requires [num_loops >= 1] and
   [0 <= e < loop_starts.(num_loops)]. *)
let[@inline] [@zero_alloc] find_loop_containing ~loop_starts ~num_loops ~prev e =
  if e >= loop_starts.(prev) && e < loop_starts.(prev + 1)
  then prev
  else if e = loop_starts.(prev + 1)
  then (
    (* [e] starts the loop after [prev]; skip over any intervening empty loops,
       which share the same start offset. *)
    let mutable loop = prev + 1 in
    while loop_starts.(loop + 1) = e do
      loop <- loop + 1
    done;
    loop)
  else if num_loops <= k_max_linear_search_loops
  then (
    let mutable loop = 0 in
    while loop_starts.(loop + 1) <= e do
      loop <- loop + 1
    done;
    loop)
  else (
    (* Binary search for the smallest [loop] in [0, num_loops - 1] with
       [loop_starts.(loop + 1) > e]. *)
    let mutable lo = 0 in
    let mutable hi = num_loops in
    while lo < hi do
      let mid = (lo + hi) / 2 in
      if loop_starts.(mid + 1) > e then hi <- mid else lo <- mid + 1
    done;
    lo)
;;

let[@inline] [@zero_alloc] edge_at ~vertices ~loop_starts loop e =
  let e1 = if e + 1 = loop_starts.(loop + 1) then loop_starts.(loop) else e + 1 in
  S2_shape.Edge.create ~v0:vertices.(e) ~v1:vertices.(e1)
;;

let[@inline] [@zero_alloc] chain_of ~loop_starts ~num_loops i =
  if num_loops = 1
  then S2_shape.Chain.create ~start:0 ~length:loop_starts.(1)
  else (
    let start = loop_starts.(i) in
    S2_shape.Chain.create ~start ~length:(loop_starts.(i + 1) - start))
;;

let[@inline] [@zero_alloc] chain_edge_of ~vertices ~loop_starts ~num_loops i j =
  let n = num_loop_vertices_raw ~loop_starts i in
  let k = if j + 1 = n then 0 else j + 1 in
  if num_loops = 1
  then S2_shape.Edge.create ~v0:vertices.(j) ~v1:vertices.(k)
  else (
    let base = loop_starts.(i) in
    S2_shape.Edge.create ~v0:vertices.(base + j) ~v1:vertices.(base + k))
;;

let of_loops src_loops =
  let num_loops = Array.length src_loops in
  if num_loops = 0
  then (
    let reference_point = S2_shape.Reference_point.contained false in
    { num_loops = 0
    ; num_vertices = 0
    ; vertices = [||]
    ; loop_starts = [||]
    ; reference_point
    ; prev_loop = 0
    })
  else (
    let loop_starts = Array.create ~len:(num_loops + 1) 0 in
    let num_vertices = ref 0 in
    for i = 0 to num_loops - 1 do
      loop_starts.(i) <- !num_vertices;
      num_vertices := !num_vertices + Array.length src_loops.(i)
    done;
    loop_starts.(num_loops) <- !num_vertices;
    let num_vertices = !num_vertices in
    let vertices =
      if num_vertices = 0
      then [||]
      else (
        let dst = Array.create ~len:num_vertices S2_point.origin in
        for i = 0 to num_loops - 1 do
          let base = loop_starts.(i) in
          let loop = src_loops.(i) in
          for j = 0 to Array.length loop - 1 do
            dst.(base + j) <- loop.(j)
          done
        done;
        dst)
    in
    let cache = ref 0 in
    let edge e =
      if num_loops = 1
      then (
        let n = loop_starts.(1) in
        let e1 = if e + 1 = n then 0 else e + 1 in
        S2_shape.Edge.create ~v0:vertices.(e) ~v1:vertices.(e1))
      else (
        let loop = find_loop_containing ~loop_starts ~num_loops ~prev:!cache e in
        cache := loop;
        edge_at ~vertices ~loop_starts loop e)
    in
    let chain = chain_of ~loop_starts ~num_loops in
    let reference_point =
      S2_shape.get_reference_point
        ~num_edges:num_vertices
        ~num_chains:num_loops
        ~edge
        ~chain
    in
    { num_loops
    ; num_vertices
    ; vertices
    ; loop_starts
    ; reference_point
    ; prev_loop = !cache
    })
;;

let[@inline] [@zero_alloc] num_loops t = t.num_loops
let[@inline] [@zero_alloc] num_vertices t = t.num_vertices

let[@inline] [@zero_alloc] num_loop_vertices t i =
  num_loop_vertices_raw ~loop_starts:t.loop_starts i
;;

let[@inline] [@zero_alloc] loop_vertex t i j =
  if t.num_loops = 1 then t.vertices.(j) else t.vertices.(t.loop_starts.(i) + j)
;;

let[@inline] [@zero_alloc] num_edges t = t.num_vertices

let[@zero_alloc] edge t e =
  if t.num_loops = 1
  then (
    let n = t.loop_starts.(1) in
    let e1 = if e + 1 = n then 0 else e + 1 in
    S2_shape.Edge.create ~v0:t.vertices.(e) ~v1:t.vertices.(e1))
  else (
    let loop =
      find_loop_containing
        ~loop_starts:t.loop_starts
        ~num_loops:t.num_loops
        ~prev:t.prev_loop
        e
    in
    t.prev_loop <- loop;
    edge_at ~vertices:t.vertices ~loop_starts:t.loop_starts loop e)
;;

let[@inline] [@zero_alloc] dimension _ = 2
let[@inline] [@zero_alloc] num_chains t = t.num_loops

let[@inline] [@zero_alloc] chain t i =
  chain_of ~loop_starts:t.loop_starts ~num_loops:t.num_loops i
;;

let[@inline] [@zero_alloc] chain_edge t i j =
  chain_edge_of ~vertices:t.vertices ~loop_starts:t.loop_starts ~num_loops:t.num_loops i j
;;

let[@zero_alloc] chain_position t e =
  if t.num_loops = 1
  then S2_shape.Chain_position.create ~chain_id:0 ~offset:e
  else (
    let loop =
      find_loop_containing
        ~loop_starts:t.loop_starts
        ~num_loops:t.num_loops
        ~prev:t.prev_loop
        e
    in
    t.prev_loop <- loop;
    S2_shape.Chain_position.create ~chain_id:loop ~offset:(e - t.loop_starts.(loop)))
;;

let[@inline] [@zero_alloc] reference_point t = t.reference_point
let type_tag = 5

let to_shape t : S2_shape.t =
  #{ num_edges = num_edges t
   ; num_chains = num_chains t
   ; dimension = 2
   ; type_tag
   ; reference_point = reference_point t
   ; edge = edge t
   ; chain = chain t
   ; chain_edge = chain_edge t
   ; chain_position = chain_position t
   }
;;
