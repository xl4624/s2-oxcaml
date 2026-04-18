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
  }

let sexp_of_t { num_loops; num_vertices = _; vertices; loop_starts; reference_point = _ } =
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

let[@inline] num_loop_vertices_raw ~loop_starts i = loop_starts.(i + 1) - loop_starts.(i)

let edge_of ~vertices ~loop_starts ~num_loops e =
  if num_loops = 1
  then (
    let n = loop_starts.(1) in
    let e1 = if e + 1 = n then 0 else e + 1 in
    S2_shape.Edge.create ~v0:vertices.(e) ~v1:vertices.(e1))
  else (
    (* Find the loop whose range contains [e]: the smallest [loop] such that
       [loop_starts.(loop + 1) > e]. *)
    let mutable loop = 0 in
    while loop_starts.(loop + 1) <= e do
      loop <- loop + 1
    done;
    let e1 = if e + 1 = loop_starts.(loop + 1) then loop_starts.(loop) else e + 1 in
    S2_shape.Edge.create ~v0:vertices.(e) ~v1:vertices.(e1))
;;

let chain_position_of ~loop_starts ~num_loops e =
  if num_loops = 1
  then S2_shape.Chain_position.create ~chain_id:0 ~offset:e
  else (
    let mutable loop = 0 in
    while loop_starts.(loop + 1) <= e do
      loop <- loop + 1
    done;
    S2_shape.Chain_position.create ~chain_id:loop ~offset:(e - loop_starts.(loop)))
;;

let chain_of ~loop_starts ~num_loops i =
  if num_loops = 1
  then S2_shape.Chain.create ~start:0 ~length:loop_starts.(1)
  else (
    let start = loop_starts.(i) in
    S2_shape.Chain.create ~start ~length:(loop_starts.(i + 1) - start))
;;

let chain_edge_of ~vertices ~loop_starts ~num_loops i j =
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
        (* Find the first vertex in any loop to seed the array. *)
        let mutable seed = S2_point.origin in
        let mutable found = false in
        let mutable i = 0 in
        while (not found) && i < num_loops do
          if Array.length src_loops.(i) > 0
          then (
            seed <- src_loops.(i).(0);
            found <- true);
          i <- i + 1
        done;
        let dst = Array.create ~len:num_vertices seed in
        for i = 0 to num_loops - 1 do
          let base = loop_starts.(i) in
          let loop = src_loops.(i) in
          for j = 0 to Array.length loop - 1 do
            dst.(base + j) <- loop.(j)
          done
        done;
        dst)
    in
    let edge = edge_of ~vertices ~loop_starts ~num_loops in
    let chain = chain_of ~loop_starts ~num_loops in
    let reference_point =
      S2_shape.get_reference_point
        ~num_edges:num_vertices
        ~num_chains:num_loops
        ~edge
        ~chain
    in
    { num_loops; num_vertices; vertices; loop_starts; reference_point })
;;

let num_loops t = t.num_loops
let num_vertices t = t.num_vertices
let num_loop_vertices t i = num_loop_vertices_raw ~loop_starts:t.loop_starts i

let loop_vertex t i j =
  if t.num_loops = 1 then t.vertices.(j) else t.vertices.(t.loop_starts.(i) + j)
;;

let num_edges t = t.num_vertices

let edge t e =
  edge_of ~vertices:t.vertices ~loop_starts:t.loop_starts ~num_loops:t.num_loops e
;;

let dimension _ = 2
let num_chains t = t.num_loops
let chain t i = chain_of ~loop_starts:t.loop_starts ~num_loops:t.num_loops i

let chain_edge t i j =
  chain_edge_of ~vertices:t.vertices ~loop_starts:t.loop_starts ~num_loops:t.num_loops i j
;;

let chain_position t e =
  chain_position_of ~loop_starts:t.loop_starts ~num_loops:t.num_loops e
;;

let reference_point t = t.reference_point
let type_tag = 5

let to_shape t : S2_shape.t =
  #{ num_edges = (fun () -> num_edges t)
   ; edge = edge t
   ; dimension = (fun () -> 2)
   ; num_chains = (fun () -> num_chains t)
   ; chain = chain t
   ; chain_edge = chain_edge t
   ; chain_position = chain_position t
   ; reference_point = (fun () -> reference_point t)
   ; type_tag = (fun () -> type_tag)
   }
;;
