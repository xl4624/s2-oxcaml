open Core

(* A single edge incident to the target vertex: [v] is the other endpoint and [direction]
   is the running sum of edge directions between [target] and [v] (so that matched sibling
   pairs cancel to 0). An association list is used instead of the flat hash map the C++
   version keeps because S2_point.t has no hash implementation available here and the
   number of distinct neighbours in practice is small. *)
type entry =
  { v : S2_point.t
  ; mutable direction : int
  }

type t =
  { target : S2_point.t
  ; mutable entries : entry list
  }

let create target = { target; entries = [] }
let target t = t.target

let rec add_direction entries v direction =
  match entries with
  | [] -> [ { v; direction } ]
  | e :: _ when S2_point.equal e.v v ->
    e.direction <- e.direction + direction;
    entries
  | e :: rest -> e :: add_direction rest v direction
;;

let add_edge t v ~direction = t.entries <- add_direction t.entries v direction

let contains_sign t =
  (* Find the unmatched edge that is immediately clockwise from ref_dir(target) but not
     equal to it. The result is +1 iff this edge is outgoing.

     A loop with consecutive vertices A, B, C contains vertex B iff the fixed vector R =
     ref_dir(B) is contained by the wedge ABC, which is closed at A and open at C. This
     convention matches [S2_edge_crossings.vertex_crossing]. *)
  let reference_dir = S2_pointutil.ref_dir t.target in
  let rec loop entries best_point best_dir =
    match entries with
    | [] -> best_dir
    | e :: rest ->
      if e.direction <> 0
         && S2_predicates.ordered_ccw reference_dir best_point e.v t.target
      then loop rest e.v e.direction
      else loop rest best_point best_dir
  in
  loop t.entries reference_dir 0
;;

let duplicate_edges t = List.exists t.entries ~f:(fun e -> abs e.direction >= 2)
