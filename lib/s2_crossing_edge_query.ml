open Core

module Shape_edge_id = struct
  type t =
    { shape_id : int
    ; edge_id : int
    }
  [@@deriving compare, equal, sexp_of]
end

module Crossing_type = struct
  type t =
    | Interior
    | All
end

(* Brute-force threshold: if the relevant number of edges is at most this many,
   scan all of them instead of descending through the index. The constant is
   lifted from the upstream benchmarks in s2crossing_edge_query.cc:44. *)
let max_brute_force_edges = 27

(* TODO: port VisitCells / VisitRawCandidates / GetCells low-level cell-visitor
   entry points from s2crossing_edge_query.h:163-194. The current implementation
   only exposes the high-level GetCandidates / GetCrossingEdges APIs. *)

type t =
  { index : S2_shape_index.t
  ; iter : S2_shape_index.Iterator.t
  }

let create index =
  S2_shape_index.build index;
  { index; iter = S2_shape_index.iterator index }
;;

let index t = t.index

(* Sum the number of edges across all shapes, stopping as soon as the running
   total exceeds [limit] (so we never do extra work on huge indices). *)
let count_edges_up_to index ~limit =
  let n = S2_shape_index.num_shape_ids index in
  let total = ref 0 in
  let i = ref 0 in
  while !i < n && !total <= limit do
    let shape = S2_shape_index.shape index !i in
    total := !total + shape.#num_edges;
    incr i
  done;
  !total
;;

(* Sort + dedup a list of [Shape_edge_id.t] values. *)
let sort_and_dedup ids =
  let arr = Array.of_list ids in
  Array.sort arr ~compare:Shape_edge_id.compare;
  let n = Array.length arr in
  if n <= 1
  then Array.to_list arr
  else (
    let acc = ref [ arr.(n - 1) ] in
    for i = n - 2 downto 0 do
      if not (Shape_edge_id.equal arr.(i) arr.(i + 1)) then acc := arr.(i) :: !acc
    done;
    !acc)
;;

(* A pair of R2_rect.t values returned by the bound-splitting routines. *)
type child_bounds =
  #{ lo : R2_rect.t
   ; hi : R2_rect.t
   }

(* Split the 2D rectangle [edge_bound] into two child rectangles at the point
   (u, v). [u_end] and [v_end] specify which endpoint of the *second* child is
   updated; the *first* child has the complementary endpoint updated. This
   matches the endpoint-update convention in s2crossing_edge_query.cc:365-379
   (SplitBound), but the record layout inverts the order: we write the
   complementary-endpoint child as [lo] and the [u_end]/[v_end] child as [hi],
   so downstream callers always read [lo] as "u below the split" and [hi] as
   "u above the split" regardless of [u_end]. *)
let split_bound edge_bound ~u_end ~u ~v_end ~v =
  let x = R2_rect.x edge_bound in
  let y = R2_rect.y edge_bound in
  let x0 =
    if u_end = 1
    then R1_interval.create ~lo:u ~hi:(R1_interval.hi x)
    else R1_interval.create ~lo:(R1_interval.lo x) ~hi:u
  in
  let x1 =
    if u_end = 1
    then R1_interval.create ~lo:(R1_interval.lo x) ~hi:u
    else R1_interval.create ~lo:u ~hi:(R1_interval.hi x)
  in
  let y0 =
    if v_end = 1
    then R1_interval.create ~lo:v ~hi:(R1_interval.hi y)
    else R1_interval.create ~lo:(R1_interval.lo y) ~hi:v
  in
  let y1 =
    if v_end = 1
    then R1_interval.create ~lo:(R1_interval.lo y) ~hi:v
    else R1_interval.create ~lo:v ~hi:(R1_interval.hi y)
  in
  #{ lo = R2_rect.create_intervals_exn ~x:x0 ~y:y0
   ; hi = R2_rect.create_intervals_exn ~x:x1 ~y:y1
   }
;;

let diag_of_segment ~a ~b =
  if Bool.equal
       Float_u.O.(R2_point.x a > R2_point.x b)
       Float_u.O.(R2_point.y a > R2_point.y b)
  then 0
  else 1
;;

(* Split the bound at a given u-value. Returns (left_child_bound, right_child_bound). *)
let split_u_bound edge_bound u ~a ~b =
  let v =
    R1_interval.project_exn
      (R2_rect.y edge_bound)
      (S2_edge_clipping.interpolate_double
         u
         (R2_point.x a)
         (R2_point.x b)
         (R2_point.y a)
         (R2_point.y b))
  in
  let diag = diag_of_segment ~a ~b in
  split_bound edge_bound ~u_end:0 ~u ~v_end:diag ~v
;;

(* Split the bound at a given v-value. Returns (lower_child_bound, upper_child_bound). *)
let split_v_bound edge_bound v ~a ~b =
  let u =
    R1_interval.project_exn
      (R2_rect.x edge_bound)
      (S2_edge_clipping.interpolate_double
         v
         (R2_point.y a)
         (R2_point.y b)
         (R2_point.x a)
         (R2_point.x b))
  in
  let diag = diag_of_segment ~a ~b in
  split_bound edge_bound ~u_end:diag ~u ~v_end:0 ~v
;;

(* Recursive cell descent. [visit] is called with each candidate cell; return
   [false] from [visit] to terminate early. Returns [false] iff the visitor did.

   Maximum recursion depth is 30 (one per S2 cell level). Each frame holds two
   R2_rect values plus the split children, so stack usage is modest. Logic
   mirrors s2crossing_edge_query.cc:273-314. *)
let rec visit_cells_pcell t pcell edge_bound ~a ~b ~visit =
  S2_shape_index.Iterator.seek t.iter (S2_cell_id.range_min (S2_padded_cell.id pcell));
  if S2_shape_index.Iterator.is_done t.iter
  then true
  else if S2_cell_id.compare
            (S2_shape_index.Iterator.cell_id t.iter)
            (S2_cell_id.range_max (S2_padded_cell.id pcell))
          > 0
  then true
  else if S2_cell_id.equal
            (S2_shape_index.Iterator.cell_id t.iter)
            (S2_padded_cell.id pcell)
  then visit (S2_shape_index.Iterator.index_cell t.iter)
  else (
    let mid = S2_padded_cell.middle pcell in
    let center_x = R1_interval.lo (R2_rect.x mid) in
    let center_y = R1_interval.lo (R2_rect.y mid) in
    let eb_x = R2_rect.x edge_bound in
    let eb_y = R2_rect.y edge_bound in
    if Float_u.O.(R1_interval.hi eb_x < center_x)
    then clip_v_axis t edge_bound ~center:center_y ~i:0 pcell ~a ~b ~visit
    else if Float_u.O.(R1_interval.lo eb_x >= center_x)
    then clip_v_axis t edge_bound ~center:center_y ~i:1 pcell ~a ~b ~visit
    else (
      let #{ lo = left_bound; hi = right_bound } =
        split_u_bound edge_bound center_x ~a ~b
      in
      if Float_u.O.(R1_interval.hi eb_y < center_y)
      then (
        let child00 = S2_padded_cell.child_ij pcell ~i:0 ~j:0 in
        let child10 = S2_padded_cell.child_ij pcell ~i:1 ~j:0 in
        visit_cells_pcell t child00 left_bound ~a ~b ~visit
        && visit_cells_pcell t child10 right_bound ~a ~b ~visit)
      else if Float_u.O.(R1_interval.lo eb_y >= center_y)
      then (
        let child01 = S2_padded_cell.child_ij pcell ~i:0 ~j:1 in
        let child11 = S2_padded_cell.child_ij pcell ~i:1 ~j:1 in
        visit_cells_pcell t child01 left_bound ~a ~b ~visit
        && visit_cells_pcell t child11 right_bound ~a ~b ~visit)
      else
        clip_v_axis t left_bound ~center:center_y ~i:0 pcell ~a ~b ~visit
        && clip_v_axis t right_bound ~center:center_y ~i:1 pcell ~a ~b ~visit))

and clip_v_axis t edge_bound ~center ~i pcell ~a ~b ~visit =
  let eb_y = R2_rect.y edge_bound in
  if Float_u.O.(R1_interval.hi eb_y < center)
  then (
    let child = S2_padded_cell.child_ij pcell ~i ~j:0 in
    visit_cells_pcell t child edge_bound ~a ~b ~visit)
  else if Float_u.O.(R1_interval.lo eb_y >= center)
  then (
    let child = S2_padded_cell.child_ij pcell ~i ~j:1 in
    visit_cells_pcell t child edge_bound ~a ~b ~visit)
  else (
    let #{ lo = lower_bound; hi = upper_bound } = split_v_bound edge_bound center ~a ~b in
    let child0 = S2_padded_cell.child_ij pcell ~i ~j:0 in
    let child1 = S2_padded_cell.child_ij pcell ~i ~j:1 in
    visit_cells_pcell t child0 lower_bound ~a ~b ~visit
    && visit_cells_pcell t child1 upper_bound ~a ~b ~visit)
;;

(* Walk all index cells that may contain edges intersecting [a]->[b]. The edge
   is first clipped to the six cube faces (get_face_segments). For each face
   segment we find the smallest face-descendant ("edge root") that contains the
   segment's UV bound, then relate it to the index:

   - Disjoint: nothing to do.
   - Indexed: the edge root is already (or is contained by) an index cell, so
     we visit that one cell.
   - Subdivided: the edge root splits across several index cells; recurse. *)
let visit_cells t ~a ~b ~visit =
  let segments = S2_edge_clipping.get_face_segments a b in
  let continue_ = ref true in
  List.iter segments ~f:(fun segment ->
    if !continue_
    then (
      let a_uv = segment.a in
      let b_uv = segment.b in
      let edge_bound = R2_rect.from_point_pair a_uv b_uv in
      let face_id = S2_cell_id.from_face_exn segment.face in
      let face_pcell = S2_padded_cell.create face_id ~padding:#0.0 in
      let edge_root = S2_padded_cell.shrink_to_fit face_pcell edge_bound in
      match S2_shape_index.Iterator.locate_cell_id t.iter edge_root with
      | Disjoint -> ()
      | Indexed ->
        if not (visit (S2_shape_index.Iterator.index_cell t.iter)) then continue_ := false
      | Subdivided ->
        let pcell =
          if S2_cell_id.is_face edge_root
          then face_pcell
          else S2_padded_cell.create edge_root ~padding:#0.0
        in
        if not (visit_cells_pcell t pcell edge_bound ~a:a_uv ~b:b_uv ~visit)
        then continue_ := false))
;;

(* Collect candidate (shape_id, edge_id) pairs across the full index. *)
let get_candidates t ~a ~b =
  let num_edges = count_edges_up_to t.index ~limit:(max_brute_force_edges + 1) in
  if num_edges <= max_brute_force_edges
  then (
    let acc = ref [] in
    for s = S2_shape_index.num_shape_ids t.index - 1 downto 0 do
      let shape = S2_shape_index.shape t.index s in
      for e = shape.#num_edges - 1 downto 0 do
        acc := { Shape_edge_id.shape_id = s; edge_id = e } :: !acc
      done
    done;
    !acc)
  else (
    let acc = ref [] in
    visit_cells t ~a ~b ~visit:(fun cell ->
      let num_clipped = S2_shape_index.Index_cell.num_clipped cell in
      for s = 0 to num_clipped - 1 do
        let clipped = S2_shape_index.Index_cell.clipped cell s in
        let sid = S2_shape_index.Clipped_shape.shape_id clipped in
        let ne = S2_shape_index.Clipped_shape.num_edges clipped in
        for j = 0 to ne - 1 do
          let eid = S2_shape_index.Clipped_shape.edge clipped j in
          acc := { Shape_edge_id.shape_id = sid; edge_id = eid } :: !acc
        done
      done;
      true);
    sort_and_dedup !acc)
;;

(* Candidate collection limited to one shape. *)
let get_candidates_for_shape t ~a ~b ~shape_id ~(shape : S2_shape.t) =
  let num_edges = shape.#num_edges in
  if num_edges <= max_brute_force_edges
  then (
    let acc = ref [] in
    for e = num_edges - 1 downto 0 do
      acc := { Shape_edge_id.shape_id; edge_id = e } :: !acc
    done;
    !acc)
  else (
    let acc = ref [] in
    visit_cells t ~a ~b ~visit:(fun cell ->
      match S2_shape_index.Index_cell.find_clipped cell ~shape_id with
      | None -> true
      | Some clipped ->
        let ne = S2_shape_index.Clipped_shape.num_edges clipped in
        for j = 0 to ne - 1 do
          let eid = S2_shape_index.Clipped_shape.edge clipped j in
          acc := { Shape_edge_id.shape_id; edge_id = eid } :: !acc
        done;
        true);
    sort_and_dedup !acc)
;;

let filter_crossings ~a ~b ~crossing_type candidates ~shape_of =
  let min_sign =
    match (crossing_type : Crossing_type.t) with
    | All -> 0
    | Interior -> 1
  in
  let arr = Array.of_list candidates in
  let n = Array.length arr in
  let mutable crosser = S2_edge_crosser.create ~a ~b in
  let acc = ref [] in
  for i = 0 to n - 1 do
    let id = arr.(i) in
    let shape : S2_shape.t = shape_of id.Shape_edge_id.shape_id in
    let #{ v0; v1 } : S2_shape.Edge.t = shape.#edge id.Shape_edge_id.edge_id in
    let #{ state; sign } : S2_edge_crosser.with_sign =
      S2_edge_crosser.crossing_sign crosser v0 v1
    in
    crosser <- state;
    if sign >= min_sign then acc := id :: !acc
  done;
  List.rev !acc
;;

let get_crossing_edges t ~a ~b ~crossing_type =
  let candidates = get_candidates t ~a ~b in
  filter_crossings ~a ~b ~crossing_type candidates ~shape_of:(fun sid ->
    S2_shape_index.shape t.index sid)
;;

let get_crossing_edges_for_shape t ~a ~b ~shape_id ~shape ~crossing_type =
  let candidates = get_candidates_for_shape t ~a ~b ~shape_id ~shape in
  filter_crossings ~a ~b ~crossing_type candidates ~shape_of:(fun _ -> shape)
;;
