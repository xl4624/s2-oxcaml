open Core

type index_built =
  { index : S2_shape_index.t
  ; shape_id : int
  ; point_query : S2_contains_point_query.t
  }

type index_state =
  | Not_built
  | Built of index_built

type t =
  { loops : S2_loop.t array
  ; has_holes : bool
  ; num_vertices : int
  ; num_edges : int
  ; cumulative_edges : int array
      (* [cumulative_edges.(i)] is the running sum of edge counts for loops [0..i-1].
         Populated only when [num_loops > max_linear_search_loops]; for small polygons
         a linear scan over the loops is faster than a prefix-sum lookup. *)
  ; bound : S2_latlng_rect.t
  ; subregion_bound : S2_latlng_rect.t
  ; mutable index_state : index_state
  }

let sexp_of_t t =
  let loops = Array.to_list (Array.map t.loops ~f:(fun l -> S2_loop.sexp_of_t l)) in
  Sexp.List [ Sexp.Atom "S2_polygon"; Sexp.List loops ]
;;

let[@inline] num_loops t = Array.length t.loops
let[@inline] loop t k = t.loops.(k)
let loops t = Array.copy t.loops
let[@inline] is_empty t = Array.length t.loops = 0
let[@inline] is_full t = Array.length t.loops = 1 && S2_loop.is_full t.loops.(0)
let[@inline] has_holes t = t.has_holes
let[@inline] num_vertices t = t.num_vertices
let[@inline] num_edges t = t.num_edges
let[@inline] dimension _ = 2
let[@inline] cap_bound t = S2_latlng_rect.cap_bound t.bound
let[@inline] rect_bound t = t.bound
let cell_union_bound t = S2_latlng_rect.cell_union_bound t.bound

let parent t k =
  let depth = S2_loop.depth t.loops.(k) in
  if depth = 0
  then None
  else (
    let mutable j = k - 1 in
    while j >= 0 && S2_loop.depth t.loops.(j) >= depth do
      j <- j - 1
    done;
    Some j)
;;

let last_descendant t k =
  if k < 0
  then Array.length t.loops - 1
  else (
    let depth = S2_loop.depth t.loops.(k) in
    let n = Array.length t.loops in
    let mutable j = k + 1 in
    while j < n && S2_loop.depth t.loops.(j) > depth do
      j <- j + 1
    done;
    j - 1)
;;

(* Count total edges across a set of loops. A full-loop chain has 0 edges. *)
let total_num_edges loops =
  Array.fold loops ~init:0 ~f:(fun acc l -> acc + S2_loop.num_edges l)
;;

let total_num_vertices loops =
  Array.fold loops ~init:0 ~f:(fun acc l -> acc + S2_loop.num_vertices l)
;;

let any_hole loops = Array.exists loops ~f:(fun l -> S2_loop.is_hole l)

let compute_bound loops =
  let n = Array.length loops in
  let mutable b = S2_latlng_rect.empty in
  for i = 0 to n - 1 do
    let l = loops.(i) in
    if not (S2_loop.is_hole l) then b <- S2_latlng_rect.union b (S2_loop.rect_bound l)
  done;
  b
;;

let max_linear_search_loops = 12

let compute_cumulative loops =
  if Array.length loops > max_linear_search_loops
  then (
    let arr = Array.create ~len:(Array.length loops) 0 in
    let acc = ref 0 in
    Array.iteri loops ~f:(fun i l ->
      arr.(i) <- !acc;
      acc := !acc + S2_loop.num_edges l);
    arr)
  else [||]
;;

(* Shape interface implementation. We return a shape record that forwards to the
   polygon's loops. *)

let rec to_shape t : S2_shape.t =
  #{ num_edges = t.num_edges
   ; num_chains = num_loops t
   ; dimension = 2
   ; type_tag
   ; reference_point = reference_point t
   ; edge = edge t
   ; chain = chain t
   ; chain_edge = chain_edge t
   ; chain_position = chain_position t
   }

and type_tag = 1

and reference_point t =
  let contains_origin =
    Array.fold t.loops ~init:false ~f:(fun acc l ->
      Bool.( <> ) acc (S2_loop.contains_origin l))
  in
  S2_shape.Reference_point.create
    ~point:(S2_pointutil.origin ())
    ~contained:contains_origin

and chain t chain_id =
  let num_vertices_in_loop = S2_loop.num_vertices t.loops.(chain_id) in
  let start =
    if Array.length t.cumulative_edges > 0
    then t.cumulative_edges.(chain_id)
    else (
      let mutable acc = 0 in
      for j = 0 to chain_id - 1 do
        acc <- acc + S2_loop.num_vertices t.loops.(j)
      done;
      acc)
  in
  (* S2_loop represents a full loop with a single vertex, but the shape interface
     reports a chain of zero edges for full loops. *)
  let length = if num_vertices_in_loop = 1 then 0 else num_vertices_in_loop in
  S2_shape.Chain.create ~start ~length

and locate_edge t e =
  (* Returns (loop_index, offset_within_loop). *)
  if Array.length t.cumulative_edges > 0
  then (
    let n = Array.length t.cumulative_edges in
    let mutable i = 0 in
    let mutable found = false in
    let mutable remaining = e in
    while (not found) && i < n do
      if i + 1 >= n || remaining < t.cumulative_edges.(i + 1)
      then (
        remaining <- remaining - t.cumulative_edges.(i);
        found <- true)
      else i <- i + 1
    done;
    i, remaining)
  else (
    let mutable i = 0 in
    let mutable remaining = e in
    while remaining >= S2_loop.num_vertices t.loops.(i) do
      remaining <- remaining - S2_loop.num_vertices t.loops.(i);
      i <- i + 1
    done;
    i, remaining)

and edge t e =
  let i, j = locate_edge t e in
  let l = t.loops.(i) in
  let v0 = S2_loop.oriented_vertex l j in
  let v1 = S2_loop.oriented_vertex l (j + 1) in
  S2_shape.Edge.create ~v0 ~v1

and chain_edge t i j =
  let l = t.loops.(i) in
  let v0 = S2_loop.oriented_vertex l j in
  let v1 = S2_loop.oriented_vertex l (j + 1) in
  S2_shape.Edge.create ~v0 ~v1

and chain_position t e =
  let i, j = locate_edge t e in
  S2_shape.Chain_position.create ~chain_id:i ~offset:j
;;

let num_chains t = num_loops t

let get_or_build_index t =
  match t.index_state with
  | Built s -> s
  | Not_built ->
    let index = S2_shape_index.create () in
    let shape_id = S2_shape_index.add index (to_shape t) in
    let point_query = S2_contains_point_query.create index () in
    let built = { index; shape_id; point_query } in
    t.index_state <- Built built;
    built
;;

let make_polygon ~loops =
  let has_holes = any_hole loops in
  let num_vertices = total_num_vertices loops in
  let num_edges = total_num_edges loops in
  let bound = compute_bound loops in
  let subregion_bound = S2_latlng_rect_bounder.expand_for_subregions bound in
  let cumulative_edges = compute_cumulative loops in
  { loops
  ; has_holes
  ; num_vertices
  ; num_edges
  ; cumulative_edges
  ; bound
  ; subregion_bound
  ; index_state = Not_built
  }
;;

let make_empty_polygon () = make_polygon ~loops:[||]

let make_full_polygon () =
  let loops = [| S2_loop.full () |] in
  S2_loop.set_depth loops.(0) 0;
  let bound = S2_latlng_rect.full in
  { loops
  ; has_holes = false
  ; num_vertices = S2_loop.num_vertices loops.(0)
  ; num_edges = 0
  ; cumulative_edges = [||]
  ; bound
  ; subregion_bound = bound
  ; index_state = Not_built
  }
;;

let empty () = make_empty_polygon ()
let full () = make_full_polygon ()

(* --- Nesting hierarchy --------------------------------------------------- *)

(* [loop_map] records immediate children in the nesting hierarchy. We key it on
   physical identity (by loop index into a table). *)

module Loop_entry = struct
  type t =
    { loop : S2_loop.t option (* [None] represents the virtual root *)
    ; mutable children : int list (* indices into the entries table, in insertion order *)
    }
end

let insert_into_nesting ~entries ~root_index ~new_loop_index =
  let new_loop = (entries.(new_loop_index) : Loop_entry.t).loop in
  let new_loop = Option.value_exn new_loop in
  (* Descend until we find a parent whose children do not transitively contain
     new_loop. *)
  let parent_ix = ref root_index in
  let done_ = ref false in
  while not !done_ do
    let children = (entries.(!parent_ix) : Loop_entry.t).children in
    let deeper = ref None in
    List.iter children ~f:(fun child_ix ->
      match !deeper with
      | Some _ -> ()
      | None ->
        (match (entries.(child_ix) : Loop_entry.t).loop with
         | None -> ()
         | Some child_loop ->
           if S2_loop.contains_nested child_loop new_loop then deeper := Some child_ix));
    match !deeper with
    | Some ix -> parent_ix := ix
    | None -> done_ := true
  done;
  (* Now examine the parent's existing children: any that are nested in new_loop
     become its children instead. *)
  let parent_entry = (entries.(!parent_ix) : Loop_entry.t) in
  let kept = ref [] in
  let stolen = ref [] in
  List.iter parent_entry.children ~f:(fun child_ix ->
    match (entries.(child_ix) : Loop_entry.t).loop with
    | None -> kept := child_ix :: !kept
    | Some child_loop ->
      if S2_loop.contains_nested new_loop child_loop
      then stolen := child_ix :: !stolen
      else kept := child_ix :: !kept);
  parent_entry.children <- List.rev !kept @ [ new_loop_index ];
  let new_entry = (entries.(new_loop_index) : Loop_entry.t) in
  new_entry.children <- List.rev_append !stolen new_entry.children
;;

let initialise_nesting loops =
  let n = Array.length loops in
  (* entries.(0) is the virtual root; entries.(i+1) corresponds to loops.(i). *)
  let entries =
    Array.create ~len:(n + 1) ({ loop = None; children = [] } : Loop_entry.t)
  in
  entries.(0) <- { loop = None; children = [] };
  for i = 0 to n - 1 do
    entries.(i + 1) <- { loop = Some loops.(i); children = [] }
  done;
  for i = 0 to n - 1 do
    insert_into_nesting ~entries ~root_index:0 ~new_loop_index:(i + 1)
  done;
  (* Emit loops in pre-order traversal, setting depths. *)
  let output = ref [] in
  let rec walk ~depth ix =
    let entry = (entries.(ix) : Loop_entry.t) in
    (match entry.loop with
     | None -> ()
     | Some l ->
       S2_loop.set_depth l depth;
       output := l :: !output);
    let next_depth = if Option.is_none entry.loop then 0 else depth + 1 in
    List.iter entry.children ~f:(walk ~depth:next_depth)
  in
  walk ~depth:(-1) 0;
  Array.of_list (List.rev !output)
;;

let of_loops loops =
  if Array.length loops = 1 && S2_loop.is_empty loops.(0)
  then make_empty_polygon ()
  else if Array.length loops = 0
  then make_empty_polygon ()
  else (
    let reordered = initialise_nesting loops in
    make_polygon ~loops:reordered)
;;

let of_cell cell = of_loops [| S2_loop.of_cell cell |]

(* --- Oriented-loop constructor ------------------------------------------ *)

let rec of_oriented_loops loops =
  if Array.length loops = 0
  then make_empty_polygon ()
  else (
    (* Remember which loops originally contained the origin. *)
    let n = Array.length loops in
    let contained_origin = Array.map loops ~f:S2_loop.contains_origin in
    (* Normalise each loop so its area is at most 2 * pi, using the turning angle
       (curvature) with its reported error. *)
    let normalised =
      Array.map loops ~f:(fun l ->
        let curv = S2_loop.curvature l in
        let max_err = S2_loop.curvature_max_error l in
        let open Float_u.O in
        if Float_u.abs curv > max_err
        then if curv < #0.0 then S2_loop.invert l else l
        else if S2_loop.contains_origin l
        then S2_loop.invert l
        else l)
    in
    let p = of_loops normalised in
    if num_loops p = 0
    then p
    else (
      let mutable polygon_contains_origin = false in
      let mutable origin_loop_idx = 0 in
      for i = 0 to num_loops p - 1 do
        if S2_loop.contains_origin (loop p i)
        then (
          polygon_contains_origin <- not polygon_contains_origin;
          origin_loop_idx <- i)
      done;
      (* Find which slot of [normalised] matches this loop by physical identity,
         then look up the pre-normalisation origin-containment of that slot. *)
      let origin_loop = loop p origin_loop_idx in
      let mutable idx = 0 in
      for j = 0 to n - 1 do
        if phys_equal normalised.(j) origin_loop then idx <- j
      done;
      let original_contained = contained_origin.(idx) in
      if Bool.( <> ) original_contained polygon_contains_origin then invert p else p))

(* --- Area / centroid / invert ------------------------------------------- *)
and area t =
  let n = Array.length t.loops in
  let mutable total = #0.0 in
  for i = 0 to n - 1 do
    let l = t.loops.(i) in
    let open Float_u.O in
    total <- total + (Float_u.of_int (S2_loop.sign l) * S2_loop.area l)
  done;
  total

and centroid t =
  let n = Array.length t.loops in
  let mutable sum = R3_vector.zero in
  for i = 0 to n - 1 do
    let l = t.loops.(i) in
    let v = S2_loop.centroid l in
    if S2_loop.sign l < 0 then sum <- R3_vector.sub sum v else sum <- R3_vector.add sum v
  done;
  sum

and compare_loops_for_invert a b =
  (* Total ordering on loops that is independent of cyclic rotation / direction.
     Used to break ties when several loops have the same turning angle. *)
  let na = S2_loop.num_vertices a in
  let nb = S2_loop.num_vertices b in
  if na <> nb
  then na - nb
  else (
    let ca = S2_loop.canonical_first_vertex a in
    let cb = S2_loop.canonical_first_vertex b in
    if ca.#dir <> cb.#dir
    then ca.#dir - cb.#dir
    else (
      let mutable ai = ca.#first in
      let mutable bi = cb.#first in
      let mutable i = na - 1 in
      let mutable result = 0 in
      while i >= 0 && result = 0 do
        let c = R3_vector.compare (S2_loop.vertex a ai) (S2_loop.vertex b bi) in
        if c <> 0 then result <- c;
        ai <- ai + ca.#dir;
        bi <- bi + cb.#dir;
        i <- i - 1
      done;
      result))

and invert t =
  if is_empty t
  then make_full_polygon ()
  else if is_full t
  then make_empty_polygon ()
  else (
    (* Find the top-level loop (depth = 0) with the smallest turning angle. *)
    let n = Array.length t.loops in
    let mutable best = 0 in
    let mutable best_angle = S2_loop.curvature t.loops.(0) in
    let mutable have_angle = false in
    for i = 1 to n - 1 do
      if S2_loop.depth t.loops.(i) = 0
      then (
        if not have_angle then have_angle <- true;
        let angle = S2_loop.curvature t.loops.(i) in
        let lt = Float_u.( < ) angle best_angle in
        let eq = Float_u.( = ) angle best_angle in
        if lt || (eq && Int.( < ) (compare_loops_for_invert t.loops.(i) t.loops.(best)) 0)
        then (
          best <- i;
          best_angle <- angle))
    done;
    let _ = have_angle in
    let inverted_best = S2_loop.invert t.loops.(best) in
    let last_best = last_descendant t best in
    (* Build a fresh vertex-sharing copy of each loop so that mutating the new
       polygon's depths does not disturb [t]'s depth state. *)
    let new_loops = Array.create ~len:n inverted_best in
    let idx = ref 1 in
    for i = 0 to n - 1 do
      if i < best || i > last_best
      then (
        let l = S2_loop.of_vertices ~validate:false (S2_loop.vertices t.loops.(i)) in
        S2_loop.set_depth l (S2_loop.depth t.loops.(i) + 1);
        new_loops.(!idx) <- l;
        incr idx)
    done;
    for i = 0 to n - 1 do
      if i > best && i <= last_best
      then (
        let l = S2_loop.of_vertices ~validate:false (S2_loop.vertices t.loops.(i)) in
        S2_loop.set_depth l (S2_loop.depth t.loops.(i) - 1);
        new_loops.(!idx) <- l;
        incr idx)
    done;
    make_polygon ~loops:new_loops)
;;

(* --- Region interface --------------------------------------------------- *)

let brute_force_contains_point t p =
  Array.fold t.loops ~init:false ~f:(fun inside l ->
    Bool.( <> ) inside (S2_loop.contains_point l p))
;;

let contains_point t p =
  if not (S2_latlng_rect.contains_point t.bound p)
  then false
  else if t.num_vertices < 32
  then brute_force_contains_point t p
  else (
    let state = get_or_build_index t in
    S2_contains_point_query.shape_contains state.point_query ~shape_id:state.shape_id p)
;;

let boundary_approx_intersects_cell t cell =
  (* Brute-force: any loop edge crossing any cell edge signals a boundary crossing. *)
  let face = S2_cell.face cell in
  let padding =
    Float_u.O.(
      S2_edge_clipping.face_clip_error_uv_coord
      + S2_edge_clipping.intersects_rect_error_uv_dist)
  in
  let cell_bound = R2_rect.expanded_scalar (S2_cell.bound_uv cell) padding in
  let mutable hit = false in
  let num_loops_ = Array.length t.loops in
  let mutable li = 0 in
  while (not hit) && li < num_loops_ do
    let l = t.loops.(li) in
    let n = S2_loop.num_edges l in
    let mutable i = 0 in
    while (not hit) && i < n do
      let e = S2_loop.edge l i in
      let v0 = e.#v0 in
      let v1 = e.#v1 in
      (match S2_edge_clipping.clip_to_padded_face v0 v1 face ~padding with
       | None -> ()
       | Some clipped ->
         if S2_edge_clipping.intersects_rect clipped.a clipped.b cell_bound
         then hit <- true);
      i <- i + 1
    done;
    li <- li + 1
  done;
  hit
;;

let contains_cell t cell =
  if is_empty t
  then false
  else if is_full t
  then true
  else if not (S2_latlng_rect.contains_cell t.bound cell)
  then false
  else if boundary_approx_intersects_cell t cell
  then false
  else contains_point t (S2_cell.center cell)
;;

let may_intersect_cell t cell =
  if is_empty t
  then false
  else if is_full t
  then true
  else if not (S2_latlng_rect.intersects_cell t.bound cell)
  then false
  else if boundary_approx_intersects_cell t cell
  then true
  else contains_point t (S2_cell.center cell)
;;

let to_region t : S2_region.t =
  S2_region.custom
    #{ cap_bound = (fun () -> cap_bound t)
     ; rect_bound = (fun () -> rect_bound t)
     ; contains_cell = contains_cell t
     ; intersects_cell = may_intersect_cell t
     ; contains_point = contains_point t
     ; cell_union_bound = (fun () -> cell_union_bound t)
     }
;;

(* --- Polygon relations --------------------------------------------------- *)

let any_loop_contains t o = Array.exists t.loops ~f:(fun l -> S2_loop.contains l o)
let any_loop_intersects t o = Array.exists t.loops ~f:(fun l -> S2_loop.intersects l o)

let compare_boundary_loop t o =
  let mutable result = -1 in
  let i = ref 0 in
  let n = Array.length t.loops in
  while result <> 0 && !i < n do
    result <- result * -S2_loop.compare_boundary t.loops.(!i) o;
    incr i
  done;
  result
;;

let contains_boundary t o =
  Array.for_all o.loops ~f:(fun l -> compare_boundary_loop t l > 0)
;;

let excludes_boundary t o =
  Array.for_all o.loops ~f:(fun l -> compare_boundary_loop t l < 0)
;;

let contains_non_crossing_boundary t o ~reverse =
  Array.fold t.loops ~init:false ~f:(fun inside l ->
    Bool.( <> ) inside (S2_loop.contains_non_crossing_boundary l o ~reverse))
;;

let excludes_non_crossing_shells t o =
  (* [t] excludes all shells of [o]? *)
  Array.for_all o.loops ~f:(fun l ->
    S2_loop.is_hole l || not (contains_non_crossing_boundary t l ~reverse:false))
;;

let excludes_non_crossing_complement_shells t o =
  if is_empty o
  then not (is_full t)
  else if is_full o
  then true
  else (
    let n = Array.length o.loops in
    let mutable ok = true in
    for j = 0 to n - 1 do
      if ok
      then (
        let l = o.loops.(j) in
        if j > 0 && not (S2_loop.is_hole l)
        then ()
        else if contains_non_crossing_boundary t l ~reverse:(j = 0)
        then ok <- false)
    done;
    ok)
;;

(* Contains / Intersects route through S2_boolean_operation so that the
   shared-boundary semantics (shells touch but do not cross each other,
   holes handled as inverted shells) match the canonical implementation
   in s2polygon.cc:1003-1126. The single-loop fast path stays direct for
   speed; all other cases go through the boolean-operation predicate. *)
let boolean_input t =
  let state = get_or_build_index t in
  S2_boolean_operation.Polygon_input.create
    ~shape:(to_shape t)
    ~shape_index:state.index
    ~shape_id:state.shape_id
    ~area:(area t)
    ~is_empty:(is_empty t)
    ~is_full:(is_full t)
;;

let contains a b =
  if Array.length a.loops = 1 && Array.length b.loops = 1
  then S2_loop.contains a.loops.(0) b.loops.(0)
  else if (not (S2_latlng_rect.contains a.subregion_bound b.bound))
          && not
               (S1_interval.is_full
                  (S1_interval.union
                     (S2_latlng_rect.lng a.bound)
                     (S2_latlng_rect.lng b.bound)))
  then false
  else S2_boolean_operation.contains (boolean_input a) (boolean_input b)
;;

let intersects a b =
  if Array.length a.loops = 1 && Array.length b.loops = 1
  then S2_loop.intersects a.loops.(0) b.loops.(0)
  else if not (S2_latlng_rect.intersects a.bound b.bound)
  then false
  else S2_boolean_operation.intersects (boolean_input a) (boolean_input b)
;;

let equal a b =
  Array.length a.loops = Array.length b.loops
  && Array.for_alli a.loops ~f:(fun i la -> S2_loop.equal la b.loops.(i))
;;

(* --- Validation --------------------------------------------------------- *)

let find_loop_nesting_error t =
  let n = Array.length t.loops in
  let mutable err = None in
  (* Depth monotonicity. *)
  let mutable last_depth = -1 in
  let mutable i = 0 in
  while Option.is_none err && i < n do
    let d = S2_loop.depth t.loops.(i) in
    if d < 0 || d > last_depth + 1
    then err <- Some (sprintf "loop %d: invalid loop depth (%d)" i d);
    last_depth <- d;
    i <- i + 1
  done;
  (* Nesting correspondence. *)
  if Option.is_none err
  then (
    let i = ref 0 in
    while Option.is_none err && !i < n do
      let l = t.loops.(!i) in
      let last = last_descendant t !i in
      let j = ref 0 in
      while Option.is_none err && !j < n do
        if !i <> !j
        then (
          let l2 = t.loops.(!j) in
          let nested = !j >= !i + 1 && !j <= last in
          if Bool.( <> )
               (S2_loop.contains_non_crossing_boundary l l2 ~reverse:false)
               nested
          then (
            let msg =
              if nested
              then sprintf "invalid nesting: loop %d should contain loop %d" !i !j
              else sprintf "invalid nesting: loop %d should not contain loop %d" !i !j
            in
            err <- Some msg));
        incr j
      done;
      incr i
    done);
  err
;;

let find_validation_error t =
  let n = Array.length t.loops in
  let mutable err = None in
  let mutable i = 0 in
  while Option.is_none err && i < n do
    let l = t.loops.(i) in
    (match S2_loop.find_validation_error l with
     | Some msg -> err <- Some (sprintf "loop %d: %s" i msg)
     | None ->
       if S2_loop.is_empty l
       then err <- Some (sprintf "loop %d: empty loops are not allowed" i)
       else if S2_loop.is_full l && n > 1
       then err <- Some (sprintf "loop %d: full loop appears in non-full polygon" i));
    i <- i + 1
  done;
  match err with
  | Some _ -> err
  | None -> find_loop_nesting_error t
;;

let is_valid t = Option.is_none (find_validation_error t)

(* TODO: port set operations (InitToUnion, InitToIntersection, InitToDifference,
   InitToSymmetricDifference, InitToSnapped, InitToSimplified) from
   s2polygon.cc:2195-2360. *)
(* TODO: port polyline operations (IntersectWithPolyline, SubtractFromPolyline,
   ApproxIntersectWithPolyline) from s2polygon.cc:2362-2454. *)
(* TODO: port distance queries (GetDistance, GetDistanceToBoundary, Project) from
   s2polygon.cc:1543-1606. *)
(* TODO: port ApproxContains / ApproxDisjoint / BoundaryNear from
   s2polygon.cc:1128-1180. *)
(* TODO: port Encode / Decode from s2polygon.cc:1608-1804. *)
