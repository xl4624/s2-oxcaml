open Core

(* Pack a (shape_id, edge_id) pair into a single OCaml int so a [Hashtbl.t] keyed by the
   pair never has to allocate a boxed tuple per [mem]/[set]. Both ids are dense
   non-negative integers; OCaml ints are 63 bits on 64-bit, so [shape_id] takes the high
   31 bits and [edge_id] the low 32 bits without overlap for any realistic index (a shape
   with > 4G edges, or > 2G shapes in one index, would alias). *)
let[@inline] [@zero_alloc] pack_edge_key ~shape_id ~edge_id =
  (shape_id lsl 32) lor edge_id
;;

module Options = struct
  type t =
    { mutable max_results : int
    ; mutable max_distance : S1_chord_angle.t
    ; mutable max_error : S1_chord_angle.t
    ; mutable include_interiors : bool
    ; mutable use_brute_force : bool
    }

  let create ?(include_interiors = true) ?(use_brute_force = false) () =
    { max_results = Int.max_value
    ; max_distance = S1_chord_angle.infinity
    ; max_error = S1_chord_angle.zero
    ; include_interiors
    ; use_brute_force
    }
  ;;

  let copy t =
    { max_results = t.max_results
    ; max_distance = t.max_distance
    ; max_error = t.max_error
    ; include_interiors = t.include_interiors
    ; use_brute_force = t.use_brute_force
    }
  ;;

  let max_results t = t.max_results
  let max_distance t = t.max_distance
  let max_error t = t.max_error
  let include_interiors t = t.include_interiors
  let use_brute_force t = t.use_brute_force

  let with_max_results t n =
    if n < 1
    then raise_s [%message "S2_closest_edge_query.Options.with_max_results requires n>=1"];
    let t = copy t in
    t.max_results <- n;
    t
  ;;

  let with_max_distance t d =
    let t = copy t in
    t.max_distance <- d;
    t
  ;;

  let with_inclusive_max_distance t limit =
    with_max_distance t (S1_chord_angle.successor limit)
  ;;

  let with_conservative_max_distance t limit =
    let err = S2_edge_distances.get_update_min_distance_max_error limit in
    let expanded = S1_chord_angle.plus_error limit err in
    with_max_distance t (S1_chord_angle.successor expanded)
  ;;

  let with_max_error t e =
    let t = copy t in
    t.max_error <- e;
    t
  ;;

  let with_include_interiors t b =
    let t = copy t in
    t.include_interiors <- b;
    t
  ;;

  let with_use_brute_force t b =
    let t = copy t in
    t.use_brute_force <- b;
    t
  ;;
end

module Result = struct
  type t =
    { distance : S1_chord_angle.t
    ; shape_id : int
    ; edge_id : int
    }
  [@@deriving sexp_of]

  let compare x y =
    let c = S1_chord_angle.compare x.distance y.distance in
    if c <> 0
    then c
    else (
      let c = Int.compare x.shape_id y.shape_id in
      if c <> 0 then c else Int.compare x.edge_id y.edge_id)
  ;;

  let equal x y = compare x y = 0
  let is_interior t = t.shape_id >= 0 && t.edge_id < 0
  let is_empty t = t.shape_id < 0
  let empty = { distance = S1_chord_angle.infinity; shape_id = -1; edge_id = -1 }
end

type t =
  { index : S2_shape_index.t
  ; mutable options : Options.t
  ; iter : S2_shape_index.Iterator.t
  ; mutable index_num_edges : int
  ; mutable index_num_edges_limit : int
  ; mutable index_covering : S2_cell_id.t array
  ; mutable index_cells : S2_shape_index.Index_cell.Option.t array
  }

module Target = struct
  type point = { pt : S2_point.t }

  type edge =
    { a : S2_point.t
    ; b : S2_point.t
    }

  type cell = { cell : S2_cell.t }

  type shape_index =
    { index : S2_shape_index.t
    ; include_interiors : bool
    ; use_brute_force : bool
    ; mutable cached_query : t option
    }

  type t =
    | Point of point
    | Edge of edge
    | Cell of cell
    | Shape_index of shape_index

  let point pt = Point { pt }
  let edge a b = Edge { a; b }
  let cell cell = Cell { cell }

  let shape_index ?(include_interiors = true) ?(use_brute_force = false) index =
    Shape_index { index; include_interiors; use_brute_force; cached_query = None }
  ;;
end

(* Brute-force index-size thresholds per target type. Values come from the
   benchmark-derived constants in s2closest_edge_query.cc:36-63. Below these sizes,
   scanning every edge of the indexed geometry beats the priority-queue descent in the
   general case. *)
let max_brute_force_index_size = function
  | Target.Point _ -> 120
  | Target.Edge _ -> 60
  | Target.Cell _ -> 30
  | Target.Shape_index _ -> 25
;;

(* TODO: port VisitClosestEdges / VisitClosestShapes visitor-style enumeration and the
   ShapeFilter parameter from s2closest_edge_query.h:270-289. The current implementation
   always returns a materialized result list. *)

let target_uses_max_error = function
  | Target.Point _ | Edge _ | Cell _ -> false
  | Shape_index _ -> true
;;

let target_cap_bound (target : Target.t) =
  match target with
  | Point { pt } -> S2_cap.of_center_chord_angle pt S1_chord_angle.zero
  | Edge { a; b } ->
    let open Float_u.O in
    let d2 = S1_chord_angle.length2 (S2_point.chord_angle_between a b) in
    let r2 = #0.5 * d2 / (#1.0 + Float_u.sqrt (#1.0 - (#0.25 * d2))) in
    let center = R3_vector.normalize (R3_vector.add a b) in
    S2_cap.of_center_chord_angle center (S1_chord_angle.of_length2 r2)
  | Cell { cell } -> S2_cell.cap_bound cell
  | Shape_index { index; _ } ->
    let iter = S2_shape_index.iterator index in
    S2_shape_index.Iterator.begin_at iter;
    if S2_shape_index.Iterator.is_done iter
    then S2_cap.empty
    else (
      let mutable cap = S2_cap.empty in
      while not (S2_shape_index.Iterator.is_done iter) do
        let id = S2_shape_index.Iterator.cell_id iter in
        let cc = S2_cell.cap_bound (S2_cell.of_cell_id id) in
        cap <- S2_cap.add_cap cap cc;
        S2_shape_index.Iterator.next iter
      done;
      cap)
;;

let visit_containing_shape_ids (target : Target.t) query_index ~f =
  let query = S2_contains_point_query.create query_index ~vertex_model:Semi_open () in
  match target with
  | Point { pt } ->
    S2_contains_point_query.visit_containing_shapes query pt ~f:(fun id -> f id pt)
  | Edge { a; b } ->
    let mid = R3_vector.normalize (R3_vector.add a b) in
    S2_contains_point_query.visit_containing_shapes query mid ~f:(fun id -> f id mid)
  | Cell { cell } ->
    let c = S2_cell.center cell in
    S2_contains_point_query.visit_containing_shapes query c ~f:(fun id -> f id c)
  | Shape_index { index = target_index; _ } ->
    let continue_ = ref true in
    let n = S2_shape_index.num_shape_ids target_index in
    let i = ref 0 in
    while !continue_ && !i < n do
      let shape = S2_shape_index.shape target_index !i in
      let nc = shape.#num_chains in
      let tested = ref false in
      let j = ref 0 in
      while !continue_ && !j < nc do
        let #{ start = _; length } : S2_shape.Chain.t = shape.#chain !j in
        if length > 0
        then (
          tested := true;
          let #{ v0; v1 = _ } : S2_shape.Edge.t = shape.#chain_edge !j 0 in
          if not
               (S2_contains_point_query.visit_containing_shapes query v0 ~f:(fun id ->
                  f id v0))
          then continue_ := false);
        incr j
      done;
      if !continue_ && not !tested
      then (
        let ref_pt : S2_shape.Reference_point.t = shape.#reference_point in
        if ref_pt.#contained
           && not
                (S2_contains_point_query.visit_containing_shapes
                   query
                   ref_pt.#point
                   ~f:(fun id -> f id ref_pt.#point))
        then continue_ := false);
      incr i
    done;
    !continue_
;;

(* Min-heap of cell frontier entries keyed on lower-bound distance. Stored as three
   parallel arrays rather than an array of records so that S1_chord_angle (a float#) and
   S2_cell_id (int64#) slots stay tightly packed, and so that the [Index_cell.Option.t]
   slot is only written when an indexed cell is known. Standard binary-heap layout:
   children of index i are at 2i+1 and 2i+2. *)
module Cell_queue = struct
  type entry =
    #{ distance : S1_chord_angle.t
     ; id : S2_cell_id.t
     ; index_cell : S2_shape_index.Index_cell.Option.t
     }

  type t =
    { mutable distances : S1_chord_angle.t array
    ; mutable ids : S2_cell_id.t array
    ; mutable cells : S2_shape_index.Index_cell.Option.t array
    ; mutable size : int
    }

  let initial_cap = 16
  let create () = { distances = [||]; ids = [||]; cells = [||]; size = 0 }
  let[@inline] is_empty t = t.size = 0

  let clear t =
    for i = 0 to t.size - 1 do
      t.cells.(i) <- S2_shape_index.Index_cell.Option.none
    done;
    t.size <- 0
  ;;

  let grow t =
    let cap = Array.length t.distances in
    let new_cap = if cap = 0 then initial_cap else cap * 2 in
    let new_d = Array.create ~len:new_cap S1_chord_angle.zero in
    let new_i = Array.create ~len:new_cap S2_cell_id.none in
    let new_c = Array.create ~len:new_cap S2_shape_index.Index_cell.Option.none in
    for i = 0 to t.size - 1 do
      new_d.(i) <- t.distances.(i);
      new_i.(i) <- t.ids.(i);
      new_c.(i) <- t.cells.(i)
    done;
    t.distances <- new_d;
    t.ids <- new_i;
    t.cells <- new_c
  ;;

  let[@inline] less t i j = S1_chord_angle.compare t.distances.(i) t.distances.(j) < 0

  let[@inline] swap t i j =
    let d = t.distances.(i) in
    t.distances.(i) <- t.distances.(j);
    t.distances.(j) <- d;
    let id = t.ids.(i) in
    t.ids.(i) <- t.ids.(j);
    t.ids.(j) <- id;
    let c = t.cells.(i) in
    t.cells.(i) <- t.cells.(j);
    t.cells.(j) <- c
  ;;

  let sift_up t i_start =
    let mutable i = i_start in
    let mutable cont = true in
    while cont && i > 0 do
      let parent = (i - 1) / 2 in
      if less t i parent
      then (
        swap t i parent;
        i <- parent)
      else cont <- false
    done
  ;;

  let sift_down t =
    let mutable i = 0 in
    let n = t.size in
    let mutable cont = true in
    while cont do
      let l = (2 * i) + 1 in
      let r = (2 * i) + 2 in
      let mutable best = i in
      if l < n && less t l best then best <- l;
      if r < n && less t r best then best <- r;
      if best <> i
      then (
        swap t i best;
        i <- best)
      else cont <- false
    done
  ;;

  let add t (entry : entry) =
    if t.size >= Array.length t.distances then grow t;
    let i = t.size in
    t.distances.(i) <- entry.#distance;
    t.ids.(i) <- entry.#id;
    t.cells.(i) <- entry.#index_cell;
    t.size <- i + 1;
    sift_up t i
  ;;

  let pop_exn t : entry =
    if t.size = 0
    then (
      match raise_s [%message "Cell_queue.pop_exn: empty"] with
      | (_ : Nothing.t) -> .);
    let d = t.distances.(0) in
    let id = t.ids.(0) in
    let cell = t.cells.(0) in
    let last = t.size - 1 in
    if last > 0
    then (
      t.distances.(0) <- t.distances.(last);
      t.ids.(0) <- t.ids.(last);
      t.cells.(0) <- t.cells.(last));
    t.cells.(last) <- S2_shape_index.Index_cell.Option.none;
    t.size <- last;
    sift_down t;
    #{ distance = d; id; index_cell = cell }
  ;;
end

let create ?(options = Options.create ()) index () =
  (* [S2_shape_index.iterator] calls [build] internally and is idempotent, so constructing
     the iterator eagerly avoids both the [Some _]-on-first-use allocation and the
     lazy-init bookkeeping. The iterator is a small mutable struct, so the up-front cost
     is negligible even for queries that never run. *)
  let iter = S2_shape_index.iterator index in
  { index
  ; options
  ; iter
  ; index_num_edges = 0
  ; index_num_edges_limit = 0
  ; index_covering = [||]
  ; index_cells = [||]
  }
;;

let index t = t.index
let options t = t.options
let set_options t o = t.options <- o
let[@inline] [@zero_alloc] ensure_iter t = t.iter

let count_edges_up_to index ~limit =
  let n = S2_shape_index.num_shape_ids index in
  let total = ref 0 in
  let i = ref 0 in
  while !i < n && !total < limit do
    let shape = S2_shape_index.shape index !i in
    total := !total + shape.#num_edges;
    incr i
  done;
  !total
;;

type ctx =
  { query : t
  ; target : Target.t
  ; opts : Options.t
  ; mutable target_uses_max_error : bool
  ; mutable use_conservative_cell_distance : bool
  ; mutable distance_limit : S1_chord_angle.t
  ; mutable avoid_duplicates : bool
  ; mutable result_singleton : Result.t
  ; mutable result_vector : Result.t list
      (* [result_set]: kept sorted in ascending [Result.compare] order, with length at
         most [opts.max_results]. Used when [max_results] is both > 1 and < max_value. *)
  ; mutable result_set : Result.t list
      (* [tested_edges] deduplicates (shape_id, edge_id) lookups when the target may
         re-enqueue the same edge across multiple cells. Only populated when
         [avoid_duplicates] is set, which happens for [Shape_index] targets with
         [max_results > 1] and a non-zero [max_error]. The key is the pair packed into a
         single int via [pack_edge_key] so the hashtable is keyed by an immediate (no
         per-lookup tuple allocation). *)
  ; tested_edges : (int, unit) Hashtbl.t
  ; queue : Cell_queue.t
  }

let make_ctx query target opts =
  let queue = Cell_queue.create () in
  { query
  ; target
  ; opts
  ; target_uses_max_error = false
  ; use_conservative_cell_distance = false
  ; distance_limit = opts.Options.max_distance
  ; avoid_duplicates = false
  ; result_singleton = Result.empty
  ; result_vector = []
  ; result_set = []
  ; tested_edges = Hashtbl.create (module Int)
  ; queue
  }
;;

let add_result ctx (r : Result.t) =
  if ctx.opts.max_results = 1
  then (
    ctx.result_singleton <- r;
    ctx.distance_limit <- S1_chord_angle.sub r.distance ctx.opts.max_error)
  else if ctx.opts.max_results = Int.max_value
  then ctx.result_vector <- r :: ctx.result_vector
  else (
    (* Sorted insert (dedup) then truncate to [max_results]. *)
    let rec insert acc = function
      | [] -> List.rev (r :: acc)
      | x :: rest ->
        let c = Result.compare r x in
        if c < 0
        then List.rev_append acc (r :: x :: rest)
        else if c = 0
        then List.rev_append acc (x :: rest)
        else insert (x :: acc) rest
    in
    let inserted = insert [] ctx.result_set in
    let truncated = List.take inserted ctx.opts.max_results in
    let size = List.length truncated in
    ctx.result_set <- truncated;
    if size >= ctx.opts.max_results
    then (
      let last = List.last_exn truncated in
      ctx.distance_limit <- S1_chord_angle.sub last.distance ctx.opts.max_error))
;;

(* Clone an iterator by position. S2_shape_index.Iterator is mutable and does not expose a
   copy operation, so to snapshot a position we build a fresh iterator and seek it to the
   target cell id. Used during initial-covering construction, where we need a second
   cursor without disturbing the main one. *)
let clone_iter_at index cur_id =
  let it = S2_shape_index.iterator index in
  S2_shape_index.Iterator.seek it cur_id;
  it
;;

let init_covering t =
  let next = S2_shape_index.iterator t.index in
  S2_shape_index.Iterator.begin_at next;
  let last_iter = S2_shape_index.iterator t.index in
  S2_shape_index.Iterator.at_end last_iter;
  let _ = S2_shape_index.Iterator.prev last_iter in
  let cov_cap = 6 in
  let cov_arr = Array.create ~len:cov_cap S2_cell_id.none in
  let cell_arr = Array.create ~len:cov_cap S2_shape_index.Index_cell.Option.none in
  let count = ref 0 in
  let push_cover id cell_opt =
    cov_arr.(!count) <- id;
    cell_arr.(!count) <- cell_opt;
    incr count
  in
  let add_initial_range first_id first_cell last_id =
    if S2_cell_id.equal first_id last_id
    then push_cover first_id (S2_shape_index.Index_cell.Option.some first_cell)
    else (
      let level = S2_cell_id.get_common_ancestor_level first_id last_id in
      push_cover
        (S2_cell_id.parent_level first_id level)
        S2_shape_index.Index_cell.Option.none)
  in
  if not (S2_shape_index.Iterator.is_done next)
  then (
    let last_id0 = S2_shape_index.Iterator.cell_id last_iter in
    let last_cell0 = S2_shape_index.Iterator.index_cell last_iter in
    let first_id0 = S2_shape_index.Iterator.cell_id next in
    if not (S2_cell_id.equal first_id0 last_id0)
    then (
      let level = S2_cell_id.get_common_ancestor_level first_id0 last_id0 + 1 in
      let last_parent = S2_cell_id.parent_level last_id0 level in
      let mutable cur_id = S2_cell_id.parent_level first_id0 level in
      let keep_going = ref true in
      while !keep_going && not (S2_cell_id.equal cur_id last_parent) do
        let next_id = S2_shape_index.Iterator.cell_id next in
        if S2_cell_id.compare (S2_cell_id.range_max cur_id) next_id < 0
        then cur_id <- S2_cell_id.next cur_id
        else (
          let cell_first_id = next_id in
          let cell_first_cell = S2_shape_index.Iterator.index_cell next in
          S2_shape_index.Iterator.seek
            next
            (S2_cell_id.next (S2_cell_id.range_max cur_id));
          let cell_last_id =
            if S2_shape_index.Iterator.is_done next
            then last_id0
            else (
              let it = clone_iter_at t.index (S2_shape_index.Iterator.cell_id next) in
              let _ = S2_shape_index.Iterator.prev it in
              S2_shape_index.Iterator.cell_id it)
          in
          add_initial_range cell_first_id cell_first_cell cell_last_id;
          cur_id <- S2_cell_id.next cur_id;
          if S2_shape_index.Iterator.is_done next then keep_going := false)
      done);
    if not (S2_shape_index.Iterator.is_done next)
    then (
      let fid = S2_shape_index.Iterator.cell_id next in
      let fc = S2_shape_index.Iterator.index_cell next in
      let _ = last_cell0 in
      add_initial_range fid fc last_id0));
  t.index_covering <- (Array.sub [@kind bits64]) cov_arr ~pos:0 ~len:!count;
  t.index_cells <- Array.sub cell_arr ~pos:0 ~len:!count
;;

let count_cell_edges (cell : S2_shape_index.Index_cell.t) =
  let nc = S2_shape_index.Index_cell.num_clipped cell in
  let total = ref 0 in
  for s = 0 to nc - 1 do
    total
    := !total
       + S2_shape_index.Clipped_shape.num_edges (S2_shape_index.Index_cell.clipped cell s)
  done;
  !total
;;

let rec find_closest_edge ?(overrides = None) query target =
  let base = query.options in
  let base = Options.with_max_results base 1 in
  let opts =
    match overrides with
    | None -> base
    | Some f -> f base
  in
  let ctx = make_ctx query target opts in
  find_closest_edges_internal ctx;
  if Result.is_empty ctx.result_singleton then Result.empty else ctx.result_singleton

and find_closest_edges query target =
  let ctx = make_ctx query target query.options in
  find_closest_edges_internal ctx;
  collect_results ctx

and collect_results ctx =
  if ctx.opts.max_results = 1
  then if Result.is_empty ctx.result_singleton then [] else [ ctx.result_singleton ]
  else if ctx.opts.max_results = Int.max_value
  then
    List.sort ctx.result_vector ~compare:Result.compare
    |> List.remove_consecutive_duplicates ~equal:Result.equal
  else ctx.result_set

and find_closest_edges_internal ctx =
  Hashtbl.clear ctx.tested_edges;
  if S1_chord_angle.is_zero ctx.distance_limit
  then ()
  else (
    if ctx.opts.include_interiors
    then (
      let seen = Hash_set.create (module Int) in
      let max_r = ctx.opts.max_results in
      let _ =
        visit_containing_shape_ids ctx.target ctx.query.index ~f:(fun id _ ->
          Hash_set.add seen id;
          Hash_set.length seen < max_r)
      in
      Hash_set.iter seen ~f:(fun id ->
        add_result ctx { distance = S1_chord_angle.zero; shape_id = id; edge_id = -1 }));
    if S1_chord_angle.is_zero ctx.distance_limit
    then ()
    else (
      ctx.target_uses_max_error
      <- (not (S1_chord_angle.is_zero ctx.opts.max_error))
         && target_uses_max_error ctx.target;
      ctx.use_conservative_cell_distance
      <- ctx.target_uses_max_error
         && (S1_chord_angle.is_infinity ctx.distance_limit
             || S1_chord_angle.compare
                  S1_chord_angle.zero
                  (S1_chord_angle.sub ctx.distance_limit ctx.opts.max_error)
                < 0);
      let min_optimized = max_brute_force_index_size ctx.target + 1 in
      if min_optimized > ctx.query.index_num_edges_limit
         && ctx.query.index_num_edges >= ctx.query.index_num_edges_limit
      then (
        ctx.query.index_num_edges
        <- count_edges_up_to ctx.query.index ~limit:min_optimized;
        ctx.query.index_num_edges_limit <- min_optimized);
      if ctx.opts.use_brute_force || ctx.query.index_num_edges < min_optimized
      then (
        ctx.avoid_duplicates <- false;
        find_edges_brute_force ctx)
      else (
        ctx.avoid_duplicates <- ctx.target_uses_max_error && ctx.opts.max_results > 1;
        find_edges_optimized ctx)))

and find_edges_brute_force ctx =
  let n = S2_shape_index.num_shape_ids ctx.query.index in
  for shape_id = 0 to n - 1 do
    let shape = S2_shape_index.shape ctx.query.index shape_id in
    let ne = shape.#num_edges in
    for e = 0 to ne - 1 do
      maybe_add_result ctx shape shape_id e
    done
  done

and find_edges_optimized ctx =
  init_queue ctx;
  let stop = ref false in
  while (not !stop) && not (Cell_queue.is_empty ctx.queue) do
    let entry = Cell_queue.pop_exn ctx.queue in
    if S1_chord_angle.compare entry.#distance ctx.distance_limit >= 0
    then (
      Cell_queue.clear ctx.queue;
      stop := true)
    else (
      match%optional_u.S2_shape_index.Index_cell.Option entry.#index_cell with
      | Some _ -> process_edges ctx entry
      | None ->
        let iter = ensure_iter ctx.query in
        let id = entry.#id in
        let ch1 = S2_cell_id.child_exn id 1 in
        let ch3 = S2_cell_id.child_exn id 3 in
        S2_shape_index.Iterator.seek iter (S2_cell_id.range_min ch1);
        if (not (S2_shape_index.Iterator.is_done iter))
           && S2_cell_id.compare
                (S2_shape_index.Iterator.cell_id iter)
                (S2_cell_id.range_max ch1)
              <= 0
        then process_or_enqueue_from_iter ctx iter ch1;
        if S2_shape_index.Iterator.prev iter
           && S2_cell_id.compare
                (S2_shape_index.Iterator.cell_id iter)
                (S2_cell_id.range_min id)
              >= 0
        then (
          let ch0 = S2_cell_id.child_exn id 0 in
          process_or_enqueue_from_iter ctx iter ch0);
        S2_shape_index.Iterator.seek iter (S2_cell_id.range_min ch3);
        if (not (S2_shape_index.Iterator.is_done iter))
           && S2_cell_id.compare
                (S2_shape_index.Iterator.cell_id iter)
                (S2_cell_id.range_max id)
              <= 0
        then process_or_enqueue_from_iter ctx iter ch3;
        if S2_shape_index.Iterator.prev iter
           && S2_cell_id.compare
                (S2_shape_index.Iterator.cell_id iter)
                (S2_cell_id.range_min (S2_cell_id.child_exn id 2))
              >= 0
        then (
          let ch2 = S2_cell_id.child_exn id 2 in
          process_or_enqueue_from_iter ctx iter ch2))
  done

and process_edges ctx (entry : Cell_queue.entry) =
  match%optional_u.S2_shape_index.Index_cell.Option entry.#index_cell with
  | None -> ()
  | Some cell ->
    let nc = S2_shape_index.Index_cell.num_clipped cell in
    for s = 0 to nc - 1 do
      let clipped = S2_shape_index.Index_cell.clipped cell s in
      let shape_id = S2_shape_index.Clipped_shape.shape_id clipped in
      let shape = S2_shape_index.shape ctx.query.index shape_id in
      let ne = S2_shape_index.Clipped_shape.num_edges clipped in
      for j = 0 to ne - 1 do
        let eid = S2_shape_index.Clipped_shape.edge clipped j in
        maybe_add_result ctx shape shape_id eid
      done
    done

and process_or_enqueue_from_iter ctx iter id =
  if S2_cell_id.equal (S2_shape_index.Iterator.cell_id iter) id
  then
    process_or_enqueue
      ctx
      id
      (S2_shape_index.Index_cell.Option.some (S2_shape_index.Iterator.index_cell iter))
  else process_or_enqueue ctx id S2_shape_index.Index_cell.Option.none

and process_or_enqueue ctx id index_cell =
  match%optional_u.S2_shape_index.Index_cell.Option index_cell with
  | Some cell ->
    let n = count_cell_edges cell in
    if n = 0
    then ()
    else if n < 10
    then
      process_edges
        ctx
        #{ distance = S1_chord_angle.zero
         ; id
         ; index_cell = S2_shape_index.Index_cell.Option.some cell
         }
    else (
      let cell_shape = S2_cell.of_cell_id id in
      match%optional_u.S1_chord_angle.Option
        update_min_distance_to_cell ctx.target cell_shape ctx.distance_limit
      with
      | None -> ()
      | Some d ->
        let d =
          if ctx.use_conservative_cell_distance
          then S1_chord_angle.sub d ctx.opts.max_error
          else d
        in
        Cell_queue.add
          ctx.queue
          #{ distance = d; id; index_cell = S2_shape_index.Index_cell.Option.some cell })
  | None ->
    let cell_shape = S2_cell.of_cell_id id in
    (match%optional_u.S1_chord_angle.Option
       update_min_distance_to_cell ctx.target cell_shape ctx.distance_limit
     with
     | None -> ()
     | Some d ->
       let d =
         if ctx.use_conservative_cell_distance
         then S1_chord_angle.sub d ctx.opts.max_error
         else d
       in
       Cell_queue.add
         ctx.queue
         #{ distance = d; id; index_cell = S2_shape_index.Index_cell.Option.none })

and init_queue ctx =
  let cap = target_cap_bound ctx.target in
  if S2_cap.is_empty cap
  then ()
  else (
    let iter = ensure_iter ctx.query in
    let t = ctx.query in
    if ctx.opts.max_results = 1
    then (
      let center = S2_cap.center cap in
      if S2_shape_index.Iterator.locate_point iter center
      then
        process_edges
          ctx
          #{ distance = S1_chord_angle.zero
           ; id = S2_shape_index.Iterator.cell_id iter
           ; index_cell =
               S2_shape_index.Index_cell.Option.some
                 (S2_shape_index.Iterator.index_cell iter)
           });
    if S1_chord_angle.is_zero ctx.distance_limit
    then ()
    else (
      if Array.length t.index_covering = 0 then init_covering t;
      if S1_chord_angle.is_infinity ctx.distance_limit
      then
        for i = 0 to Array.length t.index_covering - 1 do
          process_or_enqueue ctx t.index_covering.(i) t.index_cells.(i)
        done
      else (
        let radius =
          S1_angle.add
            (S1_chord_angle.to_angle (S2_cap.radius_chord cap))
            (S1_chord_angle.to_angle
               (S1_chord_angle.plus_error
                  ctx.distance_limit
                  (S1_chord_angle.max_angle_error ctx.distance_limit)))
        in
        let search_cap = S2_cap.of_center_angle (S2_cap.center cap) radius in
        let coverer = S2_region_coverer.create ~max_cells:4 ~level_mod:1 () in
        let max_dist_cover =
          S2_region_coverer.fast_covering coverer (S2_region.of_cap search_cap)
        in
        let index_union = S2_cell_union.from_verbatim t.index_covering in
        let initial =
          S2_cell_union.intersection index_union max_dist_cover
          |> S2_cell_union.cell_ids_raw
        in
        let n = Array.length initial in
        let i = ref 0 in
        let j = ref 0 in
        while !i < n do
          let id_i = initial.(!i) in
          while
            !j < Array.length t.index_covering
            && S2_cell_id.compare (S2_cell_id.range_max t.index_covering.(!j)) id_i < 0
          do
            incr j
          done;
          if !j >= Array.length t.index_covering
          then i := n
          else (
            let id_j = t.index_covering.(!j) in
            if S2_cell_id.equal id_i id_j
            then (
              process_or_enqueue ctx id_j t.index_cells.(!j);
              incr i;
              incr j)
            else (
              match S2_shape_index.Iterator.locate_cell_id iter id_i with
              | Indexed ->
                process_or_enqueue
                  ctx
                  (S2_shape_index.Iterator.cell_id iter)
                  (S2_shape_index.Index_cell.Option.some
                     (S2_shape_index.Iterator.index_cell iter));
                let last_id =
                  S2_cell_id.range_max (S2_shape_index.Iterator.cell_id iter)
                in
                incr i;
                while !i < n && S2_cell_id.compare initial.(!i) last_id <= 0 do
                  incr i
                done
              | Subdivided ->
                process_or_enqueue ctx id_i S2_shape_index.Index_cell.Option.none;
                incr i
              | Disjoint -> incr i))
        done)))

and maybe_add_result ctx (shape : S2_shape.t) shape_id edge_id =
  let seen =
    if ctx.avoid_duplicates
    then (
      let key = pack_edge_key ~shape_id ~edge_id in
      if Hashtbl.mem ctx.tested_edges key
      then true
      else (
        Hashtbl.set ctx.tested_edges ~key ~data:();
        false))
    else false
  in
  if not seen
  then (
    let #{ v0; v1 } : S2_shape.Edge.t = shape.#edge edge_id in
    match%optional_u.S1_chord_angle.Option
      update_min_distance_to_edge ctx.target v0 v1 ctx.distance_limit
    with
    | None -> ()
    | Some d -> add_result ctx { distance = d; shape_id; edge_id })

and update_min_distance_to_edge (target : Target.t) v0 v1 cur =
  match target with
  | Point { pt } -> S2_edge_distances.update_min_distance pt v0 v1 cur
  | Edge { a; b } -> S2_edge_distances.update_edge_pair_min_distance a b v0 v1 cur
  | Cell { cell } ->
    let d = S2_cell.distance_to_edge cell v0 v1 in
    if S1_chord_angle.compare d cur < 0
    then S1_chord_angle.Option.some d
    else S1_chord_angle.Option.none
  | Shape_index idx -> shape_index_min_distance idx (Target.edge v0 v1) cur

and update_min_distance_to_cell (target : Target.t) cell cur =
  match target with
  | Point { pt } ->
    let d = S2_cell.distance_to_point cell pt in
    if S1_chord_angle.compare d cur < 0
    then S1_chord_angle.Option.some d
    else S1_chord_angle.Option.none
  | Edge { a; b } ->
    let d = S2_cell.distance_to_edge cell a b in
    if S1_chord_angle.compare d cur < 0
    then S1_chord_angle.Option.some d
    else S1_chord_angle.Option.none
  | Cell { cell = other } ->
    let d = S2_cell.distance_to_cell cell other in
    if S1_chord_angle.compare d cur < 0
    then S1_chord_angle.Option.some d
    else S1_chord_angle.Option.none
  | Shape_index idx -> shape_index_min_distance idx (Target.cell cell) cur

and shape_index_min_distance (idx : Target.shape_index) sub_target cur =
  let sub_query =
    match idx.cached_query with
    | Some q -> q
    | None ->
      let q = create idx.index () in
      idx.cached_query <- Some q;
      q
  in
  let sub_opts =
    Options.create
      ~include_interiors:idx.include_interiors
      ~use_brute_force:idx.use_brute_force
      ()
  in
  let sub_opts = Options.with_max_results sub_opts 1 in
  let sub_opts = Options.with_max_distance sub_opts cur in
  set_options sub_query sub_opts;
  let r = find_closest_edge sub_query sub_target in
  if Result.is_empty r
  then S1_chord_angle.Option.none
  else if S1_chord_angle.compare r.distance cur < 0
  then S1_chord_angle.Option.some r.distance
  else S1_chord_angle.Option.none
;;

(* Public wrappers. *)
let find_closest_edge query target = find_closest_edge query target

let get_distance query target =
  let r = find_closest_edge query target in
  r.distance
;;

let is_distance_less query target limit =
  let opts = Options.with_max_results query.options 1 in
  let opts = Options.with_max_distance opts limit in
  let opts = Options.with_max_error opts S1_chord_angle.straight in
  let ctx = make_ctx query target opts in
  find_closest_edges_internal ctx;
  not (Result.is_empty ctx.result_singleton)
;;

let is_distance_less_or_equal query target limit =
  let opts = Options.with_max_results query.options 1 in
  let opts = Options.with_inclusive_max_distance opts limit in
  let opts = Options.with_max_error opts S1_chord_angle.straight in
  let ctx = make_ctx query target opts in
  find_closest_edges_internal ctx;
  not (Result.is_empty ctx.result_singleton)
;;

let is_conservative_distance_less_or_equal query target limit =
  let opts = Options.with_max_results query.options 1 in
  let opts = Options.with_conservative_max_distance opts limit in
  let opts = Options.with_max_error opts S1_chord_angle.straight in
  let ctx = make_ctx query target opts in
  find_closest_edges_internal ctx;
  not (Result.is_empty ctx.result_singleton)
;;

let get_edge t (result : Result.t) =
  if Result.is_interior result
  then raise_s [%message "S2_closest_edge_query.get_edge: result is interior"];
  let shape = S2_shape_index.shape t.index result.shape_id in
  shape.#edge result.edge_id
;;

let project t p (result : Result.t) =
  if Result.is_interior result
  then p
  else (
    let (#{ v0; v1 } : S2_shape.Edge.t) = get_edge t result in
    S2_edge_distances.project p v0 v1)
;;
