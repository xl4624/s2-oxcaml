open Core

type t =
  { min_level : int
  ; max_level : int
  ; level_mod : int
  ; max_cells : int
  }

let create
  ?(min_level = 0)
  ?(max_level = S2_cell_id.max_level)
  ?(level_mod = 1)
  ?(max_cells = 8)
  ()
  =
  let min_level = Int.clamp_exn min_level ~min:0 ~max:S2_cell_id.max_level in
  let max_level = Int.clamp_exn max_level ~min:0 ~max:S2_cell_id.max_level in
  let level_mod = Int.clamp_exn level_mod ~min:1 ~max:3 in
  { min_level; max_level; level_mod; max_cells }
;;

(* Helpers *)

let[@inline] unsigned_compare (a : S2_cell_id.t) (b : S2_cell_id.t) : int =
  Stdlib_upstream_compatible.Int64_u.unsigned_compare (S2_cell_id.id a) (S2_cell_id.id b)
;;

let[@inline] unsigned_le a b = unsigned_compare a b <= 0
let max_children_shift t = 2 * t.level_mod

let true_max_level t =
  if t.level_mod = 1
  then t.max_level
  else t.max_level - ((t.max_level - t.min_level) % t.level_mod)
;;

let adjust_level t level =
  if t.level_mod > 1 && level > t.min_level
  then level - ((level - t.min_level) % t.level_mod)
  else level
;;

(* A candidate cell being considered for the covering. *)
type candidate =
  { cell : S2_cell.t
  ; mutable is_terminal : bool
  ; mutable num_children : int
  ; mutable children : candidate option array
  ; mutable priority : int
  }

(* Internal coverer state. Region callbacks are extracted from the unboxed
   S2_region.t at the call boundary so they can live in a normal boxed record. *)
type coverer =
  { opts : t
  ; intersects_cell : S2_cell.t -> bool
  ; contains_cell : S2_cell.t -> bool
  ; cell_union_bound : unit -> Int64.t list
  ; mutable result : S2_cell_id.t array
  ; mutable result_len : int
  ; mutable pq : candidate Pairing_heap.t
  ; mutable pq_size : int
  ; mutable interior_covering : bool
  }

let result_push c id =
  let cap = Array.length c.result in
  if c.result_len >= cap
  then (
    let new_cap = Int.max 8 (cap * 2) in
    let new_arr = Array.create ~len:new_cap S2_cell_id.none in
    (Array.unsafe_blit [@kind bits64])
      ~src:c.result
      ~dst:new_arr
      ~src_pos:0
      ~dst_pos:0
      ~len:cap;
    c.result <- new_arr);
  c.result.(c.result_len) <- id;
  c.result_len <- c.result_len + 1
;;

let new_coverer opts (region : S2_region.t) =
  { opts
  ; intersects_cell = region.#S2_region.intersects_cell
  ; contains_cell = region.#S2_region.contains_cell
  ; cell_union_bound = region.#S2_region.cell_union_bound
  ; result = Array.create ~len:8 S2_cell_id.none
  ; result_len = 0
  ; pq = Pairing_heap.create ~cmp:(fun a b -> compare b.priority a.priority) ()
  ; pq_size = 0
  ; interior_covering = false
  }
;;

let new_candidate c cell =
  if not (c.intersects_cell cell)
  then None
  else (
    let level = S2_cell.level cell in
    let mutable is_terminal = false in
    let mutable reject = false in
    if level >= c.opts.min_level
    then
      if c.interior_covering
      then (
        if c.contains_cell cell
        then is_terminal <- true
        else if level + c.opts.level_mod > c.opts.max_level
        then reject <- true)
      else if level + c.opts.level_mod > c.opts.max_level || c.contains_cell cell
      then is_terminal <- true;
    if reject
    then None
    else (
      let max_ch = if is_terminal then 0 else 1 lsl max_children_shift c.opts in
      Some
        { cell
        ; is_terminal
        ; num_children = 0
        ; children = Array.create ~len:max_ch None
        ; priority = 0
        }))
;;

let rec expand_children c cand cell num_levels =
  let num_levels = num_levels - 1 in
  let mutable num_terminals = 0 in
  let cell_id = S2_cell.id cell in
  for k = 0 to 3 do
    let child_id = S2_cell_id.child_exn cell_id k in
    let child_cell = S2_cell.of_cell_id child_id in
    if num_levels > 0
    then (
      if c.intersects_cell child_cell
      then num_terminals <- num_terminals + expand_children c cand child_cell num_levels)
    else (
      match new_candidate c child_cell with
      | None -> ()
      | Some child ->
        cand.children.(cand.num_children) <- Some child;
        cand.num_children <- cand.num_children + 1;
        if child.is_terminal then num_terminals <- num_terminals + 1)
  done;
  num_terminals
;;

let rec add_candidate c cand =
  if cand.is_terminal
  then result_push c (S2_cell.id cand.cell)
  else (
    let num_levels =
      if S2_cell.level cand.cell < c.opts.min_level then 1 else c.opts.level_mod
    in
    let num_terminals = expand_children c cand cand.cell num_levels in
    let mcs = max_children_shift c.opts in
    if cand.num_children = 0
    then ()
    else if (not c.interior_covering)
            && num_terminals = 1 lsl mcs
            && S2_cell.level cand.cell >= c.opts.min_level
    then (
      cand.is_terminal <- true;
      add_candidate c cand)
    else (
      let level = S2_cell.level cand.cell in
      cand.priority <- -(((level lsl mcs) + cand.num_children) lsl mcs) - num_terminals;
      Pairing_heap.add c.pq cand;
      c.pq_size <- c.pq_size + 1))
;;

let add_candidate_opt c = function
  | None -> ()
  | Some cand -> add_candidate c cand
;;

(* Adjust cell levels for level_mod compliance. *)
let adjust_cell_levels opts (cells : S2_cell_id.t array) =
  if opts.level_mod = 1
  then cells
  else (
    let n = Array.length cells in
    let out = Array.create ~len:n S2_cell_id.none in
    let mutable out_len = 0 in
    for idx = 0 to n - 1 do
      let id = cells.(idx) in
      let level = S2_cell_id.level id in
      let new_level = adjust_level opts level in
      let id = if new_level <> level then S2_cell_id.parent_level id new_level else id in
      let skip = out_len > 0 && S2_cell_id.contains out.(out_len - 1) id in
      if not skip
      then (
        while out_len > 0 && S2_cell_id.contains id out.(out_len - 1) do
          out_len <- out_len - 1
        done;
        out.(out_len) <- id;
        out_len <- out_len + 1)
    done;
    (Array.sub [@kind bits64]) out ~pos:0 ~len:out_len)
;;

(* Convert boxed Int64.t list from cell_union_bound to S2_cell_id.t array *)
let cids_of_int64_list (l : Int64.t list) : S2_cell_id.t array =
  let arr = List.to_array l in
  let n = Array.length arr in
  let result = Array.create ~len:n S2_cell_id.none in
  for i = 0 to n - 1 do
    result.(i) <- S2_cell_id.of_int64 (Int64_u.of_int64 arr.(i))
  done;
  result
;;

(* {1 Non-recursive helpers for canonicalize/reduce} *)

let replace_cells_with_ancestor (covering : S2_cell_id.t array) (id : S2_cell_id.t) =
  let raw_min = S2_cell_id.range_min id in
  let raw_max = S2_cell_id.range_max id in
  let n = Array.length covering in
  (* Find begin: first element > range_min (upper_bound on range_min) *)
  let begin_ =
    let mutable lo = 0 in
    let mutable hi = n in
    while lo < hi do
      let mid = lo + ((hi - lo) / 2) in
      if unsigned_compare covering.(mid) raw_min > 0 then hi <- mid else lo <- mid + 1
    done;
    lo
  in
  (* Find end: first element > range_max (upper_bound on range_max) *)
  let end_ =
    let mutable lo = 0 in
    let mutable hi = n in
    while lo < hi do
      let mid = lo + ((hi - lo) / 2) in
      if unsigned_compare covering.(mid) raw_max > 0 then hi <- mid else lo <- mid + 1
    done;
    lo
  in
  let result_len = begin_ + 1 + (n - end_) in
  let result = Array.create ~len:result_len S2_cell_id.none in
  (Array.unsafe_blit [@kind bits64])
    ~src:covering
    ~dst:result
    ~src_pos:0
    ~dst_pos:0
    ~len:begin_;
  result.(begin_) <- id;
  (Array.unsafe_blit [@kind bits64])
    ~src:covering
    ~dst:result
    ~src_pos:end_
    ~dst_pos:(begin_ + 1)
    ~len:(n - end_);
  result
;;

let contains_all_children opts (covering : S2_cell_id.t array) (id : S2_cell_id.t) =
  let raw_min = S2_cell_id.range_min id in
  let n = Array.length covering in
  (* lower_bound: first element >= range_min *)
  let mutable lo = 0 in
  let mutable hi = n in
  while lo < hi do
    let mid = lo + ((hi - lo) / 2) in
    if unsigned_compare covering.(mid) raw_min >= 0 then hi <- mid else lo <- mid + 1
  done;
  let level = S2_cell_id.level id + opts.level_mod in
  let mutable child = S2_cell_id.child_begin_at_level id level in
  let child_end = S2_cell_id.child_end_at_level id level in
  let mutable pos = lo in
  let mutable ok = true in
  while ok && not (S2_cell_id.equal child child_end) do
    if pos >= n || not (S2_cell_id.equal covering.(pos) child)
    then ok <- false
    else (
      pos <- pos + 1;
      child <- S2_cell_id.next child)
  done;
  ok
;;

let is_canonical_internal opts (covering : S2_cell_id.t array) =
  let n = Array.length covering in
  let true_max = true_max_level opts in
  let too_many_cells = n > opts.max_cells in
  let mutable same_parent_count = 1 in
  let mutable prev_id = S2_cell_id.none in
  let mutable ok = true in
  let mutable i = 0 in
  while ok && i < n do
    let id = covering.(i) in
    if not (S2_cell_id.is_valid id)
    then ok <- false
    else (
      let level = S2_cell_id.level id in
      if level < opts.min_level || level > true_max
      then ok <- false
      else if opts.level_mod > 1 && (level - opts.min_level) % opts.level_mod <> 0
      then ok <- false
      else if not (S2_cell_id.equal prev_id S2_cell_id.none)
      then (
        (* Check sorted and non-overlapping *)
        let prev_max = S2_cell_id.range_max prev_id in
        let curr_min = S2_cell_id.range_min id in
        if not (unsigned_compare prev_max curr_min < 0)
        then ok <- false
        else (
          let common = S2_cell_id.get_common_ancestor_level id prev_id in
          if too_many_cells && common >= opts.min_level
          then ok <- false
          else (
            let plevel = level - opts.level_mod in
            if plevel < opts.min_level
               || level <> S2_cell_id.level prev_id
               || not
                    (S2_cell_id.equal
                       (S2_cell_id.parent_level id plevel)
                       (S2_cell_id.parent_level prev_id plevel))
            then same_parent_count <- 1
            else (
              same_parent_count <- same_parent_count + 1;
              if same_parent_count = 1 lsl (2 * opts.level_mod) then ok <- false)))));
    prev_id <- covering.(i);
    i <- i + 1
  done;
  ok
;;

let reduce_covering opts (working : S2_cell_id.t array) =
  let mutable working = (Array.copy [@kind bits64]) working in
  let mutable done_ = false in
  while (not done_) && Array.length working > opts.max_cells do
    let mutable best_index = -1 in
    let mutable best_level = -1 in
    let n = Array.length working in
    for i = 0 to n - 2 do
      let level = S2_cell_id.get_common_ancestor_level working.(i) working.(i + 1) in
      let level = adjust_level opts level in
      if level > best_level
      then (
        best_level <- level;
        best_index <- i)
    done;
    if best_level < opts.min_level
    then done_ <- true
    else (
      let id = S2_cell_id.parent_level working.(best_index) best_level in
      working <- replace_cells_with_ancestor working id;
      let mutable cont = true in
      let mutable bl = best_level in
      while cont && bl > opts.min_level do
        bl <- bl - opts.level_mod;
        let parent = S2_cell_id.parent_level id bl in
        if contains_all_children opts working parent
        then working <- replace_cells_with_ancestor working parent
        else cont <- false
      done)
  done;
  working
;;

(* {1 Mutually recursive covering functions}

   The cycle is: get_initial_candidates -> fast_covering_internal_coverer ->
   canonicalize_covering_internal -> covering_internal -> get_initial_candidates *)

let rec get_initial_candidates c =
  let tmp =
    create ~max_level:c.opts.max_level ~max_cells:(Int.min 4 c.opts.max_cells) ()
  in
  let cu = fast_covering_internal_coverer tmp c in
  let raw = S2_cell_union.cell_ids_raw cu in
  let cells = adjust_cell_levels c.opts raw in
  let n = Array.length cells in
  for i = 0 to n - 1 do
    add_candidate_opt c (new_candidate c (S2_cell.of_cell_id cells.(i)))
  done

and fast_covering_internal_coverer opts c =
  let cell_ids = cids_of_int64_list (c.cell_union_bound ()) in
  let cu = S2_cell_union.from_verbatim cell_ids in
  let result = canonicalize_covering_internal opts (S2_cell_union.cell_ids_raw cu) in
  S2_cell_union.from_verbatim result

and fast_covering_internal opts (region : S2_region.t) =
  let cell_ids = cids_of_int64_list (region.#S2_region.cell_union_bound ()) in
  let cu = S2_cell_union.from_verbatim cell_ids in
  let result = canonicalize_covering_internal opts (S2_cell_union.cell_ids_raw cu) in
  S2_cell_union.from_verbatim result

and canonicalize_covering_internal opts (covering : S2_cell_id.t array) =
  let covering = (Array.copy [@kind bits64]) covering in
  (* Replace cells that are too small or don't satisfy level_mod *)
  if opts.max_level < S2_cell_id.max_level || opts.level_mod > 1
  then (
    let n = Array.length covering in
    for i = 0 to n - 1 do
      let id = covering.(i) in
      let level = S2_cell_id.level id in
      let new_level = adjust_level opts (Int.min level opts.max_level) in
      if new_level <> level then covering.(i) <- S2_cell_id.parent_level id new_level
    done);
  (* Sort and normalize *)
  let cu = S2_cell_union.create covering in
  let normalized = S2_cell_union.cell_ids_raw cu in
  (* Denormalize if needed *)
  let working =
    if opts.min_level > 0 || opts.level_mod > 1
    then S2_cell_union.denormalize cu ~min_level:opts.min_level ~level_mod:opts.level_mod
    else normalized
  in
  let excess = Array.length working - opts.max_cells in
  if excess <= 0 || is_canonical_internal opts working
  then working
  else if excess * Array.length working > 10000
  then (
    (* Use the region coverer for very large coverings *)
    let cu2 = S2_cell_union.from_verbatim working in
    let region = S2_region.of_cell_union cu2 in
    let result = covering_internal opts region false in
    S2_cell_union.cell_ids_raw result)
  else reduce_covering opts working

and covering_internal opts (region : S2_region.t) interior_covering =
  let c = new_coverer opts region in
  c.interior_covering <- interior_covering;
  get_initial_candidates c;
  while
    (not (Pairing_heap.is_empty c.pq))
    && ((not c.interior_covering) || c.result_len < c.opts.max_cells)
  do
    let cand = Pairing_heap.pop_exn c.pq in
    c.pq_size <- c.pq_size - 1;
    if c.interior_covering
       || S2_cell.level cand.cell < c.opts.min_level
       || cand.num_children = 1
       || c.result_len + c.pq_size + cand.num_children <= c.opts.max_cells
    then
      for i = 0 to cand.num_children - 1 do
        match cand.children.(i) with
        | None -> ()
        | Some child ->
          if (not c.interior_covering) || c.result_len < c.opts.max_cells
          then add_candidate c child
      done
    else (
      cand.is_terminal <- true;
      add_candidate c cand)
  done;
  (* Normalize result *)
  let raw_result = (Array.sub [@kind bits64]) c.result ~pos:0 ~len:c.result_len in
  let cu = S2_cell_union.create raw_result in
  if c.opts.min_level > 0 || c.opts.level_mod > 1
  then (
    let denorm =
      S2_cell_union.denormalize cu ~min_level:c.opts.min_level ~level_mod:c.opts.level_mod
    in
    S2_cell_union.from_verbatim denorm)
  else cu
;;

let covering t region = covering_internal t region false
let interior_covering t region = covering_internal t region true
let fast_covering t region = fast_covering_internal t region
let is_canonical t cell_ids = is_canonical_internal t cell_ids
let canonicalize_covering t cell_ids = canonicalize_covering_internal t cell_ids
