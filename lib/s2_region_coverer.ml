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

(* A candidate cell being considered for the covering. Children live in the coverer's flat
   [children_buf]; [children_start] and [num_children] mark this candidate's contiguous
   slice of that buffer. *)
type candidate =
  { cell : S2_cell.t
  ; mutable is_terminal : bool
  ; mutable num_children : int
  ; mutable children_start : int
  ; mutable priority : int
  }

module Candidate_heap = Binary_heap.Make (struct
    type t = candidate

    let[@inline] higher_priority a b = a.priority > b.priority
  end)

(* Internal coverer state. Holding the [S2_region.t] directly (rather than extracting
   callbacks) avoids allocating closures and lets the compiler specialize each dispatch
   via pattern match on the variant tag. *)
type coverer =
  { opts : t
  ; region : S2_region.t
  ; mutable result : S2_cell_id.t array
  ; mutable result_len : int
  ; mutable children_buf : candidate array
  ; mutable children_len : int
  ; pq : Candidate_heap.t
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
  ; region
  ; result = Array.create ~len:8 S2_cell_id.none
  ; result_len = 0
  ; children_buf = [||]
  ; children_len = 0
  ; pq = Candidate_heap.create ()
  ; interior_covering = false
  }
;;

(* Append [child] to the flat children buffer, growing it on demand. The first-ever append
   fills the freshly allocated slots with [child]; since we overwrite slot [children_len]
   immediately and never read beyond [children_len], the fill value is inert. *)
let push_child c child =
  let cap = Array.length c.children_buf in
  if c.children_len >= cap
  then (
    let new_cap = if cap = 0 then 32 else cap * 2 in
    let new_arr = Array.create ~len:new_cap child in
    if cap > 0
    then Array.blit ~src:c.children_buf ~src_pos:0 ~dst:new_arr ~dst_pos:0 ~len:cap;
    c.children_buf <- new_arr);
  c.children_buf.(c.children_len) <- child;
  c.children_len <- c.children_len + 1
;;

(* Decide this cell's status. Returns:
   - [-1] if the region doesn't intersect [cell] (cell is rejected).
   - [0] if the cell should be added as a non-terminal candidate.
   - [1] if the cell should be added as a terminal (leaf of the covering). This is the
         [new_candidate] "allocate or not + is_terminal" logic split out so callers can
         allocate the candidate directly without going through an [option] wrapper, saving
         the [Some _] header per accepted candidate. *)
let[@inline] classify_cell c cell =
  if not (S2_region.intersects_cell c.region cell)
  then -1
  else (
    let level = S2_cell.level cell in
    if level < c.opts.min_level
    then 0
    else if c.interior_covering
    then
      if S2_region.contains_cell c.region cell
      then 1
      else if level + c.opts.level_mod > c.opts.max_level
      then -1
      else 0
    else if level + c.opts.level_mod > c.opts.max_level
            || S2_region.contains_cell c.region cell
    then 1
    else 0)
;;

let[@inline] make_candidate cell ~is_terminal =
  { cell; is_terminal; num_children = 0; children_start = 0; priority = 0 }
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
      if S2_region.intersects_cell c.region child_cell
      then num_terminals <- num_terminals + expand_children c cand child_cell num_levels)
    else (
      let status = classify_cell c child_cell in
      if status >= 0
      then (
        let is_terminal = status = 1 in
        push_child c (make_candidate child_cell ~is_terminal);
        cand.num_children <- cand.num_children + 1;
        if is_terminal then num_terminals <- num_terminals + 1))
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
    (* Reserve this candidate's children slice at the current end of the flat buffer, then
       expand: new children are appended in order. *)
    cand.children_start <- c.children_len;
    let num_terminals = expand_children c cand cand.cell num_levels in
    let mcs = 2 * c.opts.level_mod in
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
      Candidate_heap.add c.pq cand))
;;

let[@inline] try_add_new_candidate c cell =
  let status = classify_cell c cell in
  if status >= 0 then add_candidate c (make_candidate cell ~is_terminal:(status = 1))
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

(* {1 Non-recursive helpers for canonicalize/reduce} *)

(* Shift [end_..len) left into [begin_+1..], then write [id] at position [begin_].
   Returns the new length. Requires [begin_ + 1 <= end_], which holds in
   [reduce_covering] because [id] is either the common ancestor of two adjacent
   cells or a parent known to contain all its children, so at least two cells
   fall in the [range_min..range_max] window. *)
let replace_cells_with_ancestor_in_place
  (covering : S2_cell_id.t array)
  len
  (id : S2_cell_id.t)
  =
  let raw_min = S2_cell_id.range_min id in
  let raw_max = S2_cell_id.range_max id in
  (* upper_bound on range_min *)
  let begin_ =
    let mutable lo = 0 in
    let mutable hi = len in
    while lo < hi do
      let mid = lo + ((hi - lo) / 2) in
      if S2_cell_id.compare covering.(mid) raw_min > 0 then hi <- mid else lo <- mid + 1
    done;
    lo
  in
  (* upper_bound on range_max, starting from [begin_] since it's monotonic *)
  let end_ =
    let mutable lo = begin_ in
    let mutable hi = len in
    while lo < hi do
      let mid = lo + ((hi - lo) / 2) in
      if S2_cell_id.compare covering.(mid) raw_max > 0 then hi <- mid else lo <- mid + 1
    done;
    lo
  in
  let tail_len = len - end_ in
  for i = 0 to tail_len - 1 do
    covering.(begin_ + 1 + i) <- covering.(end_ + i)
  done;
  covering.(begin_) <- id;
  begin_ + 1 + tail_len
;;

let contains_all_children opts (covering : S2_cell_id.t array) len (id : S2_cell_id.t) =
  let raw_min = S2_cell_id.range_min id in
  (* lower_bound: first element >= range_min *)
  let mutable lo = 0 in
  let mutable hi = len in
  while lo < hi do
    let mid = lo + ((hi - lo) / 2) in
    if S2_cell_id.compare covering.(mid) raw_min >= 0 then hi <- mid else lo <- mid + 1
  done;
  let level = S2_cell_id.level id + opts.level_mod in
  let mutable child = S2_cell_id.child_begin_at_level id level in
  let child_end = S2_cell_id.child_end_at_level id level in
  let mutable pos = lo in
  let mutable ok = true in
  while ok && not (S2_cell_id.equal child child_end) do
    if pos >= len || not (S2_cell_id.equal covering.(pos) child)
    then ok <- false
    else (
      pos <- pos + 1;
      child <- S2_cell_id.next child)
  done;
  ok
;;

let is_canonical_internal opts (covering : S2_cell_id.t array) len =
  let true_max = true_max_level opts in
  let too_many_cells = len > opts.max_cells in
  let mutable same_parent_count = 1 in
  let mutable prev_id = S2_cell_id.none in
  let mutable ok = true in
  let mutable i = 0 in
  while ok && i < len do
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
        if not (S2_cell_id.compare prev_max curr_min < 0)
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

(* Mutates [working] in place (shifting elements left as cells are collapsed into common
   ancestors). Takes ownership of [working], which must be large enough to hold
   [working_len] initial entries. Returns the new length; the unused tail of [working] is
   left with stale data and must not be read beyond the result. *)
let reduce_covering opts (working : S2_cell_id.t array) working_len =
  let mutable wlen = working_len in
  let mutable done_ = false in
  while (not done_) && wlen > opts.max_cells do
    let mutable best_index = -1 in
    let mutable best_level = -1 in
    for i = 0 to wlen - 2 do
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
      wlen <- replace_cells_with_ancestor_in_place working wlen id;
      let mutable cont = true in
      let mutable bl = best_level in
      while cont && bl > opts.min_level do
        bl <- bl - opts.level_mod;
        let parent = S2_cell_id.parent_level id bl in
        if contains_all_children opts working wlen parent
        then wlen <- replace_cells_with_ancestor_in_place working wlen parent
        else cont <- false
      done)
  done;
  wlen
;;

(* {1 Mutually recursive covering functions}

   The cycle is: get_initial_candidates -> fast_covering_internal_coverer ->
   canonicalize_covering_internal -> covering_internal -> get_initial_candidates *)

let rec get_initial_candidates c =
  let tmp =
    create ~max_level:c.opts.max_level ~max_cells:(Int.min 4 c.opts.max_cells) ()
  in
  (* Use the canonicalized array directly - wrapping it in an [S2_cell_union.t] only to
     immediately [cell_ids_raw] it back would allocate two extra copies of the array per
     covering. *)
  let canonical =
    canonicalize_covering_internal tmp (S2_region.cell_union_bound c.region)
  in
  let cells = adjust_cell_levels c.opts canonical in
  let n = Array.length cells in
  for i = 0 to n - 1 do
    try_add_new_candidate c (S2_cell.of_cell_id cells.(i))
  done

and fast_covering_internal opts (region : S2_region.t) =
  let cell_ids = S2_region.cell_union_bound region in
  let result = canonicalize_covering_internal opts cell_ids in
  S2_cell_union.of_verbatim_owned result

and canonicalize_covering_internal opts (covering : S2_cell_id.t array) =
  (* Replace cells that are too small or don't satisfy [level_mod]. When adjustment is
     needed, emit into a fresh array so the caller's input is not mutated; otherwise pass
     the input straight through, because [S2_cell_union.create] copies its argument before
     mutating it. *)
  let covering =
    if opts.max_level < S2_cell_id.max_level || opts.level_mod > 1
    then (
      let n = Array.length covering in
      let out = Array.create ~len:n S2_cell_id.none in
      for i = 0 to n - 1 do
        let id = covering.(i) in
        let level = S2_cell_id.level id in
        let new_level = adjust_level opts (Int.min level opts.max_level) in
        out.(i)
        <- (if new_level <> level then S2_cell_id.parent_level id new_level else id)
      done;
      out)
    else covering
  in
  let cu = S2_cell_union.create covering in
  let normalized = S2_cell_union.cell_ids_raw cu in
  (* Denormalize if needed *)
  let working =
    if opts.min_level > 0 || opts.level_mod > 1
    then S2_cell_union.denormalize cu ~min_level:opts.min_level ~level_mod:opts.level_mod
    else normalized
  in
  let working_len = Array.length working in
  let excess = working_len - opts.max_cells in
  if excess <= 0 || is_canonical_internal opts working working_len
  then working
  else if excess * working_len > 10000
  then (
    (* Use the region coverer for very large coverings *)
    let cu2 = S2_cell_union.of_verbatim_owned working in
    let region = S2_region.of_cell_union cu2 in
    let result = covering_internal opts region false in
    S2_cell_union.cell_ids_raw result)
  else (
    let new_len = reduce_covering opts working working_len in
    (Array.sub [@kind bits64]) working ~pos:0 ~len:new_len)

and covering_internal opts (region : S2_region.t) interior_covering =
  let c = new_coverer opts region in
  c.interior_covering <- interior_covering;
  get_initial_candidates c;
  while
    (not (Candidate_heap.is_empty c.pq))
    && ((not c.interior_covering) || c.result_len < c.opts.max_cells)
  do
    let cand = Candidate_heap.pop_exn c.pq in
    if c.interior_covering
       || S2_cell.level cand.cell < c.opts.min_level
       || cand.num_children = 1
       || c.result_len + Candidate_heap.length c.pq + cand.num_children
          <= c.opts.max_cells
    then
      for i = 0 to cand.num_children - 1 do
        let child = c.children_buf.(cand.children_start + i) in
        if (not c.interior_covering) || c.result_len < c.opts.max_cells
        then add_candidate c child
      done
    else (
      cand.is_terminal <- true;
      add_candidate c cand)
  done;
  (* Normalize result. [raw_result] is freshly owned, so we can transfer it to
     [of_raw_owned] which skips the defensive copy that [create] would make. *)
  let raw_result = (Array.sub [@kind bits64]) c.result ~pos:0 ~len:c.result_len in
  let cu = S2_cell_union.of_raw_owned raw_result in
  if c.opts.min_level > 0 || c.opts.level_mod > 1
  then (
    let denorm =
      S2_cell_union.denormalize cu ~min_level:c.opts.min_level ~level_mod:c.opts.level_mod
    in
    S2_cell_union.of_verbatim_owned denorm)
  else cu
;;

let covering t region = covering_internal t region false
let interior_covering t region = covering_internal t region true
let fast_covering t region = fast_covering_internal t region
let is_canonical t cell_ids = is_canonical_internal t cell_ids (Array.length cell_ids)
let canonicalize_covering t cell_ids = canonicalize_covering_internal t cell_ids
