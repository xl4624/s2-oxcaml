open Core

module Error = struct
  type t =
    { code : string
    ; message : string
    }
  [@@deriving sexp_of]

  let ok = { code = "OK"; message = "" }
  let is_ok t = String.equal t.code "OK"
  let create ~code ~message = { code; message }
end

module Edge_type = struct
  type t =
    | Directed
    | Undirected
  [@@deriving sexp_of, equal]
end

module Degenerate_edges = struct
  type t =
    | Discard
    | Discard_excess
    | Keep
  [@@deriving sexp_of, equal]
end

module Duplicate_edges = struct
  type t =
    | Merge
    | Keep
  [@@deriving sexp_of, equal]
end

module Sibling_pairs = struct
  type t =
    | Discard
    | Discard_excess
    | Keep
    | Require
    | Create
  [@@deriving sexp_of, equal]
end

module Graph_options = struct
  type t =
    { edge_type : Edge_type.t
    ; degenerate_edges : Degenerate_edges.t
    ; duplicate_edges : Duplicate_edges.t
    ; sibling_pairs : Sibling_pairs.t
    ; allow_vertex_filtering : bool
    }
  [@@deriving sexp_of]

  let default =
    { edge_type = Edge_type.Directed
    ; degenerate_edges = Degenerate_edges.Keep
    ; duplicate_edges = Duplicate_edges.Keep
    ; sibling_pairs = Sibling_pairs.Keep
    ; allow_vertex_filtering = true
    }
  ;;

  let create ~edge_type ~degenerate_edges ~duplicate_edges ~sibling_pairs =
    { edge_type
    ; degenerate_edges
    ; duplicate_edges
    ; sibling_pairs
    ; allow_vertex_filtering = true
    }
  ;;

  let equal a b =
    Edge_type.equal a.edge_type b.edge_type
    && Degenerate_edges.equal a.degenerate_edges b.degenerate_edges
    && Duplicate_edges.equal a.duplicate_edges b.duplicate_edges
    && Sibling_pairs.equal a.sibling_pairs b.sibling_pairs
    && Bool.equal a.allow_vertex_filtering b.allow_vertex_filtering
  ;;
end

let k_max_input_edge_id = Int.max_value
let k_no_input_edge_id = Int.max_value - 1

(* Minimal growing array for values whose layout cannot be stored in [Dynarray.t] (which
   requires a value layout). We use this for S2_point.t which is a float64 product. *)
module Point_buffer = struct
  type t =
    { mutable arr : S2_point.t array
    ; mutable len : int
    }

  let zero = S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#0.0
  let create () = { arr = Array.create ~len:8 zero; len = 0 }
  let length t = t.len

  let get t i =
    if i < 0 || i >= t.len then invalid_arg "Point_buffer.get";
    t.arr.(i)
  ;;

  let grow t =
    let n = Array.length t.arr in
    let new_arr = Array.create ~len:(Int.max 8 (n * 2)) zero in
    for i = 0 to t.len - 1 do
      new_arr.(i) <- t.arr.(i)
    done;
    t.arr <- new_arr
  ;;

  let add_last t p =
    if t.len = Array.length t.arr then grow t;
    t.arr.(t.len) <- p;
    t.len <- t.len + 1
  ;;

  let clear t = t.len <- 0

  let to_array t =
    if t.len = 0
    then [||]
    else (
      let out = Array.create ~len:t.len t.arr.(0) in
      for i = 1 to t.len - 1 do
        out.(i) <- t.arr.(i)
      done;
      out)
  ;;
end

module Id_set_lexicon = struct
  (* Interns sorted integer sets so many edges sharing the same set can share storage. Id
     [0] is reserved for the empty set. *)
  type t =
    { sets : int array Dynarray.t
    ; index : (string, int) Hashtbl.t
    }

  let empty_set_id = 0

  let create () =
    let sets = Dynarray.create () in
    Dynarray.add_last sets [||];
    let index = Hashtbl.create (module String) in
    Hashtbl.set index ~key:"" ~data:0;
    { sets; index }
  ;;

  let clear t =
    Dynarray.clear t.sets;
    Dynarray.add_last t.sets [||];
    Hashtbl.clear t.index;
    Hashtbl.set t.index ~key:"" ~data:0
  ;;

  let key_of ids =
    let buf = Buffer.create (Array.length ids * 8) in
    Array.iter ids ~f:(fun i ->
      Buffer.add_string buf (Int.to_string i);
      Buffer.add_char buf ',');
    Buffer.contents buf
  ;;

  let normalize ids =
    let copy = Array.copy ids in
    Array.sort copy ~compare:Int.compare;
    let n = Array.length copy in
    if n <= 1
    then copy
    else (
      let out = Array.create ~len:n 0 in
      out.(0) <- copy.(0);
      let mutable j = 1 in
      for i = 1 to n - 1 do
        if copy.(i) <> copy.(i - 1)
        then (
          out.(j) <- copy.(i);
          j <- j + 1)
      done;
      if j = n then out else Array.sub out ~pos:0 ~len:j)
  ;;

  let add t ids =
    let ids = normalize ids in
    if Array.length ids = 0
    then empty_set_id
    else (
      let k = key_of ids in
      match Hashtbl.find t.index k with
      | Some id -> id
      | None ->
        let id = Dynarray.length t.sets in
        Dynarray.add_last t.sets ids;
        Hashtbl.set t.index ~key:k ~data:id;
        id)
  ;;

  let add_singleton t i = add t [| i |]
  let id_set t id = Dynarray.get t.sets id

  let copy t =
    let sets = Dynarray.create () in
    Dynarray.iter (fun a -> Dynarray.add_last sets (Array.copy a)) t.sets;
    let index = Hashtbl.copy t.index in
    { sets; index }
  ;;
end

module Graph = struct
  module Edge = struct
    type t =
      { src : int
      ; dst : int
      }

    let create ~src ~dst = { src; dst }
    let src t = t.src
    let dst t = t.dst
    let reverse (e : t) = { src = e.dst; dst = e.src }
    let equal (a : t) (b : t) = a.src = b.src && a.dst = b.dst

    let compare (a : t) (b : t) =
      let c = Int.compare a.src b.src in
      if c <> 0 then c else Int.compare a.dst b.dst
    ;;
  end

  type t =
    { options : Graph_options.t
    ; vertices : S2_point.t array
    ; edges : Edge.t array
    ; input_edge_id_set_ids : int array
    ; input_edge_id_set_lexicon : Id_set_lexicon.t
    ; is_full_polygon_predicate : t -> bool * Error.t
    }

  let options t = t.options
  let num_vertices t = Array.length t.vertices
  let vertex t i = t.vertices.(i)
  let num_edges t = Array.length t.edges
  let edge t i = t.edges.(i)

  let get_in_edge_ids t =
    let n = Array.length t.edges in
    let ids = Array.init n ~f:Fn.id in
    Array.sort ids ~compare:(fun a b ->
      let ea = t.edges.(a) in
      let eb = t.edges.(b) in
      let c = Int.compare ea.dst eb.dst in
      if c <> 0
      then c
      else (
        let c = Int.compare ea.src eb.src in
        if c <> 0 then c else Int.compare a b));
    ids
  ;;

  let get_sibling_map t =
    let n = Array.length t.edges in
    let sibling = Array.create ~len:n (-1) in
    let lookup = Hashtbl.create (module String) in
    Array.iteri t.edges ~f:(fun i e ->
      let key = sprintf "%d,%d" e.src e.dst in
      Hashtbl.add_multi lookup ~key ~data:i);
    Array.iteri t.edges ~f:(fun i e ->
      if sibling.(i) = -1
      then (
        let rev_key = sprintf "%d,%d" e.dst e.src in
        match Hashtbl.find lookup rev_key with
        | Some (j :: rest) ->
          sibling.(i) <- j;
          if j <> i then sibling.(j) <- i;
          Hashtbl.set lookup ~key:rev_key ~data:rest
        | _ -> ()));
    sibling
  ;;

  let input_edge_ids t e =
    Id_set_lexicon.id_set t.input_edge_id_set_lexicon t.input_edge_id_set_ids.(e)
  ;;

  let min_input_edge_id t e =
    let ids = input_edge_ids t e in
    if Array.length ids = 0 then k_no_input_edge_id else ids.(0)
  ;;

  let is_full_polygon t = t.is_full_polygon_predicate t

  let process_edges
    ~(options : Graph_options.t ref)
    ~(edges : Edge.t array ref)
    ~(input_ids : int array ref)
    ~(id_set_lexicon : Id_set_lexicon.t)
    =
    let opts = !options in
    (match opts.edge_type with
     | Edge_type.Undirected ->
       raise_s [%message "S2_builder: undirected edges not yet implemented"]
     | Directed -> ());
    (match opts.sibling_pairs with
     | Sibling_pairs.Require | Create ->
       raise_s [%message "S2_builder: sibling_pairs Require/Create not yet implemented"]
     | _ -> ());
    let edges_in = !edges in
    let input_ids_in = !input_ids in
    let n = Array.length edges_in in
    assert (Array.length input_ids_in = n);
    (* Sort order indices by (src, dst) with edge id as stable tie-break. *)
    let order = Array.init n ~f:Fn.id in
    Array.sort order ~compare:(fun a b ->
      let c = Edge.compare edges_in.(a) edges_in.(b) in
      if c <> 0 then c else Int.compare a b);
    (* Count reverse-edge occurrences for each (src, dst). *)
    let rev_count = Hashtbl.create (module String) in
    Array.iter edges_in ~f:(fun e ->
      let key = sprintf "%d,%d" e.dst e.src in
      Hashtbl.update rev_count key ~f:(function
        | Some c -> c + 1
        | None -> 1));
    let out_edges = Dynarray.create () in
    let out_ids = Dynarray.create () in
    let merge_ids edge_ids =
      match edge_ids with
      | [ id ] -> input_ids_in.(id)
      | _ ->
        let all =
          List.concat_map edge_ids ~f:(fun eid ->
            Array.to_list (Id_set_lexicon.id_set id_set_lexicon input_ids_in.(eid)))
        in
        Id_set_lexicon.add id_set_lexicon (Array.of_list all)
    in
    let i = ref 0 in
    while !i < n do
      let eid = order.(!i) in
      let e = edges_in.(eid) in
      let src = e.src in
      let dst = e.dst in
      let j = ref (!i + 1) in
      while
        !j < n
        &&
        let other = edges_in.(order.(!j)) in
        other.src = src && other.dst = dst
      do
        j := !j + 1
      done;
      let i0 = !i in
      let j0 = !j in
      let run_ids = List.init (j0 - i0) ~f:(fun k -> order.(i0 + k)) in
      let n_out = j0 - i0 in
      let is_degenerate = src = dst in
      let n_in =
        if is_degenerate
        then 0
        else (
          let key = sprintf "%d,%d" src dst in
          match Hashtbl.find rev_count key with
          | Some c -> c
          | None -> 0)
      in
      let merge =
        match opts.duplicate_edges with
        | Duplicate_edges.Merge -> true
        | Keep ->
          (match opts.sibling_pairs with
           | Sibling_pairs.Discard | Discard_excess -> true
           | _ -> false)
      in
      let emit_count count =
        if count >= 1
        then
          if merge
          then (
            Dynarray.add_last out_edges (Edge.create ~src ~dst);
            Dynarray.add_last out_ids (merge_ids run_ids))
          else
            for k = 0 to count - 1 do
              Dynarray.add_last out_edges (Edge.create ~src ~dst);
              Dynarray.add_last out_ids input_ids_in.(List.nth_exn run_ids k)
            done
      in
      if is_degenerate
      then (
        match opts.degenerate_edges with
        | Degenerate_edges.Discard -> ()
        | Discard_excess ->
          let has_incident = ref false in
          Array.iter edges_in ~f:(fun other ->
            if other.src <> other.dst && (other.src = src || other.dst = src)
            then has_incident := true);
          if not !has_incident then emit_count (if merge then 1 else n_out)
        | Keep -> emit_count (if merge then 1 else n_out))
      else (
        match opts.sibling_pairs with
        | Sibling_pairs.Keep -> emit_count (if merge then 1 else n_out)
        | Discard -> if n_out > n_in then emit_count (if merge then 1 else n_out - n_in)
        | Discard_excess ->
          if n_out >= n_in then emit_count (if merge then 1 else Int.max 1 (n_out - n_in))
        | Require | Create -> ());
      i := j0
    done;
    let out_n = Dynarray.length out_edges in
    let pairs =
      Array.init out_n ~f:(fun k -> Dynarray.get out_edges k, Dynarray.get out_ids k)
    in
    Array.sort pairs ~compare:(fun (a, _) (b, _) -> Edge.compare a b);
    edges := Array.map pairs ~f:fst;
    input_ids := Array.map pairs ~f:snd;
    Error.ok
  ;;

  module Loop_type = struct
    type t =
      | Simple
      | Circuit
  end

  (* Build a left-turn map: for each edge e = (v0, v1), which outgoing edge around v1
     follows e in clockwise order. This is a simplified version of
     s2builder_graph.cc:236-320 that works for graphs without degenerate edges or
     duplicate edges (i.e., the output of ProcessEdges for the polygon layer options). For
     each vertex, we sort incoming and outgoing edges CCW around the vertex and pair them
     up. *)
  let get_left_turn_map t _in_edge_ids =
    let n = num_edges t in
    let left = Array.create ~len:n (-1) in
    let num_v = num_vertices t in
    let out_edges = Array.create ~len:num_v [] in
    let in_edges = Array.create ~len:num_v [] in
    for e = n - 1 downto 0 do
      let ed = t.edges.(e) in
      out_edges.(ed.src) <- e :: out_edges.(ed.src);
      in_edges.(ed.dst) <- e :: in_edges.(ed.dst)
    done;
    for v = 0 to num_v - 1 do
      let incoming = Array.of_list in_edges.(v) in
      let outgoing = Array.of_list out_edges.(v) in
      let n_in = Array.length incoming in
      let n_out = Array.length outgoing in
      if n_in > 0 && n_out > 0
      then
        if n_in = 1 && n_out = 1
        then left.(incoming.(0)) <- outgoing.(0)
        else (
          (* Reference: "other" endpoint of the first incoming edge. *)
          let ref_point =
            let e = incoming.(0) in
            t.vertices.(t.edges.(e).src)
          in
          let v_pt = t.vertices.(v) in
          let other_in e = t.vertices.(t.edges.(e).src) in
          let other_out e = t.vertices.(t.edges.(e).dst) in
          let cmp_ccw_generic get_other a b =
            if Int.equal a b
            then 0
            else if S2_edge_crossings.ordered_ccw
                      ref_point
                      (get_other a)
                      (get_other b)
                      v_pt
            then -1
            else 1
          in
          Array.sort incoming ~compare:(cmp_ccw_generic other_in);
          Array.sort outgoing ~compare:(cmp_ccw_generic other_out);
          let m = Int.min n_in n_out in
          for k = 0 to m - 1 do
            left.(incoming.(k)) <- outgoing.(k)
          done)
    done;
    left
  ;;

  let canonicalize_loop_order ~min_input_ids loop =
    let n = Array.length loop in
    if n <= 1
    then loop
    else (
      let mutable best = 0 in
      let mutable best_id = min_input_ids.(loop.(0)) in
      for i = 1 to n - 1 do
        let id = min_input_ids.(loop.(i)) in
        if id > best_id
        then (
          best <- i;
          best_id <- id)
      done;
      (* We want the "best" edge at the last position (index n-1), so rotate by (best + 1)
         positions. *)
      let rotated = Array.create ~len:n loop.(0) in
      for i = 0 to n - 1 do
        rotated.(i) <- loop.((i + best + 1) mod n)
      done;
      rotated)
  ;;

  let canonicalize_loop_vector ~min_input_ids loops =
    Array.sort loops ~compare:(fun a b ->
      let key l =
        if Array.length l = 0 then k_no_input_edge_id else min_input_ids.(l.(0))
      in
      Int.compare (key a) (key b))
  ;;

  let get_directed_loops t loop_type =
    (match loop_type with
     | Loop_type.Circuit ->
       raise_s [%message "S2_builder: Loop_type.Circuit not implemented"]
     | Simple -> ());
    (match t.options.degenerate_edges with
     | Degenerate_edges.Discard | Discard_excess -> ()
     | Keep ->
       raise_s
         [%message "S2_builder: get_directed_loops requires Discard or Discard_excess"]);
    (match t.options.edge_type with
     | Edge_type.Undirected ->
       raise_s [%message "S2_builder: get_directed_loops requires directed edges"]
     | Directed -> ());
    let in_edge_ids = get_in_edge_ids t in
    let left = get_left_turn_map t in_edge_ids in
    let n = num_edges t in
    let loops = Dynarray.create () in
    let used = Array.create ~len:n false in
    let min_input_ids = Array.init n ~f:(fun e -> min_input_edge_id t e) in
    for start = 0 to n - 1 do
      if not used.(start)
      then (
        let path = Dynarray.create () in
        let path_index = Array.create ~len:(num_vertices t) (-1) in
        let mutable e = start in
        let mutable loop_done = false in
        while (not loop_done) && e >= 0 && not used.(e) do
          let ed = t.edges.(e) in
          let src = ed.src in
          if path_index.(src) >= 0
          then (
            let pos = path_index.(src) in
            let len = Dynarray.length path - pos in
            let loop = Array.create ~len (-1) in
            for i = 0 to len - 1 do
              loop.(i) <- Dynarray.get path (pos + i);
              used.(loop.(i)) <- true;
              let ee = t.edges.(loop.(i)) in
              path_index.(ee.src) <- -1
            done;
            for _ = 0 to len - 1 do
              let _ : int = Dynarray.pop_last path in
              ()
            done;
            Dynarray.add_last loops (canonicalize_loop_order ~min_input_ids loop);
            if Dynarray.length path = 0
            then loop_done <- true
            else (
              let last_edge = Dynarray.get path (Dynarray.length path - 1) in
              e <- left.(last_edge);
              if e < 0 || used.(e) then loop_done <- true))
          else (
            path_index.(src) <- Dynarray.length path;
            Dynarray.add_last path e;
            e <- left.(e))
        done)
    done;
    let loops_arr =
      Array.init (Dynarray.length loops) ~f:(fun i -> Dynarray.get loops i)
    in
    canonicalize_loop_vector ~min_input_ids loops_arr;
    loops_arr, Error.ok
  ;;
end

module Snap_function = struct
  let k_max_snap_radius () = S1_angle.of_degrees #70.0

  type t =
    { snap_radius : S1_angle.t
    ; min_vertex_separation : S1_angle.t
    ; min_edge_vertex_separation : S1_angle.t
    ; snap_point : S2_point.t -> S2_point.t
    }

  let identity_with_radius snap_radius =
    { snap_radius
    ; min_vertex_separation = snap_radius
    ; min_edge_vertex_separation = S1_angle.mul snap_radius #0.5
    ; snap_point = (fun p -> p)
    }
  ;;

  let identity () = identity_with_radius S1_angle.zero

  let unimplemented_snap_point name (_ : S2_point.t) : S2_point.t =
    match
      raise_s [%message "S2_builder.Snap_function not implemented" (name : string)]
    with
    | (_ : Nothing.t) -> .
  ;;

  let int_lat_lng ~exponent:_ =
    { snap_radius = S1_angle.zero
    ; min_vertex_separation = S1_angle.zero
    ; min_edge_vertex_separation = S1_angle.zero
    ; snap_point = unimplemented_snap_point "int_lat_lng"
    }
  ;;

  let s2_cell_id ~level:_ =
    { snap_radius = S1_angle.zero
    ; min_vertex_separation = S1_angle.zero
    ; min_edge_vertex_separation = S1_angle.zero
    ; snap_point = unimplemented_snap_point "s2_cell_id"
    }
  ;;
end

module Options = struct
  type t =
    { snap_function : Snap_function.t
    ; edge_snap_radius : S1_angle.t
    ; intersection_tolerance : S1_angle.t
    ; split_crossing_edges : bool
    ; idempotent : bool
    }

  let default () =
    let snap_function = Snap_function.identity () in
    { snap_function
    ; edge_snap_radius = snap_function.snap_radius
    ; intersection_tolerance = S1_angle.zero
    ; split_crossing_edges = false
    ; idempotent = true
    }
  ;;

  let create
    ?(snap_function = Snap_function.identity ())
    ?(split_crossing_edges = false)
    ?(idempotent = true)
    ~intersection_tolerance
    ()
    =
    let effective_tol =
      if split_crossing_edges
      then (
        let min_tol = S2_edge_crossings.intersection_error in
        if S1_angle.compare intersection_tolerance min_tol < 0
        then min_tol
        else intersection_tolerance)
      else intersection_tolerance
    in
    let edge_snap_radius = S1_angle.add snap_function.snap_radius effective_tol in
    { snap_function
    ; edge_snap_radius
    ; intersection_tolerance = effective_tol
    ; split_crossing_edges
    ; idempotent
    }
  ;;

  let max_edge_deviation t = S1_angle.mul t.edge_snap_radius #1.1
end

module Layer = struct
  type t =
    { graph_options : Graph_options.t
    ; build : Graph.t -> Error.t
    ; name : string
    }
end

type is_full_polygon_predicate = Graph.t -> bool * Error.t

type layer_state =
  { layer : Layer.t
  ; edges_begin : int
  ; mutable predicate : is_full_polygon_predicate
  }

type t =
  { options : Options.t
  ; input_vertices : Point_buffer.t
  ; input_edges : (int * int) Dynarray.t
  ; layers : layer_state Dynarray.t
  ; mutable snapping_requested : bool
  ; mutable snapping_needed : bool
  }

let create (options : Options.t) =
  let snapping_requested = S1_angle.compare options.edge_snap_radius S1_angle.zero > 0 in
  { options
  ; input_vertices = Point_buffer.create ()
  ; input_edges = Dynarray.create ()
  ; layers = Dynarray.create ()
  ; snapping_requested
  ; snapping_needed = false
  }
;;

let options t = t.options

let is_full_polygon_unspecified _g =
  ( false
  , Error.create
      ~code:"BUILDER_IS_FULL_PREDICATE_NOT_SPECIFIED"
      ~message:"A polygon layer produced a degenerate result and no predicate was set" )
;;

let is_full_polygon value _g = value, Error.ok

let start_layer t layer =
  let state =
    { layer
    ; edges_begin = Dynarray.length t.input_edges
    ; predicate = is_full_polygon false
    }
  in
  Dynarray.add_last t.layers state
;;

let require_layer t =
  if Dynarray.length t.layers = 0
  then raise_s [%message "S2_builder: no layer started; call start_layer first"]
;;

let add_vertex t p =
  require_layer t;
  let n = Point_buffer.length t.input_vertices in
  if n > 0 && S2_point.equal (Point_buffer.get t.input_vertices (n - 1)) p
  then n - 1
  else (
    Point_buffer.add_last t.input_vertices p;
    n)
;;

let current_layer_graph_options t =
  let n = Dynarray.length t.layers in
  (Dynarray.get t.layers (n - 1)).layer.graph_options
;;

let add_edge t v0 v1 =
  require_layer t;
  let opts = current_layer_graph_options t in
  match opts.degenerate_edges with
  | Degenerate_edges.Discard when S2_point.equal v0 v1 -> ()
  | _ ->
    let j0 = add_vertex t v0 in
    let j1 = add_vertex t v1 in
    Dynarray.add_last t.input_edges (j0, j1)
;;

let add_point t p = add_edge t p p

let add_polyline t vs =
  let n = Array.length vs in
  for i = 0 to n - 2 do
    add_edge t vs.(i) vs.(i + 1)
  done
;;

let add_loop t vs =
  let n = Array.length vs in
  if n = 0
  then ()
  else
    for i = 0 to n - 1 do
      add_edge t vs.(i) vs.((i + 1) mod n)
    done
;;

let add_shape t (shape : S2_shape.t) =
  for e = 0 to shape.#num_edges - 1 do
    let ed : S2_shape.Edge.t = shape.#edge e in
    add_edge t ed.#v0 ed.#v1
  done
;;

let add_intersection t p =
  if S1_angle.compare t.options.intersection_tolerance S1_angle.zero <= 0
  then
    raise_s [%message "S2_builder.add_intersection requires intersection_tolerance > 0"];
  t.snapping_needed <- true;
  let _ : int = add_vertex t p in
  ()
;;

let add_is_full_polygon_predicate t predicate =
  require_layer t;
  let n = Dynarray.length t.layers in
  let state = Dynarray.get t.layers (n - 1) in
  state.predicate <- predicate
;;

(* Site selection and snapping pipeline. This is the core of [build]; it replaces roughly
   1500 lines of C++ Voronoi machinery with the minimum needed to pass the polygon-layer
   tests:

   - When snap_radius = 0 and idempotent = true (the default identity snap function), emit
     the "ChooseAllVerticesAsSites" path from s2builder.cc:587-613: exact dedup of input
     vertices, then route each input edge through the deduplicated site ids.

   - When snap_radius > 0, fall back to a simple O(n^2) cluster merge: every input vertex
     joins the first existing site within snap_radius. The result is topology-preserving
     for well-separated inputs but NOT bit-exact with the Voronoi-based s2builder.cc
     pipeline (ChooseInitialSites + AddExtraSites + SnapEdge). Near-degenerate
     configurations - particularly ones where three or more sites fall within
     [min_vertex_separation, snap_radius] of each other - can choose different
     representative sites than the C++ reference.

   - When split_crossing_edges = true, enumerate all pairwise interior crossings before
     site selection and add each intersection point as an extra input vertex.

   TODO: port the full Voronoi site selection pipeline from s2builder.cc
   (ChooseInitialSites, CollectSiteEdges, AddExtraSites, SnapEdge) for bit-exact parity.
   Requires S2PointIndex, GetVoronoiSiteExclusion, and EdgeCircumcenterSign, which are not
   yet ported. *)

let add_edge_crossings t =
  let n_edges = Dynarray.length t.input_edges in
  let new_points = Point_buffer.create () in
  for i = 0 to n_edges - 1 do
    let a0_id, a1_id = Dynarray.get t.input_edges i in
    let a0 = Point_buffer.get t.input_vertices a0_id in
    let a1 = Point_buffer.get t.input_vertices a1_id in
    let mutable crosser = S2_edge_crosser.create ~a:a0 ~b:a1 in
    for j = i + 1 to n_edges - 1 do
      let b0_id, b1_id = Dynarray.get t.input_edges j in
      let b0 = Point_buffer.get t.input_vertices b0_id in
      let b1 = Point_buffer.get t.input_vertices b1_id in
      let r : S2_edge_crosser.with_sign = S2_edge_crosser.crossing_sign crosser b0 b1 in
      crosser <- r.#state;
      if r.#sign > 0
      then (
        let p = S2_edge_crossings.get_intersection a0 a1 b0 b1 in
        Point_buffer.add_last new_points p)
    done
  done;
  if Point_buffer.length new_points > 0
  then (
    t.snapping_needed <- true;
    for k = 0 to Point_buffer.length new_points - 1 do
      Point_buffer.add_last t.input_vertices (Point_buffer.get new_points k)
    done)
;;

let dedupe_and_snap_sites t =
  let n = Point_buffer.length t.input_vertices in
  let snap_fn = t.options.snap_function in
  let snap_radius_ca = S1_chord_angle.of_angle snap_fn.snap_radius in
  let zero_radius = S1_chord_angle.is_zero snap_radius_ca in
  let site_of = Array.create ~len:n 0 in
  let sites = Point_buffer.create () in
  for i = 0 to n - 1 do
    let v = Point_buffer.get t.input_vertices i in
    let p = snap_fn.snap_point v in
    if not (S2_point.equal p v) then t.snapping_needed <- true;
    let found = ref (-1) in
    let n_sites = Point_buffer.length sites in
    if zero_radius
    then
      for j = 0 to n_sites - 1 do
        if !found < 0 && S2_point.equal (Point_buffer.get sites j) p then found := j
      done
    else
      for j = 0 to n_sites - 1 do
        if !found < 0
        then (
          let q = Point_buffer.get sites j in
          let d = S2_point.chord_angle_between p q in
          if S1_chord_angle.compare d snap_radius_ca <= 0 then found := j)
      done;
    if !found >= 0
    then (
      if not (S2_point.equal v (Point_buffer.get sites !found))
      then t.snapping_needed <- true;
      site_of.(i) <- !found)
    else (
      site_of.(i) <- Point_buffer.length sites;
      Point_buffer.add_last sites p)
  done;
  Point_buffer.to_array sites, site_of
;;

let snap_edge t ~sites ~site_of (v0_id, v1_id) =
  let s0 = site_of.(v0_id) in
  let s1 = site_of.(v1_id) in
  if not t.snapping_needed
  then if s0 = s1 then [| s0 |] else [| s0; s1 |]
  else (
    let v0 = sites.(s0) in
    let v1 = sites.(s1) in
    if s0 = s1
    then [| s0 |]
    else (
      let edge_snap_radius_ca = S1_chord_angle.of_angle t.options.edge_snap_radius in
      let n_sites = Array.length sites in
      (* Parallel arrays of site id and edge parameter along [v0, v1]. *)
      let ids = Dynarray.create () in
      let ts = Array.create ~len:(n_sites + 2) #0.0 in
      let t_at = ref 0 in
      let add id tparam =
        Dynarray.add_last ids id;
        ts.(!t_at) <- tparam;
        t_at := !t_at + 1
      in
      add s0 #0.0;
      add s1 #1.0;
      for k = 0 to n_sites - 1 do
        if k <> s0 && k <> s1
        then (
          let p = sites.(k) in
          if S2_edge_distances.is_distance_less p v0 v1 edge_snap_radius_ca
          then (
            let tp = S2_edge_distances.get_distance_fraction p v0 v1 in
            if Float_u.O.(tp > #0.0 && tp < #1.0) then add k tp))
      done;
      let n = Dynarray.length ids in
      let perm = Array.init n ~f:Fn.id in
      Array.sort perm ~compare:(fun a b -> Float_u.compare ts.(a) ts.(b));
      let chain = Dynarray.create () in
      Array.iter perm ~f:(fun idx ->
        let sid = Dynarray.get ids idx in
        let nn = Dynarray.length chain in
        if nn = 0 || Dynarray.get chain (nn - 1) <> sid then Dynarray.add_last chain sid);
      Array.init (Dynarray.length chain) ~f:(fun i -> Dynarray.get chain i)))
;;

let build t =
  if (not t.options.idempotent) && t.snapping_requested then t.snapping_needed <- true;
  if t.options.split_crossing_edges then add_edge_crossings t;
  let sites, site_of = dedupe_and_snap_sites t in
  let lexicon = Id_set_lexicon.create () in
  let n_layers = Dynarray.length t.layers in
  let n_input_edges = Dynarray.length t.input_edges in
  let mutable result = Error.ok in
  let layer_end i =
    if i + 1 = n_layers
    then n_input_edges
    else (Dynarray.get t.layers (i + 1)).edges_begin
  in
  for li = 0 to n_layers - 1 do
    if Error.is_ok result
    then (
      let state = Dynarray.get t.layers li in
      let e_begin = state.edges_begin in
      let e_end = layer_end li in
      let layer_edges = Dynarray.create () in
      let layer_input_ids = Dynarray.create () in
      for e = e_begin to e_end - 1 do
        let v0, v1 = Dynarray.get t.input_edges e in
        let chain = snap_edge t ~sites ~site_of (v0, v1) in
        let input_id = Id_set_lexicon.add_singleton lexicon e in
        if Array.length chain = 1
        then (
          match state.layer.graph_options.degenerate_edges with
          | Degenerate_edges.Discard -> ()
          | _ ->
            Dynarray.add_last
              layer_edges
              (Graph.Edge.create ~src:chain.(0) ~dst:chain.(0));
            Dynarray.add_last layer_input_ids input_id)
        else
          for i = 1 to Array.length chain - 1 do
            Dynarray.add_last
              layer_edges
              (Graph.Edge.create ~src:chain.(i - 1) ~dst:chain.(i));
            Dynarray.add_last layer_input_ids input_id
          done
      done;
      let edges_ref =
        ref
          (Array.init (Dynarray.length layer_edges) ~f:(fun i ->
             Dynarray.get layer_edges i))
      in
      let ids_ref =
        ref
          (Array.init (Dynarray.length layer_input_ids) ~f:(fun i ->
             Dynarray.get layer_input_ids i))
      in
      let opts_ref = ref state.layer.graph_options in
      let err =
        Graph.process_edges
          ~options:opts_ref
          ~edges:edges_ref
          ~input_ids:ids_ref
          ~id_set_lexicon:lexicon
      in
      if not (Error.is_ok err) then result <- err;
      if Error.is_ok result
      then (
        let predicate = state.predicate in
        let g : Graph.t =
          { options = !opts_ref
          ; vertices = sites
          ; edges = !edges_ref
          ; input_edge_id_set_ids = !ids_ref
          ; input_edge_id_set_lexicon = lexicon
          ; is_full_polygon_predicate = predicate
          }
        in
        let err = state.layer.build g in
        if not (Error.is_ok err) then result <- err))
  done;
  Point_buffer.clear t.input_vertices;
  Dynarray.clear t.input_edges;
  Dynarray.clear t.layers;
  t.snapping_needed <- false;
  result
;;
