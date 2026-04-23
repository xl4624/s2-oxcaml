open Core

module Cell_relation = struct
  type t =
    | Indexed
    | Subdivided
    | Disjoint
end

module Clipped_shape = struct
  type t =
    { shape_id : int
    ; contains_center : bool
    ; edges : int array
    }

  let shape_id t = t.shape_id
  let contains_center t = t.contains_center
  let num_edges t = Array.length t.edges
  let edge t i = t.edges.(i)
end

module Index_cell = struct
  type t = { shapes : Clipped_shape.t array } [@@unboxed]

  let num_clipped t = Array.length t.shapes

  let clipped t i =
    if i < 0 || i >= Array.length t.shapes
    then failwith "S2_shape_index.Index_cell.clipped: index out of range";
    t.shapes.(i)
  ;;

  let find_clipped t ~shape_id =
    let n = Array.length t.shapes in
    let rec loop i =
      if i >= n
      then None
      else if t.shapes.(i).shape_id = shape_id
      then Some t.shapes.(i)
      else loop (i + 1)
    in
    loop 0
  ;;
end

(* Boxed wrapper so [S2_shape.t] (an unboxed record) can live inside
   hashtables and other containers that require a [value] layout. *)
type shape_box = { shape : S2_shape.t }

(* Boxed (cell_id, cell) pair so that bits64-layout cell ids can participate
   in the intermediate list used while the build is in progress. The sorted
   arrays produced by [rebuild_cell_ids] drop this wrapper. *)
type build_entry =
  { cell_id : S2_cell_id.t
  ; cell : Index_cell.t
  }

type face_edge =
  { shape_id : int
  ; edge_id : int
  ; max_level : int
  ; has_interior : bool
  ; a : R2_point.t
  ; b : R2_point.t
  ; v0 : S2_point.t
  ; v1 : S2_point.t
  }

type clipped_edge =
  { face_edge : face_edge
  ; bound : R2_rect.t
  }

type index =
  { mutable shapes : (int, shape_box) Hashtbl.Poly.t
  ; mutable next_shape_id : int
  ; max_edges_per_cell : int
  ; cell_padding : float#
  ; mutable build_entries : build_entry list
  ; mutable cell_ids_sorted : S2_cell_id.t array
  ; mutable cells_sorted : Index_cell.t array
  ; mutable fresh : bool
  ; mutable pending_additions_pos : int
  }

type t = index

(* Padding applied to each cell when clipping edges to cell boundaries.
   Matches [MutableS2ShapeIndex::kCellPadding] so that small numerical errors
   in clip coordinates do not drop an edge from the cells it actually
   crosses. *)
let cell_padding = S2_edge_clipping.shape_index_cell_padding

(* A well-defined "outside" point used by the interior-tracking crosser so
   that parity flips are counted from a consistent reference. Any point far
   from the shapes works; we use face 0's lower-left corner. *)
let tracker_origin () = R3_vector.normalize (S2_coords.face_uv_to_xyz 0 (-#1.0) (-#1.0))

(* Ray-cast from the shape's reference point to [p], counting crossings.
   Used during indexing to establish whether the tracker's focus point lies
   inside a newly added shape. O(num_edges); not intended for use in hot
   query paths. *)
let contains_brute_force (shape : S2_shape.t) p =
  if shape.#dimension <> 2
  then false
  else (
    let ref_pt = shape.#reference_point in
    if S2_point.equal ref_pt.#point p
    then ref_pt.#contained
    else
      let open S2_edge_crosser in
      let mutable crosser = create ~a:ref_pt.#point ~b:p in
      let mutable inside = ref_pt.#contained in
      for e = 0 to shape.#num_edges - 1 do
        let #{ v0; v1 } : S2_shape.Edge.t = shape.#edge e in
        let #{ state; crossing } = edge_or_vertex_crossing crosser v0 v1 in
        crosser <- state;
        if crossing then inside <- not inside
      done;
      inside)
;;

let max_level_for_edge v0 v1 =
  let open Float_u.O in
  let cell_size = R3_vector.norm (S2_point.sub v0 v1) * #1.0 in
  S2_metrics.get_level_for_max_value S2_metrics.avg_edge cell_size
;;

let interpolate_line x ax bx ay by =
  let open Float_u.O in
  if ax = bx
  then ay
  else if Float_u.abs (ax - x) <= Float_u.abs (bx - x)
  then ay + ((by - ay) * (x - ax) / (bx - ax))
  else by + ((ay - by) * (x - bx) / (ax - bx))
;;

type tracker =
  { mutable is_active : bool
  ; mutable a : S2_point.t
  ; mutable b : S2_point.t
  ; mutable next_cell_id : S2_cell_id.t
  ; mutable crosser : S2_edge_crosser.t
  ; mutable shape_ids : int list
  }

(* Maintain a sorted list of shape ids by toggling membership: present ids
   are removed, absent ids are inserted in order. Used by the interior
   tracker to track "which polygons currently contain the focus point" as
   it sweeps along the Hilbert curve. *)
let rec toggle_sorted id = function
  | [] -> [ id ]
  | x :: xs ->
    if x = id then xs else if id < x then id :: x :: xs else x :: toggle_sorted id xs
;;

let create_tracker () =
  let origin = tracker_origin () in
  let next_cell_id =
    S2_cell_id.child_begin_at_level (S2_cell_id.from_face_exn 0) S2_cell_id.max_level
  in
  let t =
    { is_active = false
    ; a = origin
    ; b = origin
    ; next_cell_id
    ; crosser = S2_edge_crosser.create ~a:origin ~b:origin
    ; shape_ids = []
    }
  in
  let start = tracker_origin () in
  t.a <- t.b;
  t.b <- start;
  t.crosser <- S2_edge_crosser.create ~a:t.a ~b:t.b;
  t
;;

let tracker_draw_to t b =
  t.a <- t.b;
  t.b <- b;
  t.crosser <- S2_edge_crosser.create ~a:t.a ~b:t.b
;;

let tracker_move_to t b = t.b <- b
let tracker_set_next_cell_id t cell_id = t.next_cell_id <- S2_cell_id.range_min cell_id

let tracker_at_cell_id t cell_id =
  S2_cell_id.equal (S2_cell_id.range_min cell_id) t.next_cell_id
;;

let tracker_add_shape t ~shape_id ~contains_focus =
  t.is_active <- true;
  if contains_focus then t.shape_ids <- toggle_sorted shape_id t.shape_ids
;;

let tracker_test_edge t fe =
  if fe.has_interior
  then (
    let #{ state; crossing } : S2_edge_crosser.with_bool =
      S2_edge_crosser.edge_or_vertex_crossing t.crosser fe.v0 fe.v1
    in
    t.crosser <- state;
    if crossing then t.shape_ids <- toggle_sorted fe.shape_id t.shape_ids)
;;

(* Clip a face edge to each of the 6 cube faces it may cross, padding the
   face bounds by [cell_padding]. If both endpoints lie strictly inside one
   face (with room for padding) we take the fast path and skip the 6-way
   clip. The resulting (per-face) lists drive the face-by-face recursive
   subdivision in [update_face_edges]. *)
let add_face_edge (fe : face_edge) (all_edges : face_edge list array) =
  let v0 = fe.v0 in
  let v1 = fe.v1 in
  let a_face = S2_coords.get_face (S2_point.to_r3 v0) in
  let b_face = S2_coords.get_face (S2_point.to_r3 v1) in
  let max_uv = Float_u.O.(#1.0 - cell_padding) in
  if Int.equal a_face b_face
  then (
    let a = S2_coords.valid_face_xyz_to_uv a_face (S2_point.to_r3 v0) in
    let b = S2_coords.valid_face_xyz_to_uv a_face (S2_point.to_r3 v1) in
    if Float_u.O.(
         Float_u.abs (R2_point.x a) <= max_uv
         && Float_u.abs (R2_point.y a) <= max_uv
         && Float_u.abs (R2_point.x b) <= max_uv
         && Float_u.abs (R2_point.y b) <= max_uv)
    then (
      let fe = { fe with a; b } in
      all_edges.(a_face) <- fe :: all_edges.(a_face))
    else
      for face = 0 to 5 do
        match S2_edge_clipping.clip_to_padded_face v0 v1 face ~padding:cell_padding with
        | None -> ()
        | Some clipped ->
          let fe = { fe with a = clipped.a; b = clipped.b } in
          all_edges.(face) <- fe :: all_edges.(face)
      done)
  else
    for face = 0 to 5 do
      match S2_edge_clipping.clip_to_padded_face v0 v1 face ~padding:cell_padding with
      | None -> ()
      | Some clipped ->
        let fe = { fe with a = clipped.a; b = clipped.b } in
        all_edges.(face) <- fe :: all_edges.(face)
    done
;;

let test_all_edges edges t =
  List.iter edges ~f:(fun ce ->
    if ce.face_edge.has_interior then tracker_test_edge t ce.face_edge)
;;

let count_shapes (edges : clipped_edge list) shape_ids =
  let shape_ids = Array.of_list shape_ids in
  let n_ids = Array.length shape_ids in
  let count = ref 0 in
  let last_shape_id = ref (-1) in
  let shape_id_idx = ref 0 in
  List.iter edges ~f:(fun ce ->
    let sid = ce.face_edge.shape_id in
    if sid <> !last_shape_id
    then (
      incr count;
      last_shape_id := sid;
      let stop = ref false in
      while !shape_id_idx < n_ids && not !stop do
        let clipped_next = shape_ids.(!shape_id_idx) in
        if clipped_next > sid
        then stop := true
        else (
          if clipped_next < sid then incr count;
          incr shape_id_idx)
      done));
  !count + (n_ids - !shape_id_idx)
;;

let add_shape_internal (idx : index) shape_id (shape : S2_shape.t) all_edges t =
  let has_interior = shape.#dimension = 2 in
  if has_interior
  then tracker_add_shape t ~shape_id ~contains_focus:(contains_brute_force shape t.b);
  for e = 0 to shape.#num_edges - 1 do
    let #{ v0; v1 } : S2_shape.Edge.t = shape.#edge e in
    let fe =
      { shape_id
      ; edge_id = e
      ; max_level = max_level_for_edge v0 v1
      ; has_interior
      ; a = R2_point.zero
      ; b = R2_point.zero
      ; v0
      ; v1
      }
    in
    add_face_edge fe all_edges
  done
;;

let rebuild_cell_ids (t : index) =
  let entries =
    List.sort t.build_entries ~compare:(fun a b -> S2_cell_id.compare a.cell_id b.cell_id)
  in
  let n = List.length entries in
  let ids = Array.create ~len:n S2_cell_id.none in
  let cells = Array.create ~len:n { Index_cell.shapes = [||] } in
  List.iteri entries ~f:(fun i e ->
    ids.(i) <- e.cell_id;
    cells.(i) <- e.cell);
  t.cell_ids_sorted <- ids;
  t.cells_sorted <- cells;
  t.build_entries <- []
;;

let shrink_to_fit_index (_t : index) pcell bound ~is_first_update:_ =
  S2_padded_cell.shrink_to_fit pcell bound
;;

let update_bound_edge edge ~u_end ~u ~v_end ~v =
  let x =
    if u_end = 0
    then R1_interval.create ~lo:u ~hi:(R1_interval.hi (R2_rect.x edge.bound))
    else R1_interval.create ~lo:(R1_interval.lo (R2_rect.x edge.bound)) ~hi:u
  in
  let y =
    if v_end = 0
    then R1_interval.create ~lo:v ~hi:(R1_interval.hi (R2_rect.y edge.bound))
    else R1_interval.create ~lo:(R1_interval.lo (R2_rect.y edge.bound)) ~hi:v
  in
  { edge with bound = R2_rect.create_intervals_exn ~x ~y }
;;

let clip_u_bound edge ~u_end ~u =
  let x_iv = R2_rect.x edge.bound in
  let skip =
    if u_end = 0
    then Float_u.O.(R1_interval.lo x_iv >= u)
    else Float_u.O.(R1_interval.hi x_iv <= u)
  in
  if skip
  then edge
  else (
    let e = edge.face_edge in
    let v =
      R1_interval.project_exn
        (R2_rect.y edge.bound)
        (interpolate_line
           u
           (R2_point.x e.a)
           (R2_point.x e.b)
           (R2_point.y e.a)
           (R2_point.y e.b))
    in
    let positive_slope =
      Bool.equal
        Float_u.O.(R2_point.x e.a > R2_point.x e.b)
        Float_u.O.(R2_point.y e.a > R2_point.y e.b)
    in
    let v_end = if Bool.equal (u_end = 1) positive_slope then 1 else 0 in
    update_bound_edge edge ~u_end ~u ~v_end ~v)
;;

let clip_v_bound edge ~v_end ~v =
  let y_iv = R2_rect.y edge.bound in
  let skip =
    if v_end = 0
    then Float_u.O.(R1_interval.lo y_iv >= v)
    else Float_u.O.(R1_interval.hi y_iv <= v)
  in
  if skip
  then edge
  else (
    let e = edge.face_edge in
    let u =
      R1_interval.project_exn
        (R2_rect.x edge.bound)
        (interpolate_line
           v
           (R2_point.y e.a)
           (R2_point.y e.b)
           (R2_point.x e.a)
           (R2_point.x e.b))
    in
    let positive_slope =
      Bool.equal
        Float_u.O.(R2_point.x e.a > R2_point.x e.b)
        Float_u.O.(R2_point.y e.a > R2_point.y e.b)
    in
    let u_end = if Bool.equal (v_end = 1) positive_slope then 1 else 0 in
    update_bound_edge edge ~u_end ~u ~v_end ~v)
;;

let clip_v_axis edge middle_y =
  let y_lo = R1_interval.lo middle_y in
  let y_hi = R1_interval.hi middle_y in
  let yb = R2_rect.y edge.bound in
  if Float_u.O.(R1_interval.hi yb <= y_lo)
  then `Lower edge
  else if Float_u.O.(R1_interval.lo yb >= y_hi)
  then `Upper edge
  else `Both (clip_v_bound edge ~v_end:1 ~v:y_hi, clip_v_bound edge ~v_end:0 ~v:y_lo)
;;

(* Attempt to emit [pcell] as a leaf index cell. Returns [true] if the cell
   was accepted (either because it has few enough "long" edges relative to
   [max_edges_per_cell] or because it is empty with no tracked shapes), and
   [false] to request further subdivision by [update_edges]. When accepted,
   the tracker is advanced through the cell so that per-cell [contains_center]
   flags reflect the correct set of interior shapes. *)
let make_index_cell (t : index) pcell edges tracker : bool =
  match edges, tracker.shape_ids with
  | [], [] -> true
  | _ ->
    let p_level = S2_padded_cell.level pcell in
    let count =
      List.fold edges ~init:0 ~f:(fun acc ce ->
        acc + if p_level < ce.face_edge.max_level then 1 else 0)
    in
    if count > t.max_edges_per_cell
    then false
    else (
      if tracker.is_active && not (List.is_empty edges)
      then (
        if not (tracker_at_cell_id tracker (S2_padded_cell.id pcell))
        then tracker_move_to tracker (S2_padded_cell.entry_vertex pcell);
        tracker_draw_to tracker (S2_padded_cell.center pcell);
        test_all_edges edges tracker);
      let cshape_ids = tracker.shape_ids in
      let num_shapes = count_shapes edges cshape_ids in
      let cell_shapes = ref [] in
      let e_next = ref 0 in
      let edges_arr = Array.of_list edges in
      let n_e = Array.length edges_arr in
      let c_next_idx = ref 0 in
      let cshape_ids_arr = Array.of_list cshape_ids in
      let n_c = Array.length cshape_ids_arr in
      for _i = 0 to num_shapes - 1 do
        let eshape_id =
          if !e_next >= n_e
          then t.next_shape_id
          else edges_arr.(!e_next).face_edge.shape_id
        in
        let cshape_id =
          if !c_next_idx >= n_c then t.next_shape_id else cshape_ids_arr.(!c_next_idx)
        in
        let e_begin = !e_next in
        let clipped : Clipped_shape.t =
          if cshape_id < eshape_id
          then (
            incr c_next_idx;
            { shape_id = cshape_id; contains_center = true; edges = [||] })
          else (
            while !e_next < n_e && edges_arr.(!e_next).face_edge.shape_id = eshape_id do
              incr e_next
            done;
            let n_edge = !e_next - e_begin in
            let ed = Array.create ~len:n_edge 0 in
            for k = 0 to n_edge - 1 do
              ed.(k) <- edges_arr.(e_begin + k).face_edge.edge_id
            done;
            let contains_center =
              if cshape_id = eshape_id
              then (
                incr c_next_idx;
                true)
              else false
            in
            { shape_id = eshape_id; contains_center; edges = ed })
        in
        cell_shapes := clipped :: !cell_shapes
      done;
      let shapes_arr = Array.of_list (List.rev !cell_shapes) in
      let icell : Index_cell.t = { shapes = shapes_arr } in
      t.build_entries
      <- { cell_id = S2_padded_cell.id pcell; cell = icell } :: t.build_entries;
      if tracker.is_active && not (List.is_empty edges)
      then (
        tracker_draw_to tracker (S2_padded_cell.exit_vertex pcell);
        test_all_edges edges tracker;
        tracker_set_next_cell_id tracker (S2_padded_cell.id pcell));
      true)
;;

let rec skip_cell_range t ~begin_ ~end_ tracker =
  match tracker.shape_ids with
  | [] -> ()
  | _ :: _ ->
    let u = S2_cell_union.from_begin_end begin_ end_ in
    for i = 0 to S2_cell_union.num_cells u - 1 do
      let cell_id = S2_cell_union.cell_id u i in
      let pcell = S2_padded_cell.create cell_id ~padding:t.cell_padding in
      update_edges t pcell [] tracker ~disjoint_from_index:true
    done

and update_edges t pcell edges tracker ~disjoint_from_index:_ =
  if not (make_index_cell t pcell edges tracker)
  then (
    let mid = S2_padded_cell.middle pcell in
    let middle_x = R2_rect.x mid in
    let middle_y = R2_rect.y mid in
    let mx_lo = R1_interval.lo middle_x in
    let mx_hi = R1_interval.hi middle_x in
    let my_lo = R1_interval.lo middle_y in
    let my_hi = R1_interval.hi middle_y in
    let c00 = ref [] in
    let c01 = ref [] in
    let c10 = ref [] in
    let c11 = ref [] in
    let push r e = r := e :: !r in
    List.iter edges ~f:(fun edge ->
      let xb = R2_rect.x edge.bound in
      let yb = R2_rect.y edge.bound in
      if Float_u.O.(R1_interval.hi xb <= mx_lo)
      then (
        match clip_v_axis edge middle_y with
        | `Lower e -> push c00 e
        | `Upper e -> push c01 e
        | `Both (a, b) ->
          push c00 a;
          push c01 b)
      else if Float_u.O.(R1_interval.lo xb >= mx_hi)
      then (
        match clip_v_axis edge middle_y with
        | `Lower e -> push c10 e
        | `Upper e -> push c11 e
        | `Both (a, b) ->
          push c10 a;
          push c11 b)
      else if Float_u.O.(R1_interval.hi yb <= my_lo)
      then (
        push c00 (clip_u_bound edge ~u_end:1 ~u:mx_hi);
        push c10 (clip_u_bound edge ~u_end:0 ~u:mx_lo))
      else if Float_u.O.(R1_interval.lo yb >= my_hi)
      then (
        push c01 (clip_u_bound edge ~u_end:1 ~u:mx_hi);
        push c11 (clip_u_bound edge ~u_end:0 ~u:mx_lo))
      else (
        let left = clip_u_bound edge ~u_end:1 ~u:mx_hi in
        (match clip_v_axis left middle_y with
         | `Lower e -> push c00 e
         | `Upper e -> push c01 e
         | `Both (a, b) ->
           push c00 a;
           push c01 b);
        let right = clip_u_bound edge ~u_end:0 ~u:mx_lo in
        match clip_v_axis right middle_y with
        | `Lower e -> push c10 e
        | `Upper e -> push c11 e
        | `Both (a, b) ->
          push c10 a;
          push c11 b));
    for pos = 0 to 3 do
      let #(i, j) = S2_padded_cell.child_ij_of_pos pcell ~pos in
      (* Edges are pushed onto [c00/c01/c10/c11] via cons, so reversing now
         restores the original (shape_id, edge_id) ascending order used to
         build [clipped_edges] at the face level. Cells emit edges in that
         order. *)
      let lst =
        match i, j with
        | 0, 0 -> List.rev !c00
        | 0, 1 -> List.rev !c01
        | 1, 0 -> List.rev !c10
        | 1, 1 -> List.rev !c11
        | _ -> assert false
      in
      if (not (List.is_empty lst)) || not (List.is_empty tracker.shape_ids)
      then (
        let child_p = S2_padded_cell.child_ij pcell ~i ~j in
        update_edges t child_p lst tracker ~disjoint_from_index:true)
    done)
;;

let update_face_edges (t : index) face face_edges tracker ~is_first_update =
  let face_edges = List.rev face_edges in
  let num_edges = List.length face_edges in
  if num_edges = 0 && List.is_empty tracker.shape_ids
  then ()
  else (
    let clipped_edges =
      List.map face_edges ~f:(fun (fe : face_edge) ->
        let bound = R2_rect.from_point_pair fe.a fe.b in
        { face_edge = fe; bound })
    in
    let clipped_edges =
      List.sort clipped_edges ~compare:(fun x y ->
        let c = Int.compare x.face_edge.shape_id y.face_edge.shape_id in
        if c <> 0 then c else Int.compare x.face_edge.edge_id y.face_edge.edge_id)
    in
    let rec fold_bound acc = function
      | [] -> acc
      | ce :: rest -> fold_bound (R2_rect.add_rect acc ce.bound) rest
    in
    let bound = fold_bound R2_rect.empty clipped_edges in
    let face_id = S2_cell_id.from_face_exn face in
    let pcell = S2_padded_cell.create face_id ~padding:t.cell_padding in
    let disjoint_from_index = is_first_update in
    if num_edges > 0
    then (
      let shrunk_id = shrink_to_fit_index t pcell bound ~is_first_update in
      if not (S2_cell_id.equal shrunk_id face_id)
      then (
        skip_cell_range
          t
          ~begin_:(S2_cell_id.range_min face_id)
          ~end_:(S2_cell_id.range_min shrunk_id)
          tracker;
        let pcell2 = S2_padded_cell.create shrunk_id ~padding:t.cell_padding in
        update_edges t pcell2 clipped_edges tracker ~disjoint_from_index;
        let after_max = S2_cell_id.next (S2_cell_id.range_max shrunk_id) in
        let face_end = S2_cell_id.next (S2_cell_id.range_max face_id) in
        skip_cell_range t ~begin_:after_max ~end_:face_end tracker)
      else update_edges t pcell clipped_edges tracker ~disjoint_from_index)
    else update_edges t pcell clipped_edges tracker ~disjoint_from_index)
;;

(* Main indexing entry point. Discards any previous cell map, projects every
   shape's edges onto the 6 cube faces, then recursively subdivides each face
   until every leaf cell has at most [max_edges_per_cell] non-long edges.
   Interior tracking happens in a single sweep along the Hilbert curve, so
   containment flags for polygon shapes emerge naturally as the tracker's
   focus crosses edges. *)
let apply_updates_internal (t : index) =
  t.build_entries <- [];
  t.cell_ids_sorted <- [||];
  t.cells_sorted <- [||];
  let tracker = create_tracker () in
  let all_edges = Array.init 6 ~f:(fun _ -> []) in
  for id = 0 to t.next_shape_id - 1 do
    match Hashtbl.Poly.find t.shapes id with
    | None -> ()
    | Some { shape } -> add_shape_internal t id shape all_edges tracker
  done;
  let is_first = ref true in
  for face = 0 to 5 do
    update_face_edges t face all_edges.(face) tracker ~is_first_update:!is_first;
    is_first := false
  done;
  rebuild_cell_ids t;
  t.pending_additions_pos <- t.next_shape_id;
  t.fresh <- true
;;

let create ?(max_edges_per_cell = 10) () =
  { shapes = Hashtbl.Poly.create ()
  ; next_shape_id = 0
  ; max_edges_per_cell
  ; cell_padding
  ; build_entries = []
  ; cell_ids_sorted = [||]
  ; cells_sorted = [||]
  ; fresh = true
  ; pending_additions_pos = 0
  }
;;

let add (t : index) shape =
  let id = t.next_shape_id in
  Hashtbl.Poly.set t.shapes ~key:id ~data:{ shape };
  t.next_shape_id <- id + 1;
  t.fresh <- false;
  id
;;

let num_shape_ids t = t.next_shape_id

let shape t id =
  if id < 0 || id >= t.next_shape_id
  then (
    match raise_s [%message "S2_shape_index.shape: id out of range" (id : int)] with
    | (_ : Nothing.t) -> .)
  else (
    match Hashtbl.Poly.find t.shapes id with
    | Some { shape } -> shape
    | None ->
      (match raise_s [%message "S2_shape_index.shape: missing shape" (id : int)] with
       | (_ : Nothing.t) -> .))
;;

let build t = if not t.fresh then apply_updates_internal t
let is_fresh t = t.fresh

module Iterator = struct
  type t =
    { index : index
    ; mutable position : int
    ; mutable cur_id : S2_cell_id.t
    ; mutable cur_cell : Index_cell.t option
    }

  let refresh it =
    let cells = it.index.cell_ids_sorted in
    let n = Array.length cells in
    if it.position < n
    then (
      it.cur_id <- cells.(it.position);
      it.cur_cell <- Some it.index.cells_sorted.(it.position))
    else (
      it.cur_id <- S2_cell_id.sentinel;
      it.cur_cell <- None)
  ;;

  let create index = { index; position = 0; cur_id = S2_cell_id.none; cur_cell = None }

  let begin_at it =
    it.position <- 0;
    refresh it
  ;;

  let at_end it =
    it.position <- Array.length it.index.cell_ids_sorted;
    refresh it
  ;;

  let seek it target =
    let cells = it.index.cell_ids_sorted in
    let n = Array.length cells in
    let rec go lo hi =
      if lo = hi
      then lo
      else (
        let mid = (lo + hi) / 2 in
        if S2_cell_id.compare cells.(mid) target < 0 then go (mid + 1) hi else go lo mid)
    in
    it.position <- go 0 n;
    refresh it
  ;;

  let next it =
    it.position <- it.position + 1;
    refresh it
  ;;

  let prev it =
    if it.position <= 0
    then false
    else (
      it.position <- it.position - 1;
      refresh it;
      true)
  ;;

  let is_done it = S2_cell_id.equal it.cur_id S2_cell_id.sentinel
  let cell_id it = it.cur_id

  let index_cell it =
    match it.cur_cell with
    | Some c -> c
    | None -> failwith "S2_shape_index.Iterator.index_cell: iterator at end"
  ;;

  let locate_point it p =
    let target = S2_cell_id.from_point (S2_point.to_r3 p) in
    seek it target;
    if (not (is_done it))
       && S2_cell_id.compare (S2_cell_id.range_min it.cur_id) target <= 0
    then true
    else if prev it && S2_cell_id.compare (S2_cell_id.range_max it.cur_id) target >= 0
    then true
    else false
  ;;

  let locate_cell_id it target =
    seek it (S2_cell_id.range_min target);
    if not (is_done it)
    then
      if S2_cell_id.compare it.cur_id target >= 0
         && S2_cell_id.compare (S2_cell_id.range_min it.cur_id) target <= 0
      then Cell_relation.Indexed
      else if S2_cell_id.compare it.cur_id (S2_cell_id.range_max target) <= 0
      then Cell_relation.Subdivided
      else if prev it && S2_cell_id.compare (S2_cell_id.range_max it.cur_id) target >= 0
      then Cell_relation.Indexed
      else Cell_relation.Disjoint
    else if prev it && S2_cell_id.compare (S2_cell_id.range_max it.cur_id) target >= 0
    then Cell_relation.Indexed
    else Cell_relation.Disjoint
  ;;
end

(* TODO: port the following from mutable_s2shape_index.cc:
   - Remove / RemoveAll shape support (shape id reuse is still disallowed)
   - Minimize (drop cell map while keeping shapes)
   - Encode / Decode
   - SpaceUsed and S2MemoryTracker integration
   - EncodedS2ShapeIndex (a read-only sibling that works off encoded bytes)
*)

let iterator t =
  build t;
  let it = Iterator.create t in
  Iterator.begin_at it;
  it
;;
