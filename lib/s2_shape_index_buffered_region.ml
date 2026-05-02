open Core

type t =
  { region : S2_shape_index_region.t
  ; radius : S1_chord_angle.t
      (* Successor of [radius]; used to convert a strict "less than [radius]" test into an
         inclusive "less than or equal to [radius]" test in the closest-edge query. This
         preserves correct behaviour when [radius = 0]. *)
  ; radius_successor : S1_chord_angle.t
  ; query : S2_closest_edge_query.t
  }

let create index radius =
  let region = S2_shape_index_region.create index in
  let options = S2_closest_edge_query.Options.create ~include_interiors:true () in
  let query = S2_closest_edge_query.create ~options index () in
  { region; radius; radius_successor = S1_chord_angle.successor radius; query }
;;

let create_angle index radius =
  if S1_angle.compare radius S1_angle.zero < 0
  then (
    match
      raise_s
        [%message
          "S2_shape_index_buffered_region.create_angle: negative radius"
            ~radians:(Float_u.to_float (S1_angle.radians radius) : float)]
    with
    | (_ : Nothing.t) -> .)
  else create index (S1_chord_angle.of_angle radius)
;;

let index t = S2_shape_index_region.index t.region
let radius t = t.radius

let cap_bound t =
  let orig_cap = S2_shape_index_region.cap_bound t.region in
  if S2_cap.is_empty orig_cap
  then S2_cap.empty
  else
    S2_cap.of_center_angle
      (S2_cap.center orig_cap)
      (S1_angle.add (S2_cap.radius_angle orig_cap) (S1_chord_angle.to_angle t.radius))
;;

let rect_bound t =
  let orig_rect = S2_shape_index_region.rect_bound t.region in
  S2_latlng_rect.expanded_by_distance orig_rect (S1_chord_angle.to_angle t.radius)
;;

(* Replace each cell in the unbuffered covering with the four (or three) cells at the
   chosen [max_level] sharing a vertex with the original cell. This grows the covering by
   ~16x in area but always contains the original cell buffered by [radius]. If the buffer
   radius is larger than any cell at level 0, fall back to the six face cells. If the
   unbuffered covering already contains a face cell, do the same. *)
let cell_union_bound t =
  let radius_rad = S1_angle.radians (S1_chord_angle.to_angle t.radius) in
  let max_level =
    S2_metrics.get_level_for_min_value S2_metrics.min_width radius_rad - 1
  in
  if max_level < 0
  then S2_cap.cell_union_bound S2_cap.full
  else (
    let orig_cellids = S2_shape_index_region.cell_union_bound t.region in
    (* The unbuffered covering has at most 6 cells; each contributes at most 4 vertex
       neighbours, so 24 is a safe upper bound. *)
    let buf = Array.create ~len:(Array.length orig_cellids * 4) S2_cell_id.none in
    let mutable len = 0 in
    let mutable use_full = false in
    let i = ref 0 in
    while (not use_full) && !i < Array.length orig_cellids do
      let id = orig_cellids.(!i) in
      if S2_cell_id.is_face id
      then use_full <- true
      else (
        let level = Stdlib.min max_level (S2_cell_id.level id - 1) in
        let neighbors = S2_cell_id.vertex_neighbors id level in
        for k = 0 to Array.length neighbors - 1 do
          buf.(len) <- neighbors.(k);
          len <- len + 1
        done);
      incr i
    done;
    if use_full
    then S2_cap.cell_union_bound S2_cap.full
    else (Array.sub [@kind bits64]) buf ~pos:0 ~len)
;;

let contains_cell t cell =
  (* If the buffered region covers the entire sphere, every cell is contained. *)
  if S1_chord_angle.compare t.radius_successor S1_chord_angle.straight > 0
  then true
  else if S2_shape_index_region.contains_cell t.region cell
  then true
  else (
    (* Approximate the cell by its bounding cap. If the buffer radius is smaller than the
       cell's own bounding cap radius, the cell can never be entirely contained. *)
    let cap = S2_cell.cap_bound cell in
    if S1_chord_angle.compare t.radius (S2_cap.radius_chord cap) < 0
    then false
    else (
      (* Containment heuristic: verify that the closest edge of the indexed geometry to
         the cell center is within [radius - cap_radius] of the center. By the triangle
         inequality, every point in the cell is then within [radius] of that edge. *)
      let target = S2_closest_edge_query.Target.point (S2_cell.center cell) in
      let limit = S1_chord_angle.sub t.radius_successor (S2_cap.radius_chord cap) in
      S2_closest_edge_query.is_distance_less t.query target limit))
;;

let may_intersect_cell t cell =
  let target = S2_closest_edge_query.Target.cell cell in
  S2_closest_edge_query.is_distance_less t.query target t.radius_successor
;;

let contains_point t p =
  let target = S2_closest_edge_query.Target.point p in
  S2_closest_edge_query.is_distance_less t.query target t.radius_successor
;;

let to_region t =
  S2_region.custom
    #{ cap_bound = (fun () -> cap_bound t)
     ; rect_bound = (fun () -> rect_bound t)
     ; contains_cell = (fun cell -> contains_cell t cell)
     ; intersects_cell = (fun cell -> may_intersect_cell t cell)
     ; contains_point = (fun p -> contains_point t p)
     ; cell_union_bound = (fun () -> cell_union_bound t)
     }
;;
