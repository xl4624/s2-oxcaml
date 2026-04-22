open Core
module G = S2_builder.Graph

type output = { mutable polygon : S2_polygon.t option }

let create_output () = { polygon = None }

let result t =
  match t.polygon with
  | Some p -> p
  | None ->
    raise_s [%message "S2_polygon_layer.result: layer has not produced a polygon yet"]
;;

let loop_of_edge_sequence (g : G.t) edge_ids =
  (* Each directed loop is described by a list of edge ids. The S2_loop is
     built from the [src] vertex of each edge; the final edge's [dst] equals
     the first edge's [src] and is implicit in the closed loop. *)
  let n = Array.length edge_ids in
  if n = 0
  then S2_loop.empty ()
  else (
    let first_edge = G.edge g edge_ids.(0) in
    let first = G.vertex g (G.Edge.src first_edge) in
    let vs = Array.create ~len:n first in
    for i = 1 to n - 1 do
      let e = G.edge g edge_ids.(i) in
      vs.(i) <- G.vertex g (G.Edge.src e)
    done;
    S2_loop.of_vertices ~validate:false vs)
;;

let build_from_graph (g : G.t) ~output : S2_builder.Error.t =
  let num_edges = G.num_edges g in
  if num_edges = 0
  then (
    let is_full, err = G.is_full_polygon g in
    if not (S2_builder.Error.is_ok err)
    then err
    else (
      output.polygon <- Some (if is_full then S2_polygon.full () else S2_polygon.empty ());
      S2_builder.Error.ok))
  else (
    let loops, err = G.get_directed_loops g G.Loop_type.Simple in
    if not (S2_builder.Error.is_ok err)
    then err
    else (
      let polygon_loops =
        Array.map loops ~f:(fun edges -> loop_of_edge_sequence g edges)
      in
      output.polygon <- Some (S2_polygon.of_oriented_loops polygon_loops);
      S2_builder.Error.ok))
;;

let layer output : S2_builder.Layer.t =
  let graph_options =
    S2_builder.Graph_options.create
      ~edge_type:S2_builder.Edge_type.Directed
      ~degenerate_edges:S2_builder.Degenerate_edges.Discard
      ~duplicate_edges:S2_builder.Duplicate_edges.Keep
      ~sibling_pairs:S2_builder.Sibling_pairs.Discard
  in
  { graph_options
  ; build = (fun g -> build_from_graph g ~output)
  ; name = "s2_polygon_layer"
  }
;;
