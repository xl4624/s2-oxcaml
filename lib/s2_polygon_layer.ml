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
  (* Each directed loop is represented by its edge ids in traversal order.
     The S2_loop takes the [src] vertex of each edge; the final edge's [dst]
     equals the first edge's [src], which is implicit in the closed loop
     representation. Validation is skipped because the graph-level simple-loop
     assembly already guarantees a well-formed cycle. *)
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

let validation_error_of msg =
  (* The OCaml [S2_polygon.find_validation_error] returns a string only, so we
     surface a single generic code. C++ distinguishes specific codes such as
     POLYGON_LOOPS_SHARE_EDGE or LOOP_SELF_INTERSECTION; callers wanting that
     granularity must inspect [message]. *)
  S2_builder.Error.create ~code:"INVALID_POLYGON" ~message:msg
;;

let build_from_graph (g : G.t) ~output ~validate : S2_builder.Error.t =
  let finalize polygon =
    output.polygon <- Some polygon;
    if validate
    then (
      match S2_polygon.find_validation_error polygon with
      | None -> S2_builder.Error.ok
      | Some msg -> validation_error_of msg)
    else S2_builder.Error.ok
  in
  let num_edges = G.num_edges g in
  if num_edges = 0
  then (
    let is_full, err = G.is_full_polygon g in
    if not (S2_builder.Error.is_ok err)
    then err
    else finalize (if is_full then S2_polygon.full () else S2_polygon.empty ()))
  else (
    let loops, err = G.get_directed_loops g G.Loop_type.Simple in
    if not (S2_builder.Error.is_ok err)
    then err
    else (
      let polygon_loops =
        Array.map loops ~f:(fun edges -> loop_of_edge_sequence g edges)
      in
      finalize (S2_polygon.of_oriented_loops polygon_loops)))
;;

let layer ?(validate = false) output : S2_builder.Layer.t =
  (* Graph options match s2builderutil_s2polygon_layer.cc:75-83: directed edges,
     drop degenerate edges, keep duplicates (the assembler tolerates them), and
     drop sibling pairs so zero-area regions do not produce spurious loops. *)
  let graph_options =
    S2_builder.Graph_options.create
      ~edge_type:S2_builder.Edge_type.Directed
      ~degenerate_edges:S2_builder.Degenerate_edges.Discard
      ~duplicate_edges:S2_builder.Duplicate_edges.Keep
      ~sibling_pairs:S2_builder.Sibling_pairs.Discard
  in
  { graph_options
  ; build = (fun g -> build_from_graph g ~output ~validate)
  ; name = "s2_polygon_layer"
  }
;;

(* TODO: port undirected-edge support from s2builderutil_s2polygon_layer.cc:85-103. *)
(* TODO: port label-set tracking (LabelSetIds / IdSetLexicon) from
   s2builderutil_s2polygon_layer.cc:121-174. *)
