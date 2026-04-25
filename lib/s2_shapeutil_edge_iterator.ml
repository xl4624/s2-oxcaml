open Core

type t =
  { index : S2_shape_index.t
  ; mutable shape_id : int
  ; mutable num_edges : int
  ; mutable edge_id : int
  }

let shape_id t = t.shape_id
let edge_id t = t.edge_id
let is_done t = t.shape_id >= S2_shape_index.num_shape_ids t.index

let next t =
  let n = S2_shape_index.num_shape_ids t.index in
  let stop = ref false in
  while not !stop do
    t.edge_id <- t.edge_id + 1;
    if t.edge_id < t.num_edges
    then stop := true
    else (
      t.shape_id <- t.shape_id + 1;
      if t.shape_id >= n
      then stop := true
      else (
        let shape = S2_shape_index.shape t.index t.shape_id in
        t.num_edges <- shape.#num_edges;
        t.edge_id <- -1))
  done
;;

let create index =
  let t = { index; shape_id = -1; num_edges = 0; edge_id = -1 } in
  next t;
  t
;;

let shape_edge_id t =
  S2_shapeutil_shape_edge_id.create ~shape_id:t.shape_id ~edge_id:t.edge_id
;;

let edge t =
  if is_done t then raise_s [%message "S2_shapeutil_edge_iterator.edge: iterator is done"];
  let shape = S2_shape_index.shape t.index t.shape_id in
  shape.#edge t.edge_id
;;

let equal a b =
  phys_equal a.index b.index && a.shape_id = b.shape_id && a.edge_id = b.edge_id
;;
