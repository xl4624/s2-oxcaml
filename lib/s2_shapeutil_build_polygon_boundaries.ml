open Core

module Loop = struct
  type t = { shape : S2_shape.t }

  let of_shape shape = { shape }
  let shape t = t.shape
end

let flat_id_offsets components =
  let n = List.length components in
  let offsets = Array.create ~len:(n + 1) 0 in
  List.iteri components ~f:(fun i comp ->
    offsets.(i + 1) <- offsets.(i) + List.length comp);
  offsets
;;

(* The loop boundaries do not cross, so a nesting hierarchy can be defined by
   choosing any point on the sphere as the "point at infinity": loop A then
   contains loop B if A contains the boundary of B and A does not contain the
   point at infinity. The implementation uses [S2_pointutil.origin] for that
   role.

   Algorithm:
   1. Build a shape index over every loop that does not contain origin. Each
      multi-loop component contributes exactly one outer loop (the unique loop
      that contains origin); a single-loop component is treated as outer
      regardless of orientation.
   2. For each component, look up the indexed loops that contain its outer
      loop's first vertex; that set is the component's "ancestors".
   3. Pair each component's outer loop with an ancestor whose own ancestor
      depth is one less. Outer loops at depth 0 are stitched together into
      the trailing face. *)

let build_polygon_boundaries (components : Loop.t list list) : int list list =
  let n_comp = List.length components in
  if n_comp = 0
  then []
  else (
    let offsets = flat_id_offsets components in
    let index = S2_shape_index.create () in
    let indexed_to_global = Dynarray.create () in
    let indexed_component = Dynarray.create () in
    let outer_global = Array.create ~len:n_comp (-1) in
    let outer_first_vertex = Array.create ~len:n_comp S2_point.origin in
    let origin = S2_pointutil.origin () in
    List.iteri components ~f:(fun i comp ->
      let m = List.length comp in
      let outer_count = ref 0 in
      List.iteri comp ~f:(fun j loop ->
        let shape = Loop.shape loop in
        let global_id = offsets.(i) + j in
        if m > 1
           && not
                (S2_shapeutil_contains_brute_force.contains_brute_force
                   shape
                   ~point:origin)
        then (
          let _ : int = S2_shape_index.add index shape in
          Dynarray.add_last indexed_to_global global_id;
          Dynarray.add_last indexed_component i)
        else (
          outer_global.(i) <- global_id;
          outer_first_vertex.(i) <- (shape.#edge 0).#v0;
          incr outer_count));
      if !outer_count <> 1
      then
        raise_s
          [%message
            "S2_shapeutil_build_polygon_boundaries: component is not a subdivision"
              ~component_index:(i : int)
              ~outer_loop_count:(!outer_count : int)]);
    S2_shape_index.build index;
    let pq = S2_contains_point_query.create index () in
    let ancestors : int array array = Array.create ~len:n_comp [||] in
    for i = 0 to n_comp - 1 do
      let acc = Dynarray.create () in
      let (_ : bool) =
        S2_contains_point_query.visit_containing_shapes
          pq
          outer_first_vertex.(i)
          ~f:(fun id ->
            Dynarray.add_last acc id;
            true)
      in
      ancestors.(i) <- Dynarray.to_array acc
    done;
    let n_indexed = Dynarray.length indexed_to_global in
    let children : int Dynarray.t array =
      Array.init (n_indexed + 1) ~f:(fun _ -> Dynarray.create ())
    in
    let no_ancestor_slot = n_indexed in
    for i = 0 to n_comp - 1 do
      let depth = Array.length ancestors.(i) in
      let ancestor_id = ref (-1) in
      if depth > 0
      then
        Array.iter ancestors.(i) ~f:(fun cand ->
          let cand_component = Dynarray.get indexed_component cand in
          if Array.length ancestors.(cand_component) = depth - 1
          then (
            if !ancestor_id <> -1
            then
              raise_s
                [%message
                  "S2_shapeutil_build_polygon_boundaries: ambiguous ancestor"
                    ~component_index:(i : int)];
            ancestor_id := cand));
      let slot = if !ancestor_id < 0 then no_ancestor_slot else !ancestor_id in
      Dynarray.add_last children.(slot) outer_global.(i)
    done;
    let face_of_slot slot ~final =
      let acc = ref (Option.to_list final) in
      for k = Dynarray.length children.(slot) - 1 downto 0 do
        acc := Dynarray.get children.(slot) k :: !acc
      done;
      !acc
    in
    let indexed_faces =
      List.init n_indexed ~f:(fun k ->
        face_of_slot k ~final:(Some (Dynarray.get indexed_to_global k)))
    in
    let outer_face = face_of_slot no_ancestor_slot ~final:None in
    indexed_faces @ [ outer_face ])
;;
