open Core

(* The two distinguished single-vertex chains. A loop with a single vertex is
   interpreted as the empty loop when the vertex is in the northern hemisphere
   (so the origin lies outside) and as the full loop otherwise. The sentinel
   vertices coincide with the poles, which we also need during bound
   computation, so both names are exposed for readability.
   See s2loop.h:212-229 for the empty/full loop encoding convention. *)
let north_pole = S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:#1.0
let south_pole = S2_point.of_coords ~x:#0.0 ~y:#0.0 ~z:(-#1.0)
let empty_vertex = north_pole
let full_vertex = south_pole

type index_cache =
  { idx : S2_shape_index.t
  ; sid : int
  ; point_query : S2_contains_point_query.t
  ; mutable crossing_query : S2_crossing_edge_query.t option
  }

type t =
  { vertices : S2_point.t array
  ; origin_inside : bool
  ; bound : S2_latlng_rect.t
  ; subregion_bound : S2_latlng_rect.t
  ; mutable depth : int
  ; mutable index_cache : index_cache option
  }

let sexp_of_t
  { vertices
  ; origin_inside = _
  ; bound = _
  ; subregion_bound = _
  ; depth = _
  ; index_cache = _
  }
  =
  let n = Array.length vertices in
  let acc = ref [] in
  for i = n - 1 downto 0 do
    acc := S2_point.sexp_of_t vertices.(i) :: !acc
  done;
  Sexp.List [ Sexp.Atom "S2_loop"; Sexp.List !acc ]
;;

let copy_array (src : S2_point.t array) =
  let n = Array.length src in
  if n = 0
  then [||]
  else (
    let dst = Array.create ~len:n src.(0) in
    for i = 1 to n - 1 do
      dst.(i) <- src.(i)
    done;
    dst)
;;

let[@inline] [@zero_alloc] num_vertices t = Array.length t.vertices

let[@inline] [@zero_alloc] vertex_raw (vs : S2_point.t array) i : S2_point.t =
  let n = Array.length vs in
  let j = i mod n in
  let j = if j < 0 then j + n else j in
  vs.(j)
;;

let[@inline] [@zero_alloc] vertex t i = vertex_raw t.vertices i
let vertices t = copy_array t.vertices
let[@inline] [@zero_alloc] depth t = t.depth
let[@inline] [@zero_alloc] set_depth t d = t.depth <- d
let[@inline] [@zero_alloc] is_hole t = t.depth land 1 <> 0
let[@inline] [@zero_alloc] sign t = if is_hole t then -1 else 1

let[@zero_alloc] oriented_vertex t i =
  let n = Array.length t.vertices in
  let j = i mod n in
  let j = if j < 0 then j + n else j in
  let j = if is_hole t then n - 1 - j else j in
  t.vertices.(j)
;;

let[@inline] [@zero_alloc] is_empty_or_full t = Array.length t.vertices = 1
let[@inline] [@zero_alloc] contains_origin t = t.origin_inside
let[@inline] [@zero_alloc] is_empty t = is_empty_or_full t && not t.origin_inside
let[@inline] [@zero_alloc] is_full t = is_empty_or_full t && t.origin_inside

(* Brute-force point containment used for bound pre-checks and reference parity.
   [contains_point] uses {!S2_shape_index} plus {!S2_contains_point_query} after the
   bound test. *)
let[@zero_alloc] brute_force_contains_point ~vertices ~origin_inside p =
  let n = Array.length vertices in
  if n = 0
  then false
  else (
    let mutable inside = origin_inside in
    let origin = S2_pointutil.origin () in
    let mutable crosser =
      S2_edge_crosser.create_with_chain ~a:origin ~b:p ~c:vertices.(0)
    in
    for i = 1 to n do
      let next = if i = n then vertices.(0) else vertices.(i) in
      let (#{ state; crossing } : S2_edge_crosser.with_bool) =
        S2_edge_crosser.chain_edge_or_vertex_crossing crosser next
      in
      crosser <- state;
      if crossing then inside <- not inside
    done;
    inside)
;;

type init_result =
  #{ origin_inside : bool
   ; bound : S2_latlng_rect.t
   ; subregion_bound : S2_latlng_rect.t
   }

(* Compute origin_inside and bounds for a freshly-built vertex array. The
   empty/full loops have their own special cases; for normal loops the
   origin_inside flag is determined by checking whether the brute-force
   crossing count from [S2_pointutil.origin] to [vertex 1] agrees with the
   wedge containment of vertex 1. The bound starts from the rect-bounder
   over all edges and is then expanded if the loop contains either pole. *)
let init_origin_and_bound (vs : S2_point.t array) : init_result =
  let n = Array.length vs in
  if n < 3
  then
    if n <> 1
    then
      #{ origin_inside = false
       ; bound = S2_latlng_rect.empty
       ; subregion_bound = S2_latlng_rect.empty
       }
    else (
      let origin_inside = Float_u.O.(S2_point.z vs.(0) < #0.0) in
      let bound = if origin_inside then S2_latlng_rect.full else S2_latlng_rect.empty in
      #{ origin_inside; bound; subregion_bound = bound })
  else (
    (* Determine origin_inside by comparing the wedge-based containment of
       [vertex 1] against the brute-force crossing count. The brute force
       call uses the not-yet-finalised origin_inside = false; if the result
       disagrees with the wedge, the true value must be true instead. *)
    let v0 = vs.(0) in
    let v1 = vs.(1) in
    let v2 = vs.(2) in
    let v1_inside =
      (not (S2_point.equal v0 v1))
      && (not (S2_point.equal v2 v1))
      && S2_edge_crossings.angle_contains_vertex v0 v1 v2
    in
    let probe = brute_force_contains_point ~vertices:vs ~origin_inside:false v1 in
    let origin_inside = Bool.( <> ) v1_inside probe in
    (* Build the rect bound by walking [n + 1] vertices so the closing edge is
       included. *)
    let mutable rb = S2_latlng_rect_bounder.create () in
    for i = 0 to n do
      rb <- S2_latlng_rect_bounder.add_point rb (vertex_raw vs i)
    done;
    let b = S2_latlng_rect_bounder.get_bound rb in
    let half_pi = Float_u.O.(Float_u.pi / #2.0) in
    let b =
      if brute_force_contains_point ~vertices:vs ~origin_inside north_pole
      then
        S2_latlng_rect.create
          ~lat:
            (R1_interval.create ~lo:(R1_interval.lo (S2_latlng_rect.lat b)) ~hi:half_pi)
          ~lng:S1_interval.full
      else b
    in
    let b =
      if S1_interval.is_full (S2_latlng_rect.lng b)
         && brute_force_contains_point ~vertices:vs ~origin_inside south_pole
      then
        S2_latlng_rect.create
          ~lat:
            (R1_interval.create
               ~lo:Float_u.O.(-half_pi)
               ~hi:(R1_interval.hi (S2_latlng_rect.lat b)))
          ~lng:S1_interval.full
      else b
    in
    let subregion_bound = S2_latlng_rect_bounder.expand_for_subregions b in
    #{ origin_inside; bound = b; subregion_bound })
;;

let make_unchecked vertices =
  let #{ origin_inside; bound; subregion_bound } = init_origin_and_bound vertices in
  { vertices; origin_inside; bound; subregion_bound; depth = 0; index_cache = None }
;;

let find_validation_error t =
  let n = Array.length t.vertices in
  let mutable err = None in
  let mutable i = 0 in
  while Option.is_none err && i < n do
    if not (S2_point.is_unit_length t.vertices.(i))
    then err <- Some (sprintf "Vertex %d is not unit length" i);
    i <- i + 1
  done;
  if Option.is_some err
  then err
  else if n < 3
  then
    if is_empty_or_full t
    then None
    else Some "Non-empty, non-full loops must have at least 3 vertices"
  else (
    let mutable i = 0 in
    while Option.is_none err && i < n do
      let a = t.vertices.(i) in
      let b = vertex t (i + 1) in
      if S2_point.equal a b
      then err <- Some (sprintf "Edge %d is degenerate (duplicate vertex)" i)
      else if S2_point.equal a (S2_point.neg b)
      then err <- Some (sprintf "Vertices %d and %d are antipodal" i ((i + 1) mod n));
      i <- i + 1
    done;
    err)
;;

let is_valid t = Option.is_none (find_validation_error t)

let of_vertices ?(validate = true) src =
  if Array.length src = 0
  then raise_s [%message "S2_loop.of_vertices: empty vertex array"];
  let t = make_unchecked (copy_array src) in
  if validate
  then (
    match find_validation_error t with
    | None -> ()
    | Some msg -> raise_s [%message "S2_loop.of_vertices: invalid loop" (msg : string)]);
  t
;;

let empty () = make_unchecked [| empty_vertex |]
let full () = make_unchecked [| full_vertex |]

let of_cell cell =
  let v0 = S2_cell.vertex cell 0 in
  let v1 = S2_cell.vertex cell 1 in
  let v2 = S2_cell.vertex cell 2 in
  let v3 = S2_cell.vertex cell 3 in
  make_unchecked [| v0; v1; v2; v3 |]
;;

let invert t =
  let n = Array.length t.vertices in
  if n = 0
  then t
  else (
    let new_vertices =
      if is_empty_or_full t
      then [| (if is_full t then empty_vertex else full_vertex) |]
      else (
        let dst = Array.create ~len:n t.vertices.(0) in
        for i = 0 to n - 1 do
          dst.(i) <- t.vertices.(n - 1 - i)
        done;
        dst)
    in
    let new_origin_inside = not t.origin_inside in
    let half_pi = Float_u.O.(Float_u.pi / #2.0) in
    let lat = S2_latlng_rect.lat t.bound in
    let #{ origin_inside = _; bound; subregion_bound } : init_result =
      if is_empty_or_full t
      then (
        let b = if new_origin_inside then S2_latlng_rect.full else S2_latlng_rect.empty in
        #{ origin_inside = new_origin_inside; bound = b; subregion_bound = b })
      else if Float_u.O.(R1_interval.lo lat > -half_pi)
              && Float_u.O.(R1_interval.hi lat < half_pi)
      then
        (* The complement of this loop contains both poles, so its bound is
           the full sphere. *)
        #{ origin_inside = new_origin_inside
         ; bound = S2_latlng_rect.full
         ; subregion_bound = S2_latlng_rect.full
         }
      else init_origin_and_bound new_vertices
    in
    { vertices = new_vertices
    ; origin_inside = new_origin_inside
    ; bound
    ; subregion_bound
    ; depth = t.depth
    ; index_cache = None
    })
;;

let is_normalized t =
  if Float_u.O.(S1_interval.length (S2_latlng_rect.lng t.bound) < Float_u.pi)
  then true
  else (
    let curv = S2_loop_measures.curvature t.vertices in
    let max_err = S2_loop_measures.curvature_max_error t.vertices in
    Float_u.O.(curv >= Float_u.neg max_err))
;;

let normalize t = if is_normalized t then t else invert t

(* For empty/full loops [S2_loop_measures] would treat the single sentinel
   vertex as a degenerate triangle and report area 0 / curvature +2pi for
   both. Short-circuit here so the caller gets the correct values:
   full = 4pi area, -2pi curvature; empty = 0 area, +2pi curvature. *)

let area t =
  if is_full t
  then Float_u.O.(#4.0 * Float_u.pi)
  else if is_empty t
  then #0.0
  else S2_loop_measures.area t.vertices
;;

let centroid t =
  if is_empty_or_full t then R3_vector.zero else S2_loop_measures.centroid t.vertices
;;

let curvature t =
  if is_full t
  then Float_u.O.(-#2.0 * Float_u.pi)
  else if is_empty t
  then Float_u.O.(#2.0 * Float_u.pi)
  else S2_loop_measures.curvature t.vertices
;;

let[@inline] [@zero_alloc] curvature_max_error t =
  S2_loop_measures.curvature_max_error t.vertices
;;

(* Region interface. *)

let[@inline] [@zero_alloc] cap_bound t = S2_latlng_rect.cap_bound t.bound
let[@inline] [@zero_alloc] rect_bound t = t.bound
let cell_union_bound t = S2_latlng_rect.cell_union_bound t.bound

(* Brute-force boundary intersection: scan loop edges against the four cell
   edges. The cell vertices are fetched once and indexed cyclically; the cache
   is stack-allocated so the function stays zero-alloc. *)
let[@zero_alloc] any_loop_edge_crosses_cell ~vertices cell =
  let n = Array.length vertices in
  if n = 0
  then false
  else (
    let cv0 = S2_cell.vertex cell 0 in
    let cv1 = S2_cell.vertex cell 1 in
    let cv2 = S2_cell.vertex cell 2 in
    let cv3 = S2_cell.vertex cell 3 in
    let local_ cell_vs = [| cv0; cv1; cv2; cv3 |] in
    let mutable hit = false in
    let mutable k = 0 in
    while (not hit) && k < 4 do
      let a = cell_vs.(k) in
      let b = cell_vs.((k + 1) land 3) in
      let mutable crosser = S2_edge_crosser.create_with_chain ~a ~b ~c:vertices.(0) in
      let mutable i = 1 in
      while (not hit) && i <= n do
        let next = if i = n then vertices.(0) else vertices.(i) in
        let (#{ state; sign } : S2_edge_crosser.with_sign) =
          S2_edge_crosser.chain_crossing_sign crosser next
        in
        crosser <- state;
        if sign >= 0 then hit <- true;
        i <- i + 1
      done;
      k <- k + 1
    done;
    hit)
;;

(* contains_cell and may_intersect_cell are defined after contains_point
   (and the index cache) so they can reuse the shared shape index. *)

(* Equality. *)

let[@zero_alloc] equal a b =
  let na = Array.length a.vertices in
  if na <> Array.length b.vertices
  then false
  else (
    let mutable eq = true in
    let mutable i = 0 in
    while eq && i < na do
      if not (S2_point.equal a.vertices.(i) b.vertices.(i)) then eq <- false;
      i <- i + 1
    done;
    eq)
;;

let[@zero_alloc] boundary_equals a b =
  let n = Array.length a.vertices in
  if n <> Array.length b.vertices
  then false
  else if is_empty_or_full a
  then Bool.( = ) (is_empty a) (is_empty b)
  else (
    let mutable found = false in
    let mutable offset = 0 in
    while (not found) && offset < n do
      if S2_point.equal a.vertices.(offset) b.vertices.(0)
      then (
        let mutable ok = true in
        let mutable i = 0 in
        while ok && i < n do
          if not (S2_point.equal (vertex a (i + offset)) b.vertices.(i)) then ok <- false;
          i <- i + 1
        done;
        if ok then found <- true);
      offset <- offset + 1
    done;
    found)
;;

let[@zero_alloc] boundary_approx_equals ~max_error a b =
  let n = Array.length a.vertices in
  if n <> Array.length b.vertices
  then false
  else if is_empty_or_full a
  then Bool.( = ) (is_empty a) (is_empty b)
  else (
    let max_error_radians =
      Packed_float_option.Unboxed.some
        (Packed_float_option.Unboxed.value max_error ~default:#1e-15)
    in
    let mutable found = false in
    let mutable offset = 0 in
    while (not found) && offset < n do
      if S2_pointutil.approx_equals ~max_error_radians a.vertices.(offset) b.vertices.(0)
      then (
        let mutable ok = true in
        let mutable i = 0 in
        while ok && i < n do
          if not
               (S2_pointutil.approx_equals
                  ~max_error_radians
                  (vertex a (i + offset))
                  b.vertices.(i))
          then ok <- false;
          i <- i + 1
        done;
        if ok then found <- true);
      offset <- offset + 1
    done;
    found)
;;

(* Shape interface. *)

let[@inline] [@zero_alloc] num_edges t =
  if is_empty_or_full t then 0 else Array.length t.vertices
;;

let[@inline] [@zero_alloc] dimension _ = 2
let[@inline] [@zero_alloc] num_chains t = if is_empty t then 0 else 1

let[@inline] [@zero_alloc] chain t _ =
  S2_shape.Chain.create ~start:0 ~length:(num_edges t)
;;

let[@zero_alloc] edge t e =
  let v0 = t.vertices.(e) in
  let v1 = vertex t (e + 1) in
  S2_shape.Edge.create ~v0 ~v1
;;

let[@inline] [@zero_alloc] chain_edge t _ j = edge t j

let[@inline] [@zero_alloc] chain_position _ e =
  S2_shape.Chain_position.create ~chain_id:0 ~offset:e
;;

let[@zero_alloc] reference_point t =
  S2_shape.Reference_point.create
    ~point:(S2_pointutil.origin ())
    ~contained:t.origin_inside
;;

let type_tag = S2_shape.Type_tag.none

let to_shape t : S2_shape.t =
  #{ num_edges = num_edges t
   ; num_chains = num_chains t
   ; dimension = 2
   ; type_tag
   ; reference_point = reference_point t
   ; edge = edge t
   ; chain = chain t
   ; chain_edge = chain_edge t
   ; chain_position = chain_position t
   }
;;

(* Lazily build and cache the loop's shape index together with point and
   crossing query objects. All index-driven predicates (contains_point,
   contains_cell, may_intersect_cell, contains, intersects) share this
   cache so the index is built at most once per loop. *)
let get_index_cache t =
  match t.index_cache with
  | Some c -> c
  | None ->
    let idx = S2_shape_index.create () in
    let sid = S2_shape_index.add idx (to_shape t) in
    let point_query = S2_contains_point_query.create idx () in
    let c = { idx; sid; point_query; crossing_query = None } in
    t.index_cache <- Some c;
    c
;;

let get_crossing_query t =
  let c = get_index_cache t in
  match c.crossing_query with
  | Some q -> q
  | None ->
    let q = S2_crossing_edge_query.create c.idx in
    c.crossing_query <- Some q;
    q
;;

let contains_point t p =
  if not (S2_latlng_rect.contains_point t.bound p)
  then false
  else (
    let c = get_index_cache t in
    S2_contains_point_query.shape_contains c.point_query ~shape_id:c.sid p)
;;

(* Does any edge of [cell] cross any edge of the loop, using the loop's
   shape index to prune? *)
let any_cell_edge_crosses_loop t cell =
  let ceq = get_crossing_query t in
  let verts =
    [| S2_cell.vertex cell 0
     ; S2_cell.vertex cell 1
     ; S2_cell.vertex cell 2
     ; S2_cell.vertex cell 3
    |]
  in
  let found = ref false in
  let k = ref 0 in
  while (not !found) && !k < 4 do
    let a = verts.(!k) in
    let b = verts.((!k + 1) land 3) in
    let xs =
      S2_crossing_edge_query.get_crossing_edges
        ceq
        ~a
        ~b
        ~crossing_type:S2_crossing_edge_query.Crossing_type.All
    in
    if Array.length xs > 0 then found := true;
    incr k
  done;
  !found
;;

let contains_cell t cell =
  if is_empty t
  then false
  else if is_full t
  then true
  else if not (S2_latlng_rect.contains_cell t.bound cell)
  then false
  else (
    let contained = ref true in
    let k = ref 0 in
    while !contained && !k < 4 do
      if not (contains_point t (S2_cell.vertex cell !k)) then contained := false;
      incr k
    done;
    !contained && not (any_cell_edge_crosses_loop t cell))
;;

let may_intersect_cell t cell =
  if is_empty t
  then false
  else if is_full t
  then true
  else if not (S2_latlng_rect.intersects_cell t.bound cell)
  then false
  else (
    let hit = ref false in
    let k = ref 0 in
    while (not !hit) && !k < 4 do
      if contains_point t (S2_cell.vertex cell !k) then hit := true;
      incr k
    done;
    if !hit
    then true
    else (
      let n = Array.length t.vertices in
      let i = ref 0 in
      while (not !hit) && !i < n do
        if S2_cell.contains_point cell t.vertices.(!i) then hit := true;
        incr i
      done;
      !hit || any_cell_edge_crosses_loop t cell))
;;

let to_region t : S2_region.t =
  S2_region.custom
    #{ cap_bound = (fun () -> cap_bound t)
     ; rect_bound = (fun () -> rect_bound t)
     ; contains_cell = contains_cell t
     ; intersects_cell = may_intersect_cell t
     ; contains_point = contains_point t
     ; cell_union_bound = (fun () -> cell_union_bound t)
     }
;;

(* Loop relations. *)

(* Locate [p] among the vertices of [t] in the half-open range (0, n]. Returns
   [-1] if not found. The returned index is chosen so that wrapping lookups
   [vertex t (i - 1)] and [vertex t (i + 1)] stay well-defined. *)
let[@zero_alloc] find_vertex t p =
  let n = Array.length t.vertices in
  let mutable result = -1 in
  let mutable i = 1 in
  while result < 0 && i <= n do
    if S2_point.equal (vertex t i) p then result <- i;
    i <- i + 1
  done;
  result
;;

type canonical_first =
  #{ first : int
   ; dir : int
   }

let[@zero_alloc] canonical_first_vertex t =
  let n = Array.length t.vertices in
  let mutable first = 0 in
  for i = 1 to n - 1 do
    if R3_vector.compare t.vertices.(i) t.vertices.(first) < 0 then first <- i
  done;
  (* Choose direction so that the neighbour in the forward direction is
     lex-smaller than the neighbour in the reverse direction. Offsetting
     [first] by [n] when dir = -1 keeps [first + k * dir] non-negative for
     any [k] in [0, n]. *)
  if R3_vector.compare (vertex t (first + 1)) (vertex t (first + n - 1)) < 0
  then #{ first; dir = 1 }
  else #{ first = first + n; dir = -1 }
;;

(* Reports whether the wedge [(a0, ab1, a2)] contains the "semiwedge" of rays
   immediately CCW from edge [(ab1, b2)]. If [reverse] is [true], CCW is
   replaced by CW; this is used when the other loop's orientation should be
   flipped (holes versus shells). *)
let[@zero_alloc] wedge_contains_semiwedge ~a0 ~ab1 ~a2 ~b2 ~reverse =
  if S2_point.equal b2 a0 || S2_point.equal b2 a2
  then Bool.( = ) (S2_point.equal b2 a0) reverse
  else S2_predicates.ordered_ccw a0 a2 b2 ab1
;;

let contains_nested a b =
  if not (S2_latlng_rect.contains a.subregion_bound b.bound)
  then false
  else if is_empty_or_full a || Array.length b.vertices < 2
  then is_full a || is_empty b
  else (
    (* By contract, [a] and [b] either share vertex 1 of [b] or they are
       properly nested. *)
    let v1 = vertex b 1 in
    let m = find_vertex a v1 in
    if m < 0
    then contains_point a v1
    else
      S2_wedge_relations.wedge_contains
        ~a0:(vertex a (m - 1))
        ~ab1:v1
        ~a2:(vertex a (m + 1))
        ~b0:(vertex b 0)
        ~b2:(vertex b 2))
;;

let contains_non_crossing_boundary a b ~reverse =
  if not (S2_latlng_rect.intersects a.bound b.bound)
  then false
  else if is_full a
  then true
  else if is_full b
  then false
  else (
    let v0 = vertex b 0 in
    let m = find_vertex a v0 in
    if m < 0
    then contains_point a v0
    else
      wedge_contains_semiwedge
        ~a0:(vertex a (m - 1))
        ~ab1:v0
        ~a2:(vertex a (m + 1))
        ~b2:(vertex b 1)
        ~reverse)
;;

(* Reports whether any pair of edges from [a] and [b] cross at a point
   interior to both. Used by {!contains}, {!intersects}, and
   {!compare_boundary}. Uses [b]'s shape index plus {!S2_crossing_edge_query}
   to probe each edge of [a] against [b]. *)
let any_proper_crossing a b =
  let na = Array.length a.vertices in
  if na = 0 || Array.length b.vertices = 0
  then false
  else (
    let ceq = get_crossing_query b in
    let found = ref false in
    let i = ref 0 in
    while (not !found) && !i < na do
      let v0 = a.vertices.(!i) in
      let v1 = vertex a (!i + 1) in
      let xs =
        S2_crossing_edge_query.get_crossing_edges
          ceq
          ~a:v0
          ~b:v1
          ~crossing_type:S2_crossing_edge_query.Crossing_type.Interior
      in
      if Array.length xs > 0 then found := true;
      incr i
    done;
    !found)
;;

let contains a b =
  if not (S2_latlng_rect.contains a.subregion_bound b.bound)
  then false
  else if is_empty_or_full a || is_empty_or_full b
  then is_full a || is_empty b
  else if any_proper_crossing a b
  then false
  else (
    (* At every shared vertex, [a]'s wedge must contain [b]'s wedge. *)
    let na = Array.length a.vertices in
    let nb = Array.length b.vertices in
    let mutable found_shared = false in
    let mutable bad = false in
    let mutable i = 0 in
    while (not bad) && i < na do
      let vi = a.vertices.(i) in
      let mutable j = 0 in
      while (not bad) && j < nb do
        if S2_point.equal vi b.vertices.(j)
        then (
          found_shared <- true;
          if not
               (S2_wedge_relations.wedge_contains
                  ~a0:(vertex a (i - 1))
                  ~ab1:vi
                  ~a2:(vertex a (i + 1))
                  ~b0:(vertex b (j - 1))
                  ~b2:(vertex b (j + 1)))
          then bad <- true);
        j <- j + 1
      done;
      i <- i + 1
    done;
    if bad
    then false
    else if found_shared
    then true
    else if not (contains_point a (vertex b 0))
    then false
    else if (S2_latlng_rect.contains b.subregion_bound a.bound
             || S2_latlng_rect.is_full (S2_latlng_rect.union b.bound a.bound))
            && contains_point b (vertex a 0)
    then false
    else true)
;;

let intersects a b =
  if is_empty a || is_empty b
  then false
  else if is_full a || is_full b
  then true
  else if not (S2_latlng_rect.intersects a.bound b.bound)
  then false
  else if any_proper_crossing a b
  then true
  else (
    let na = Array.length a.vertices in
    let nb = Array.length b.vertices in
    let mutable found_shared = false in
    let mutable hit = false in
    let mutable i = 0 in
    while (not hit) && i < na do
      let vi = a.vertices.(i) in
      let mutable j = 0 in
      while (not hit) && j < nb do
        if S2_point.equal vi b.vertices.(j)
        then (
          found_shared <- true;
          if S2_wedge_relations.wedge_intersects
               ~a0:(vertex a (i - 1))
               ~ab1:vi
               ~a2:(vertex a (i + 1))
               ~b0:(vertex b (j - 1))
               ~b2:(vertex b (j + 1))
          then hit <- true);
        j <- j + 1
      done;
      i <- i + 1
    done;
    if hit
    then true
    else if found_shared
    then false
    else if (S2_latlng_rect.contains a.subregion_bound b.bound
             || S2_latlng_rect.is_full (S2_latlng_rect.union a.bound b.bound))
            && contains_point a (vertex b 0)
    then true
    else if S2_latlng_rect.contains b.subregion_bound a.bound
            && contains_point b (vertex a 0)
    then true
    else false)
;;

let compare_boundary a b =
  if not (S2_latlng_rect.intersects a.bound b.bound)
  then -1
  else if is_full a
  then 1
  else if is_full b
  then -1
  else if any_proper_crossing a b
  then 0
  else (
    let reverse = is_hole b in
    let na = Array.length a.vertices in
    let nb = Array.length b.vertices in
    let mutable found_shared = false in
    let mutable contains_edge = false in
    let mutable excludes_edge = false in
    let mutable i = 0 in
    while ((not contains_edge) || not excludes_edge) && i < na do
      let vi = a.vertices.(i) in
      let mutable j = 0 in
      while ((not contains_edge) || not excludes_edge) && j < nb do
        if S2_point.equal vi b.vertices.(j)
        then (
          found_shared <- true;
          if wedge_contains_semiwedge
               ~a0:(vertex a (i - 1))
               ~ab1:vi
               ~a2:(vertex a (i + 1))
               ~b2:(vertex b (j + 1))
               ~reverse
          then contains_edge <- true
          else excludes_edge <- true);
        j <- j + 1
      done;
      i <- i + 1
    done;
    if contains_edge && excludes_edge
    then 0
    else if found_shared
    then if contains_edge then 1 else -1
    else if contains_point a (vertex b 0)
    then 1
    else -1)
;;

(* TODO: port GetDistance / GetDistanceToBoundary from s2loop.cc:298-306. *)
(* TODO: port Project / ProjectToBoundary from s2loop.cc:309-314. *)
(* TODO: port BoundaryNear from s2loop.cc:349. *)
(* TODO: port Encode / Decode, including compressed form, from
   s2loop.cc:424-441, 572-587. *)
