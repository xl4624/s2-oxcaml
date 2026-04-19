open Core

type t = { vertices : S2_point.t array }

let sexp_of_t { vertices } =
  let n = Array.length vertices in
  let acc = ref [] in
  for i = n - 1 downto 0 do
    acc := S2_point.sexp_of_t vertices.(i) :: !acc
  done;
  Sexp.List [ Sexp.Atom "S2_polyline"; Sexp.List !acc ]
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

let num_vertices t = Array.length t.vertices
let vertex t i = t.vertices.(i)
let vertices t = copy_array t.vertices

let is_valid t =
  let n = num_vertices t in
  let valid = ref true in
  let i = ref 0 in
  while !valid && !i < n do
    if not (S2_point.is_unit_length t.vertices.(!i)) then valid := false;
    incr i
  done;
  let i = ref 1 in
  while !valid && !i < n do
    let prev = t.vertices.(!i - 1) in
    let cur = t.vertices.(!i) in
    if S2_point.equal prev cur || S2_point.equal prev (S2_point.neg cur)
    then valid := false;
    incr i
  done;
  !valid
;;

let of_vertices ?(validate = true) src =
  let t = { vertices = copy_array src } in
  if validate && not (is_valid t)
  then raise_s [%message "S2_polyline.of_vertices: invalid polyline"];
  t
;;

let length t = S2_polyline_measures.length t.vertices
let centroid t = S2_polyline_measures.centroid t.vertices

type suffix =
  #{ point : S2_point.t
   ; next_vertex : int
   }

type projection =
  #{ point : S2_point.t
   ; next_vertex : int
   }

let get_suffix t fraction : suffix =
  let n = num_vertices t in
  if n <= 0 then raise_s [%message "S2_polyline.get_suffix: empty polyline"];
  if Float_u.O.(fraction <= #0.0)
  then #{ point = t.vertices.(0); next_vertex = 1 }
  else (
    let total = S1_angle.radians (length t) in
    let mutable target = Float_u.O.(fraction * total) in
    let mutable result : suffix = #{ point = t.vertices.(n - 1); next_vertex = n } in
    let mutable found = false in
    let mutable i = 1 in
    while (not found) && i < n do
      let a = t.vertices.(i - 1) in
      let b = t.vertices.(i) in
      let len_i = S1_angle.radians (S2_point.distance a b) in
      if Float_u.O.(target < len_i)
      then (
        let r = S2_edge_distances.get_point_on_line a b (S1_angle.of_radians target) in
        let next = if S2_point.equal r b then i + 1 else i in
        result <- #{ point = r; next_vertex = next };
        found <- true)
      else target <- Float_u.O.(target - len_i);
      i <- i + 1
    done;
    result)
;;

let interpolate t fraction =
  let (#{ point; next_vertex = _ } : suffix) = get_suffix t fraction in
  point
;;

let un_interpolate t point next_vertex =
  let n = num_vertices t in
  if n <= 0 then raise_s [%message "S2_polyline.un_interpolate: empty polyline"];
  if n < 2
  then #0.0
  else (
    let mutable sum = #0.0 in
    for i = 1 to next_vertex - 1 do
      let d = S1_angle.radians (S2_point.distance t.vertices.(i - 1) t.vertices.(i)) in
      sum <- Float_u.O.(sum + d)
    done;
    let last_d =
      S1_angle.radians (S2_point.distance t.vertices.(next_vertex - 1) point)
    in
    let length_to_point = Float_u.O.(sum + last_d) in
    for i = next_vertex to n - 1 do
      let d = S1_angle.radians (S2_point.distance t.vertices.(i - 1) t.vertices.(i)) in
      sum <- Float_u.O.(sum + d)
    done;
    Float_u.min #1.0 Float_u.O.(length_to_point / sum))
;;

let project t point : projection =
  let n = num_vertices t in
  if n <= 0 then raise_s [%message "S2_polyline.project: empty polyline"];
  if n = 1
  then #{ point = t.vertices.(0); next_vertex = 1 }
  else (
    let mutable min_dist = #10.0 in
    let mutable min_index = -1 in
    for i = 1 to n - 1 do
      let d =
        S1_angle.radians
          (S2_edge_distances.get_distance point t.vertices.(i - 1) t.vertices.(i))
      in
      if Float_u.O.(d < min_dist)
      then (
        min_dist <- d;
        min_index <- i)
    done;
    let closest =
      S2_edge_distances.project point t.vertices.(min_index - 1) t.vertices.(min_index)
    in
    let next =
      if S2_point.equal closest t.vertices.(min_index) then min_index + 1 else min_index
    in
    #{ point = closest; next_vertex = next })
;;

let is_on_right t point =
  let n = num_vertices t in
  if n < 2
  then raise_s [%message "S2_polyline.is_on_right: requires at least two vertices"];
  let #{ point = closest; next_vertex } = project t point in
  if S2_point.equal closest t.vertices.(next_vertex - 1)
     && next_vertex > 1
     && next_vertex < n
  then
    if S2_point.equal point t.vertices.(next_vertex - 1)
    then false
    else
      S2_predicates.ordered_ccw
        t.vertices.(next_vertex - 2)
        point
        t.vertices.(next_vertex)
        t.vertices.(next_vertex - 1)
  else (
    let nv = if next_vertex = n then next_vertex - 1 else next_vertex in
    S2_predicates.sign point t.vertices.(nv) t.vertices.(nv - 1))
;;

let reverse t =
  let n = num_vertices t in
  if n = 0
  then { vertices = [||] }
  else (
    let dst = Array.create ~len:n t.vertices.(0) in
    for i = 0 to n - 1 do
      dst.(i) <- t.vertices.(n - 1 - i)
    done;
    { vertices = dst })
;;

let equal a b =
  let na = num_vertices a in
  if na <> num_vertices b
  then false
  else (
    let mutable equal = true in
    let mutable i = 0 in
    while equal && i < na do
      if not (S2_point.equal a.vertices.(i) b.vertices.(i)) then equal <- false;
      i <- i + 1
    done;
    equal)
;;

let approx_equal ~max_error a b =
  let na = num_vertices a in
  if na <> num_vertices b
  then false
  else (
    let max_error_radians =
      Packed_float_option.Unboxed.some
        (Packed_float_option.Unboxed.value max_error ~default:#1e-15)
    in
    let mutable equal = true in
    let mutable i = 0 in
    while equal && i < na do
      if not (S2_pointutil.approx_equals ~max_error_radians a.vertices.(i) b.vertices.(i))
      then equal <- false;
      i <- i + 1
    done;
    equal)
;;

let rect_bound t =
  let n = num_vertices t in
  let mutable rb = S2_latlng_rect_bounder.create () in
  for i = 0 to n - 1 do
    rb <- S2_latlng_rect_bounder.add_point rb t.vertices.(i)
  done;
  S2_latlng_rect_bounder.get_bound rb
;;

let cap_bound t = S2_latlng_rect.cap_bound (rect_bound t)
let cell_union_bound t = S2_cap.cell_union_bound (cap_bound t)
let contains_cell _ _ = false
let contains_point _ _ = false

let may_intersect_cell t cell =
  let n = num_vertices t in
  if n = 0
  then false
  else (
    let mutable hit = false in
    let mutable i = 0 in
    while (not hit) && i < n do
      if S2_cell.contains_point cell t.vertices.(i) then hit <- true;
      i <- i + 1
    done;
    if hit
    then true
    else (
      let v0 = S2_cell.vertex cell 0 in
      let v1 = S2_cell.vertex cell 1 in
      let v2 = S2_cell.vertex cell 2 in
      let v3 = S2_cell.vertex cell 3 in
      let cell_vertices = [| v0; v1; v2; v3 |] in
      let mutable j = 0 in
      while (not hit) && j < 4 do
        let a = cell_vertices.(j) in
        let b = cell_vertices.((j + 1) land 3) in
        let mutable crosser = S2_edge_crosser.create_with_chain ~a ~b ~c:t.vertices.(0) in
        let mutable k = 1 in
        while (not hit) && k < n do
          let (#{ state; sign } : S2_edge_crosser.with_sign) =
            S2_edge_crosser.chain_crossing_sign crosser t.vertices.(k)
          in
          crosser <- state;
          if sign >= 0 then hit <- true;
          k <- k + 1
        done;
        j <- j + 1
      done;
      hit))
;;

let intersects a b =
  let na = num_vertices a in
  let nb = num_vertices b in
  if na = 0 || nb = 0
  then false
  else if not (S2_latlng_rect.intersects (rect_bound a) (rect_bound b))
  then false
  else (
    let mutable hit = false in
    let mutable i = 1 in
    while (not hit) && i < na do
      let mutable crosser =
        S2_edge_crosser.create_with_chain
          ~a:a.vertices.(i - 1)
          ~b:a.vertices.(i)
          ~c:b.vertices.(0)
      in
      let mutable j = 1 in
      while (not hit) && j < nb do
        let (#{ state; sign } : S2_edge_crosser.with_sign) =
          S2_edge_crosser.chain_crossing_sign crosser b.vertices.(j)
        in
        crosser <- state;
        if sign >= 0 then hit <- true;
        j <- j + 1
      done;
      i <- i + 1
    done;
    hit)
;;

(* Worker for [subsample_vertices]. Returns the maximal end index such that the
   line segment from [vertices.(start)] to [vertices.(end)] passes within
   [tolerance] of every interior vertex, in order. *)
let find_end_vertex (vertices : S2_point.t array) tolerance start =
  let n = Array.length vertices in
  let origin = vertices.(start) in
  let frame = S2_point.get_frame origin in
  let mutable current_wedge = S1_interval.full in
  let mutable last_distance = #0.0 in
  let mutable index = start + 1 in
  let mutable stop = false in
  while (not stop) && index < n do
    let candidate = vertices.(index) in
    let distance = S1_angle.radians (S2_point.distance origin candidate) in
    if Float_u.O.(distance > Float_u.pi () / #2.0 && last_distance > #0.0)
    then stop <- true
    else if Float_u.O.(distance < last_distance && last_distance > tolerance)
    then stop <- true
    else (
      last_distance <- distance;
      if Float_u.O.(distance <= tolerance)
      then index <- index + 1
      else (
        let direction = S2_point.to_frame frame candidate in
        let center = Float_u.atan2 (S2_point.y direction) (S2_point.x direction) in
        if not (S1_interval.contains current_wedge center)
        then stop <- true
        else (
          let half_angle =
            Float_u.asin Float_u.O.(Float_u.sin tolerance / Float_u.sin distance)
          in
          let target = S1_interval.expanded (S1_interval.from_point center) half_angle in
          current_wedge <- S1_interval.intersection current_wedge target;
          index <- index + 1)))
  done;
  index - 1
;;

let subsample_vertices t tolerance =
  let n = num_vertices t in
  if n < 1
  then [||]
  else (
    let tol = Float_u.max (S1_angle.radians tolerance) #0.0 in
    let result = Array.create ~len:n 0 in
    let mutable count = 1 in
    result.(0) <- 0;
    let mutable index = 0 in
    while index + 1 < n do
      let next_index = find_end_vertex t.vertices tol index in
      if not (S2_point.equal t.vertices.(next_index) t.vertices.(index))
      then (
        result.(count) <- next_index;
        count <- count + 1);
      index <- next_index
    done;
    Array.sub result ~pos:0 ~len:count)
;;

let num_edges t =
  let n = num_vertices t in
  if n = 0 then 0 else n - 1
;;

let edge t e = S2_shape.Edge.create ~v0:t.vertices.(e) ~v1:t.vertices.(e + 1)
let dimension _ = 1
let num_chains t = Int.min 1 (num_edges t)
let chain t _ = S2_shape.Chain.create ~start:0 ~length:(num_edges t)
let chain_edge t _ j = S2_shape.Edge.create ~v0:t.vertices.(j) ~v1:t.vertices.(j + 1)
let chain_position _ e = S2_shape.Chain_position.create ~chain_id:0 ~offset:e
let reference_point _ = S2_shape.Reference_point.contained false
let type_tag = 2

let to_shape t : S2_shape.t =
  #{ num_edges = num_edges t
   ; num_chains = num_chains t
   ; dimension = 1
   ; type_tag
   ; reference_point = reference_point t
   ; edge = edge t
   ; chain = chain t
   ; chain_edge = chain_edge t
   ; chain_position = chain_position t
   }
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
