open Core

type methods =
  #{ cap_bound : unit -> S2_cap.t
   ; rect_bound : unit -> S2_latlng_rect.t
   ; contains_cell : S2_cell.t -> bool
   ; intersects_cell : S2_cell.t -> bool
   ; contains_point : S2_point.t -> bool
   ; cell_union_bound : unit -> S2_cell_id.t array
   }

type t =
  | Cap of S2_cap.t
  | Rect of S2_latlng_rect.t
  | Cell of S2_cell.t
  | Cell_union of S2_cell_union.t
  | Custom of methods

let sexp_of_t _ = Sexp.Atom "<S2_region.t>"

(* Returns true if the cap intersects any point of the cell EXCLUDING the
   already-checked vertices.

   Uses a local recursive helper for the edge loop so we can return as soon as the
   answer is decided - OCaml [for] has no [break]/[continue], and in a coverage hot
   path the mutable-flag pattern wastes iterations after a rejection or acceptance. *)
let cap_intersects_cell_interior cap cell ~v0 ~v1 ~v2 ~v3 =
  let radius = S2_cap.radius_chord cap in
  if Stdlib.( >= ) (S1_chord_angle.compare radius S1_chord_angle.right) 0
  then false
  else if S2_cap.is_empty cap
  then false
  else if S2_cell.contains_point cell (S2_cap.center cap)
  then true
  else
    let open Float_u.O in
    let center = S2_cap.center cap in
    let sin2_angle = S1_chord_angle.sin2 radius in
    let rec loop k =
      if Stdlib.( >= ) k 4
      then false
      else (
        let edge = S2_cell.edge_raw cell k in
        let dot = R3_vector.dot center edge in
        if dot > #0.0
        then loop (Stdlib.( + ) k 1)
        else if dot * dot > sin2_angle * R3_vector.norm2 edge
        then false
        else (
          let dir = R3_vector.cross edge center in
          let v_k =
            match k with
            | 0 -> v0
            | 1 -> v1
            | 2 -> v2
            | _ -> v3
          in
          let v_kp1 =
            match Stdlib.( land ) (Stdlib.( + ) k 1) 3 with
            | 0 -> v0
            | 1 -> v1
            | 2 -> v2
            | _ -> v3
          in
          if R3_vector.dot dir v_k < #0.0 && R3_vector.dot dir v_kp1 > #0.0
          then true
          else loop (Stdlib.( + ) k 1)))
    in
    loop 0
;;

(* Short-circuit signals for the cap-cell vertex-check loops. Module-level so they are
   statically allocated (no per-call identity construction, unlike [let exception]) and
   [raise_notrace] avoids stack-trace capture. *)
exception Cap_missed_vertex
exception Cap_contains_vertex

(* Lazy vertex evaluation: compute vertex k, check, exit early if the answer is decided.
   In the coverer hot path [cap_contains_cell] usually fails on the first vertex (the cap
   is smaller than the cell at early tree levels), so eager computation of all 4 vertices
   wastes ~3 [R3_vector.normalize] calls per check. *)
let cap_contains_cell cap cell =
  try
    let v0 = S2_cell.vertex cell 0 in
    if not (S2_cap.contains_point cap v0) then raise_notrace Cap_missed_vertex;
    let v1 = S2_cell.vertex cell 1 in
    if not (S2_cap.contains_point cap v1) then raise_notrace Cap_missed_vertex;
    let v2 = S2_cell.vertex cell 2 in
    if not (S2_cap.contains_point cap v2) then raise_notrace Cap_missed_vertex;
    let v3 = S2_cell.vertex cell 3 in
    if not (S2_cap.contains_point cap v3) then raise_notrace Cap_missed_vertex;
    let comp = S2_cap.complement cap in
    not (cap_intersects_cell_interior comp cell ~v0 ~v1 ~v2 ~v3)
  with
  | Cap_missed_vertex -> false
;;

let cap_intersects_cell cap cell =
  try
    let v0 = S2_cell.vertex cell 0 in
    if S2_cap.contains_point cap v0 then raise_notrace Cap_contains_vertex;
    let v1 = S2_cell.vertex cell 1 in
    if S2_cap.contains_point cap v1 then raise_notrace Cap_contains_vertex;
    let v2 = S2_cell.vertex cell 2 in
    if S2_cap.contains_point cap v2 then raise_notrace Cap_contains_vertex;
    let v3 = S2_cell.vertex cell 3 in
    if S2_cap.contains_point cap v3 then raise_notrace Cap_contains_vertex;
    cap_intersects_cell_interior cap cell ~v0 ~v1 ~v2 ~v3
  with
  | Cap_contains_vertex -> true
;;

let of_cap c = Cap c
let of_rect r = Rect r
let of_cell c = Cell c
let of_cell_union u = Cell_union u
let custom m = Custom m

let[@inline] cap_bound = function
  | Cap c -> c
  | Rect r -> S2_latlng_rect.cap_bound r
  | Cell c -> S2_cell.cap_bound c
  | Cell_union u -> S2_cell_union.cap_bound u
  | Custom m -> m.#cap_bound ()
;;

let[@inline] rect_bound = function
  | Cap c -> S2_latlng_rect.from_cap c
  | Rect r -> r
  | Cell c -> S2_latlng_rect.from_cell c
  | Cell_union u -> S2_cell_union.rect_bound u
  | Custom m -> m.#rect_bound ()
;;

let[@inline] contains_cell t cell =
  match t with
  | Cap c -> cap_contains_cell c cell
  | Rect r -> S2_latlng_rect.contains_cell r cell
  | Cell c -> S2_cell.contains_cell c cell
  | Cell_union u -> S2_cell_union.contains_cell u cell
  | Custom m -> m.#contains_cell cell
;;

let[@inline] intersects_cell t cell =
  match t with
  | Cap c -> cap_intersects_cell c cell
  | Rect r -> S2_latlng_rect.intersects_cell r cell
  | Cell c -> S2_cell.intersects_cell c cell
  | Cell_union u -> S2_cell_union.intersects_cell u cell
  | Custom m -> m.#intersects_cell cell
;;

let[@inline] contains_point t p =
  match t with
  | Cap c -> S2_cap.contains_point c p
  | Rect r -> S2_latlng_rect.contains_point r p
  | Cell c -> S2_cell.contains_point c p
  | Cell_union u -> S2_cell_union.contains_point u p
  | Custom m -> m.#contains_point p
;;

let[@inline] cell_union_bound = function
  | Cap c -> S2_cap.cell_union_bound c
  | Rect r -> S2_latlng_rect.cell_union_bound r
  | Cell c -> S2_cell.cell_union_bound c
  | Cell_union u -> S2_cell_union.cell_union_bound u
  | Custom m -> m.#cell_union_bound ()
;;
