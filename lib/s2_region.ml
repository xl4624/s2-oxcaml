open Core

type t =
  #{ cap_bound : unit -> S2_cap.t
   ; rect_bound : unit -> S2_latlng_rect.t
   ; contains_cell : S2_cell.t -> bool
   ; intersects_cell : S2_cell.t -> bool
   ; contains_point : S2_point.t -> bool
   ; cell_union_bound : unit -> Int64.t list
   }

let sexp_of_t _ = Sexp.Atom "<S2_region.t>"

(* Returns true if the cap intersects any point of the cell EXCLUDING the
   already-checked vertices. *)
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
    let mutable found = false in
    let mutable rejected = false in
    for k = 0 to 3 do
      if (not found) && not rejected
      then (
        let edge = S2_cell.edge_raw cell k in
        let dot = R3_vector.dot center edge in
        if not (dot > #0.0)
        then
          if dot * dot > sin2_angle * R3_vector.norm2 edge
          then rejected <- true
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
            then found <- true))
    done;
    found
;;

let cap_contains_cell cap cell =
  let v0 = S2_cell.vertex cell 0 in
  let v1 = S2_cell.vertex cell 1 in
  let v2 = S2_cell.vertex cell 2 in
  let v3 = S2_cell.vertex cell 3 in
  if not (S2_cap.contains_point cap v0)
  then false
  else if not (S2_cap.contains_point cap v1)
  then false
  else if not (S2_cap.contains_point cap v2)
  then false
  else if not (S2_cap.contains_point cap v3)
  then false
  else (
    let comp = S2_cap.complement cap in
    not (cap_intersects_cell_interior comp cell ~v0 ~v1 ~v2 ~v3))
;;

let cap_intersects_cell cap cell =
  let v0 = S2_cell.vertex cell 0 in
  let v1 = S2_cell.vertex cell 1 in
  let v2 = S2_cell.vertex cell 2 in
  let v3 = S2_cell.vertex cell 3 in
  if S2_cap.contains_point cap v0
  then true
  else if S2_cap.contains_point cap v1
  then true
  else if S2_cap.contains_point cap v2
  then true
  else if S2_cap.contains_point cap v3
  then true
  else cap_intersects_cell_interior cap cell ~v0 ~v1 ~v2 ~v3
;;

let of_cap c =
  #{ cap_bound = (fun () -> c)
   ; rect_bound = (fun () -> S2_latlng_rect.from_cap c)
   ; contains_cell = (fun cell -> cap_contains_cell c cell)
   ; intersects_cell = (fun cell -> cap_intersects_cell c cell)
   ; contains_point = (fun p -> S2_cap.contains_point c p)
   ; cell_union_bound = (fun () -> S2_cap.cell_union_bound c)
   }
;;

let of_rect r =
  #{ cap_bound = (fun () -> S2_latlng_rect.cap_bound r)
   ; rect_bound = (fun () -> r)
   ; contains_cell = (fun cell -> S2_latlng_rect.contains_cell r cell)
   ; intersects_cell = (fun cell -> S2_latlng_rect.intersects_cell r cell)
   ; contains_point = (fun p -> S2_latlng_rect.contains_point r p)
   ; cell_union_bound = (fun () -> S2_latlng_rect.cell_union_bound r)
   }
;;

let of_cell c =
  #{ cap_bound = (fun () -> S2_cell.cap_bound c)
   ; rect_bound = (fun () -> S2_latlng_rect.from_cell c)
   ; contains_cell = (fun other -> S2_cell.contains_cell c other)
   ; intersects_cell = (fun other -> S2_cell.intersects_cell c other)
   ; contains_point = (fun p -> S2_cell.contains_point c p)
   ; cell_union_bound = (fun () -> S2_cell.cell_union_bound c)
   }
;;

let of_cell_union u =
  #{ cap_bound = (fun () -> S2_cell_union.cap_bound u)
   ; rect_bound = (fun () -> S2_cell_union.rect_bound u)
   ; contains_cell = (fun cell -> S2_cell_union.contains_cell u cell)
   ; intersects_cell = (fun cell -> S2_cell_union.intersects_cell u cell)
   ; contains_point = (fun p -> S2_cell_union.contains_point u p)
   ; cell_union_bound = (fun () -> S2_cell_union.cell_union_bound u)
   }
;;
