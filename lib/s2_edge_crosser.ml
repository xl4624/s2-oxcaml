open Core

(* Stateful edge crosser for a fixed edge AB.

   The fixed edge AB is stored along with its cross product and the outward-
   facing tangents at A and B (computed lazily the first time the slow path
   runs). For each chain vertex C we cache the orientation [acb] of triangle
   ACB; this lets [chain_crossing_sign] reject most non-crossings with a
   single triage-sign call on triangle ABD.

   The whole state is an unboxed record. All "mutating" operations return a
   new [t]; callers typically keep it in a [let mutable] so that assignment
   rewrites the underlying storage in place. Query operations that both
   advance the chain and produce a result return an unboxed [#{ state; sign }]
   pair. *)

type t =
  #{ a : R3_vector.t
   ; b : R3_vector.t
   ; a_cross_b : R3_vector.t
   ; have_tangents : int
   ; a_tangent : R3_vector.t
   ; b_tangent : R3_vector.t
   ; c : R3_vector.t
   ; acb : int
   ; bda : int
   }

type with_sign =
  #{ state : t
   ; sign : int
   }

type with_bool =
  #{ state : t
   ; crossing : bool
   }

(* Tangent error bound: (1.5 + 1/sqrt(3)) * DBL_EPSILON. *)
let tangent_error =
  Float_u.O.((#1.5 + (#1.0 / Float_u.sqrt #3.0)) * #2.220446049250313e-16)
;;

let[@inline] create ~a ~b =
  #{ a
   ; b
   ; a_cross_b = R3_vector.cross a b
   ; have_tangents = 0
   ; a_tangent = R3_vector.zero
   ; b_tangent = R3_vector.zero
   ; c = R3_vector.zero
   ; acb = 0
   ; bda = 0
   }
;;

let[@inline] init t ~a ~b =
  #{ a
   ; b
   ; a_cross_b = R3_vector.cross a b
   ; have_tangents = 0
   ; a_tangent = R3_vector.zero
   ; b_tangent = R3_vector.zero
   ; c = R3_vector.zero
   ; acb = 0
   ; bda = t.#bda (* unused; keep layout stable *)
   }
;;

let[@inline] a t = t.#a
let[@inline] b t = t.#b
let[@inline] c t = t.#c

let[@inline] restart_at t c =
  #{ a = t.#a
   ; b = t.#b
   ; a_cross_b = t.#a_cross_b
   ; have_tangents = t.#have_tangents
   ; a_tangent = t.#a_tangent
   ; b_tangent = t.#b_tangent
   ; c
   ; acb = Int.neg (S2_edge_crossings.triage_sign t.#a t.#b c)
   ; bda = t.#bda
   }
;;

let[@inline] create_with_chain ~a ~b ~c = restart_at (create ~a ~b) c

(* Ensure [have_tangents] is set; if not, compute and store the tangents.
   Returns an updated [t] with [have_tangents = 1]. *)
let[@inline] ensure_tangents t =
  if Int.equal t.#have_tangents 1
  then t
  else (
    let norm = S2_point.robust_cross_prod t.#a t.#b in
    #{ a = t.#a
     ; b = t.#b
     ; a_cross_b = t.#a_cross_b
     ; have_tangents = 1
     ; a_tangent = R3_vector.cross t.#a norm
     ; b_tangent = R3_vector.cross norm t.#b
     ; c = t.#c
     ; acb = t.#acb
     ; bda = t.#bda
     })
;;

(* Slow path of chain_crossing_sign.
   Precondition: [t.#bda] is already set to TriageSign(a, b, d). Returns the
   updated state (which may have [have_tangents] flipped, and [acb]/[bda]
   promoted from 0 to the exact sign) together with the crossing sign. *)
let crossing_sign_internal t d =
  let t = ensure_tangents t in
  let open Float_u.O in
  let c_dot_at = R3_vector.dot t.#c t.#a_tangent in
  let d_dot_at = R3_vector.dot d t.#a_tangent in
  let c_dot_bt = R3_vector.dot t.#c t.#b_tangent in
  let d_dot_bt = R3_vector.dot d t.#b_tangent in
  if (c_dot_at > tangent_error && d_dot_at > tangent_error)
     || (c_dot_bt > tangent_error && d_dot_bt > tangent_error)
  then #{ state = t; sign = -1 }
  else if S2_point.equal t.#a t.#c
          || S2_point.equal t.#a d
          || S2_point.equal t.#b t.#c
          || S2_point.equal t.#b d
  then #{ state = t; sign = 0 }
  else if S2_point.equal t.#a t.#b || S2_point.equal t.#c d
  then #{ state = t; sign = -1 }
  else (
    let acb =
      if Int.equal t.#acb 0
      then Int.neg (S2_edge_crossings.expensive_sign t.#a t.#b t.#c)
      else t.#acb
    in
    let bda =
      if Int.equal t.#bda 0 then S2_edge_crossings.expensive_sign t.#a t.#b d else t.#bda
    in
    let t =
      #{ a = t.#a
       ; b = t.#b
       ; a_cross_b = t.#a_cross_b
       ; have_tangents = t.#have_tangents
       ; a_tangent = t.#a_tangent
       ; b_tangent = t.#b_tangent
       ; c = t.#c
       ; acb
       ; bda
       }
    in
    if not (Int.equal bda acb)
    then #{ state = t; sign = -1 }
    else (
      let cbd = Int.neg (S2_edge_crossings.sign t.#c d t.#b) in
      if not (Int.equal cbd acb)
      then #{ state = t; sign = -1 }
      else (
        let dac = S2_edge_crossings.sign t.#c d t.#a in
        if not (Int.equal dac acb)
        then #{ state = t; sign = -1 }
        else #{ state = t; sign = 1 })))
;;

(* Commit [new_c] / [new_acb] at the end of a chain operation so the next call
   sees the correct cached triangle orientation. *)
let[@inline] commit_chain_step t ~new_c ~new_acb =
  #{ a = t.#a
   ; b = t.#b
   ; a_cross_b = t.#a_cross_b
   ; have_tangents = t.#have_tangents
   ; a_tangent = t.#a_tangent
   ; b_tangent = t.#b_tangent
   ; c = new_c
   ; acb = new_acb
   ; bda = t.#bda
   }
;;

let chain_crossing_sign t d =
  (* Fast path: if ACB and BDA have opposite non-zero orientations, CD cannot
     cross AB. triage_sign is invariant under rotation, so ABD has the same
     orientation as BDA. *)
  let bda = S2_edge_crossings.triage_sign t.#a t.#b d in
  if Int.equal t.#acb (-bda) && not (Int.equal bda 0)
  then (
    let state = commit_chain_step t ~new_c:d ~new_acb:(-bda) in
    #{ state; sign = -1 })
  else (
    let t_with_bda =
      #{ a = t.#a
       ; b = t.#b
       ; a_cross_b = t.#a_cross_b
       ; have_tangents = t.#have_tangents
       ; a_tangent = t.#a_tangent
       ; b_tangent = t.#b_tangent
       ; c = t.#c
       ; acb = t.#acb
       ; bda
       }
    in
    let #{ state; sign } = crossing_sign_internal t_with_bda d in
    let state = commit_chain_step state ~new_c:d ~new_acb:(-state.#bda) in
    #{ state; sign })
;;

let[@inline] crossing_sign t c d =
  let t = if S2_point.equal c t.#c then t else restart_at t c in
  chain_crossing_sign t d
;;

let chain_edge_or_vertex_crossing t d =
  (* Save c since [chain_crossing_sign] overwrites it. *)
  let c_prev = t.#c in
  let #{ state; sign } = chain_crossing_sign t d in
  match sign with
  | -1 -> #{ state; crossing = false }
  | 0 -> #{ state; crossing = S2_edge_crossings.vertex_crossing t.#a t.#b c_prev d }
  | _ -> #{ state; crossing = true }
;;

let[@inline] edge_or_vertex_crossing t c d =
  let t = if S2_point.equal c t.#c then t else restart_at t c in
  chain_edge_or_vertex_crossing t d
;;

(* Signed version of VertexCrossing: returns +1 if the shared vertex crossing
   is in the counter-clockwise sense, 0 if it is not a valid vertex crossing. *)
let signed_vertex_crossing a b c d =
  if S2_point.equal a b || S2_point.equal c d
  then 0
  else if S2_point.equal a c
  then
    if S2_point.equal b d || S2_edge_crossings.ordered_ccw (S2_point.ortho a) d b a
    then 1
    else 0
  else if S2_point.equal b d
  then if S2_edge_crossings.ordered_ccw (S2_point.ortho b) c a b then 1 else 0
  else if S2_point.equal a d
  then
    if S2_point.equal b c || S2_edge_crossings.ordered_ccw (S2_point.ortho a) c b a
    then -1
    else 0
  else if S2_point.equal b c
  then if S2_edge_crossings.ordered_ccw (S2_point.ortho b) d a b then -1 else 0
  else 0
;;

let[@inline] last_interior_crossing_sign t =
  (* When AB crosses CD, the crossing sign is Sign(ABC). The edge crosser
     stores the sign of the *next* triangle ACB, which happens to be equal. *)
  t.#acb
;;

let chain_signed_edge_or_vertex_crossing t d =
  let c_prev = t.#c in
  let #{ state; sign } = chain_crossing_sign t d in
  match sign with
  | -1 -> #{ state; sign = 0 }
  | 0 -> #{ state; sign = signed_vertex_crossing state.#a state.#b c_prev d }
  | _ -> #{ state; sign = last_interior_crossing_sign state }
;;

let[@inline] signed_edge_or_vertex_crossing t c d =
  let t = if S2_point.equal c t.#c then t else restart_at t c in
  chain_signed_edge_or_vertex_crossing t d
;;
