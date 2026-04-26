open Core

(* Performance strategy.  The fast path of [chain_crossing_sign] is a
   single call to [triage_sign] on triangle ABD: if the cached
   orientation of ACB disagrees with ABD (both non-zero), CD cannot
   cross AB and we are done.  When triage is ambiguous we compute the
   outward tangents at A and B once (see [ensure_tangents]) and use
   them to reject AB-vs-CD pairs that sit entirely outside the
   hemispheres defined by A and B.  Only pairs that survive both stages
   fall through to [expensive_sign] plus the full four-triangle
   orientation check.  See s2edge_crosser.h:261-279. *)

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

(* Upper bound on the rounding error of a tangent-dot-product.  Any
   dot product with magnitude less than this is treated as
   indeterminate and the slow path is taken.  The constant
   (1.5 + 1/sqrt(3)) * DBL_EPSILON is derived in
   s2edge_crossings_internal.h. *)
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
  (* [bda] is a scratch field only read inside a single call to
     [chain_crossing_sign]; we propagate the old value solely to keep
     the unboxed record layout stable and to avoid an unnecessary zero
     store. *)
  #{ a
   ; b
   ; a_cross_b = R3_vector.cross a b
   ; have_tangents = 0
   ; a_tangent = R3_vector.zero
   ; b_tangent = R3_vector.zero
   ; c = R3_vector.zero
   ; acb = 0
   ; bda = t.#bda
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

(* Compute outward-facing tangents at A and B the first time they are
   needed.  Once populated, [have_tangents = 1] prevents recomputation
   for the lifetime of this fixed edge.  The tangents are orthogonal to
   the great circle AB and rotated 90 degrees outward from A and B
   respectively; they define the two hemispheres that must contain
   either C or D for a crossing to be possible. *)
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

(* Slow path of [chain_crossing_sign]: runs only when the cheap triage
   fails.  Precondition: [t.#bda] already holds the triage sign of
   triangle ABD.  The function may populate the tangents, promote
   [acb]/[bda] from 0 to an exact sign via [expensive_sign], and
   finally check the remaining two triangles (CBD and DAC).  All four
   triangles ACB, CBD, BDA, DAC must share the same orientation for AB
   to actually cross CD.  See s2edge_crosser.cc:40-152. *)
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

(* Finalize a chain step.  After computing the crossing sign for edge
   CD, the *next* C is D and the *next* orientation of ACB is the
   negation of the old BDA (since ACB rotates to DAB = -ABD = -BDA).
   Writing both fields together keeps the unboxed layout coherent for
   the next call. *)
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
  (* Fast path: if ACB and BDA have opposite non-zero orientations, C
     and D lie on opposite sides of the great circle through AB but
     rotated in opposite senses, so CD cannot cross AB.  Note that
     [triage_sign] is invariant under cyclic rotation of its arguments,
     so ABD has the same sign as BDA. *)
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
  (* [chain_crossing_sign] advances the chain vertex to d, so we snapshot
     the pre-call C before running it; the vertex-crossing test below
     still needs the original C. *)
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

let[@inline] last_interior_crossing_sign t =
  (* When AB crosses CD, the signed crossing direction equals
     Sign(ABC).  We don't store that directly, but the cached
     orientation of the *next* triangle ACB - written just before we
     return +1 from [chain_crossing_sign] - has exactly the same sign
     by cyclic rotation. *)
  t.#acb
;;

let chain_signed_edge_or_vertex_crossing t d =
  let c_prev = t.#c in
  let #{ state; sign } = chain_crossing_sign t d in
  match sign with
  | -1 -> #{ state; sign = 0 }
  | 0 ->
    #{ state; sign = S2_edge_crossings.signed_vertex_crossing state.#a state.#b c_prev d }
  | _ -> #{ state; sign = last_interior_crossing_sign state }
;;

let[@inline] signed_edge_or_vertex_crossing t c d =
  let t = if S2_point.equal c t.#c then t else restart_at t c in
  chain_signed_edge_or_vertex_crossing t d
;;
