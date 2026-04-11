open Core

(* Stateful edge crosser that mirrors C++ S2EdgeCrosser / Go EdgeCrosser.

   The fixed edge AB is stored along with its cross product and the outward-
   facing tangents at A and B (computed lazily the first time the slow path
   runs). For each chain vertex C we cache the orientation [acb] of triangle
   ACB; this lets [chain_crossing_sign] reject most non-crossings with a
   single triage-sign call on triangle ABD. *)

(* All fields are S2_point.t (= R3_vector.t, an unboxed record of three
   float#s) or int. Mutability is provided by a boxed record whose fields
   directly contain the unboxed values, so assigning a new point writes the
   three float#s in place with no allocation. *)
type t =
  { mutable a : S2_point.t
  ; mutable b : S2_point.t
  ; mutable a_cross_b : R3_vector.t
  ; mutable have_tangents : bool
  ; mutable a_tangent : S2_point.t
  ; mutable b_tangent : S2_point.t
  ; mutable c : S2_point.t
  ; mutable acb : int
  ; mutable bda : int
  }

(* Matches C++ kError = (1.5 + 1/sqrt(3)) * DBL_EPSILON. *)
let tangent_error =
  Float_u.O.((#1.5 + (#1.0 / Float_u.sqrt #3.0)) * #2.220446049250313e-16)
;;

let create ~a ~b =
  { a
  ; b
  ; a_cross_b = R3_vector.cross (S2_point.to_r3 a) (S2_point.to_r3 b)
  ; have_tangents = false
  ; a_tangent = R3_vector.zero
  ; b_tangent = R3_vector.zero
  ; c = R3_vector.zero
  ; acb = 0
  ; bda = 0
  }
;;

let init t ~a ~b =
  t.a <- a;
  t.b <- b;
  t.a_cross_b <- R3_vector.cross (S2_point.to_r3 a) (S2_point.to_r3 b);
  t.have_tangents <- false;
  t.c <- R3_vector.zero;
  t.acb <- 0;
  t.bda <- 0
;;

let a t = t.a
let b t = t.b
let c t = t.c

let restart_at t c =
  t.c <- c;
  t.acb <- Int.neg (S2_edge_crossings.triage_sign t.a t.b c)
;;

let create_with_chain ~a ~b ~c =
  let t = create ~a ~b in
  restart_at t c;
  t
;;

(* Slow path of chain_crossing_sign, mirroring C++ CrossingSignInternal2.
   Precondition: t.bda is already set to TriageSign(a, b, d). *)
let crossing_sign_internal t d =
  if not t.have_tangents
  then (
    let norm = S2_point.robust_cross_prod t.a t.b in
    t.a_tangent <- R3_vector.cross (S2_point.to_r3 t.a) norm;
    t.b_tangent <- R3_vector.cross norm (S2_point.to_r3 t.b);
    t.have_tangents <- true);
  let open Float_u.O in
  let c_dot_at = R3_vector.dot (S2_point.to_r3 t.c) t.a_tangent in
  let d_dot_at = R3_vector.dot (S2_point.to_r3 d) t.a_tangent in
  let c_dot_bt = R3_vector.dot (S2_point.to_r3 t.c) t.b_tangent in
  let d_dot_bt = R3_vector.dot (S2_point.to_r3 d) t.b_tangent in
  if (c_dot_at > tangent_error && d_dot_at > tangent_error)
     || (c_dot_bt > tangent_error && d_dot_bt > tangent_error)
  then -1
  else if S2_point.equal t.a t.c
          || S2_point.equal t.a d
          || S2_point.equal t.b t.c
          || S2_point.equal t.b d
  then 0
  else if S2_point.equal t.a t.b || S2_point.equal t.c d
  then -1
  else (
    if Int.equal t.acb 0
    then t.acb <- Int.neg (S2_edge_crossings.expensive_sign t.a t.b t.c);
    if Int.equal t.bda 0 then t.bda <- S2_edge_crossings.expensive_sign t.a t.b d;
    if not (Int.equal t.bda t.acb)
    then -1
    else (
      let cbd = Int.neg (S2_edge_crossings.sign t.c d t.b) in
      if not (Int.equal cbd t.acb)
      then -1
      else (
        let dac = S2_edge_crossings.sign t.c d t.a in
        if not (Int.equal dac t.acb) then -1 else 1)))
;;

let chain_crossing_sign t d =
  (* Fast path: if ACB and BDA have opposite non-zero orientations, CD cannot
     cross AB. triage_sign is invariant under rotation, so ABD has the same
     orientation as BDA. *)
  let bda = S2_edge_crossings.triage_sign t.a t.b d in
  if Int.equal t.acb (-bda) && not (Int.equal bda 0)
  then (
    t.c <- d;
    t.acb <- -bda;
    -1)
  else (
    t.bda <- bda;
    let result = crossing_sign_internal t d in
    t.c <- d;
    t.acb <- -t.bda;
    result)
;;

let crossing_sign t c d =
  if not (S2_point.equal c t.c) then restart_at t c;
  chain_crossing_sign t d
;;

let chain_edge_or_vertex_crossing t d =
  (* Save c since it is clobbered by chain_crossing_sign. *)
  let c_prev = t.c in
  match chain_crossing_sign t d with
  | -1 -> false
  | 0 -> S2_edge_crossings.vertex_crossing t.a t.b c_prev d
  | _ -> true
;;

let edge_or_vertex_crossing t c d =
  if not (S2_point.equal c t.c) then restart_at t c;
  chain_edge_or_vertex_crossing t d
;;

(* Signed version of VertexCrossing, mirroring C++ S2::SignedVertexCrossing. *)
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

let last_interior_crossing_sign t =
  (* When AB crosses CD, the crossing sign is Sign(ABC). The edge crosser
     stores the sign of the *next* triangle ACB, which happens to be equal. *)
  t.acb
;;

let chain_signed_edge_or_vertex_crossing t d =
  let c_prev = t.c in
  match chain_crossing_sign t d with
  | -1 -> 0
  | 0 -> signed_vertex_crossing t.a t.b c_prev d
  | _ -> last_interior_crossing_sign t
;;

let signed_edge_or_vertex_crossing t c d =
  if not (S2_point.equal c t.c) then restart_at t c;
  chain_signed_edge_or_vertex_crossing t d
;;
