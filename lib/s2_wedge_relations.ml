open Core

module Relation = struct
  type t =
    | Equals
    | Properly_contains
    | Is_properly_contained
    | Properly_overlaps
    | Is_disjoint
  [@@deriving sexp_of, compare, equal]
end

let[@inline] wedge_contains ~a0 ~ab1 ~a2 ~b0 ~b2 =
  (* For A to contain B (where each loop interior is defined to be its left
     side), the CCW edge order around ab1 must be a2 b2 b0 a0. Split the test
     into two three-point ordered_ccw checks. *)
  S2_predicates.ordered_ccw a2 b2 b0 ab1 && S2_predicates.ordered_ccw b0 a0 a2 ab1
;;

let[@inline] wedge_intersects ~a0 ~ab1 ~a2 ~b0 ~b2 =
  (* For A not to intersect B, the CCW edge order around ab1 must be
     a0 b2 b0 a2. It is important to write the conditions as negatives
     (not ordered_ccw ...) rather than Ordered(c, b, a, o) to get correct
     results when two vertices coincide. *)
  not (S2_predicates.ordered_ccw a0 b2 b0 ab1 && S2_predicates.ordered_ccw b0 a2 a0 ab1)
;;

let get_wedge_relation ~a0 ~ab1 ~a2 ~b0 ~b2 =
  (* There are 6 possible circular edge orderings at the shared vertex:
       (1) a2 b2 b0 a0  -> A contains B
       (2) a2 a0 b0 b2  -> B contains A
       (3) a2 a0 b2 b0  -> A and B are disjoint
       (4) a2 b0 a0 b2  -> A and B intersect in one wedge
       (5) a2 b2 a0 b0  -> A and B intersect in one wedge
       (6) a2 b0 b2 a0  -> A and B intersect in two wedges
     Cases 4, 5, and 6 are not distinguished. When edges overlap several
     orderings may hold; we pick the most specific. *)
  if S2_point.equal a0 b0 && S2_point.equal a2 b2
  then Relation.Equals
  else if S2_predicates.ordered_ccw a0 a2 b2 ab1
  then
    (* Cases 1, 5, 6 -- and case 2 if a2 == b2. *)
    if S2_predicates.ordered_ccw b2 b0 a0 ab1
    then Relation.Properly_contains
    else if S2_point.equal a2 b2
    then Relation.Is_properly_contained
    else Relation.Properly_overlaps
  else if (* Cases 2, 3, or 4. *)
          S2_predicates.ordered_ccw a0 b0 b2 ab1
  then Relation.Is_properly_contained
  else if S2_predicates.ordered_ccw a0 b0 a2 ab1
  then Relation.Is_disjoint
  else Relation.Properly_overlaps
;;
