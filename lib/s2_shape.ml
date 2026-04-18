open Core

module Edge = struct
  type t =
    #{ v0 : S2_point.t
     ; v1 : S2_point.t
     }

  let sexp_of_t #{ v0; v1 } =
    Sexp.List [ Sexp.Atom "Edge"; S2_point.sexp_of_t v0; S2_point.sexp_of_t v1 ]
  ;;

  let[@inline] create ~v0 ~v1 = #{ v0; v1 }
  let[@inline] reversed #{ v0; v1 } = #{ v0 = v1; v1 = v0 }
  let[@inline] is_degenerate #{ v0; v1 } = S2_point.equal v0 v1
  let[@inline] incoming #{ v0 = _; v1 } p = S2_point.equal v1 p
  let[@inline] outgoing #{ v0; v1 = _ } p = S2_point.equal v0 p
  let[@inline] incident_on e p = outgoing e p || incoming e p

  let[@inline] compare a b =
    match S2_point.compare a.#v0 b.#v0 with
    | 0 -> S2_point.compare a.#v1 b.#v1
    | c -> c
  ;;

  let[@inline] equal a b = S2_point.equal a.#v0 b.#v0 && S2_point.equal a.#v1 b.#v1
end

module Edge_sort = Unboxed_array.Make_3_3 (struct
    type t = Edge.t
  end)

module Chain = struct
  type t =
    #{ start : int
     ; length : int
     }

  let sexp_of_t #{ start; length } =
    Sexp.List
      [ Sexp.Atom "Chain"
      ; Sexp.List [ Sexp.Atom "start"; Sexp.Atom (Int.to_string start) ]
      ; Sexp.List [ Sexp.Atom "length"; Sexp.Atom (Int.to_string length) ]
      ]
  ;;

  let[@inline] create ~start ~length = #{ start; length }
  let[@inline] equal a b = a.#start = b.#start && a.#length = b.#length
end

module Chain_position = struct
  type t =
    #{ chain_id : int
     ; offset : int
     }

  let sexp_of_t #{ chain_id; offset } =
    Sexp.List
      [ Sexp.Atom "Chain_position"
      ; Sexp.List [ Sexp.Atom "chain_id"; Sexp.Atom (Int.to_string chain_id) ]
      ; Sexp.List [ Sexp.Atom "offset"; Sexp.Atom (Int.to_string offset) ]
      ]
  ;;

  let[@inline] create ~chain_id ~offset = #{ chain_id; offset }
  let[@inline] equal a b = a.#chain_id = b.#chain_id && a.#offset = b.#offset
end

module Reference_point = struct
  type t =
    #{ point : S2_point.t
     ; contained : bool
     }

  let sexp_of_t #{ point; contained } =
    Sexp.List
      [ Sexp.Atom "Reference_point"
      ; S2_point.sexp_of_t point
      ; Sexp.Atom (Bool.to_string contained)
      ]
  ;;

  let[@inline] create ~point ~contained = #{ point; contained }
  let[@inline] contained contained = #{ point = S2_point.origin; contained }

  let[@inline] equal a b =
    Bool.equal a.#contained b.#contained && S2_point.equal a.#point b.#point
  ;;
end

module Type_tag = struct
  type t = int

  let none = 0
  let next_available = 6
  let min_user = 8192
end

type t =
  #{ num_edges : unit -> int
   ; edge : int -> Edge.t
   ; dimension : unit -> int
   ; num_chains : unit -> int
   ; chain : int -> Chain.t
   ; chain_edge : int -> int -> Edge.t
   ; chain_position : int -> Chain_position.t
   ; reference_point : unit -> Reference_point.t
   ; type_tag : unit -> Type_tag.t
   }

let sexp_of_t _ = Sexp.Atom "<S2_shape.t>"
let is_empty t = t.#num_edges () = 0 && (t.#dimension () < 2 || t.#num_chains () = 0)
let is_full t = t.#num_edges () = 0 && t.#dimension () = 2 && t.#num_chains () > 0

(* Returns 0 if [vtest] is balanced (matched sibling pairs only), otherwise
   the sign reported by [S2_contains_vertex_query]: positive means contained,
   negative means not contained. *)
let contains_sign_at_vertex ~vtest ~num_edges ~edge =
  let q = S2_contains_vertex_query.create vtest in
  for e = 0 to num_edges - 1 do
    let #{ v0; v1 } : Edge.t = edge e in
    if S2_point.equal v0 vtest then S2_contains_vertex_query.add_edge q v1 ~direction:1;
    if S2_point.equal v1 vtest then S2_contains_vertex_query.add_edge q v0 ~direction:(-1)
  done;
  S2_contains_vertex_query.contains_sign q
;;

let get_reference_point ~num_edges ~num_chains ~edge ~chain =
  if num_edges = 0
  then Reference_point.contained (num_chains > 0)
  else (
    let #{ v0 = first_v0; v1 = _ } : Edge.t = edge 0 in
    let first_sign = contains_sign_at_vertex ~vtest:first_v0 ~num_edges ~edge in
    if first_sign <> 0
    then Reference_point.create ~point:first_v0 ~contained:(first_sign > 0)
    else (
      (* No unmatched edge at the first vertex; sort forward and reverse edges
         and find the first edge present in one list but not the other. *)
      let edges = Array.create ~len:num_edges (edge 0) in
      let rev_edges = Array.create ~len:num_edges (Edge.reversed edges.(0)) in
      for i = 0 to num_edges - 1 do
        let e = edge i in
        edges.(i) <- e;
        rev_edges.(i) <- Edge.reversed e
      done;
      Edge_sort.sort edges ~compare:Edge.compare;
      Edge_sort.sort rev_edges ~compare:Edge.compare;
      let mutable found_point = S2_point.origin in
      let mutable found_sign = 0 in
      let mutable i = 0 in
      while i < num_edges && found_sign = 0 do
        let e = edges.(i) in
        let r = rev_edges.(i) in
        let cmp = Edge.compare e r in
        if cmp < 0
        then (
          let #{ v0; v1 = _ } : Edge.t = e in
          let s = contains_sign_at_vertex ~vtest:v0 ~num_edges ~edge in
          if s <> 0
          then (
            found_point <- v0;
            found_sign <- s))
        else if cmp > 0
        then (
          let #{ v0; v1 = _ } : Edge.t = r in
          let s = contains_sign_at_vertex ~vtest:v0 ~num_edges ~edge in
          if s <> 0
          then (
            found_point <- v0;
            found_sign <- s));
        i <- i + 1
      done;
      if found_sign <> 0
      then Reference_point.create ~point:found_point ~contained:(found_sign > 0)
      else (
        (* Every vertex is balanced: the shape is empty or full depending on
           whether any chain has zero edges. *)
        let mutable any_empty_chain = false in
        for i = 0 to num_chains - 1 do
          let #{ start = _; length } : Chain.t = chain i in
          if length = 0 then any_empty_chain <- true
        done;
        Reference_point.contained any_empty_chain)))
;;
