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
