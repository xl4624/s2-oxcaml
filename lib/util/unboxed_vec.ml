open Core

(* Kind-polymorphic growable array via ppx_template. Instantiated below for the non-value
   kinds we use in the S2 port:

   - R2_point.t : (float64 & float64) mod external_
   - R3_vector.t / S2_point.t : (float64 & float64 & float64) mod external_

   High-level operations like [Array.iter] and [Array.blit] are not yet kind-polymorphic
   over unboxed products, so we implement push / copy / to_array with manual [for] loops
   backed by layout-polymorphic [Array.get] / [Array.set]. *)

module%template
  [@kind
    k = ((float64 & float64) mod external_, (float64 & float64 & float64) mod external_)] Make (E : sig
    type t : k

    val default : t
  end) =
struct
  type t =
    { mutable arr : E.t array
    ; mutable len : int
    }

  let create ?(initial_capacity = 8) () =
    { arr = Array.create ~len:initial_capacity E.default; len = 0 }
  ;;

  let[@inline] length b = b.len
  let[@inline] get b i = b.arr.(i)
  let[@inline] set b i v = b.arr.(i) <- v
  let[@inline] last b = b.arr.(b.len - 1)

  let grow b =
    let cap = Array.length b.arr in
    let new_cap = Int.max 8 (cap * 2) in
    let new_arr = Array.create ~len:new_cap E.default in
    for i = 0 to b.len - 1 do
      new_arr.(i) <- b.arr.(i)
    done;
    b.arr <- new_arr
  ;;

  let[@inline] push b v =
    if b.len >= Array.length b.arr then grow b;
    b.arr.(b.len) <- v;
    b.len <- b.len + 1
  ;;

  let to_array b =
    let out = Array.create ~len:b.len E.default in
    for i = 0 to b.len - 1 do
      out.(i) <- b.arr.(i)
    done;
    out
  ;;
end
