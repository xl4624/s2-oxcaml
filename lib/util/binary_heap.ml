open Core

module Make (Elt : sig
    type t

    val higher_priority : t -> t -> bool
  end) =
struct
  type t =
    { mutable data : Elt.t array
    ; mutable len : int
    }

  let create () = { data = [||]; len = 0 }
  let[@inline] is_empty h = h.len = 0
  let[@inline] length h = h.len

  let grow h fill =
    let cap = Array.length h.data in
    let new_cap = if cap = 0 then 16 else cap * 2 in
    let new_arr = Array.create ~len:new_cap fill in
    if cap > 0 then Array.blit ~src:h.data ~src_pos:0 ~dst:new_arr ~dst_pos:0 ~len:cap;
    h.data <- new_arr
  ;;

  let rec sift_up h i =
    if i > 0
    then (
      let parent = (i - 1) / 2 in
      if Elt.higher_priority h.data.(i) h.data.(parent)
      then (
        let tmp = h.data.(i) in
        h.data.(i) <- h.data.(parent);
        h.data.(parent) <- tmp;
        sift_up h parent))
  ;;

  let rec sift_down h i =
    let left = (2 * i) + 1 in
    let right = (2 * i) + 2 in
    let mutable best = i in
    if left < h.len && Elt.higher_priority h.data.(left) h.data.(best) then best <- left;
    if right < h.len && Elt.higher_priority h.data.(right) h.data.(best)
    then best <- right;
    if best <> i
    then (
      let tmp = h.data.(i) in
      h.data.(i) <- h.data.(best);
      h.data.(best) <- tmp;
      sift_down h best)
  ;;

  let add h x =
    if h.len >= Array.length h.data then grow h x;
    let i = h.len in
    h.data.(i) <- x;
    h.len <- i + 1;
    sift_up h i
  ;;

  let pop_exn h =
    if h.len = 0 then failwith "Binary_heap.pop_exn: empty heap";
    let top = h.data.(0) in
    let new_len = h.len - 1 in
    h.len <- new_len;
    if new_len > 0
    then (
      h.data.(0) <- h.data.(new_len);
      sift_down h 0);
    top
  ;;
end
