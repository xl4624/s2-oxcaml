open Core

(* The ppx_template variant covers the two simple product kinds. The sort body only uses
   the layout-polymorphic [Array.length] / [Array.get] / [Array.set] primitives, so it
   specializes correctly for each instance.

   The hybrid algorithm: insertion sort for small n, iterative heap sort otherwise. Both
   are in-place and allocation-free. *)

module%template
  [@kind
    k = ((float64 & float64) mod external_, (float64 & float64 & float64) mod external_)] Make (E : sig
    type t : k
  end) =
struct
  let insertion_threshold = 16

  let insertion_sort (arr : E.t array) ~(compare : E.t -> E.t -> int) =
    let n = Array.length arr in
    for i = 1 to n - 1 do
      let x = arr.(i) in
      let mutable j = i in
      while j > 0 && compare arr.(j - 1) x > 0 do
        arr.(j) <- arr.(j - 1);
        j <- j - 1
      done;
      arr.(j) <- x
    done
  ;;

  (* Sift [root] down the max-heap rooted at index 0 and of size [heap_size]. *)
  let sift_down (arr : E.t array) ~(compare : E.t -> E.t -> int) ~root ~heap_size =
    let mutable node = root in
    let mutable keep_going = true in
    while keep_going do
      let left = (2 * node) + 1 in
      if left >= heap_size
      then keep_going <- false
      else (
        let right = left + 1 in
        let largest =
          if right < heap_size && compare arr.(right) arr.(left) > 0 then right else left
        in
        if compare arr.(largest) arr.(node) <= 0
        then keep_going <- false
        else (
          let tmp = arr.(node) in
          arr.(node) <- arr.(largest);
          arr.(largest) <- tmp;
          node <- largest))
    done
  ;;

  let heap_sort (arr : E.t array) ~(compare : E.t -> E.t -> int) =
    let n = Array.length arr in
    for i = (n / 2) - 1 downto 0 do
      sift_down arr ~compare ~root:i ~heap_size:n
    done;
    for last = n - 1 downto 1 do
      let tmp = arr.(0) in
      arr.(0) <- arr.(last);
      arr.(last) <- tmp;
      sift_down arr ~compare ~root:0 ~heap_size:last
    done
  ;;

  let sort (arr : E.t array) ~(compare : E.t -> E.t -> int) =
    let n = Array.length arr in
    if n < 2
    then ()
    else if n < insertion_threshold
    then insertion_sort arr ~compare
    else heap_sort arr ~compare
  ;;
end

(* Manual instantiation for the nested kind
   [((float64 & float64 & float64) & (float64 & float64 & float64)) mod external_].
   ppx_template cannot represent this because it deliberately flattens adjacent [&]
   products (see the comment in its [ast_pattern_helpers] parser). The body is textually
   identical to the ppx_template variant above. *)
module Make_3_3 (E : sig
    type t : ((float64 & float64 & float64) & (float64 & float64 & float64)) mod external_
  end) =
struct
  let insertion_threshold = 16

  let insertion_sort (arr : E.t array) ~(compare : E.t -> E.t -> int) =
    let n = Array.length arr in
    for i = 1 to n - 1 do
      let x = arr.(i) in
      let mutable j = i in
      while j > 0 && compare arr.(j - 1) x > 0 do
        arr.(j) <- arr.(j - 1);
        j <- j - 1
      done;
      arr.(j) <- x
    done
  ;;

  let sift_down (arr : E.t array) ~(compare : E.t -> E.t -> int) ~root ~heap_size =
    let mutable node = root in
    let mutable keep_going = true in
    while keep_going do
      let left = (2 * node) + 1 in
      if left >= heap_size
      then keep_going <- false
      else (
        let right = left + 1 in
        let largest =
          if right < heap_size && compare arr.(right) arr.(left) > 0 then right else left
        in
        if compare arr.(largest) arr.(node) <= 0
        then keep_going <- false
        else (
          let tmp = arr.(node) in
          arr.(node) <- arr.(largest);
          arr.(largest) <- tmp;
          node <- largest))
    done
  ;;

  let heap_sort (arr : E.t array) ~(compare : E.t -> E.t -> int) =
    let n = Array.length arr in
    for i = (n / 2) - 1 downto 0 do
      sift_down arr ~compare ~root:i ~heap_size:n
    done;
    for last = n - 1 downto 1 do
      let tmp = arr.(0) in
      arr.(0) <- arr.(last);
      arr.(last) <- tmp;
      sift_down arr ~compare ~root:0 ~heap_size:last
    done
  ;;

  let sort (arr : E.t array) ~(compare : E.t -> E.t -> int) =
    let n = Array.length arr in
    if n < 2
    then ()
    else if n < insertion_threshold
    then insertion_sort arr ~compare
    else heap_sort arr ~compare
  ;;
end
