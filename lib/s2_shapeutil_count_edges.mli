(** Edge-counting helpers for {!S2_shape_index.t}.

    Sums {!S2_shape.t} [num_edges] across every shape in the index. The {e up_to} variant
    stops as soon as the running total reaches the requested bound, so it runs in time
    proportional to the number of shapes visited rather than the number of shapes in the
    index.

    Both functions ignore shape identity and geometry: they only read each shape's
    declared [num_edges]. A point shape contributes one "edge" per point (degenerate
    edge), matching the C++ convention. *)

open Core

(** [count_edges idx] returns the total number of edges across every shape in [idx]. Runs
    in time linear in the number of shapes. *)
val count_edges : S2_shape_index.t -> int

(** [count_edges_up_to idx ~max_edges] returns the total number of edges across shapes in
    [idx], stopping as soon as the running sum reaches [max_edges]. The bound is checked
    {e after} each shape is added, so a non-empty index always contributes at least its
    first shape's edge count - even when [max_edges <= 0]. On an empty index the return
    value is [0]. *)
val count_edges_up_to : S2_shape_index.t -> max_edges:int -> int
