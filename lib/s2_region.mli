(** A two-dimensional region on the unit sphere.

    The purpose of this interface is to allow complex regions to be approximated as
    simpler regions. The interface is restricted to methods that are useful for computing
    approximations.

    Unlike the corresponding C++ {{!S2Region} virtual class}, an OCaml [S2_region.t] is a
    record-of-functions captured at construction time. The constructors {!of_cap},
    {!of_rect}, {!of_cell}, and {!of_cell_union} build a region from each concrete S2
    type. *)

open Core

type t =
  #{ cap_bound : unit -> S2_cap.t
   ; rect_bound : unit -> S2_latlng_rect.t
   ; contains_cell : S2_cell.t -> bool
   ; intersects_cell : S2_cell.t -> bool
   ; contains_point : S2_point.t -> bool
   ; cell_union_bound : unit -> Int64.t list
   }

val sexp_of_t : t -> Sexp.t [@@zero_alloc ignore]

(** {1 Constructors} *)

(** [of_cap c] wraps a cap as a region. *)
val of_cap : S2_cap.t -> t

(** [of_rect r] wraps a latitude-longitude rectangle as a region. *)
val of_rect : S2_latlng_rect.t -> t

(** [of_cell c] wraps a cell as a region. *)
val of_cell : S2_cell.t -> t

(** [of_cell_union u] wraps a cell union as a region. *)
val of_cell_union : S2_cell_union.t -> t
