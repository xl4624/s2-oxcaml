(** A two-dimensional region on the unit sphere.

    The purpose of this interface is to allow complex regions to be approximated as
    simpler regions. The interface is restricted to methods that are useful for computing
    approximations.

    An OCaml [S2_region.t] is a variant whose constructors wrap each concrete S2 type.

    User-defined regions (polygons, polylines, custom shapes) go through {!custom}, which
    takes a {!methods} record of callbacks. *)

open Core

(** Callbacks used by {!custom} to implement a user-defined region. The fields mirror the
    methods exposed at module level. *)
type methods =
  #{ cap_bound : unit -> S2_cap.t
   ; rect_bound : unit -> S2_latlng_rect.t
   ; contains_cell : S2_cell.t -> bool
   ; intersects_cell : S2_cell.t -> bool
   ; contains_point : S2_point.t -> bool
   ; cell_union_bound : unit -> S2_cell_id.t array
   }

type t =
  | Cap of S2_cap.t
  | Rect of S2_latlng_rect.t
  | Cell of S2_cell.t
  | Cell_union of S2_cell_union.t
  | Custom of methods

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

(** [custom m] wraps a user-supplied methods record. Use this for regions that are not
    natively covered by the built-in constructors. *)
val custom : methods -> t

(** {1 Region methods}

    Each accessor dispatches on the variant tag. Pattern-matching [t] directly is also
    fine, and can let the compiler eliminate the dispatch at known-tag call sites. *)

(** [cap_bound t] returns a bounding {!S2_cap.t} that contains [t]. *)
val cap_bound : t -> S2_cap.t

(** [rect_bound t] returns a bounding {!S2_latlng_rect.t} that contains [t]. *)
val rect_bound : t -> S2_latlng_rect.t

(** [contains_cell t cell] returns [true] if [cell] is fully contained in [t]. *)
val contains_cell : t -> S2_cell.t -> bool

(** [intersects_cell t cell] returns [true] if [cell] intersects [t]. *)
val intersects_cell : t -> S2_cell.t -> bool

(** [contains_point t p] returns [true] if the unit-length point [p] lies in [t]. *)
val contains_point : t -> S2_point.t -> bool

(** [cell_union_bound t] returns a small cell-union cover of [t]. *)
val cell_union_bound : t -> S2_cell_id.t array
