(** Computes a bounding rectangle that contains all edges defined by a vertex chain
    [v0, v1, v2, ...]. All vertices must be unit length. Note that the bounding rectangle
    of an edge can be larger than the bounding rectangle of its endpoints (e.g. an edge
    passing through a pole).

    The bounds are computed conservatively to account for numerical errors when [S2Point]
    values are converted to [S2LatLng] values. More precisely, if [L] is a closed edge
    chain (loop) such that the interior of [L] does not contain either pole, and [P] is
    any point such that [L] contains [P], then the rect bound of [L] contains
    [S2LatLng.of_point P]. *)

open Core

[@@@zero_alloc all]

(** The bounder state. [create] yields an empty bound; each {!add_point} or {!add_latlng}
    call updates it with the next vertex of the chain. *)
type t :
  (float64 & float64 & float64)
  & (float64 & float64)
  & ((float64 & float64) & (float64 & float64))

(** A fresh bounder with an empty bound. *)
val create : unit -> t

(** [add_point t p] adds the next vertex [p] to the chain and returns the updated bounder.
    [p] must have unit length. Repeated vertices are handled gracefully. *)
val add_point : t -> S2_point.t -> t

(** [add_latlng t ll] adds the next vertex expressed as a latitude/longitude pair and
    returns the updated bounder. Equivalent to converting [ll] to a unit-length [S2_point]
    and calling {!add_point}, but cheaper when starting from a latlng. *)
val add_latlng : t -> S2_latlng.t -> t

(** [get_bound t] returns the bounding rectangle of the vertices accumulated so far. The
    result satisfies the guarantee described at the top of this file. *)
val get_bound : t -> S2_latlng_rect.t

(** [expand_for_subregions bound] expands [bound] so that it is guaranteed to contain the
    bounds of any subregion whose bounds are computed using this module. That is, if [L]
    is a loop that does not contain either pole, and [S] is a loop with [L.contains s],
    then [expand_for_subregions (rect_bound L)] contains [rect_bound S]. *)
val expand_for_subregions : S2_latlng_rect.t -> S2_latlng_rect.t

(** [max_error_for_tests ()] returns the maximum latitude/longitude error in {!get_bound}
    provided that the result does not include either pole. It is intended for tests (e.g.
    as a tolerance passed to {!S2_latlng_rect.approx_equal_latlng}). *)
val max_error_for_tests : unit -> S2_latlng.t
[@@zero_alloc ignore]
