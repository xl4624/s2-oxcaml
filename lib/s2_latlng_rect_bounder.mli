(** Incremental bounder that computes a conservative latitude-longitude rectangle
    containing every geodesic edge in a vertex chain [v0, v1, v2, ...].

    Bounding an edge is not the same as bounding its endpoints: a great-circle edge
    passing near a pole reaches higher (or lower) latitudes than either endpoint, so this
    module tracks both endpoints and the interior maximum/minimum of each edge. The result
    accounts for the floating-point error introduced when converting [S2Point] values to
    [S2LatLng], and satisfies the following guarantee: if [L] is a closed edge chain
    (loop) whose interior contains neither pole, and [P] is any point contained by [L],
    then [S2_latlng.of_point P] lies inside [get_bound (bounder-over L)].

    Typical use: allocate with {!create}, feed vertices via {!add_point} or {!add_latlng},
    then read {!get_bound}. The type is a small unboxed record updated functionally, so
    "feeding a vertex" returns a new value. *)

open Core

[@@@zero_alloc all]

(** Bounder state: the last vertex added (as an {!S2_point.t} and as an {!S2_latlng.t}),
    together with the accumulated rectangle bound. *)
type t :
  (float64 & float64 & float64)
  & (float64 & float64)
  & ((float64 & float64) & (float64 & float64))

(** [create ()] returns a fresh bounder whose accumulated bound is empty. *)
val create : unit -> t

(** [add_point t p] extends the chain with [p] and returns the updated bounder. [p] must
    have unit length. Repeated vertices are handled gracefully: adding the same vertex
    twice in a row has the same effect as adding it once. *)
val add_point : t -> S2_point.t -> t

(** [add_latlng t ll] is equivalent to [add_point t (S2_latlng.to_point ll)] but avoids a
    round-trip when the caller already has the latitude-longitude form. *)
val add_latlng : t -> S2_latlng.t -> t

(** [get_bound t] returns the current bounding rectangle, expanded by the per-conversion
    error budget so that the containment guarantee described in the module header holds. *)
val get_bound : t -> S2_latlng_rect.t

(** [expand_for_subregions bound] expands [bound] so that it is guaranteed to contain the
    bounds of every subregion computed by this bounder. Concretely: if [L] is a loop that
    does not contain either pole and [S] is a loop with [L] containing [S], then
    [expand_for_subregions (rect_bound L)] contains [rect_bound S]. Use this to make a
    parent-loop bound safe for comparison against child-loop bounds. *)
val expand_for_subregions : S2_latlng_rect.t -> S2_latlng_rect.t

(** [max_error_for_tests ()] returns a per-coordinate error budget suitable as [max_error]
    for {!S2_latlng_rect.approx_equal_latlng}. The bound is valid when the result does not
    include either pole. Intended for test tolerances only. *)
val max_error_for_tests : unit -> S2_latlng.t
[@@zero_alloc ignore]
