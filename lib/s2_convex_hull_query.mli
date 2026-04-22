(** [S2_convex_hull_query] builds the convex hull of any collection of points, polylines,
    loops, and polygons. It returns a single convex {!S2_loop.t}.

    The convex hull is defined as the smallest convex region on the sphere that contains
    all of the input geometry. A region is "convex" if for every pair of points inside the
    region, the geodesic between them is also inside the region.

    Containment of input geometry follows the reference library:
    - Each input loop and polygon is contained by the convex hull exactly.
    - Each input point is either contained by the convex hull or is a vertex of it.
    - For each input polyline, the convex hull contains all of its vertices.

    Use the [add_*] functions to feed geometry in, then call {!convex_hull}. The query
    does not reset after {!convex_hull}; more geometry can be added and the hull computed
    again. This implements Andrew's monotone-chain variant of the Graham scan, in
    [O(n log n)] time and [O(n)] space. This type is not thread-safe. *)

open Core

type t

(** [create ()] constructs an empty query. *)
val create : unit -> t

(** [add_point t p] adds the single point [p] to the input geometry. *)
val add_point : t -> S2_point.t -> unit

(** [add_polyline t pl] adds every vertex of [pl] to the input geometry. *)
val add_polyline : t -> S2_polyline.t -> unit

(** [add_loop t l] adds every vertex of [l] to the input geometry. The empty and full
    loops contribute only their bound. *)
val add_loop : t -> S2_loop.t -> unit

(** [add_polygon t p] adds the vertices of every top-level shell of [p] (loops at depth
    [0]). Holes do not contribute to the hull. *)
val add_polygon : t -> S2_polygon.t -> unit

(** [cap_bound t] returns a bounding cap for the input geometry provided so far. Does not
    clear the query. *)
val cap_bound : t -> S2_cap.t

(** [convex_hull t] returns the convex hull of the input geometry as a single
    {!S2_loop.t}.

    - If no geometry has been added, the result is {!S2_loop.empty}.
    - If the geometry spans more than half of the sphere, the result is {!S2_loop.full}.
    - If the geometry contains [1] or [2] points (or a single edge), the result is a tiny
      three-vertex loop that is a superset of the input vertices.

    Does not clear the query. *)
val convex_hull : t -> S2_loop.t
