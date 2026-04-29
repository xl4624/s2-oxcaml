(** Builds the spherical convex hull of a collection of points, polylines, loops, and
    polygons. The hull is returned as a single convex {!S2_loop.t}.

    The convex hull is the smallest convex region on the sphere that contains all of the
    input geometry. A region is "convex" if for every pair of points inside it, the
    geodesic between them is also inside it.

    Input containment rules:
    - Each input loop or polygon is contained by the hull in the {!S2_polygon.contains}
      sense.
    - Each input point either lies in the hull's interior or is a vertex of the hull.
      ({!S2_loop.t} does not necessarily contain its own vertices, so "vertex" and
      "contained point" are distinct cases.)
    - For each input polyline, the hull contains all of its vertices under the rule above;
      convexity then guarantees the polyline edges are contained as well.

    Typical usage:

    {[
      let q = S2_convex_hull_query.create () in
      List.iter points ~f:(S2_convex_hull_query.add_point q);
      let hull = S2_convex_hull_query.convex_hull q in
      ...
    ]}

    {!convex_hull} does not reset the query; more geometry may be added and another hull
    computed. Algorithm: Andrew's monotone-chain variant of the Graham scan, [O(n log n)]
    time and [O(n)] space, where [n] is the total number of vertices added. Not
    thread-safe: every method is mutating. *)

type t

(** [create ()] constructs an empty query. *)
val create : unit -> t

(** [add_point t p] adds the unit-length point [p] to the input geometry. *)
val add_point : t -> S2_point.t -> unit

(** [add_polyline t pl] adds every vertex of [pl] to the input geometry. The polyline's
    edges are not added explicitly - convexity of the final hull is what guarantees they
    are contained. *)
val add_polyline : t -> S2_polyline.t -> unit

(** [add_loop t l] adds every vertex of [l] to the input geometry. The empty and full
    loops do not contribute any vertices but still update the bounding rectangle. *)
val add_loop : t -> S2_loop.t -> unit

(** [add_polygon t p] adds the vertices of every top-level shell of [p] (loops at depth
    [0]). Holes are skipped because they lie strictly inside their enclosing shell and
    therefore cannot contribute to the hull. *)
val add_polygon : t -> S2_polygon.t -> unit

(** [cap_bound t] returns a bounding {!S2_cap.t} for the input geometry added so far. Does
    not clear the query. *)
val cap_bound : t -> S2_cap.t

(** [convex_hull t] returns the convex hull of the input geometry added so far.

    - If no geometry has been added, returns {!S2_loop.empty}.
    - If the geometry spans more than a hemisphere, returns {!S2_loop.full}.
    - If fewer than [3] distinct input points remain after deduplication, returns a tiny
      three-vertex loop that is a superset of those points (or the full loop, for
      antipodal endpoints).

    Does not clear the query. *)
val convex_hull : t -> S2_loop.t
