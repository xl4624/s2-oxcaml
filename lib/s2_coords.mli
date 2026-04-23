(** Constants and conversions for the S2 cell decomposition coordinate systems.

    S2 tiles the unit sphere by projecting the six faces of a cube onto it, then
    recursively subdividing each face into four children. Converting between a point on
    the sphere and a 64-bit cell id involves several intermediate coordinate systems:

    - [(face, i, j)]: leaf-cell integer coordinates. [i] and [j] are in [\[0, 2^30)] and
      identify a particular leaf cell on the given face.
    - [(face, s, t)]: cell-space coordinates. [s] and [t] are reals in [[0, 1]]; the
      center of a face corresponds to [(0.5, 0.5)].
    - [(face, si, ti)]: discrete cell-space coordinates obtained by multiplying [s] and
      [t] by [2^31] and rounding. Values lie in [[0, 2^31]] and encode both cell edges and
      centers exactly.
    - [(face, u, v)]: cube-space coordinates in [[-1, 1]]. This is the cell-space position
      after a nonlinear transform designed to make the sphere cells more uniform in area.
      The quadratic projection is used throughout (area ratio about 2.1, worst-case edge
      ratio about 1.8).
    - [(face, u, v, w)]: per-face right-handed 3D frame with [w] along the face normal.
    - [(x, y, z)]: direction vectors, not necessarily unit length.

    The face indexing ensures that the Hilbert curve joins continuously between adjacent
    faces, so sequentially increasing cell ids trace a continuous space-filling curve over
    the entire sphere. *)

[@@@zero_alloc all]

(** Maximum absolute error in a [(u, v)] coordinate when projecting from XYZ. The
    division-per-coordinate projection is bounded by [0.5 *. DBL_EPSILON] for values with
    magnitude less than two. *)
val max_xyz_to_uv_error : float#

(** Number of subdivision levels needed to specify a leaf cell ([= 30]). *)
val max_cell_level : int

(** Upper bound on leaf [i]- or [j]-indices: valid values are in [[0, limit_ij - 1]]. *)
val limit_ij : int

(** Upper bound on [si]- or [ti]-values: valid values are in [[0, max_si_ti]] (note the
    inclusive upper bound - the extra position encodes the far edge of a face). *)
val max_si_ti : int

(** [st_to_uv s] projects a cell-space coordinate [s] to cube-space [u] using the
    quadratic projection. *)
val st_to_uv : float# -> float#

(** [uv_to_st u] is the inverse of {!st_to_uv}. Because the two projections are not
    implemented as exact inverses in IEEE arithmetic, [uv_to_st (st_to_uv x) = x] may fail
    by a few ULPs. *)
val uv_to_st : float# -> float#

(** [ij_to_st_min i] returns the [s]- or [t]-coordinate of the low edge of leaf cell
    column [i]. [i] must lie in [[0, limit_ij]] (one beyond the normal range is allowed,
    which gives [s = 1.0]). *)
val ij_to_st_min : int -> float#

(** [st_to_ij s] returns the [i]- or [j]-index of the leaf cell that contains [s]. Results
    outside [[0, limit_ij - 1]] are clamped into range, so negative and non-finite
    non-positive inputs map to 0 and inputs at or beyond 1 map to [limit_ij - 1]. *)
val st_to_ij : float# -> int

(** [si_ti_to_st si] converts a discrete cell-space coordinate to its continuous
    equivalent. *)
val si_ti_to_st : int -> float#

(** [st_to_si_ti s] returns the [si]- or [ti]-value nearest to [s] using half-to-even
    rounding. The result may fall outside [[0, max_si_ti]] when [s] does. *)
val st_to_si_ti : float# -> int

(** [face_uv_to_xyz face u v] converts cube-face coordinates to a direction vector on the
    biunit cube. The result is not unit length; normalize it to land on the sphere. *)
val face_uv_to_xyz : int -> float# -> float# -> R3_vector.t

(** [get_face p] returns the face ([0..5]) containing the given direction vector. For
    points on the boundary between two faces, the result is arbitrary but deterministic
    (whichever face [R3_vector.largest_abs_component] picks). *)
val get_face : R3_vector.t -> int

(** [valid_face_xyz_to_uv face p] projects [p] onto [face]. Requires that
    [R3_vector.dot p (get_norm face) > 0]. The returned [(u, v)] may lie outside
    [[-1, 1]]. The behavior is undefined if the face is not valid for [p]; use
    {!face_xyz_to_uv} when the face may be wrong. *)
val valid_face_xyz_to_uv : int -> R3_vector.t -> R2_point.t

(** [face_xyz_to_uv face p] is {!valid_face_xyz_to_uv} wrapped in an option: returns
    [R2_point.Option.none] when [p] lies in the opposite hemisphere from [face]. *)
val face_xyz_to_uv : int -> R3_vector.t -> R2_point.Option.t

(** [face_xyz_to_uv_exn face p] is {!face_xyz_to_uv} but raises when the face is invalid
    for the given point. *)
val face_xyz_to_uv_exn : int -> R3_vector.t -> R2_point.t

(** [face_xyz_to_uvw face p] rotates [p] into the right-handed [(u, v, w)] frame of
    [face], where [w] is the face normal. The result has the same magnitude as [p]. *)
val face_xyz_to_uvw : int -> R3_vector.t -> R3_vector.t

(** [face_si_ti_to_xyz face si ti] converts discrete cell-space coordinates to a direction
    vector by composing {!si_ti_to_st}, {!st_to_uv}, and {!face_uv_to_xyz}. The result is
    not unit length. *)
val face_si_ti_to_xyz : int -> int -> int -> R3_vector.t

(** [xyz_to_face_si_ti p] returns [(face, si, ti, level)] where [(face, si, ti)] are the
    discrete cell coordinates nearest to [p]. If [p] is exactly the center of a cell,
    [level] is the level of that cell; otherwise [level] is [-1]. *)
val xyz_to_face_si_ti : R3_vector.t -> #(int * int * int * int)

(** [get_u_norm face u] returns the outward normal of the plane through the origin that
    contains the edge with constant [u] on [face]. Not normalized. *)
val get_u_norm : int -> float# -> R3_vector.t

(** [get_v_norm face v] returns the outward normal of the plane through the origin that
    contains the edge with constant [v] on [face]. Not normalized. *)
val get_v_norm : int -> float# -> R3_vector.t

(** [get_norm face] returns the unit-length outward normal of [face]. *)
val get_norm : int -> R3_vector.t

(** [get_u_axis face] returns the unit-length [u]-axis of [face] in XYZ coordinates. *)
val get_u_axis : int -> R3_vector.t

(** [get_v_axis face] returns the unit-length [v]-axis of [face] in XYZ coordinates. *)
val get_v_axis : int -> R3_vector.t

(** [get_uvw_axis face axis] returns the unit-length axis [0=u], [1=v], or [2=w] of [face]
    in XYZ coordinates. *)
val get_uvw_axis : int -> int -> R3_vector.t

(** [get_uvw_face face axis direction] returns the neighbouring face adjacent to [face]
    along the given axis direction. [axis] is [0=u], [1=v], [2=w]; [direction] is
    [0=negative], [1=positive]. For example [get_uvw_face 4 0 1] is the face adjacent to
    face 4 in the positive-[u] direction. *)
val get_uvw_face : int -> int -> int -> int
