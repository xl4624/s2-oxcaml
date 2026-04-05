(** S2_coords contains constants and utility functions for the S2 cell decomposition
    coordinate systems.

    These functions define the cube-to-sphere projection used by the S2 cell
    decomposition. In the process of converting a latitude-longitude pair to a 64-bit cell
    id, several coordinate systems are used: (face, i, j), (face, s, t), (face, si, ti),
    (face, u, v), and (x, y, z) direction vectors. *)

[@@@zero_alloc all]

(** The maximum absolute error in U/V coordinates when converting from XYZ. *)
val max_xyz_to_uv_error : float#

(** The maximum subdivision level (number of levels needed to specify a leaf cell). *)
val max_cell_level : int

(** The maximum index of a valid leaf cell plus one. The range of valid leaf cell indices
    is [0..limit_ij - 1]. *)
val limit_ij : int

(** The maximum value of an si- or ti-coordinate. The range of valid (si, ti) values is
    [0..max_si_ti]. *)
val max_si_ti : int

(** [st_to_uv s] converts an s- or t-value to the corresponding u- or v-value. Uses the
    quadratic projection. *)
val st_to_uv : float# -> float#

(** [uv_to_st u] is the inverse of [st_to_uv]. Note that [uv_to_st (st_to_uv x)] may not
    equal [x] exactly due to numerical errors. *)
val uv_to_st : float# -> float#

(** [ij_to_st_min i] converts a leaf cell index to the minimum corresponding s- or
    t-value. The argument must be in the range [0..2**30]. *)
val ij_to_st_min : int -> float#

(** [st_to_ij s] returns the leaf cell index containing the given s- or t-value. Results
    are clamped to the range of valid leaf cell indices. *)
val st_to_ij : float# -> int

(** [si_ti_to_st si] converts an si- or ti-value to the corresponding s- or t-value. *)
val si_ti_to_st : int -> float#

(** [st_to_si_ti s] returns the si- or ti-coordinate nearest to the given s- or t-value.
    The result may be outside the range of valid (si, ti)-values. *)
val st_to_si_ti : float# -> int

(** [face_uv_to_xyz face u v] converts (face, u, v) coordinates to a direction vector (not
    necessarily unit length). *)
val face_uv_to_xyz : int -> float# -> float# -> R3_vector.t

(** [get_face p] returns the face containing the given direction vector. For points on the
    boundary between faces, the result is arbitrary but deterministic. *)
val get_face : R3_vector.t -> int

(** [valid_face_xyz_to_uv face p] converts a direction vector to (u, v) on the given face.
    Assumes the face is valid for the point (i.e., the dot product of p with the face
    normal is positive). *)
val valid_face_xyz_to_uv : int -> R3_vector.t -> R2_point.t

(** [face_xyz_to_uv face p] returns the (u, v) on the given face if the dot product of [p]
    with the given face normal is positive, [R2_point.Option.none] otherwise. *)
val face_xyz_to_uv : int -> R3_vector.t -> R2_point.Option.t

(** [face_xyz_to_uv_exn face p] is like [face_xyz_to_uv] but raises if the face is invalid
    for the given point. *)
val face_xyz_to_uv_exn : int -> R3_vector.t -> R2_point.t

(** [face_xyz_to_uvw face p] transforms the given point [p] to the (u, v, w) coordinate
    frame of the given face where the w-axis represents the face normal. *)
val face_xyz_to_uvw : int -> R3_vector.t -> R3_vector.t

(** [face_si_ti_to_xyz face si ti] converts (face, si, ti) coordinates to a direction
    vector (not necessarily unit length). *)
val face_si_ti_to_xyz : int -> int -> int -> R3_vector.t

(** [xyz_to_face_si_ti p] converts a direction vector to (face, si, ti) coordinates and
    the level if [p] is exactly equal to the center of a cell (-1 otherwise). Returns
    [(face, si, ti, level)]. *)
val xyz_to_face_si_ti : R3_vector.t -> #(int * int * int * int)

(** [get_u_norm face u] returns the right-handed normal (not necessarily unit length) for
    an edge in the direction of the positive v-axis at the given u-value on the given
    face. *)
val get_u_norm : int -> float# -> R3_vector.t

(** [get_v_norm face v] returns the right-handed normal (not necessarily unit length) for
    an edge in the direction of the positive u-axis at the given v-value on the given
    face. *)
val get_v_norm : int -> float# -> R3_vector.t

(** [get_norm face] returns the unit-length normal for the given face. *)
val get_norm : int -> R3_vector.t

(** [get_u_axis face] returns the u-axis for the given face. *)
val get_u_axis : int -> R3_vector.t

(** [get_v_axis face] returns the v-axis for the given face. *)
val get_v_axis : int -> R3_vector.t

(** [get_uvw_axis face axis] returns the unit-length axis for the given face. axis: 0=u,
    1=v, 2=w (face normal). *)
val get_uvw_axis : int -> int -> R3_vector.t

(** [get_uvw_face face axis direction] returns the face in the given direction
    (0=negative, 1=positive) of the given axis (0=u, 1=v, 2=w) with respect to the (u,
    v, w) coordinate system of the given face. *)
val get_uvw_face : int -> int -> int -> int
