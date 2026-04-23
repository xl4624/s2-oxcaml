(** Shape and size constants for S2 cells.

    S2 cells are arranged in a hierarchy of 30 levels: face cells at level 0 cover the
    sphere, and every cell at level [k] splits into four children at level [k+1]. This
    module exposes derivative constants (e.g. minimum cell width, maximum cell area) that
    callers typically use to pick a cell level satisfying a condition - for example, "the
    smallest level where every cell diagonal is at most 0.1 radians".

    Each metric is represented as a {!t} carrying a dimension ([1] for lengths, [2] for
    areas) and a derivative. Use {!get_value} to evaluate the metric at a particular
    level, and {!get_level_for_max_value} / {!get_level_for_min_value} /
    {!get_closest_level} to invert.

    The minimum and maximum bounds are valid at every level but may be conservative for
    very large cells (face cells in particular).

    Limitations:
    - The constants assume the quadratic cell projection. The S2 library supports only
      this projection at runtime, so there is no way to select another. *)

open Core

[@@@zero_alloc all]

(** A cell metric. [dim] is 1 for length metrics and 2 for area metrics. [deriv] is a
    derivative that must be multiplied by a length or area in (s,t)-space to get a useful
    value. *)
type t =
  #{ dim : int
   ; deriv : Float_u.t
   }

(** [dim m] returns the dimension of [m] (1 for length, 2 for area). *)
val dim : t -> int

(** [deriv m] returns the derivative of [m]. *)
val deriv : t -> float#

(** [get_value m level] returns the value of the metric for cells at the given level. The
    value is either a length or an area on the unit sphere, depending on the particular
    metric. *)
val get_value : t -> int -> float#

(** [get_level_for_max_value m value] returns the minimum level such that the metric is at
    most [value], or {!max_cell_level} if there is no such level. Requires that [value] is
    a number (not NaN). The return value is always a valid level.

    For example, [get_level_for_max_value max_diag 0.1] returns the minimum level such
    that all cell diagonal lengths are 0.1 or smaller. *)
val get_level_for_max_value : t -> float# -> int

(** [get_level_for_min_value m value] returns the maximum level such that the metric is at
    least [value], or 0 if there is no such level. Requires that [value] is a number (not
    NaN). The return value is always a valid level.

    For example, [get_level_for_min_value min_width 0.1] returns the maximum level such
    that all cells have a minimum width of 0.1 or larger. *)
val get_level_for_min_value : t -> float# -> int

(** [get_closest_level m value] returns the level at which the metric has approximately
    the given value. The return value is always a valid level.

    For example, [get_closest_level avg_edge 0.1] returns the level at which the average
    cell edge length is approximately 0.1. *)
val get_closest_level : t -> float# -> int

(** The maximum valid cell level (30). *)
val max_cell_level : int

(** {1 Angle span metrics}

    Each cell is bounded by four planes passing through its four edges and the center of
    the sphere. These metrics relate to the angle between each pair of opposite bounding
    planes. *)

val min_angle_span : t
val max_angle_span : t
val avg_angle_span : t

(** {1 Width metrics}

    The width of a cell is defined here as the perpendicular distance between a pair of
    opposite edges. A cell has two widths, one in each direction. *)

val min_width : t
val max_width : t
val avg_width : t

(** {1 Edge length metrics}

    Bound the minimum, maximum, or average edge length of a cell at a given level. *)

val min_edge : t
val max_edge : t
val avg_edge : t

(** The maximum edge aspect ratio over all cells at any level, where the edge aspect ratio
    of a cell is defined as the ratio of its longest edge length to its shortest edge
    length. *)
val max_edge_aspect : float#

(** {1 Diagonal length metrics}

    The maximum diagonal is also the maximum diameter of any cell, and also the maximum
    geometric width. *)

val min_diag : t
val max_diag : t
val avg_diag : t

(** The maximum diagonal aspect ratio over all cells at any level, where the diagonal
    aspect ratio of a cell is defined as the ratio of its longest diagonal length to its
    shortest diagonal length. *)
val max_diag_aspect : float#

(** {1 Area metrics}

    The minimum, maximum, and (exact) average area of cells at a given level. *)

val min_area : t
val max_area : t
val avg_area : t
