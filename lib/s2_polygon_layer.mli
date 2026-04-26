(** An {!S2_builder.Layer.t} that assembles the snapped edges in an {!S2_builder.Graph.t}
    into an {!S2_polygon.t}.

    The layer requests the following graph processing from {!S2_builder} before it sees
    the graph:
    - Directed edges only (input edges must be oriented with the polygon interior on their
      left).
    - Degenerate edges (endpoints snap to the same vertex) are discarded.
    - Duplicate edges are kept.
    - Sibling edge pairs (zero-area degenerate regions) are discarded.

    If the resulting graph has no edges, the builder's full-polygon predicate decides
    whether the output is the {!S2_polygon.empty} or {!S2_polygon.full} polygon.

    Typical usage:
    {[
      let output = S2_polygon_layer.create_output () in
      let builder = S2_builder.create (S2_builder.Options.default ()) in
      S2_builder.start_layer builder (S2_polygon_layer.layer output);
      S2_builder.add_loop builder vertices;
      let err = S2_builder.build builder in
      assert (S2_builder.Error.is_ok err);
      let polygon = S2_polygon_layer.result output
    ]}

    {1 Limitations}

    Not yet supported:
    - Undirected input edges (the layer always uses directed edges).
    - Per-edge label tracking ([LabelSetIds] / [IdSetLexicon]). *)

open Core

(** Mutable output slot. {!S2_builder.build} populates this through the layer callback. *)
type output

(** [create_output ()] returns a fresh output with no polygon attached. *)
val create_output : unit -> output

(** [result output] returns the assembled polygon. Raises if the builder has not yet
    filled [output] (that is, if [S2_builder.build] has not been called, or if the build
    returned an error before reaching the layer). *)
val result : output -> S2_polygon.t

(** [layer ?validate output] returns the {!S2_builder.Layer.t} value to pass to
    {!S2_builder.start_layer}. The layer records its polygon into [output] when the
    builder calls it.

    If [validate] is [true] (default [false]), the layer runs
    {!S2_polygon.find_validation_error} on the assembled polygon and, if it reports an
    error, surfaces it through the builder with code ["INVALID_POLYGON"]. The polygon is
    still stored in [output] either way, so callers can inspect a partially valid result. *)
val layer : ?validate:bool -> output -> S2_builder.Layer.t
