(** [S2_polygon_layer] assembles a snapped {!S2_builder.Graph.t} into an {!S2_polygon.t}.

    The layer discards degenerate edges and sibling edge pairs before assembling loops,
    matching the upstream C++ defaults.

    Usage:
    {[
      let output = S2_polygon_layer.create_output () in
      let builder = S2_builder.create (S2_builder.Options.default ()) in
      S2_builder.start_layer builder (S2_polygon_layer.layer output);
      S2_builder.add_loop builder vertices;
      let err = S2_builder.build builder in
      assert (S2_builder.Error.is_ok err);
      let polygon = S2_polygon_layer.result output
    ]}

    Deferred features (TODO): label tracking, validation, undirected edges. *)

open Core

(** Mutable output slot. [S2_builder] fills this during [build]. *)
type output

(** [create_output ()] returns a fresh empty output. *)
val create_output : unit -> output

(** [result output] returns the assembled polygon after [S2_builder.build] has been
    called. Raises if the layer has not yet produced a polygon. *)
val result : output -> S2_polygon.t

(** [layer output] returns the {!S2_builder.Layer.t} to pass to [S2_builder.start_layer]. *)
val layer : output -> S2_builder.Layer.t
