# S2 Port - Module Dependency Graph & TODO

Each entry lists the module, its OCaml name, reference files, and dependencies.
The orchestrator should launch all modules in a tier once their deps are done.

**Legend:** `[x]` = merged to main, `[ ]` = not started

Completed tiers (0-5, critical path, and most of tier 6) are omitted. Remaining
work is listed below.

---

## Tier 6 - Shapes and geometry types

- [x] **s2_loop** - `S2_loop` (fully index-driven: `contains_point`,
      `contains_cell`, `may_intersect_cell`, `contains`, `intersects` all
      use a shared lazily-built shape index plus `S2_contains_point_query`
      / `S2_crossing_edge_query`)

- [x] **s2_polygon** - `S2_polygon` (no encode/decode, no boolean ops, no
      snap/simplify, no polyline intersect; Contains / Intersects use the
      Go-style boundary algorithm and disagree with C++
      `S2BooleanOperation` on shared-boundary edges, so multi-loop
      polygons are excluded from the polygon-polygon relations fixture)
  - Go: `s2/polygon.go` | C++: `s2polygon.h`, `s2polygon.cc`
  - Deps: `s2_loop`, `s2_cell_union`, `s2_latlng_rect`, `s2_cap`

## Tier 7 - Spatial index and queries

- [x] **s2_shape_index** - `S2_shape_index` (mutable add + full rebuild; incremental absorb /
      shape removal not ported; no encode/decode)
  - Go: `s2/shapeindex.go` | C++: `s2shape_index.h`, `mutable_s2shape_index.h`
  - Deps: `s2_shape`, `s2_cell_id`, `s2_point`

- [x] **s2_contains_point_query** - `S2_contains_point_query` (all three
      vertex models - `Open`, `Semi_open`, `Closed` - are now implemented
      for 2D shapes)
  - Go: `s2/contains_point_query.go` | C++: `s2contains_point_query.h`
  - Deps: `s2_shape_index`, `s2_edge_crosser`

- [x] **s2_crossing_edge_query** - `S2_crossing_edge_query`
  - Go: `s2/crossing_edge_query.go` | C++: `s2crossing_edge_query.h`, `s2crossing_edge_query.cc`
  - Deps: `s2_shape_index`, `s2_edge_crosser`, `s2_padded_cell`

- [x] **s2_closest_edge_query** - `S2_closest_edge_query` (all four target types:
      Point, Edge, Cell, Shape_index; no visitor API / shape filter)
  - Go: `s2/edge_query.go` | C++: `s2closest_edge_query.h`, `s2closest_edge_query_base.h`
  - Deps: `s2_shape_index`, `s2_edge_distances`

- [x] **s2_convex_hull_query** - `S2_convex_hull_query`
      (the `CapBoundExpandedToHemisphere` test is omitted from the fixture
      because it depends on a bit-precise hemisphere threshold in
      `S2LatLngRect.GetCapBound` that the OCaml port does not yet match)
  - Go: `s2/convex_hull_query.go` | C++: `s2convex_hull_query.h`, `s2convex_hull_query.cc`
  - Deps: `s2_point`, `s2_loop`, `s2_cap`

## Tier 8 - Builder

- [x] **s2_builder** - `S2_builder` (public API, `Graph`, `Options`,
      `Snap_function.identity`, `Layer`, `AddEdge`/`AddIntersection`/`Build`,
      `S2_polygon_layer` sibling module covering the surface required by
      `s2_boolean_operation`; scope deferred: the full Voronoi
      snap-rounding algorithm (s2builder.cc:727-1252 - `ChooseInitialSites`,
      `CollectSiteEdges`, `MaybeAddExtraSites`, `SnapEdge` Voronoi loop) is
      replaced by a simplified cluster-merge for non-zero snap radius that
      preserves topology but is not bit-exact with C++;
      `IntLatLngSnapFunction` / `S2CellIdSnapFunction` stubs raise; undirected
      edges, `ForceVertex`, labels, `simplify_edge_chains`, memory tracking,
      and lax/polyline layers are not implemented. Requires `S2PointIndex` +
      `s2pred::GetVoronoiSiteExclusion` + `EdgeCircumcenterSign` to complete
      the faithful algorithm.)
  - Go: `s2/builder.go` | C++: `s2builder.h`, `s2builder.cc`
  - Deps: `s2_shape_index`, `s2_polygon`, `s2_polyline`, `s2_edge_crosser`

- [x] **s2_boolean_operation** - `S2_boolean_operation` (predicate-only,
      polygon-only: `is_empty`, `intersects`, `contains`, `equals` for two
      polygon shapes under default `Polygon_model.Semi_open`; replaces the
      Go-style boundary algorithm in `S2_polygon.contains` / `intersects`
      for multi-loop cases. Implemented as a simplified per-edge
      containment test rather than a full CrossingProcessor port - captures
      the SEMI_OPEN shared-boundary semantics needed by
      `S2_polygon.contains p p` and similar shared-edge predicates without
      the ~3,400 lines of the full C++ module. Scope deferred: set-operation
      output via an `S2_builder.Layer.t`, polyline and point inputs, `Open`
      and `Closed` polygon models, mixed-dimension operands, and any case
      involving invalid polygons with non-normalised loop nesting. These
      raise or silently disagree with C++ on such inputs; `two_shells` in
      the fixture exercises the valid multi-loop path.)
  - Go: (not in Go) | C++: `s2boolean_operation.h`, `s2boolean_operation.cc`
  - Deps: `s2_builder`, `s2_shape_index`
