# S2 Port - Module Dependency Graph & TODO

Each entry lists the module, its OCaml name, reference files, and dependencies.
The orchestrator should launch all modules in a tier once their deps are done.

**Legend:** `[x]` = merged to main, `[ ]` = not started

Completed tiers (0-5, critical path, and most of tier 6) are omitted. Remaining
work is listed below.

---

## Tier 6 - Shapes and geometry types

- [x] **s2_loop** - `S2_loop` (index-driven ops deferred until `s2_shape_index`)

- [ ] **s2_polygon** - `S2_polygon`
  - Go: `s2/polygon.go` | C++: `s2polygon.h`, `s2polygon.cc`
  - Deps: `s2_loop`, `s2_cell_union`, `s2_latlng_rect`, `s2_cap`

## Tier 7 - Spatial index and queries

- [x] **s2_shape_index** - `S2_shape_index` (mutable add + full rebuild; incremental absorb /
      shape removal not ported; no encode/decode)
  - Go: `s2/shapeindex.go` | C++: `s2shape_index.h`, `mutable_s2shape_index.h`
  - Deps: `s2_shape`, `s2_cell_id`, `s2_point`

- [x] **s2_contains_point_query** - `S2_contains_point_query` (2d shapes: Semi_open only)
  - Go: `s2/contains_point_query.go` | C++: `s2contains_point_query.h`
  - Deps: `s2_shape_index`, `s2_edge_crosser`

- [ ] **s2_crossing_edge_query** - `S2_crossing_edge_query`
  - Go: `s2/crossing_edge_query.go` | C++: `s2crossing_edge_query.h`, `s2crossing_edge_query.cc`
  - Deps: `s2_shape_index`, `s2_edge_crosser`, `s2_padded_cell`

- [ ] **s2_closest_edge_query** - `S2_closest_edge_query`
  - Go: `s2/edge_query.go` | C++: `s2closest_edge_query.h`, `s2closest_edge_query_base.h`
  - Deps: `s2_shape_index`, `s2_edge_distances`

- [ ] **s2_convex_hull_query** - `S2_convex_hull_query`
  - Go: `s2/convex_hull_query.go` | C++: `s2convex_hull_query.h`, `s2convex_hull_query.cc`
  - Deps: `s2_point`, `s2_loop`, `s2_cap`

## Tier 8 - Builder

- [ ] **s2_builder** - `S2_builder`
  - Go: `s2/builder.go` | C++: `s2builder.h`, `s2builder.cc`
  - Deps: `s2_shape_index`, `s2_polygon`, `s2_polyline`, `s2_edge_crosser`

- [ ] **s2_boolean_operation** - `S2_boolean_operation`
  - Go: (not in Go) | C++: `s2boolean_operation.h`, `s2boolean_operation.cc`
  - Deps: `s2_builder`, `s2_shape_index`
