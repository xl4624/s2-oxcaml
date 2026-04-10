# S2 Port - Module Dependency Graph & TODO

Each entry lists the module, its OCaml name, reference files, and dependencies.
The orchestrator should launch all modules in a tier once their deps are done.

**Legend:** `[x]` = merged to main, `[ ]` = not started

---

## Tier 0 - No internal dependencies

- [x] **r1_interval** - `R1_interval`
  - Go: `r1/interval.go` | C++: `r1interval.h`, `r1interval.cc`
  - Deps: none

- [x] **s1_angle** - `S1_angle`
  - Go: `s1/angle.go` | C++: `s1angle.h`, `s1angle.cc`
  - Deps: none

- [x] **s1_interval** - `S1_interval`
  - Go: `s1/interval.go` | C++: `s1interval.h`, `s1interval.cc`
  - Deps: none

- [x] **r2_point** - `R2_point`
  - Go: `r2/rect.go` (Point type only) | C++: `r2.h`
  - Deps: none

## Tier 1 - Depends on Tier 0

- [x] **s1_chord_angle** - `S1_chord_angle`
  - Go: `s1/chordangle.go` | C++: `s1chord_angle.h`, `s1chord_angle.cc`
  - Deps: `s1_angle`

- [x] **r3_vector** - `R3_vector`
  - Go: `r3/vector.go` | C++: (inline in s2 headers, see `s2point.h` for usage)
  - Deps: `s1_angle`

- [x] **r2_rect** - `R2_rect`
  - Go: `r2/rect.go` (Rect type) | C++: `r2rect.h`, `r2rect.cc`
  - Deps: `r1_interval`, `r2_point`

## Tier 2 - Depends on Tier 1

- [x] **s2_point** - `S2_point`
  - Go: `s2/point.go` | C++: `s2point.h`, `s2point.cc`
  - Deps: `r3_vector`, `s1_angle`, `s1_chord_angle`

- [x] **s2_coords** - `S2_coords`
  - Go: `s2/stuv.go` | C++: `s2coords.h`, `s2coords.cc`
  - Deps: `r2_point`, `r3_vector`

## Tier 3 - Depends on Tier 2

- [x] **s2_latlng** - `S2_latlng`
  - Go: `s2/latlng.go` | C++: `s2latlng.h`, `s2latlng.cc`
  - Deps: `s2_point`, `s1_angle`

- [x] **s2_cell_id** - `S2_cell_id`
  - Go: `s2/cellid.go` | C++: `s2cell_id.h`, `s2cell_id.cc`
  - Deps: `s2_coords`, `s2_point`, `s2_latlng`, `r1_interval`, `r2_point`, `s1_angle`

## Tier 4 - Depends on Tier 3

- [x] **s2_cap** - `S2_cap`
  - Go: `s2/cap.go` | C++: `s2cap.h`, `s2cap.cc`
  - Deps: `s2_point`, `s1_chord_angle`, `s1_angle`

- [x] **s2_cell** - `S2_cell`
  - Go: `s2/cell.go` | C++: `s2cell.h`, `s2cell.cc`
  - Deps: `s2_cell_id`, `s2_point`, `s2_coords`, `r2_rect`, `r1_interval`

- [x] **s2_latlng_rect** - `S2_latlng_rect`
  - Go: `s2/rect.go` | C++: `s2latlng_rect.h`, `s2latlng_rect.cc`
  - Deps: `r1_interval`, `s1_interval`, `s2_latlng`, `s2_point`

- [x] **s2_cell_union** - `S2_cell_union`
  - Go: `s2/cellunion.go` | C++: `s2cell_union.h`, `s2cell_union.cc`
  - Deps: `s2_cell_id`, `s1_angle`

- [x] **s2_edge_crossings** - `S2_edge_crossings`
  - Go: `s2/edge_crossings.go` | C++: `s2edge_crossings.h`, `s2edge_crossings.cc`
  - Deps: `s2_point`, `r3_vector`

- [x] **s2_edge_distances** - `S2_edge_distances`
  - Go: `s2/edge_distances.go` | C++: `s2edge_distances.h`, `s2edge_distances.cc`
  - Deps: `s2_point`, `s1_angle`, `s1_chord_angle`

- [x] **s2_centroids** - `S2_centroids`
  - Go: `s2/centroids.go` | C++: `s2centroids.h`, `s2centroids.cc`
  - Deps: `s2_point`, `r3_vector`

- [ ] **s2_edge_clipping** - `S2_edge_clipping`
  - Go: `s2/edge_clipping.go` | C++: `s2edge_clipping.h`, `s2edge_clipping.cc`
  - Deps: `s2_point`, `r2_point`

- [ ] **s2_measures** - `S2_measures`
  - Go: `s2/point_measures.go` | C++: `s2measures.h`, `s2measures.cc`
  - Deps: `s2_point`

- [ ] **s2_predicates** - `S2_predicates`
  - Go: `s2/predicates.go` | C++: `s2predicates.h`, `s2predicates.cc`
  - Deps: `s2_point`

- [ ] **s2_wedge_relations** - `S2_wedge_relations`
  - Go: `s2/wedge_relations.go` | C++: `s2wedge_relations.h`, `s2wedge_relations.cc`
  - Deps: `s2_point`

- [x] **s2_metrics** - `S2_metrics`
  - Go: `s2/metric.go` | C++: `s2metrics.h`
  - Deps: none

- [x] **s2_pointutil** - `S2_pointutil`
  - Go: (scattered in `s2/point.go`) | C++: `s2pointutil.h`, `s2pointutil.cc`
  - Deps: `s2_point`

## Tier 5 - Depends on Tier 4

- [ ] **s2_edge_crosser** - `S2_edge_crosser`
  - Go: `s2/edge_crosser.go` | C++: `s2edge_crosser.h`
  - Deps: `s2_edge_crossings`, `s2_point`

- [ ] **s2_padded_cell** - `S2_padded_cell`
  - Go: `s2/paddedcell.go` | C++: `s2padded_cell.h`, `s2padded_cell.cc`
  - Deps: `s2_cell_id`, `r2_rect`, `s2_coords`

- [ ] **s2_latlng_rect_bounder** - `S2_latlng_rect_bounder`
  - Go: `s2/rect_bounder.go` | C++: `s2latlng_rect_bounder.h`, `s2latlng_rect_bounder.cc`
  - Deps: `s2_latlng_rect`, `s2_point`

- [ ] **s2_contains_vertex_query** - `S2_contains_vertex_query`
  - Go: `s2/contains_vertex_query.go` | C++: `s2contains_vertex_query.h`, `s2contains_vertex_query.cc`
  - Deps: `s2_point`

- [ ] **s2_projections** - `S2_projections`
  - Go: `s2/projections.go` | C++: `s2projections.h`, `s2projections.cc`
  - Deps: `s2_latlng`, `r2_point`

- [ ] **s2_loop_measures** - `S2_loop_measures`
  - Go: (in `s2/loop.go`) | C++: `s2loop_measures.h`, `s2loop_measures.cc`
  - Deps: `s2_point`, `s2_measures`, `s2_centroids`

- [ ] **s2_polyline_measures** - `S2_polyline_measures`
  - Go: (in `s2/polyline.go`) | C++: `s2polyline_measures.h`, `s2polyline_measures.cc`
  - Deps: `s2_point`, `s1_angle`

- [ ] **earth** - `Earth`
  - Go: `earth/earth.go` | C++: (no direct C++ equivalent)
  - Deps: `s1_angle`, `s2_point`, `s2_latlng`

- [ ] **s2_edge_tessellator** - `S2_edge_tessellator`
  - Go: `s2/edge_tessellator.go` | C++: `s2edge_tessellator.h`, `s2edge_tessellator.cc`
  - Deps: `s2_point`, `s2_projections`, `s2_edge_distances`

## Tier 6 - Shapes and geometry types

- [ ] **s2_shape** - `S2_shape`
  - Go: `s2/shape.go` | C++: `s2shape.h`
  - Deps: `s2_point`

- [ ] **s2_lax_polyline** - `S2_lax_polyline`
  - Go: `s2/lax_polyline.go` | C++: `s2lax_polyline_shape.h`, `s2lax_polyline_shape.cc`
  - Deps: `s2_shape`, `s2_point`

- [ ] **s2_lax_loop** - `S2_lax_loop`
  - Go: `s2/lax_loop.go` | C++: `s2lax_loop_shape.h`, `s2lax_loop_shape.cc`
  - Deps: `s2_shape`, `s2_point`

- [ ] **s2_lax_polygon** - `S2_lax_polygon`
  - Go: `s2/lax_polygon.go` | C++: `s2lax_polygon_shape.h`, `s2lax_polygon_shape.cc`
  - Deps: `s2_shape`, `s2_point`

- [ ] **s2_polyline** - `S2_polyline`
  - Go: `s2/polyline.go` | C++: `s2polyline.h`, `s2polyline.cc`
  - Deps: `s2_point`, `s1_angle`, `s2_latlng`, `s2_edge_distances`

- [ ] **s2_loop** - `S2_loop`
  - Go: `s2/loop.go` | C++: `s2loop.h`, `s2loop.cc`
  - Deps: `s2_point`, `s2_edge_crosser`, `s2_latlng_rect`, `s2_cap`, `s2_cell`

- [ ] **s2_polygon** - `S2_polygon`
  - Go: `s2/polygon.go` | C++: `s2polygon.h`, `s2polygon.cc`
  - Deps: `s2_loop`, `s2_cell_union`, `s2_latlng_rect`, `s2_cap`

## Tier 7 - Spatial index and queries

- [ ] **s2_shape_index** - `S2_shape_index`
  - Go: `s2/shapeindex.go` | C++: `s2shape_index.h`, `mutable_s2shape_index.h`
  - Deps: `s2_shape`, `s2_cell_id`, `s2_point`

- [ ] **s2_contains_point_query** - `S2_contains_point_query`
  - Go: `s2/contains_point_query.go` | C++: `s2contains_point_query.h`
  - Deps: `s2_shape_index`, `s2_edge_crosser`

- [ ] **s2_crossing_edge_query** - `S2_crossing_edge_query`
  - Go: `s2/crossing_edge_query.go` | C++: `s2crossing_edge_query.h`, `s2crossing_edge_query.cc`
  - Deps: `s2_shape_index`, `s2_edge_crosser`, `s2_padded_cell`

- [ ] **s2_closest_edge_query** - `S2_closest_edge_query`
  - Go: `s2/edge_query.go` | C++: `s2closest_edge_query.h`, `s2closest_edge_query_base.h`
  - Deps: `s2_shape_index`, `s2_edge_distances`

- [ ] **s2_region_coverer** - `S2_region_coverer`
  - Go: `s2/regioncoverer.go` | C++: `s2region_coverer.h`, `s2region_coverer.cc`
  - Deps: `s2_cell_union`, `s2_cell`, `s2_padded_cell`, `s2_cap`, `s2_latlng_rect`

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
