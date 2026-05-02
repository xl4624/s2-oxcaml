## Tier 9 - Shapeutil helpers

Small helper modules layered on `S2_shape` / `S2_shape_index`. Several of
these are already partially reproduced inline inside other modules; this tier
pulls them out as first-class ports so callers can use them directly.

- [x] **s2_edge_vector_shape** - `S2_edge_vector_shape` (mutable builder:
      `create` / `of_edges` / `singleton`, then `add` + `set_dimension`;
      one chain per edge; reference point is fixed "not contained")
  - C++: `s2edge_vector_shape.h` (header-only)
  - Deps: `s2_shape`

- [x] **s2_point_vector_shape** - `S2_point_vector_shape` (immutable:
      `create` / `of_points`; each point is a degenerate edge and its own
      single-edge chain; dimension fixed at [0]; reference point is the
      fixed "not contained" point; encoding not ported so [type_tag] is
      [none])
  - C++: `s2point_vector_shape.h` (header-only)
  - Deps: `s2_shape`

- [x] **s2_wrapped_shape** - `S2_wrapped_shape` (borrows an existing shape
      without owning it; every accessor delegates to the wrapped shape;
      `type_tag` is fixed to `Type_tag.none` so the wrapper is treated as
      uncoded regardless of the backing shape's tag)
  - C++: `s2wrapped_shape.h` (header-only, 54 lines)
  - Deps: `s2_shape`

- [x] **s2_shapeutil_count_edges** - `S2_shapeutil_count_edges` (two free
      functions: `count_edges` and `count_edges_up_to`; the bound is checked
      after each shape is added, mirroring C++ so a non-empty index always
      contributes at least its first shape's edge count)
  - C++: `s2shapeutil_count_edges.h` (header-only)
  - Deps: `s2_shape`, `s2_shape_index`

- [x] **s2_shapeutil_count_vertices** - `S2_shapeutil_count_vertices`
      (`of_shape` dispatches on dimension: 0 -> `num_chains`, 1 ->
      `num_edges + num_chains`, 2 -> `num_edges`; `of_index` sums over every
      shape. Raises on invalid dimensions.)
  - C++: `s2shapeutil_count_vertices.h`, `.cc`
  - Deps: `s2_shape`, `s2_shape_index`

- [x] **s2_shapeutil_shape_edge_id** - `S2_shapeutil_shape_edge_id` (boxed
      record `{ shape_id; edge_id }` with [compare], [equal], [sexp_of], plus a
      [none] sentinel mirroring the C++ default `(-1, -1)`. The existing
      `S2_crossing_edge_query.Shape_edge_id` is now an alias to this module.)
  - C++: `s2shapeutil_shape_edge_id.h` (header-only)
  - Deps: none

- [x] **s2_shapeutil_shape_edge** - `S2_shapeutil_shape_edge` (boxed record
      `{ id; edge }` pairing an `S2_shapeutil_shape_edge_id.t` with the
      `S2_shape.Edge.t` endpoints; exposes `create`, `v0`, `v1` matching the
      C++ struct's accessors. No upstream test exists; the OCaml test pins
      constructor + accessor round-trip via fixture.)
  - C++: `s2shapeutil_shape_edge.h` (header-only)
  - Deps: `s2_shapeutil_shape_edge_id`, `s2_point`

- [x] **s2_shapeutil_edge_iterator** - `S2_shapeutil_edge_iterator` (mutable
      iterator over every edge in an `S2_shape_index.t` in
      [(shape_id, edge_id)] order; exposes `create`, `is_done`, `next`,
      `shape_id`, `edge_id`, `shape_edge_id`, `edge`, and `equal` (compares by
      physical-equal index plus matching ids). The upstream `Remove` test is
      skipped pending the deferred `S2_shape_index.release` work.)
  - Go: `s2/shapeutil_edge_iterator.go` | C++: `s2shapeutil_edge_iterator.h`, `.cc`
  - Deps: `s2_shape_index`

- [x] **s2_shapeutil_contains_brute_force** -
      `S2_shapeutil_contains_brute_force.contains_brute_force` (linear-time
      point-in-shape test: walks edges from the shape's reference point and
      flips containment on each edge-or-vertex crossing, matching C++ exactly.
      Always [false] for shapes with dimension < 2.)
  - C++: `s2shapeutil_contains_brute_force.h`, `.cc`
  - Deps: `s2_shape`, `s2_edge_crosser`

- [x] **s2_shapeutil_get_reference_point** -
      `S2_shapeutil_get_reference_point.get_reference_point` (thin wrapper
      that unpacks an `S2_shape.t` and dispatches to the existing
      `S2_shape.get_reference_point` algorithm. Caller is responsible for
      passing a dimension-2 shape; behaviour is otherwise unspecified. The
      sort-based fallback for the all-balanced case and the empty/full
      conventions live in `S2_shape`.)
  - C++: `s2shapeutil_get_reference_point.h`, `.cc`
  - Deps: `s2_shape`

- [x] **s2_shapeutil_edge_wrap** - `S2_shapeutil_edge_wrap` (two free
      functions: `next_edge_wrap` and `prev_edge_wrap`. Polygon chains always
      wrap; polyline chains wrap only when the last edge's [v1] equals the
      first edge's [v0] (closed polyline); point shapes always return [-1].)
  - C++: `s2shapeutil_edge_wrap.h`, `.cc`
  - Deps: `s2_shape`

- [x] **s2_shapeutil_conversion** - `S2_shapeutil_conversion`
      (`shape_to_points`, `shape_to_polyline`, `shape_to_polygon`. Now uses
      `S2_shape_measures.chain_vertices` for the per-chain vertex walk.
      `shape_to_polygon` recognises the full-polygon convention up front
      and routes single-loop shapes through `S2_polygon.of_loops` and
      multi-loop shapes through `S2_polygon.of_oriented_loops`.)
  - C++: `s2shapeutil_conversion.h`, `.cc`
  - Deps: `s2_shape`, `s2_polygon`, `s2_polyline`, `s2_shape_measures`

- [x] **s2_shapeutil_build_polygon_boundaries** -
      `S2_shapeutil_build_polygon_boundaries.build_polygon_boundaries`
      (groups loop components into faces by inferring nesting against
      `S2_pointutil.origin`. Inputs are wrapped in
      `S2_shapeutil_build_polygon_boundaries.Loop.t` because `S2_shape.t`
      cannot inhabit a polymorphic list directly. Output ids are flat shape
      ids assigned in input order.)
  - C++: `s2shapeutil_build_polygon_boundaries.h`, `.cc`
  - Deps: `s2_shape_index`, `s2_contains_point_query`,
    `s2_shapeutil_contains_brute_force`

- [x] **s2_shapeutil_visit_crossing_edge_pairs** -
      `S2_shapeutil_visit_crossing_edge_pairs` (`visit_crossing_edge_pairs`,
      `visit_crossing_edge_pairs_two`, `find_self_intersection`). The
      single-index walk mirrors C++ exactly (per-cell buffer, drop-one
      adjacent-neighbour optimisation when [need_adjacent = false]). The
      two-index walk uses a simpler "iterate edges of A, query B with
      `S2_crossing_edge_query.get_candidates`" strategy instead of the
      upstream cell-range pipeline; result set is identical, runtime is
      slightly worse on inputs where many A-edges share a B-cell footprint.
      `find_self_intersection` reproduces the upstream
      `FindSelfIntersection`/`FindCrossingError` error messages.
  - C++: `s2shapeutil_visit_crossing_edge_pairs.h`, `.cc`
  - Deps: `s2_shape_index`, `s2_crossing_edge_query`,
    `s2_shapeutil_shape_edge`, `s2_shapeutil_edge_iterator`,
    `s2_edge_crosser`, `s2_wedge_relations`

## Tier 10 - Shape-index measures and regions

- [x] **s2_shape_measures** - `S2_shape_measures` (`length`, `perimeter`,
      `area`, `approx_area`, `centroid`, `chain_vertices`). The
      area/perimeter helpers route through `S2_loop_measures` per chain so
      holes cancel against shells via `signed_area`. `chain_vertices` is
      now exposed and replaces the duplicated helper that lived inside
      `s2_shapeutil_conversion`.
  - C++: `s2shape_measures.h`, `.cc`
  - Deps: `s2_shape`, `s2_loop_measures`, `s2_polyline_measures`

- [x] **s2_shape_index_measures** - `S2_shape_index_measures` (`dimension`,
      `num_points`, `length`, `perimeter`, `area`, `approx_area`, `centroid`).
      Each function walks every shape in the index and folds the per-shape
      result from `S2_shape_measures`. `centroid` ignores shapes whose
      dimension is below the index-wide maximum, matching the documented
      "centroid of the maximal-dimension shapes" semantics.
  - C++: `s2shape_index_measures.h`, `.cc`
  - Deps: `s2_shape_index`, `s2_shape_measures`

- [x] **s2_shape_index_region** - `S2_shape_index_region` (wraps an
      `S2_shape_index` as an `S2_region` via `to_region`; exposes
      `cap_bound`, `rect_bound`, `cell_union_bound`, `contains_cell`,
      `may_intersect_cell`, and `contains_point`). Containment / intersection
      tests reuse the iterator from a private `S2_contains_point_query`
      configured with the `Semi_open` vertex model. Edge-vs-cell intersection
      uses `S2_edge_clipping.clip_to_padded_face` followed by
      `intersects_rect` on the cell's UV bound padded by the doc'd error.
      The lower-level helper `shape_contains_at` was promoted to the public
      `S2_contains_point_query` API so the region can answer cell containment
      against an already-located clipped shape without redoing
      `locate_point`. The C++ `VisitIntersectingShapes`/`VisitIntersectingShapeIds`
      helpers are deferred (the upstream tests for them rely on `S2Fractal`,
      which has not been ported).
  - Go: `s2/shapeindex_region.go` | C++: `s2shape_index_region.h` (header-only, ~475 lines)
  - Deps: `s2_shape_index`, `s2_region`, `s2_contains_point_query`, `s2_edge_clipping`, `s2_cell_union`

- [x] **s2_shape_index_buffered_region** - `S2_shape_index_buffered_region`
      (wraps an index plus a radius as an `S2_region` whose buffered geometry
      contains every point within [radius] of any indexed point. `cap_bound`
      and `rect_bound` widen the unbuffered bounds; `cell_union_bound`
      replaces each cell in the unbuffered covering with its vertex
      neighbours at the chosen [max_level], and falls back to the six face
      cells when the radius is larger than any cell at level 0 or when the
      unbuffered covering already touches a face cell. `contains_cell` uses
      the upstream cap-based heuristic; `may_intersect_cell` and
      `contains_point` route through `S2_closest_edge_query.is_distance_less`
      against [radius_successor] so that `radius = 0` still treats
      coincident points as contained. The empty-index and full-polygon
      upstream tests are exercised in expect-snapshots since the C++
      behaviour for those cases relies on NaN-driven `std::min` which the
      OCaml port does not reproduce.)
  - C++: `s2shape_index_buffered_region.h`, `.cc` (~130 lines)
  - Deps: `s2_shape_index_region`, `s2_closest_edge_query`

- [ ] **s2_shape_nesting_query** - `S2_shape_nesting_query`
  - C++: `s2shape_nesting_query.h`, `.cc` (~290 lines)
  - Deps: `s2_shape_index`, `s2_contains_point_query`

## Tier 11 - Point and cell indices / queries

- [ ] **s2_point_index** - `S2_point_index` (required to complete the faithful
      Voronoi algorithm in `S2_builder`)
  - C++: `s2point_index.h` (header-only, ~380 lines)
  - Deps: `s2_cell_id`, `s2_point`

- [ ] **s2_point_region** - `S2_point_region` (single-point `S2_region`)
  - C++: `s2point_region.h`, `.cc` (~80 lines)
  - Deps: `s2_region`, `s2_point`, `s2_cap`

- [ ] **s2_closest_point_query** - `S2_closest_point_query` (+ `_base`)
  - C++: `s2closest_point_query.h`, `.cc`, `s2closest_point_query_base.h` (~70 lines on top)
  - Deps: `s2_point_index`, `s2_edge_distances`

- [ ] **s2_cell_index** - `S2_cell_index`
  - Go: `s2/cell_index.go` | C++: `s2cell_index.h`, `.cc` (~150 lines)
  - Deps: `s2_cell_id`

- [ ] **s2_closest_cell_query** - `S2_closest_cell_query` (+ `_base`)
  - C++: `s2closest_cell_query.h`, `.cc`, `s2closest_cell_query_base.h` (~120 lines on top)
  - Deps: `s2_cell_index`, `s2_cell`

- [ ] **s2_furthest_edge_query** - `S2_furthest_edge_query`
      (mirrors `S2_closest_edge_query` with inverted ordering)
  - C++: `s2furthest_edge_query.h`, `.cc` (~120 lines)
  - Deps: `s2_closest_edge_query`

- [ ] **s2_hausdorff_distance_query** - `S2_hausdorff_distance_query`
  - C++: `s2hausdorff_distance_query.h`, `.cc` (~165 lines)
  - Deps: `s2_closest_edge_query`, `s2_furthest_edge_query`

- [ ] **s2_chain_interpolation_query** - `S2_chain_interpolation_query`
  - C++: `s2chain_interpolation_query.h`, `.cc` (~165 lines)
  - Deps: `s2_shape`, `s2_edge_distances`

## Tier 12 - Region compositions

- [ ] **s2_region_union** - `S2_region_union`
  - Go: `s2/regionunion.go` | C++: `s2region_union.h`, `.cc` (~120 lines)
  - Deps: `s2_region`, `s2_cell_union`

- [ ] **s2_region_intersection** - `S2_region_intersection`
  - C++: `s2region_intersection.h`, `.cc` (~115 lines)
  - Deps: `s2_region`, `s2_cell_union`

- [ ] **s2_region_term_indexer** - `S2_region_term_indexer`
  - C++: `s2region_term_indexer.h`, `.cc` (~290 lines)
  - Deps: `s2_region_coverer`

- [ ] **s2_region_sharder** - `S2_region_sharder`
  - C++: `s2region_sharder.h`, `.cc` (~150 lines)
  - Deps: `s2_region_coverer`, `s2_cell_union`

## Tier 13 - Validation

- [ ] **s2_validation_query** - `S2_validation_query`
      (supersedes `S2Polygon::FindValidationError` and enables wiring the
      builder layers' `validate` option. Also used to finalise an index-
      driven validity check inside `S2_polygon`.)
  - C++: `s2validation_query.h` (header-only, ~1340 lines)
  - Deps: `s2_shape_index`, `s2_shapeutil_visit_crossing_edge_pairs`

## Tier 14 - Builder extensions and layers

- [ ] **s2_builderutil_snap_functions** - `S2_builderutil.Snap_functions`
      (real implementations of `IntLatLngSnapFunction` and
      `S2CellIdSnapFunction` - currently stubs that raise inside
      `S2_builder.Snap_function`)
  - C++: `s2builderutil_snap_functions.h`, `.cc`
  - Deps: `s2_builder`, `s2_cell_id`

- [ ] **s2_builderutil_graph_shape** - `S2_builderutil.Graph_shape`
  - C++: `s2builderutil_graph_shape.h` (header-only)
  - Deps: `s2_builder` (Graph), `s2_shape`

- [ ] **s2_builderutil_closed_set_normalizer** - `S2_builderutil.Closed_set_normalizer`
  - C++: `s2builderutil_closed_set_normalizer.h`, `.cc`
  - Deps: `s2_builder`

- [ ] **s2_builderutil_find_polygon_degeneracies** - `S2_builderutil.Find_polygon_degeneracies`
  - C++: `s2builderutil_find_polygon_degeneracies.h`, `.cc`
  - Deps: `s2_builder`, `s2_shapeutil_visit_crossing_edge_pairs`

- [ ] **s2_builderutil_get_snapped_winding_delta** - `S2_builderutil.Get_snapped_winding_delta`
  - C++: `s2builderutil_get_snapped_winding_delta.h`, `.cc`
  - Deps: `s2_builder`

- [ ] **s2_builderutil_s2polyline_layer** - `S2_polyline_layer`
      (parallel to the existing `S2_polygon_layer`)
  - C++: `s2builderutil_s2polyline_layer.h`, `.cc`
  - Deps: `s2_builder`, `s2_polyline`

- [ ] **s2_builderutil_s2polyline_vector_layer** - `S2_polyline_vector_layer`
  - C++: `s2builderutil_s2polyline_vector_layer.h`, `.cc`
  - Deps: `s2_builder`, `s2_polyline`

- [ ] **s2_builderutil_s2point_vector_layer** - `S2_point_vector_layer`
  - C++: `s2builderutil_s2point_vector_layer.h`, `.cc`
  - Deps: `s2_builder`, `s2_point_vector_shape`

- [ ] **s2_builderutil_lax_polygon_layer** - `S2_lax_polygon_layer`
  - C++: `s2builderutil_lax_polygon_layer.h`, `.cc`
  - Deps: `s2_builder`, `s2_lax_polygon`

- [ ] **s2_builderutil_lax_polyline_layer** - `S2_lax_polyline_layer`
  - C++: `s2builderutil_lax_polyline_layer.h`, `.cc`
  - Deps: `s2_builder`, `s2_lax_polyline`

- [ ] **s2_winding_operation** - `S2_winding_operation` (winding-number set ops)
  - C++: `s2winding_operation.h`, `.cc` (~560 lines)
  - Deps: `s2_builder`, `s2_builderutil_get_snapped_winding_delta`

- [ ] **s2_buffer_operation** - `S2_buffer_operation` (buffer a geometry by a distance)
  - C++: `s2buffer_operation.h`, `.cc` (~770 lines)
  - Deps: `s2_builder`, `s2_winding_operation`

## Tier 15 - Polyline alignment and simplification

- [ ] **s2_polyline_simplifier** - `S2_polyline_simplifier`
  - C++: `s2polyline_simplifier.h`, `.cc` (~250 lines)
  - Deps: `s2_polyline`

- [ ] **s2_polyline_alignment** - `S2_polyline_alignment`
  - C++: `s2polyline_alignment.h`, `.cc` (~430 lines)
  - Deps: `s2_polyline`

## Tier 16 - Encoding and serialization

Encoding is pervasive in the C++ library and unlocks Encode/Decode across
many ported modules. Do this as a block so existing types can bolt it on.

- [ ] **encoded_uint_vector** - `Encoded_uint_vector`
  - C++: `encoded_uint_vector.h` (header-only)

- [ ] **encoded_string_vector** - `Encoded_string_vector`
  - C++: `encoded_string_vector.h`, `.cc`

- [ ] **encoded_s2cell_id_vector** - `Encoded_s2cell_id_vector`
  - C++: `encoded_s2cell_id_vector.h`, `.cc`
  - Deps: `encoded_uint_vector`, `s2_cell_id`

- [ ] **s2_point_compression** - `S2_point_compression`
  - Go: `s2/pointcompression.go` | C++: `s2point_compression.h`, `.cc`
  - Deps: `s2_point`, `s2_cell_id`

- [ ] **encoded_s2point_vector** - `Encoded_s2point_vector`
  - C++: `encoded_s2point_vector.h`, `.cc`
  - Deps: `s2_point_compression`

- [ ] **s2_shapeutil_coding** - `S2_shapeutil.coding`
  - C++: `s2shapeutil_coding.h`, `.cc`
  - Deps: `encoded_s2point_vector`

- [ ] **encoded_s2shape_index** - `Encoded_s2_shape_index`
  - C++: `encoded_s2shape_index.h`, `.cc`
  - Deps: `s2_shape_index`, `s2_shapeutil_coding`, `encoded_s2cell_id_vector`

- [ ] **s2_coder** - `S2_coder` (generic coder framework used by Encode/Decode)
  - C++: `s2coder.h` (header-only)

## Tier 17 - Miscellaneous / nice-to-haves

- [ ] **s2_density_tree** - `S2_density_tree`
  - C++: `s2density_tree.h`, `.cc` (~1070 lines)
  - Deps: `s2_shape_index`, `s2_region_coverer`

- [ ] **s2_r2_rect_region** - `S2_r2_rect` region projection
  - C++: `s2r2rect.h`, `.cc` (~95 lines)
  - Deps: `r2_rect`, `s2_region`

- [ ] **s2_fractal** - `S2_fractal` (testing helper; likely skipped)
  - C++: `s2fractal.h`, `.cc` (~155 lines)
  - Deps: `s2_loop`, `s2_polyline`

---

## Scope deferred within already-ported modules

These are carry-over TODOs from the existing ports. Each lives as a `(* TODO:
... *)` comment in the relevant file; listed here so they aren't forgotten.

### Serialization / encoding

Most of this work is blocked on Tier 16. Mentioned per-module so callers can
track which types still lack Encode/Decode:

- [ ] `s1_angle`: `S1Angle::Coder` serialization interface
- [ ] `s2_cell_id`: Encode/Decode (binary form; tokens already supported)
- [ ] `s2_cell`: Encode/Decode
- [ ] `s2_cell_union`: Encode/Decode
- [ ] `s2_latlng_rect`: Encode/Decode
- [ ] `s2_loop`: Encode/Decode (including compressed form)
- [ ] `s2_polyline`: Encode/Decode
- [ ] `s2_polygon`: Encode/Decode
- [ ] `s2_shape_index`: Encode/Decode (+ `EncodedS2ShapeIndex` read-only sibling)
- [ ] `s2_lax_loop` / `s2_lax_polygon` / `s2_lax_polyline`: Encode/Decode and
      the `Encoded*` lazy variants

### Numerical / geometry conversions

- [x] `s1_angle`: `S2Point` and `S2LatLng` two-argument constructors -
      provided as `S2_point.distance` and `S2_latlng.distance`. Placed on
      the higher-level types instead of `S1_angle` to keep the latter free
      of circular dependencies.
- [x] `s1_chord_angle`: `FastUpperBoundFrom` (radians-squared upper bound)
- [x] `s2_latlng`: `FromUnsignedE6` / `FromUnsignedE7` (proto fixed32 round-trip)
- [x] `s2_latlng`: `ToStringInDegrees` ("lat,lng" formatted string)

### Distance and projection APIs

- [x] `s2_edge_distances`: `S1ChordAngle`-argument overloads of `GetPointOnRay`,
      `GetPointOnLine`, `GetPointToLeft`, `GetPointToRight` exposed as
      `*_chord` siblings using `S1_chord_angle.sin`/`cos` directly. Loses
      accuracy near 180 degrees; the `S1_angle` forms remain for
      callers that may pass distances close to pi.
- [x] `s2_edge_distances`: expose `project_with_cross` as a public API mirroring
      the C++ overload in `s2edge_distances.h:119-120`
- [x] `s2_edge_crossings`: expose standalone `SignedVertexCrossing` (currently
      only reachable as a private helper inside `s2_edge_crosser`)
- [ ] `s2_loop`: `GetDistance` / `GetDistanceToBoundary` / `Project` /
      `ProjectToBoundary` / `BoundaryNear`
- [ ] `s2_polyline`: `GetDistance` / `Project` (nearest-point, nearest-distance)
- [ ] `s2_polyline`: `NearlyCovers` / `ApproxEquals` polyline-to-polyline
- [ ] `s2_polygon`: `GetDistance` / `GetDistanceToBoundary` / `Project`
- [ ] `s2_polygon`: `ApproxContains` / `ApproxDisjoint` / `BoundaryNear`
- [x] `s2_latlng_rect`: exact `Intersects(const S2Cell&)` exposed as
      `intersects_s2_cell`. The conservative `intersects_cell` is kept as the
      cheap rect-bound test used by `S2_region` and other early-rejection
      paths.
- [x] `s2_latlng_rect`: `ExpandedByDistance` negative-distance shrink branch,
      `BoundaryIntersects`, `IntersectsLngEdge`, `IntersectsLatEdge`

### Predicates

- [ ] `s2_predicates`: `CompareEdgeDistance`, `CompareEdgePairDistance`,
      `CompareEdgeDirections`
- [ ] `s2_predicates`: `CircleEdgeIntersectionSign`, `CircleEdgeIntersectionOrdering`
- [ ] `s2_predicates`: `EdgeCircumcenterSign`, `GetVoronoiSiteExclusion`
      (required for faithful `S2_builder` Voronoi snapping)
- [x] `s2_predicates`: low-level `TriageSign`, `ExpensiveSign`, `UnperturbedSign`
      entry points (for callers that precompute cross products)

### Query extensions

- [ ] `s2_crossing_edge_query`: `VisitCells` / `VisitRawCandidates` / `GetCells`
- [ ] `s2_closest_edge_query`: `VisitClosestEdges` / `VisitClosestShapes`,
      `ShapeFilter`

### Builder scope deferred

- [ ] `s2_builder`: full Voronoi snap-rounding pipeline (`ChooseInitialSites`,
      `CollectSiteEdges`, `MaybeAddExtraSites`, `SnapEdge` Voronoi loop) for
      bit-exact parity with C++ - blocked on `S2_point_index` +
      `GetVoronoiSiteExclusion` + `EdgeCircumcenterSign`
- [ ] `s2_builder`: real `IntLatLngSnapFunction` and `S2CellIdSnapFunction`
      (Tier 14 `s2_builderutil_snap_functions`)
- [ ] `s2_builder`: undirected edges (`Edge_type.Undirected`)
- [ ] `s2_builder`: `Sibling_pairs.Require` and `Sibling_pairs.Create`
- [ ] `s2_builder`: `ForceVertex`, labels, `simplify_edge_chains`, memory
      tracking
- [ ] `s2_builder.Graph`: `Loop_type.Circuit` (repeated-edge cycle detection)
- [ ] `s2_polygon_layer`: undirected-edge support
- [ ] `s2_polygon_layer`: label-set tracking (`LabelSetIds`, `IdSetLexicon`)
- [ ] `s2_polygon_layer`: wire up `validate` option to call validation
      (blocked on Tier 13 `s2_validation_query`)

### Polygon scope deferred

- [ ] `s2_polygon`: `InitToUnion`, `InitToIntersection`, `InitToDifference`,
      `InitToSymmetricDifference`, `InitToSnapped`, `InitToSimplified`
      (set operations with polygon output, as opposed to the existing boolean
      predicates; requires Tier 14 layers to be wired through
      `s2_boolean_operation`)
- [ ] `s2_polygon`: `IntersectWithPolyline`, `SubtractFromPolyline`,
      `ApproxIntersectWithPolyline`
- [ ] `s2_polygon`: index-driven validation (uses the index-free subset today;
      blocked on Tier 13)

### Boolean operation scope deferred

- [ ] `s2_boolean_operation`: set-operation output via an `S2_builder.Layer.t`
      (as opposed to the four predicates which are implemented)
- [ ] `s2_boolean_operation`: polyline and point inputs, mixed-dimension operands
- [ ] `s2_boolean_operation`: `Polygon_model.Open` and `Polygon_model.Closed`
- [ ] `s2_boolean_operation`: robustness for invalid-nesting inputs

### Shape-index scope deferred

- [ ] `s2_shape_index`: `Remove` / `RemoveAll` (shape id reuse still disallowed)
- [ ] `s2_shape_index`: `Minimize` (drop cell map while keeping shapes)
- [ ] `s2_shape_index`: `SpaceUsed` and `S2MemoryTracker` integration

### Lax shape scope deferred

- [ ] `s2_lax_loop`: `S2VertexIdLaxLoopShape` (vertex-id storage variant)
- [ ] `s2_lax_loop`: `S2LaxClosedPolylineShape` (interior-less variant)
- [ ] `s2_lax_polyline`: `S2LaxClosedPolylineShape`
- [ ] `s2_lax_polygon`: construction from an existing `S2_polygon`
- [ ] `s2_lax_loop` / `s2_lax_polyline`: construction from the boxed polyline
      / loop equivalents

---

## Canonical-data fixture gaps

- [ ] `s2_convex_hull_query`: `CapBoundExpandedToHemisphere` upstream test case
      is omitted from the fixture; depends on matching the bit-precise
      hemisphere threshold in `S2LatLngRect::GetCapBound`.
- [ ] `s2_polygon`: the polygon-polygon relations fixture excludes multi-loop
      polygons that would stress shared-boundary semantics; now that
      `s2_boolean_operation` drives those predicates, the fixture can be
      widened once output-producing set operations land.

---

## Quickcheck coverage gaps

Modules with non-trivial invariants whose `test/quickcheck/<name>.ml` is
missing. Each item is scoped to one or two property ideas: a generator that
exercises real geometry, plus the law(s) the property checks. AGENTS.md
explains the file layout and how to wire `let%test_unit` cases into the
shared `inline_tests` stanza.

### Queries

- [ ] `s2_contains_point_query`: build an index over a generated polygon,
      check that for random points, `contains` matches
      `S2_polygon.contains_point`. Also test the three vertex models against
      a hand-rolled brute-force "is on boundary?" classifier.
- [ ] `s2_crossing_edge_query`: index a random polygon, query a random
      geodesic edge, and check that `get_candidates` is a superset of the
      brute-force list of edges that cross it. For `visit_crossings`, check
      every emitted pair actually crosses (`S2_edge_crossings.crossing_sign
      > 0`) and no crossing pair is missed.
- [ ] `s2_closest_edge_query`: random polygon + random target point;
      `closest_edge` distance must equal the brute-force minimum
      `S2_edge_distances.distance` over all edges. Also: `is_distance_less
      ~limit:d` matches `closest_edge < d`, and `is_distance_less_or_equal`
      matches `<= d`.
- [ ] `s2_convex_hull_query`: hull of a random point set must contain every
      input point and every input point must be on the hull or interior;
      idempotence (`hull (hull pts) = hull pts` up to vertex order).
- [ ] `s2_region_coverer`: for random regions (cap, rect, polygon-as-region),
      the covering must (a) contain the region per `Contains(cell)` /
      `MayIntersect(cell)` semantics (every cell in the cover may intersect
      the region; their union covers it), (b) honour `max_cells` for the
      output (with the documented slack), (c) honour `min_level` /
      `max_level` / `level_mod`. Also: `is_canonical (canonicalize_covering
      ids)` is always true.

### Shape-index regions and measures

- [ ] `s2_shape_index_region`: for a random shape index, `contains_point p`
      must match `S2_contains_point_query.contains` over the same index;
      `contains_cell` must imply `may_intersect_cell`.
- [ ] `s2_shape_index_buffered_region`: for a random index + radius, a point
      `p` is contained iff there is some indexed point at most `radius`
      away. Cross-check `contains_point` against a brute-force scan of edges
      with `S2_edge_distances`.
- [ ] `s2_shape_index_measures`: `area`, `perimeter`, `length`, `centroid`
      summed over a multi-shape index must equal the per-shape sums via
      `S2_shape_measures`.
- [ ] `s2_shape_measures`: `chain_vertices` then `length`/`perimeter` on the
      walked vertices must match the direct measure call. `centroid (s) +
      centroid (s')` over disjoint chains must equal `centroid (s ++ s')`.

### Polygon and boolean ops

- [ ] `s2_polygon`: random polygon construction + `contains_point` must
      match the loop-walk reference implementation. `intersects (a, b) =
      intersects (b, a)`. `contains a a = true`.
- [ ] `s2_boolean_operation`: idempotence (`intersects (a, a) = not
      (is_empty a)`), commutativity, and de Morgan-style relations between
      the four implemented predicates (`contains`, `intersects`,
      `equals`, `disjoint`) on random pairs of polygons.
- [ ] `s2_polygon_layer`: build a polygon with `S2_builder` and check that
      the layer's output recovers it (round-trip), modulo expected
      normalization (loop ordering, hole orientation).

### Shape utilities

- [ ] `s2_shape`: `get_reference_point` consistency: walking edges from the
      reference point's `contained` flag and flipping on every crossing
      lands at the same containment value as a brute-force point-in-shape
      test for many random query points.
- [ ] `s2_shapeutil_count_edges` / `count_vertices`: random polygon's totals
      match `Array.fold` over per-shape `num_edges` / dimension-based vertex
      counts.
- [ ] `s2_shapeutil_edge_iterator`: iteration visits each `(shape_id,
      edge_id)` exactly once across a multi-shape index, in
      `(shape_id, edge_id)`-lexicographic order.
- [ ] `s2_shapeutil_visit_crossing_edge_pairs`: every emitted pair `(a, b)`
      satisfies `S2_edge_crossings.crossing_sign a.v0 a.v1 b.v0 b.v1 > 0`,
      and the set of emitted pairs equals the brute-force enumeration on
      small inputs (<= 200 edges).
- [ ] `s2_shapeutil_contains_brute_force`: matches `S2_polygon.contains_point`
      for random polygon + random points.
- [ ] `s2_shapeutil_conversion`: `shape_to_polygon (S2_polygon.to_shape p)`
      returns a polygon equal to `p` (modulo loop ordering).
- [ ] `s2_shapeutil_get_reference_point`: agrees with each shape's own
      `S2_shape.get_reference_point` on random dimension-2 shapes.
- [ ] `s2_edge_vector_shape` / `s2_point_vector_shape`: `num_edges` matches
      input length, `chain` accessors round-trip (chain[i]'s edge equals
      `edge i`), reference point is fixed `not contained` by spec.
- [ ] `s2_wrapped_shape`: every accessor delegates exactly to the wrapped
      shape on random inputs (modulo the pinned `Type_tag.none`).

### Edge tessellator and builder

- [ ] `s2_edge_tessellator`: for a random projection + edge + tolerance,
      every consecutive sample pair on the tessellated polyline is within
      tolerance of the underlying geodesic. Output reaches both endpoints.

### Region wrappers

- [ ] `s2_region`: `of_cap`, `of_rect`, `of_cell`, `of_cell_union`, and
      `Custom` all delegate to the underlying type's `contains_cell` /
      `intersects_cell` / `contains_point` on random inputs.

---

## Benchmark coverage gaps

`bench/bench_misc.ml` covers only `Sign` / `RobustSignSimple` / `PointArea`
/ `PointFromLatLng` / `LatLngGetDistance`; `bench/bench_region_coverer.ml`
covers cap / cell / cell-union coverings. The Go-vs-OxCaml comparison
(`bench/compare_go.py`, `bench/compare.txt`) only knows about those eight
buckets. Most query-heavy code paths and polygon/loop operations have no
microbenchmark. Each item below is a `Bench.Test.create` (or
`create_indexed`) skeleton and a Go-side counterpart to add to
`compare_go.py` if the upstream Go bench exists. Land them one or two at
a time so `compare.txt` stays interpretable.

### Polygon and loop hot paths

- [ ] `s2_polygon.contains_point`: random polygon (10/100/1000 vertices),
      random query point. Index-built polygon vs index-free.
- [ ] `s2_polygon.intersects` and `s2_polygon.contains` (polygon vs
      polygon): same shape sizes, two random polygons.
- [ ] `s2_polygon.area` / `s2_polygon.perimeter`: trivial pure
      computations, but useful as a fixed point against the Go bench.
- [ ] `s2_loop.contains_point`: random loop, random point. Compare the
      brute-force walk path (small loops) against the
      `S2_contains_point_query` path (large loops).
- [ ] `s2_loop.area` / `s2_loop.centroid`.
- [ ] `s2_lax_loop` / `s2_lax_polygon` / `s2_lax_polyline`: constructor
      from a vertex array, plus `to_shape` + a single `S2_shape.edge`
      access per chain. Useful to validate the `[@@unboxed]` wrappers
      stay zero-alloc.

### Indexes and queries

- [ ] `s2_shape_index.build`: time-to-first-query as a function of total
      edge count. One curve for points, one for polygons.
- [ ] `s2_closest_edge_query`: query-ready index + random target, vary
      target distance from "inside the index" to "far away" to exercise
      both fast-paths and the heap-pruning path. The current
      `Cell_queue` design is delicate enough that a bench is the only
      way to catch perf regressions.
- [ ] `s2_crossing_edge_query.get_candidates`: random target edge over a
      random index. The `kMaxBruteForceEdges = 27` cutoff (lifted from
      the upstream benchmarks) deserves an OxCaml-side sweep to confirm
      it is still optimal under our different micro-architecture.
- [ ] `s2_contains_point_query.contains`: random index + many random
      point queries. The three vertex models should each get their own
      bucket if there is meaningful divergence.
- [ ] `s2_shape_index_region.cell_union_bound`: random index, depth and
      `max_level` sweep. Compare against the conservative
      `S2_cap.cell_union_bound` baseline.

### Builder and boolean operations

- [ ] `s2_builder.Build`: small / medium / large input edge counts,
      varying the snap function (Identity vs IntLatLng vs CellId once
      Tier 14's `s2_builderutil_snap_functions` lands). Tracks the
      builder's typically-O(n log n) growth.
- [ ] `s2_boolean_operation`: each of the four predicates
      (`contains`, `intersects`, `equals`, `disjoint`) on representative
      polygon-vs-polygon pairs. Important for Tier 13 work because the
      validation query reuses these.

### Edge primitives

- [ ] `s2_edge_distances`: `distance` / `is_distance_less` /
      `update_min_distance` on synthetic point + edge inputs. These are
      called millions of times per closest-edge query; even a 5%
      improvement in the inner loop pays off broadly.
- [ ] `s2_predicates.expensive_sign`: dedicated bench beyond `Sign`. The
      simple-precision and stable-sign fast paths each merit their own
      bucket.
- [ ] `s2_edge_clipping.clip_to_padded_face`: synthetic UV-edge stream,
      the input shape that `s2_shape_index_region.may_intersect_cell`
      hits in real workloads.

---

## Documentation / AGENTS.md compliance

AGENTS.md mandates that comments in `lib/` (including `.mli` doc
comments) must be self-contained: no upstream cross-references like
"Mirrors X", "matches the upstream X", "see s2foo.cc:42",
"as in S2Foo::Bar", etc. The following call sites violate that rule and
should be reworded to explain the *reason* directly (or removed if there
is no reason left to capture).

### `.mli` doc-comment violations (user-facing)

- [ ] `lib/s2_lax_polyline.mli:77`: `[type_tag]` doc references
      `S2LaxPolylineShape::kTypeTag`. Drop the upstream name; just state
      the integer tag and its meaning.
- [ ] `lib/s2_lax_polygon.mli:122`: same pattern with
      `S2LaxPolygonShape::kTypeTag`.
- [ ] `lib/s2_shapeutil_edge_wrap.mli:26`: "Mirrors {!next_edge_wrap} ..."
      - state the relation directly ("returns the predecessor of [edge_id]
      ...") instead of cross-referencing.
- [ ] `lib/s2_shapeutil_build_polygon_boundaries.mli:39`:
      "(mirroring the upstream contract)" - drop the qualifier; the
      invariants are already listed.
- [ ] `lib/s2_shape_index.mli:24`: "Thread-safety matches the C++
      library" - reword as a positive statement of what the OCaml port
      guarantees.
- [ ] `lib/s2_shape_index.mli:32`: "equivalent to C++
      [MutableS2ShapeIndex]" - drop the equivalence.
- [ ] `lib/s2_shapeutil_count_edges.mli:10`: "matching the C++
      convention".
- [ ] `lib/s2_shapeutil_contains_brute_force.mli:8`: "matching the C++
      default in ...".
- [ ] `lib/s2_shapeutil_shape_edge_id.mli:20`: "default value of an
      uninitialised {!t} on the C++ side".
- [ ] `lib/s2_predicates.mli:17`: "the following upstream predicates are
      not yet ported" - the {1 Limitations} block is the right place for
      a list, but it shouldn't say "upstream"; just name them.
- [ ] `lib/s2_padded_cell.mli:93`: "Padded cells mirror the C++ class
      closely" - this whole sentence can go; the API is the contract.
- [ ] `lib/s2_pointutil.mli:4`: "the analogous C++ utilities live in" -
      drop the cross-reference.
- [ ] `lib/r3_vector.mli:20` and `lib/r2_point.mli:19,24`: refer to
      "the underlying [Vector3_d] / [Vector2_d] in C++" in a Limitations
      block. Replace with a list of the unported component-wise ops by
      name only.
- [ ] `lib/s2_lax_loop.mli:23`: "[S2VertexIdLaxLoopShape] from the C++
      library" - drop the source attribution; just say what is missing.
- [ ] `lib/s2_closest_edge_query.mli:27`: "[ShapeFilter] parameter
      present in the C++ library" - reword without the cross-reference.
- [ ] `lib/s2_crossing_edge_query.mli:16`: "from the C++ library
      ([VisitCells], ...)" - just list the missing functions.
- [ ] `lib/s2_shape.mli:181`: "The following C++ features are not
      exposed" - reword as "Not implemented:" or similar.
- [ ] `lib/s2_latlng_rect.mli:277`: same pattern, "The following C++
      features are not exposed" - reword.
- [ ] `lib/s2_projections.mli:17`: "[wrap_destination] wraps only along
      the x axis. The C++ default wraps along any axis" - state our
      semantics, then note the divergence as "The two-axis wrap variant
      is not provided" without naming C++.

### `.ml` implementation-comment violations

- [ ] `lib/s2_crossing_edge_query.ml:11-12`: "lifted from the upstream
      benchmarks in s2crossing_edge_query.cc:44" - replace with the
      derivation, or just state the constant.
- [ ] `lib/s2_crossing_edge_query.ml:142,219`: more file:line refs.
- [ ] `lib/s2_closest_edge_query.ml:154`: "benchmark-derived constants
      in s2closest_edge_query.cc:36-63".
- [ ] `lib/s2_closest_edge_query.ml:165`: "ShapeFilter parameter from
      s2closest_edge_query.h:270-289".
- [ ] `lib/s2_loop_measures.ml:35,42`: "matches s2loop_measures.h:..."
      and "s2loop_measures.h:258-362".
- [ ] `lib/s2_latlng_rect_bounder.ml:19`: "see
      s2latlng_rect_bounder.cc:AddInternal".
- [ ] `lib/s2_lax_polygon.ml:20`: "s2lax_polygon_shape.h:270-295 for the
      upstream reasoning".
- [ ] `lib/s2_metrics.ml:56`: "plus Mathematica in the upstream library;
      see s2metrics.cc for the alternative linear analysis".
- [ ] `lib/s2_projections.ml:108`: "the upstream default also wraps
      along [y]".
- [ ] `lib/s2_boolean_operation.ml:3-7,250`: "this port replaces the C++
      CrossingProcessor state machine (s2boolean_operation.cc:...)" plus
      "(s2boolean_operation.cc:2476-2505)".
- [ ] `lib/s2_edge_crossings.ml:10`: "from the analysis in
      s2predicates_internal.h".
- [ ] `lib/s2_edge_crosser.ml:9,35,115`: three file:line refs.
- [ ] `lib/earth.ml:37`: "See s2earth.cc:52-66".
- [ ] `lib/s2_builder.ml:383,756,760`: a few file:line refs and "1500
      lines of C++ Voronoi machinery".
- [ ] `lib/s2_shape_index.ml:824` and similar `(* TODO: port ... from
      <file>:<lines> *)` comments throughout the codebase: these belong
      in this `TODO.md` file, not in the source. Sweep them out and
      either move the unique content here or delete it where this file
      already covers it.

---

## Tooling and CI

- [ ] No CI configuration is checked in. Add a GitHub Actions workflow
      that runs `make build` and `make test` on every PR. The fixture
      generators link against a system-installed s2geometry; document
      that dependency in `AGENTS.md` and either install it from source
      in CI or skip `make fixtures` and rely on the checked-in JSON.
- [ ] `make test TEST=<module>` re-runs the *whole* shared
      `quickcheck/` and `expect_snapshots/` libraries. As those grow
      this will get noticeable; investigate whether dune supports a
      single-`let%test_unit` filter so we can scope to one property at
      a time during iteration.
- [ ] `bench/compare_go.py` knows about a hardcoded list of eight bench
      buckets. Once the bench coverage gaps above are addressed, the
      bucket map and `compare.txt` formatting need to grow with them.
      Consider auto-discovery: have each `Bench.Test.create` register
      itself in a registry that the comparison script can read via
      `-list`.

---

## Unboxing / allocation work

Opportunities to drop boxing or allocation across the library. Each item is
scoped so an agent can take one, verify a benchmark or `zero_alloc` check,
and land it independently.

### Types that should derive `unboxed_option`

`unboxed_option` already lives on `r1_interval`, `r2_point`, `r2_rect`,
`r3_vector`, `s1_chord_angle`, `s2_cap`, `s2_latlng`, `s2_point`, `s1_angle`,
`s1_interval`, `s2_latlng_rect`, and `s2_cell_id`. Extend to the remaining
types that have an obvious sentinel so callers can skip `Option`-header
allocation.

- [x] `s2_cell_id` (`type t : bits64`): `[@@deriving unboxed_option { sentinel
      = true }]`. The PPX-chosen sentinel is `Int64.min_value`, which is
      distinct from both `S2_cell_id.none` (all zeros) and `S2_cell_id.sentinel`
      (all ones), so existing reserved values keep their meaning.
- [x] `s1_angle` (alias of `Float_u.t`): `[@@deriving unboxed_option { sentinel
      = true }]` (sentinel is `Float_u.nan ()`).
- [x] `s1_interval` (`float# & float#`): `[@@deriving unboxed_option { sentinel
      = true }]`, matching the `r1_interval` pattern.
- [x] `s2_latlng_rect` (two intervals): `[@@deriving unboxed_option { sentinel
      = true }]`, now that `s1_interval` and `r1_interval` both expose
      sentinels.
- [x] `s2_cell` (unboxed record with `id`, `face`, `level`, `orientation`,
      `uv`): plain `[@@deriving unboxed_option]`. `ppx_uopt` now classifies
      otherwise-unrecognised value-layout fields (the immediate `int`s here)
      as opaque and materialises tagged-mode placeholders via
      `(Stdlib.Obj.magic 0 : <field_type>)`. `Option.is_none` only inspects the
      leading bool tag, so the placeholder is never observed.

### Single-field record wrappers that should be `[@@unboxed]`

These wrap one value-layout field and exist only for typing. Marking them
`[@@unboxed]` removes the record header without touching callers.

- [x] `s2_cell_union.t = { cell_ids : S2_cell_id.t array }`
- [x] `s2_polyline.t = { vertices : S2_point.t array }`
- [x] `s2_lax_polyline.t = { vertices : S2_point.t array }`
- [x] `s2_shape_index.Index_cell.t = { shapes : Clipped_shape.t array }`
- Note: `s2_shape_index.shape_box = { shape : S2_shape.t }` must stay boxed
  because its entire purpose is to give the unboxed record a value layout
  for `Hashtbl` storage; leave it.

### Growable buffers and priority queues over unboxed elements

- [x] `util/binary_heap.ml` retired. Both call sites went bespoke
      because parallel arrays beat any kind-polymorphic `Make` for these
      heaps: sift_up/sift_down compares on a single column, so keeping the
      key column tightly packed is a bigger win than packing the full
      record. `s2_closest_edge_query.Cell_queue` already used parallel
      arrays for distance/cell_id/index_cell triples; `s2_region_coverer`
      now does the same for cell_id/num_children/children_start/priority,
      and the boxed `candidate` record plus `Binary_heap.Make` are gone.
- [x] `s2_builder.Point_buffer` is now a `Unboxed_vec.Make` instantiation
      (parameterised at `(float64 & float64 & float64) mod external_`).
      `Unboxed_vec` gained a `clear` operation to cover the one user the
      bespoke buffer needed.
- [ ] `util/unboxed_vec.ml` covers only pair and triple `float64` kinds.
      Extend the `[@kind ...]` list whenever a new call site needs a
      different product (e.g., a `int & int` buffer for `(shape_id,
      edge_id)` pairs in `s2_shapeutil_visit_crossing_edge_pairs`).
- [ ] `util/unboxed_array.ml` sort functor covers only a handful of
      float-product kinds. Expand as needed; e.g., `s2_shape_index` builds
      sort arrays of `build_entry` that could be sorted in-place if
      converted to an unboxed representation.

### Option boxing on mutable query / index state

- [x] `s2_loop.index_cache.crossing_query`: dropped the inner `option`
      and now build the crossing query eagerly alongside the point query.
      Both share the dominant cost (the shape index build), so an unused
      crossing query just costs one iterator allocation, and the per-loop
      `Some _` allocation is gone.
- [x] `s2_closest_edge_query` per-entry `S2_shape_index.Index_cell.t option`
      slots dropped: `S2_shape_index.Index_cell` now derives
      `unboxed_option { none = #{ shapes = [||] } }`, with the empty cell as
      the sentinel (real index cells always have at least one clipped
      shape). The per-entry `Some _ / None` allocations on the
      `process_or_enqueue` and queue paths are gone; pattern matches use
      `match%optional_u.S2_shape_index.Index_cell.Option`. `is_none` uses
      `Stdlib.( = )` on the array sentinel, so it lowers to `caml_equal`
      and the deriver applies `[@@zero_alloc assume]` (runtime cost is zero
      because the empty array is shared and the comparison short-circuits
      on length).
- [x] `s2_closest_edge_query.t.iter` is now constructed eagerly at query
      creation, so the field is `Iterator.t` (not `option`) and `ensure_iter`
      is a single field load. The iterator is a small mutable struct and
      `S2_shape_index.iterator` is idempotent over `build`, so the up-front
      cost is negligible. Separately, `Iterator`'s internal
      `cur_cell : Index_cell.t option` was migrated to `Index_cell.Option.t`,
      so `next` / `seek` / `refresh` no longer allocate per cell-id move.

### Value-layout intermediates inside shape-index builds

`s2_shape_index.ml` holds several boxed records whose fields are all float
or bits64. These allocate per clipped edge during `Build`. Migrating them
to unboxed records (where feasible) or to parallel arrays cuts GC churn
for large indexes.

- [ ] `face_edge = { shape_id : int; edge_id : int; max_level : int;
      has_interior : bool; a : R2_point.t; b : R2_point.t; v0 : S2_point.t;
      v1 : S2_point.t }`
- [ ] `clipped_edge = { face_edge : face_edge; bound : R2_rect.t }`
      (depends on the previous item).
- [ ] `build_entry = { cell_id : S2_cell_id.t; cell : Index_cell.t }`:
      record itself stays boxed (mixed `bits64 & value` layout, can't be
      unboxed for list/array storage), but storage moved from
      `build_entry list` to `build_entry Dynarray.t`. Drops the per-cell
      list-cons cell; the `Array.sort` in `rebuild_cell_ids` now operates
      on a copied flat array. Same change applied to `face_edge list array`
      (per-face accumulator in `apply_updates_internal`) -> `face_edge
      Dynarray.t array`.

### Callback / closure allocation on shape conversion

- [ ] `s2_shape.t` stores four closure fields (`edge`, `chain`,
      `chain_edge`, `chain_position`). Every `S2_loop.to_shape`,
      `S2_polygon.to_shape`, and lax-shape conversion allocates a fresh
      closure tuple. Investigate interning a per-module constant vtable
      (closures that only close over the subject value) so repeated
      `to_shape` calls are zero-alloc.
- [ ] `s2_region.Custom of methods` forces callers to keep the closure
      record live. Where concrete callers only need `contains_cell` /
      `intersects_cell` we could offer typed helpers that take the
      underlying value directly and skip the variant box.

### `Packed_float_option.Unboxed` coverage

Already adopted by `approx_equal` on `R1_interval`, `S1_interval`,
`R2_rect`, `R3_vector`, `S2_latlng`, `S2_latlng_rect`, `S2_point`,
`S2_polyline`, plus `R1_interval.project` and
`S2_pointutil.approx_equal_within_error_radians`. Extend coverage:

- [ ] Audit new APIs that will land with Tier 11 (`s2_closest_point_query`,
      `s2_hausdorff_distance_query`) and Tier 15 (`s2_polyline_simplifier`)
      to return `Packed_float_option.Unboxed.t` where a boxed
      `float option` would be tempting.
- [ ] `s2_edge_distances` exposes only raising / always-valid variants.
      Offer non-raising siblings for queries that can legitimately miss
      (e.g., perpendicular foot off the edge) that return
      `Packed_float_option.Unboxed.t`.

### Boxed `option` returns on hot paths

Several public APIs return a value-layout `option` that allocates a `Some _`
on every successful call. Each one wants either an `unboxed_option` deriver
on the result type, or a sentinel-style API rewrite.

- [x] `S2_shape_index.Index_cell.find_clipped` now returns
      `Clipped_shape.Option.t`. `Clipped_shape.t` was converted to an unboxed
      record `#{ shape_id : int; contains_center : bool; edges : int array }`
      and derives `unboxed_option { none = #{ shape_id = -1 } }`. Under
      `ppx_uopt`'s partial-override semantics, fields listed in `none = ...`
      are the `is_none` discriminators and omitted fields are payload-only
      (placeholder `Obj.magic 0`, never observed); `is_none` here is one
      integer compare on `shape_id`, statically `[@@zero_alloc]`. The build
      pipeline switched from a list accumulator to a pre-sized array because
      lists require value-layout elements.
- [x] `S2_edge_clipping.clip_to_face` and `S2_edge_clipping.clip_edge`
      now return `Clipped_uv.Option.t`. `Clipped_uv` lives as a submodule of
      `S2_edge_clipping`, with `t = #{ a : R2_point.t; b : R2_point.t }` and
      `[@@deriving sexp_of, unboxed_option { sentinel = true }]`. Callers
      branch via `match%optional_u.S2_edge_clipping.Clipped_uv.Option`.
- [x] `S2_polygon.parent : t -> int -> int option`. Now returns plain
      `int`, with `-1` for "no parent" (depth-0 loops), matching the
      convention `last_descendant` already uses for "k < 0".

### Hashtbl / Hash_set on unboxed keys

Current usages key by `int` or a value-layout wrapper. A bits64-keyed or
`(int & int)`-keyed hash set is needed to avoid wrapping:

- [x] `s2_closest_edge_query.tested_edges` keys `(shape_id, edge_id)` pairs
      packed into a single `int` via `pack_edge_key` (high 31 bits =
      shape_id, low 32 bits = edge_id). The hashtable is now keyed by
      `int` (immediate), so `Hashtbl.mem` / `Hashtbl.set` allocate
      nothing. Aliases only if a single index has more than 2G shapes or
      a single shape has more than 4G edges; both are far above any
      realistic input. A future `s2_validation_query` can reuse the same
      helper.

### Test-side boxing

- [x] Sweep `test/test_*.ml` for leftover `float_of_json_exn` (the boxed
      variant). Per AGENTS.md it is a smell outside quickcheck generators;
      fixture-driven tests should be fully `float#`-native.
      Two non-trivial holdouts remain: `test_s2_measures.ml`'s
      `List.fold` accumulator (would require switching to a
      mutable-local pattern because `'a` in `'a list` /`'a ref` must be
      value-layout), and `test_s2_cap.ml`'s encode/decode test where the
      "actual" value comes from `get_le_f64_from_string` (boxed
      bytes-to-float helper); both sides are intentionally boxed there.
- [x] Sweep for `Alcotest.(check (float _))` at sites where
      `Test_helpers.check_float_u` would apply. No raw Alcotest float
      testables in use; the project standardised on
      `Test_helpers.check_float` / `check_float_u` already, and the
      remaining boxed `check_float` call sites are the two
      intentional holdouts noted above.

---

## Style / consistency cleanups

Small, mechanical fixes that don't fit anywhere above. Tracked here so we
don't lose track of them; pick up whenever touching the affected file.

- [x] Replace the remaining `failwith` / `invalid_arg` call sites in
      `lib/` with `raise_s [%message ...]` per AGENTS.md ("Prefer
      `Core.raise_s` over `failwith` or `invalid_arg`"). The three
      previously-listed sites in `s2_shape_index` and `binary_heap` already
      use `raise_s`; the last holdout was
      `lib/s2_builder.ml:100` (`Point_buffer.get` index check).
- Naming note: `S2_pointutil.approx_equals` and
  `S2_loop.boundary_approx_equals` use the plural `_equals` suffix
  whereas `R1_interval.approx_equal`, `R2_rect.approx_equal`,
  `S2_point.approx_equal`, `S2_latlng.approx_equal`, `S2_cap.approx_equal`
  use singular `_equal`. The -s variants come from C++ (`S2::ApproxEquals`,
  `S2Loop::BoundaryApproxEquals`); the rest match the OCaml-style
  predicate convention. Leaving the inconsistency as-is for now since
  changing the -s variants would just trade one inconsistency for
  another, but flagging it here so future contributors know it was a
  conscious choice.
- [ ] `lib/r2_point.ml:20` flags `R2Edge` (pair of `R2_point.t`) as a
      possible port. No internal caller currently needs it; revisit only
      if a downstream port (closest-cell-query, polygon-rasterization)
      asks for it.
