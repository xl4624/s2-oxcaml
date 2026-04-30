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

- [ ] **s2_shape_index_measures** - `S2_shape_index_measures`
  - C++: `s2shape_index_measures.h`, `.cc` (~100 lines)
  - Deps: `s2_shape_index`, `s2_shape_measures`

- [ ] **s2_shape_index_region** - `S2_shape_index_region`
      (wraps an `S2_shape_index` as an `S2_region` - supplies `Contains`,
      `MayIntersect`, bounds for cells. Required before layered operations
      that expect a generic region.)
  - Go: `s2/shapeindex_region.go` | C++: `s2shape_index_region.h` (header-only, ~475 lines)
  - Deps: `s2_shape_index`, `s2_region`, `s2_contains_point_query`, `s2_crossing_edge_query`

- [ ] **s2_shape_index_buffered_region** - `S2_shape_index_buffered_region`
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
- [ ] `s2_latlng_rect`: exact `Intersects(const S2Cell&)`
- [ ] `s2_latlng_rect`: `ExpandedByDistance`, `BoundaryIntersects`,
      `IntersectsLngEdge`, `IntersectsLatEdge`

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

- [ ] `util/binary_heap.ml` currently requires `type t : value` for its
      element, so `s2_closest_edge_query` and `s2_region_coverer` still box
      their priority-queue entries. Add a ppx-templated `Make` variant
      covering the element kinds those queues actually use (distance +
      payload pair, typically `float64 & value`).
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
- [ ] `s2_closest_edge_query.t.iter : S2_shape_index.Iterator.t option`
      still boxes on iterator creation; needs an `Unboxed_option` on
      `S2_shape_index.Iterator` (or an explicit "valid" flag) - more
      involved because `Iterator` carries internal mutable state.

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
      paired with its parallel array during intermediate sort; either mark
      `[@@unboxed]` once `cell_id` lives in a struct field supporting it,
      or migrate to two parallel arrays.

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
