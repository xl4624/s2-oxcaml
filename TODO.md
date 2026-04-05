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

- [ ] **s2_cell** - `S2_cell`
  - Go: `s2/cell.go` | C++: `s2cell.h`, `s2cell.cc`
  - Deps: `s2_cell_id`, `s2_point`, `s2_coords`, `r2_rect`, `r1_interval`

- [ ] **s2_latlng_rect** - `S2_latlng_rect`
  - Go: `s2/rect.go` | C++: `s2latlng_rect.h`, `s2latlng_rect.cc`
  - Deps: `r1_interval`, `s1_interval`, `s2_latlng`, `s2_point`

- [ ] **s2_cell_union** - `S2_cell_union`
  - Go: `s2/cellunion.go` | C++: `s2cell_union.h`, `s2cell_union.cc`
  - Deps: `s2_cell_id`, `s1_angle`

- [ ] **s2_edge_crossings** - `S2_edge_crossings`
  - Go: `s2/edge_crossings.go` | C++: `s2edge_crossings.h`, `s2edge_crossings.cc`
  - Deps: `s2_point`, `r3_vector`

- [ ] **s2_edge_distances** - `S2_edge_distances`
  - Go: `s2/edge_distances.go` | C++: `s2edge_distances.h`, `s2edge_distances.cc`
  - Deps: `s2_point`, `s1_angle`, `s1_chord_angle`

- [ ] **s2_centroids** - `S2_centroids`
  - Go: `s2/centroids.go` | C++: `s2centroids.h`, `s2centroids.cc`
  - Deps: `s2_point`, `r3_vector`

## Tier 5 - Higher-level (expand as needed)

- [ ] **earth** - `Earth`
  - Go: `earth/earth.go` | C++: (no direct C++ equivalent)
  - Deps: `s1_angle`, `s2_point`, `s2_latlng`
