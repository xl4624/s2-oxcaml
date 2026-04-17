// Minimal C++ region-coverer benchmark, mirroring the Go test in
// ~/dev/geo/s2/regioncoverer_test.go and the OCaml bench in
// bench/bench_region_coverer.ml.
//
// Output format (one line per sub-bench): NAME N ITERS NS_PER_OP
// e.g.:   Cap 2 21308 17523.4
//
// Usage: ./bench [quota_seconds]
//   default quota is 1s per sub-benchmark.

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <vector>

#include "absl/random/random.h"
#include "s2/s1angle.h"
#include "s2/s2cap.h"
#include "s2/s2cell.h"
#include "s2/s2cell_id.h"
#include "s2/s2cell_union.h"
#include "s2/s2metrics.h"
#include "s2/s2point.h"
#include "s2/s2region_coverer.h"

static constexpr int kNumRegions = 1000;

static absl::BitGen& rand_gen() {
  static absl::BitGen gen(std::seed_seq{0x13375eed});
  return gen;
}

static double UniformFloat(double lo, double hi) {
  return absl::Uniform(rand_gen(), lo, hi);
}

static S2Point RandomPoint() {
  return S2Point(UniformFloat(-1.0, 1.0), UniformFloat(-1.0, 1.0),
                 UniformFloat(-1.0, 1.0))
      .Normalize();
}

static S2CellId RandomCellIdForLevel(int level) {
  int face = absl::Uniform(rand_gen(), 0, S2CellId::kNumFaces);
  uint64_t pos =
      absl::Uniform<uint64_t>(rand_gen()) & ((1ULL << S2CellId::kPosBits) - 1);
  return S2CellId::FromFacePosLevel(face, pos, level);
}

static S2CellUnion RandomCellUnionAtLevel(int level, int n) {
  std::vector<S2CellId> ids;
  ids.reserve(n);
  for (int i = 0; i < n; ++i) ids.push_back(RandomCellIdForLevel(level));
  return S2CellUnion(std::move(ids));  // constructor normalizes
}

static S2Cap RandomCap(double min_area, double max_area) {
  double r = absl::Uniform(rand_gen(), 0.0, 1.0);
  double area = max_area * std::pow(min_area / max_area, r);
  return S2Cap::FromCenterArea(RandomPoint(), area);
}

// Auto-scale the inner loop so each scenario runs at least [quota] seconds.
// Returns ns/op.
template <class F>
static double BenchAuto(F run, double quota) {
  int n = 100;
  double elapsed = 0.0;
  int total_iters = 0;
  while (elapsed < quota) {
    auto start = std::chrono::steady_clock::now();
    for (int i = 0; i < n; ++i) run(i);
    auto end = std::chrono::steady_clock::now();
    elapsed = std::chrono::duration<double>(end - start).count();
    total_iters = n;
    if (elapsed < quota) n *= 2;
  }
  return elapsed * 1e9 / total_iters;
}

int main(int argc, char** argv) {
  double quota = (argc > 1) ? std::atof(argv[1]) : 1.0;

  S2RegionCoverer::Options opts;
  opts.set_min_level(0);
  opts.set_max_level(30);
  opts.set_level_mod(1);
  opts.set_max_cells(8);
  S2RegionCoverer coverer(opts);

  const double avg_area_max_level =
      S2::kAvgArea.GetValue(S2CellId::kMaxLevel);
  const double min_a = 0.1 * avg_area_max_level;
  const double max_a = 4.0 * M_PI;

  // Cap: n=2..16 (all identical inputs - matches Go's param).
  for (int n = 2; n <= 16; ++n) {
    std::vector<S2Cap> regions;
    regions.reserve(kNumRegions);
    for (int i = 0; i < kNumRegions; ++i)
      regions.push_back(RandomCap(min_a, max_a));
    double ns = BenchAuto(
        [&](int i) {
          S2CellUnion cov = coverer.GetCovering(regions[i % kNumRegions]);
          asm volatile("" ::"r"(&cov) : "memory");  // prevent DCE
        },
        quota);
    std::printf("Cap %d %.1f\n", n, ns);
  }

  // Cell: n=2..16, level in [MaxLevel-n, MaxLevel]. Same as Go.
  for (int n = 2; n <= 16; ++n) {
    std::vector<S2Cell> regions;
    regions.reserve(kNumRegions);
    for (int i = 0; i < kNumRegions; ++i) {
      int level = S2CellId::kMaxLevel - absl::Uniform(rand_gen(), 0, n);
      regions.push_back(S2Cell(RandomCellIdForLevel(level)));
    }
    double ns = BenchAuto(
        [&](int i) {
          S2CellUnion cov = coverer.GetCovering(regions[i % kNumRegions]);
          asm volatile("" ::"r"(&cov) : "memory");
        },
        quota);
    std::printf("Cell %d %.1f\n", n, ns);
  }

  // CellUnion: n=2..10, size = 2^n, level 22 (matches our OCaml + patched Go).
  for (int n = 2; n <= 10; ++n) {
    int size = 1 << n;
    std::vector<S2CellUnion> regions;
    regions.reserve(kNumRegions);
    for (int i = 0; i < kNumRegions; ++i)
      regions.push_back(RandomCellUnionAtLevel(22, size));
    double ns = BenchAuto(
        [&](int i) {
          S2CellUnion cov = coverer.GetCovering(regions[i % kNumRegions]);
          asm volatile("" ::"r"(&cov) : "memory");
        },
        quota);
    std::printf("CellUnion %d %.1f\n", n, ns);
  }

  return 0;
}
