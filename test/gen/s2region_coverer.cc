// Golden data generator for S2RegionCoverer.
//
// Covers the following upstream C++ tests:
//   - TEST(S2RegionCoverer, RandomCells)        -- single-cell coverings
//   - TEST(S2RegionCoverer, RandomCaps)         -- covering + interior covering
//   - TEST(IsCanonical, *)                      -- canonical validation
//   - TEST(CanonicalizeCovering, *)             -- canonicalize
//   - TEST(S2RegionCoverer, InteriorCovering)   -- interior covering of cell
//   difference
//   - TEST(GetFastCovering, HugeFixedLevelCovering) -- fast covering

#include "s2/s2region_coverer.h"

#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>
#include <vector>

#include "absl/random/random.h"
#include "s2/s1angle.h"
#include "s2/s1chord_angle.h"
#include "s2/s2cap.h"
#include "s2/s2cell.h"
#include "s2/s2cell_id.h"
#include "s2/s2cell_union.h"
#include "s2/s2latlng.h"
#include "s2/s2point.h"
#include "s2/s2random.h"

using json = nlohmann::json;
using std::vector;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static json cell_ids_json(const vector<S2CellId> &ids) {
    json arr = json::array();
    for (const auto &id : ids)
        arr.push_back(id.ToToken());
    return arr;
}

static json cell_ids_json(const S2CellUnion &cu) {
    return cell_ids_json(cu.cell_ids());
}

// TEST(S2RegionCoverer, RandomCells)
// Single-cell covering: for a cell, GetCovering with max_cells=1 should return
// exactly that cell.
static json gen_random_cells() {
    absl::BitGen bitgen;
    json arr = json::array();
    for (int i = 0; i < 100; ++i) {
        S2CellId id = s2random::CellId(bitgen);
        S2RegionCoverer::Options options;
        options.set_max_cells(1);
        S2RegionCoverer coverer(options);
        S2CellUnion covering = coverer.GetCovering(S2Cell(id));
        json e;
        e["cell_id"] = id.ToToken();
        e["covering"] = cell_ids_json(covering);
        arr.push_back(e);
    }
    return arr;
}

// TEST(S2RegionCoverer, RandomCaps) -- deterministic subset
// Generate coverings of caps with various options and record them for parity.
static json gen_cap_coverings() {
    absl::BitGen bitgen;
    json arr = json::array();

    struct TestCase {
        double lat, lng, radius_deg;
        int min_level, max_level, max_cells, level_mod;
    };

    TestCase cases[] = {
        // Small cap, default-ish options
        {40.0, -74.0, 0.01, 0, 30, 8, 1},
        // Medium cap, limited cells
        {0.0, 0.0, 5.0, 0, 30, 4, 1},
        // Large cap
        {60.0, 30.0, 45.0, 0, 30, 8, 1},
        // With min_level
        {-33.0, 151.0, 1.0, 4, 30, 8, 1},
        // With max_level
        {45.0, 90.0, 10.0, 0, 8, 8, 1},
        // With level_mod=2
        {10.0, -10.0, 2.0, 0, 30, 8, 2},
        // With level_mod=3
        {-20.0, 80.0, 3.0, 0, 30, 8, 3},
        // Tiny cap, many cells
        {0.0, 0.0, 0.001, 0, 30, 20, 1},
        // Very large cap
        {0.0, 0.0, 89.0, 0, 30, 8, 1},
        // Small cap, few cells
        {40.0, -74.0, 0.5, 0, 30, 3, 1},
        // min_level + level_mod
        {0.0, 0.0, 10.0, 2, 30, 8, 2},
        // Fixed level
        {40.0, -74.0, 1.0, 10, 10, 1000000, 1},
    };

    for (const auto &tc : cases) {
        S2Cap cap(S2LatLng::FromDegrees(tc.lat, tc.lng).ToPoint(),
                  S1Angle::Degrees(tc.radius_deg));
        S2RegionCoverer::Options options;
        options.set_min_level(tc.min_level);
        options.set_max_level(tc.max_level);
        options.set_max_cells(tc.max_cells);
        options.set_level_mod(tc.level_mod);
        S2RegionCoverer coverer(options);

        vector<S2CellId> covering, interior;
        coverer.GetCovering(cap, &covering);
        coverer.GetInteriorCovering(cap, &interior);

        // Verify determinism
        vector<S2CellId> covering2;
        coverer.GetCovering(cap, &covering2);

        json e;
        e["center"] = point_json(cap.center());
        e["radius_deg"] = tc.radius_deg;
        e["min_level"] = tc.min_level;
        e["max_level"] = tc.max_level;
        e["max_cells"] = tc.max_cells;
        e["level_mod"] = tc.level_mod;
        e["covering"] = cell_ids_json(covering);
        e["interior"] = cell_ids_json(interior);
        e["deterministic"] = (covering == covering2);

        // Validate covering properties
        json props = json::object();
        for (S2CellId id : covering) {
            int level = id.level();
            props["all_valid"] = id.is_valid();
            props["min_level_ok"] = (level >= tc.min_level);
            props["max_level_ok"] = (level <= tc.max_level);
            props["level_mod_ok"] =
                ((level - tc.min_level) % tc.level_mod == 0);
        }
        e["props"] = props;

        // Check interior covering: each cell must be contained by the cap
        bool interior_valid = true;
        for (S2CellId id : interior) {
            if (!cap.Contains(S2Cell(id))) {
                interior_valid = false;
                break;
            }
        }
        e["interior_valid"] = interior_valid;

        arr.push_back(e);
    }
    return arr;
}

// TEST(IsCanonical, *) -- various canonical/non-canonical coverings
static json gen_is_canonical() {
    json arr = json::array();

    auto add = [&](const std::string &name,
                   const vector<std::string> &cell_strs, int min_level,
                   int max_level, int max_cells, int level_mod, bool expected) {
        vector<S2CellId> ids;
        for (const auto &s : cell_strs)
            ids.push_back(S2CellId::FromDebugString(s));
        S2RegionCoverer::Options options;
        options.set_min_level(min_level);
        options.set_max_level(max_level);
        options.set_max_cells(max_cells);
        options.set_level_mod(level_mod);
        S2RegionCoverer coverer(options);
        bool actual = coverer.IsCanonical(ids);

        json e;
        e["name"] = name;
        e["cell_ids"] = cell_ids_json(ids);
        e["min_level"] = min_level;
        e["max_level"] = max_level;
        e["max_cells"] = max_cells;
        e["level_mod"] = level_mod;
        e["expected"] = expected;
        e["actual"] = actual;
        arr.push_back(e);
    };

    // TEST(IsCanonical, InvalidS2CellId)
    add("valid_face", {"1/"}, 0, 30, 8, 1, true);
    // Can't test invalid cell ID through token since it won't round-trip

    // TEST(IsCanonical, Unsorted)
    add("sorted", {"1/1", "1/3"}, 0, 30, 8, 1, true);
    add("unsorted", {"1/3", "1/1"}, 0, 30, 8, 1, false);

    // TEST(IsCanonical, Overlapping)
    add("non_overlapping", {"1/2", "1/33"}, 0, 30, 8, 1, true);
    add("overlapping", {"1/3", "1/33"}, 0, 30, 8, 1, false);

    // TEST(IsCanonical, MinLevel)
    add("min_level_ok", {"1/31"}, 2, 30, 8, 1, true);
    add("min_level_fail", {"1/3"}, 2, 30, 8, 1, false);

    // TEST(IsCanonical, MaxLevel)
    add("max_level_ok", {"1/31"}, 0, 2, 8, 1, true);
    add("max_level_fail", {"1/312"}, 0, 2, 8, 1, false);

    // TEST(IsCanonical, LevelMod)
    add("level_mod_ok", {"1/31"}, 0, 30, 8, 2, true);
    add("level_mod_fail", {"1/312"}, 0, 30, 8, 2, false);

    // TEST(IsCanonical, MaxCells)
    add("max_cells_ok", {"1/1", "1/3"}, 0, 30, 2, 1, true);
    add("max_cells_too_many", {"1/1", "1/3", "2/"}, 0, 30, 2, 1, false);
    add("max_cells_no_common_ancestor", {"1/123", "2/1", "3/0122"}, 0, 30, 2, 1,
        true);

    // TEST(IsCanonical, Normalized)
    add("normalized_ok", {"1/01", "1/02", "1/03", "1/10", "1/11"}, 0, 30, 8, 1,
        true);
    add("normalized_fail", {"1/00", "1/01", "1/02", "1/03", "1/10"}, 0, 30, 8,
        1, false);
    add("normalized_ok_with_gap", {"0/22", "1/01", "1/02", "1/03", "1/10"}, 0,
        30, 8, 1, true);
    add("normalized_fail_with_gap", {"0/22", "1/00", "1/01", "1/02", "1/03"}, 0,
        30, 8, 1, false);

    // level_mod=2 normalized test
    add("level_mod2_normalized_ok",
        {"1/1101", "1/1102", "1/1103", "1/1110", "1/1111", "1/1112", "1/1113",
         "1/1120", "1/1121", "1/1122", "1/1123", "1/1130", "1/1131", "1/1132",
         "1/1133", "1/1200"},
        0, 30, 20, 2, true);
    add("level_mod2_normalized_fail",
        {"1/1100", "1/1101", "1/1102", "1/1103", "1/1110", "1/1111", "1/1112",
         "1/1113", "1/1120", "1/1121", "1/1122", "1/1123", "1/1130", "1/1131",
         "1/1132", "1/1133"},
        0, 30, 20, 2, false);

    return arr;
}

// TEST(CanonicalizeCovering, *)
static json gen_canonicalize() {
    json arr = json::array();

    auto add = [&](const std::string &name,
                   const vector<std::string> &input_strs, int min_level,
                   int max_level, int max_cells, int level_mod) {
        vector<S2CellId> input;
        for (const auto &s : input_strs)
            input.push_back(S2CellId::FromDebugString(s));
        S2RegionCoverer::Options options;
        options.set_min_level(min_level);
        options.set_max_level(max_level);
        options.set_max_cells(max_cells);
        options.set_level_mod(level_mod);
        S2RegionCoverer coverer(options);
        coverer.CanonicalizeCovering(&input);

        json e;
        e["name"] = name;
        e["result"] = cell_ids_json(input);
        e["min_level"] = min_level;
        e["max_level"] = max_level;
        e["max_cells"] = max_cells;
        e["level_mod"] = level_mod;
        e["is_canonical"] = coverer.IsCanonical(input);
        arr.push_back(e);
    };

    // TEST(CanonicalizeCovering, UnsortedDuplicateCells)
    add("unsorted_duplicates", {"1/200", "1/13122", "1/20", "1/131", "1/13100"},
        0, 30, 8, 1);

    // TEST(CanonicalizeCovering, MaxLevelExceeded)
    add("max_level_exceeded", {"0/3001", "0/3002", "4/012301230123"}, 0, 2, 8,
        1);

    // TEST(CanonicalizeCovering, WrongLevelMod)
    add("wrong_level_mod", {"0/0", "1/11", "2/222", "3/3333"}, 1, 30, 8, 3);

    // TEST(CanonicalizeCovering, ReplacedByParent)
    add("replaced_by_parent",
        {"0/00", "0/01", "0/02", "0/03", "0/10", "0/11", "0/12", "0/13", "0/20",
         "0/21", "0/22", "0/23", "0/30", "0/31", "0/32", "0/33"},
        0, 30, 8, 2);

    // TEST(CanonicalizeCovering, MaxCellsMergesSmallest)
    add("max_cells_merges_smallest",
        {"0/", "1/0", "1/1", "2/01300", "2/0131313"}, 0, 30, 3, 1);

    // TEST(CanonicalizeCovering, MaxCellsMergesRepeatedly)
    add("max_cells_merges_repeatedly",
        {"0/0121", "0/0123", "1/0", "1/1", "1/2", "1/30", "1/32", "1/33",
         "1/311", "1/312", "1/313", "1/3100", "1/3101", "1/3103", "1/31021",
         "1/31023"},
        0, 30, 8, 1);

    return arr;
}

// TEST(S2RegionCoverer, InteriorCovering)
static json gen_interior_covering() {
    absl::BitGen bitgen;
    json arr = json::array();
    const int level = 12;

    // Run a few times to get stable test cases
    for (int i = 0; i < 5; ++i) {
        S2CellId small_cell =
            S2CellId(s2random::Point(bitgen)).parent(level + 2);
        S2CellId large_cell = small_cell.parent(level);
        S2CellUnion diff =
            S2CellUnion({large_cell}).Difference(S2CellUnion({small_cell}));

        S2RegionCoverer::Options options;
        options.set_max_cells(3);
        options.set_max_level(level + 3);
        options.set_min_level(level);
        S2RegionCoverer coverer(options);
        vector<S2CellId> interior;
        coverer.GetInteriorCovering(diff, &interior);

        json e;
        e["large_cell"] = large_cell.ToToken();
        e["small_cell"] = small_cell.ToToken();
        e["diff_cells"] = cell_ids_json(diff);
        e["min_level"] = level;
        e["max_level"] = level + 3;
        e["max_cells"] = 3;
        e["interior"] = cell_ids_json(interior);
        e["num_cells"] = (int)interior.size();

        // Each cell should be at level+1
        json levels = json::array();
        for (S2CellId id : interior)
            levels.push_back(id.level());
        e["cell_levels"] = levels;
        arr.push_back(e);
    }
    return arr;
}

// TEST(GetFastCovering, HugeFixedLevelCovering)
static json gen_fast_covering() {
    json arr = json::array();

    // Large cell with min_level forcing many children
    {
        S2CellId cell_id = S2CellId::FromDebugString("1/23");
        S2RegionCoverer::Options options;
        options.set_min_level(10);
        S2RegionCoverer coverer(options);
        vector<S2CellId> covering;
        coverer.GetFastCovering(S2Cell(cell_id), &covering);

        json e;
        e["name"] = "huge_fixed_level";
        e["cell_id"] = cell_id.ToToken();
        e["min_level"] = 10;
        e["max_level"] = 30;
        e["covering_size"] = (int)covering.size();
        e["min_covering_size"] = (1 << 16);
        arr.push_back(e);
    }

    // Normal fast covering of a cap
    {
        S2Cap cap(S2LatLng::FromDegrees(40, -74).ToPoint(),
                  S1Angle::Degrees(1.0));
        S2RegionCoverer coverer;
        vector<S2CellId> covering;
        coverer.GetFastCovering(cap, &covering);

        json e;
        e["name"] = "cap_fast";
        e["center"] = point_json(cap.center());
        e["radius_deg"] = 1.0;
        e["min_level"] = 0;
        e["max_level"] = 30;
        e["max_cells"] = 8;
        e["level_mod"] = 1;
        e["covering"] = cell_ids_json(covering);
        arr.push_back(e);
    }

    return arr;
}

// Covering of cell unions and rects (to exercise S2Region interface)
static json gen_region_coverings() {
    json arr = json::array();

    // Cover a cell union
    {
        S2CellUnion cu({
            S2CellId(S2LatLng::FromDegrees(40.5, -74.5).ToPoint()).parent(8),
            S2CellId(S2LatLng::FromDegrees(40.5, -73.5).ToPoint()).parent(8),
            S2CellId(S2LatLng::FromDegrees(41, -74).ToPoint()).parent(8),
        });
        S2RegionCoverer::Options options;
        options.set_max_cells(6);
        S2RegionCoverer coverer(options);
        vector<S2CellId> covering;
        coverer.GetCovering(cu, &covering);

        json e;
        e["name"] = "cell_union";
        e["input_ids"] = cell_ids_json(cu);
        e["max_cells"] = 6;
        e["covering"] = cell_ids_json(covering);
        arr.push_back(e);
    }

    // Cover a single cell
    {
        S2CellId id =
            S2CellId(S2LatLng::FromDegrees(0, 0).ToPoint()).parent(10);
        S2RegionCoverer::Options options;
        options.set_max_cells(1);
        S2RegionCoverer coverer(options);
        S2CellUnion covering = coverer.GetCovering(S2Cell(id));

        json e;
        e["name"] = "single_cell";
        e["cell_id"] = id.ToToken();
        e["max_cells"] = 1;
        e["covering"] = cell_ids_json(covering);
        arr.push_back(e);
    }

    return arr;
}

int main() {
    json root;
    root["random_cells"] = gen_random_cells();
    root["cap_coverings"] = gen_cap_coverings();
    root["is_canonical"] = gen_is_canonical();
    root["canonicalize"] = gen_canonicalize();
    root["interior_covering"] = gen_interior_covering();
    root["fast_covering"] = gen_fast_covering();
    root["region_coverings"] = gen_region_coverings();
    std::cout << root.dump(2) << std::endl;
    return 0;
}
