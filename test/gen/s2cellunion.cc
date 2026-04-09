// Golden data generator for S2CellUnion.
// Mirrors s2geometry/src/s2/s2cell_union_test.cc

#include <cstdint>
#include <iostream>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/s1angle.h"
#include "s2/s2cell.h"
#include "s2/s2cell_id.h"
#include "s2/s2cell_union.h"
#include "s2/s2metrics.h"

using json = nlohmann::json;

static json cell_id_str(S2CellId id) {
    return std::to_string(id.id());
}

static json cell_ids_json(const S2CellUnion &cu) {
    json arr = json::array();
    for (S2CellId id : cu) {
        arr.push_back(cell_id_str(id));
    }
    return arr;
}

static json cell_ids_vec_json(const std::vector<S2CellId> &ids) {
    json arr = json::array();
    for (S2CellId id : ids) {
        arr.push_back(cell_id_str(id));
    }
    return arr;
}

// TEST(S2CellUnion, WholeSphere)
static void add_whole_sphere(json &root) {
    S2CellUnion ws = S2CellUnion::WholeSphere();
    root["whole_sphere"] = {
        {"cell_ids", cell_ids_json(ws)},
        {"num_cells", ws.num_cells()},
        {"leaf_cells_covered", std::to_string(ws.LeafCellsCovered())}};
}

// TEST(S2CellUnion, IsNormalized)
static void add_is_normalized(json &root) {
    S2CellId id(S2Point(1, 0, 0));
    id = id.parent(10);
    std::vector<S2CellId> children = {id.child(0), id.child(1), id.child(2),
                                      id.child(3)};
    json cases = json::array();
    // 4 children of same parent - valid but not normalized
    cases.push_back({{"cell_ids", cell_ids_vec_json(children)},
                     {"is_valid", true},
                     {"is_normalized", false}});
    // After normalization should be just the parent
    S2CellUnion cu(children);
    cases.push_back({{"cell_ids", cell_ids_json(cu)},
                     {"is_valid", cu.IsValid()},
                     {"is_normalized", cu.IsNormalized()}});
    root["is_normalized"] = cases;
}

// TEST(S2CellUnion, Normalize) - various normalization cases
static void add_normalize(json &root) {
    json cases = json::array();

    // Case 1: face cell and its children - should normalize to just the face
    {
        S2CellId face0 = S2CellId::FromFace(0);
        std::vector<S2CellId> input = {face0, face0.child(0), face0.child(1)};
        S2CellUnion cu(input);
        cases.push_back({{"label", "face_with_children"},
                         {"input", cell_ids_vec_json(input)},
                         {"normalized", cell_ids_json(cu)}});
    }

    // Case 2: duplicate cells
    {
        S2CellId id = S2CellId::FromFace(2).child(1);
        std::vector<S2CellId> input = {id, id, id};
        S2CellUnion cu(input);
        cases.push_back({{"label", "duplicates"},
                         {"input", cell_ids_vec_json(input)},
                         {"normalized", cell_ids_json(cu)}});
    }

    // Case 3: all 4 siblings merge to parent, and cascading
    {
        S2CellId parent = S2CellId::FromFace(3).child(2);
        std::vector<S2CellId> input = {parent.child(0), parent.child(1),
                                       parent.child(2), parent.child(3)};
        S2CellUnion cu(input);
        cases.push_back({{"label", "four_siblings_merge"},
                         {"input", cell_ids_vec_json(input)},
                         {"normalized", cell_ids_json(cu)}});
    }

    // Case 4: empty
    {
        std::vector<S2CellId> input;
        S2CellUnion cu(input);
        cases.push_back({{"label", "empty"},
                         {"input", cell_ids_vec_json(input)},
                         {"normalized", cell_ids_json(cu)}});
    }

    // Case 5: unsorted input
    {
        S2CellId a = S2CellId::FromFace(5);
        S2CellId b = S2CellId::FromFace(0);
        S2CellId c = S2CellId::FromFace(3);
        std::vector<S2CellId> input = {a, b, c};
        S2CellUnion cu(input);
        cases.push_back({{"label", "unsorted"},
                         {"input", cell_ids_vec_json(input)},
                         {"normalized", cell_ids_json(cu)}});
    }

    root["normalize"] = cases;
}

// TEST(S2CellUnion, ContainsCellID) and IntersectsCellID
static void add_contains_intersects(json &root) {
    json cases = json::array();

    // Build a cell union from face 1
    S2CellId face1 = S2CellId::FromFace(1);
    S2CellUnion cu({face1});

    // Test containment / intersection for various cell ids
    struct TestCase {
        std::string label;
        S2CellId test_id;
    };

    std::vector<TestCase> tests = {
        {"face1_itself", face1},
        {"face1_child0", face1.child(0)},
        {"face1_child_begin_max", face1.child_begin(S2CellId::kMaxLevel)},
        {"face1_child_end_prev", face1.child_end().prev()},
        {"face0", S2CellId::FromFace(0)},
        {"face2", S2CellId::FromFace(2)},
        {"face1_parent_nonexistent", S2CellId::FromFace(1)},  // face1 itself
    };

    for (const auto &tc : tests) {
        cases.push_back({{"label", tc.label},
                         {"union_ids", cell_ids_json(cu)},
                         {"test_id", cell_id_str(tc.test_id)},
                         {"contains", cu.Contains(tc.test_id)},
                         {"intersects", cu.Intersects(tc.test_id)}});
    }

    // Multi-cell union
    S2CellId a = S2CellId::FromFace(0).child(1).child(2);
    S2CellId b = S2CellId::FromFace(3).child(0);
    S2CellUnion cu2({a, b});
    std::vector<TestCase> tests2 = {
        {"cell_a", a},
        {"cell_b", b},
        {"child_of_a", a.child(0)},
        {"parent_of_a", a.parent()},
        {"disjoint_face5", S2CellId::FromFace(5)},
        {"b_child_end_max", b.child_begin(S2CellId::kMaxLevel)},
    };

    for (const auto &tc : tests2) {
        cases.push_back({{"label", std::string("multi_") + tc.label},
                         {"union_ids", cell_ids_json(cu2)},
                         {"test_id", cell_id_str(tc.test_id)},
                         {"contains", cu2.Contains(tc.test_id)},
                         {"intersects", cu2.Intersects(tc.test_id)}});
    }

    root["contains_intersects"] = cases;
}

// TEST(S2CellUnion, ContainsUnion) and IntersectsUnion
static void add_contains_intersects_union(json &root) {
    json cases = json::array();

    S2CellId face1 = S2CellId::FromFace(1);
    S2CellUnion a_cu({face1});
    S2CellUnion b_cu({face1.child(0), face1.child(1)});
    S2CellUnion c_cu({S2CellId::FromFace(2)});
    S2CellUnion empty_cu;

    struct Pair {
        std::string label;
        const S2CellUnion &x;
        const S2CellUnion &y;
    };

    std::vector<Pair> pairs = {
        {"a_contains_b", a_cu, b_cu},
        {"b_not_contains_a", b_cu, a_cu},
        {"a_not_contains_c", a_cu, c_cu},
        {"empty_contains_empty", empty_cu, empty_cu},
        {"a_contains_empty", a_cu, empty_cu},
        {"empty_not_contains_a", empty_cu, a_cu},
        {"a_intersects_b", a_cu, b_cu},
        {"a_not_intersects_c", a_cu, c_cu},
        {"empty_not_intersects_a", empty_cu, a_cu},
    };

    for (const auto &p : pairs) {
        cases.push_back({{"label", p.label},
                         {"x", cell_ids_json(p.x)},
                         {"y", cell_ids_json(p.y)},
                         {"contains", p.x.Contains(p.y)},
                         {"intersects", p.x.Intersects(p.y)}});
    }

    root["contains_intersects_union"] = cases;
}

// TEST(S2CellUnion, Union/Intersection/Difference)
static void add_boolean_ops(json &root) {
    json cases = json::array();

    // Case 1: disjoint
    {
        S2CellUnion x({S2CellId::FromFace(0)});
        S2CellUnion y({S2CellId::FromFace(1)});
        cases.push_back({{"label", "disjoint_faces"},
                         {"x", cell_ids_json(x)},
                         {"y", cell_ids_json(y)},
                         {"union", cell_ids_json(x.Union(y))},
                         {"intersection", cell_ids_json(x.Intersection(y))},
                         {"difference", cell_ids_json(x.Difference(y))}});
    }

    // Case 2: one contains other
    {
        S2CellId face2 = S2CellId::FromFace(2);
        S2CellUnion x({face2});
        S2CellUnion y({face2.child(0), face2.child(1)});
        cases.push_back({{"label", "parent_contains_children"},
                         {"x", cell_ids_json(x)},
                         {"y", cell_ids_json(y)},
                         {"union", cell_ids_json(x.Union(y))},
                         {"intersection", cell_ids_json(x.Intersection(y))},
                         {"difference", cell_ids_json(x.Difference(y))}});
    }

    // Case 3: overlapping
    {
        S2CellId face3 = S2CellId::FromFace(3);
        S2CellUnion x({face3.child(0), face3.child(1)});
        S2CellUnion y({face3.child(1), face3.child(2)});
        cases.push_back({{"label", "overlapping"},
                         {"x", cell_ids_json(x)},
                         {"y", cell_ids_json(y)},
                         {"union", cell_ids_json(x.Union(y))},
                         {"intersection", cell_ids_json(x.Intersection(y))},
                         {"difference", cell_ids_json(x.Difference(y))}});
    }

    // Case 4: empty x
    {
        S2CellUnion x;
        S2CellUnion y({S2CellId::FromFace(4)});
        cases.push_back({{"label", "empty_x"},
                         {"x", cell_ids_json(x)},
                         {"y", cell_ids_json(y)},
                         {"union", cell_ids_json(x.Union(y))},
                         {"intersection", cell_ids_json(x.Intersection(y))},
                         {"difference", cell_ids_json(x.Difference(y))}});
    }

    // Case 5: both empty
    {
        S2CellUnion x;
        S2CellUnion y;
        cases.push_back({{"label", "both_empty"},
                         {"x", cell_ids_json(x)},
                         {"y", cell_ids_json(y)},
                         {"union", cell_ids_json(x.Union(y))},
                         {"intersection", cell_ids_json(x.Intersection(y))},
                         {"difference", cell_ids_json(x.Difference(y))}});
    }

    root["boolean_ops"] = cases;
}

// TEST(S2CellUnion, IntersectionWithCellId)
static void add_intersection_with_cell_id(json &root) {
    json cases = json::array();

    // Case 1: union contains the cell id
    {
        S2CellId face1 = S2CellId::FromFace(1);
        S2CellUnion cu({face1});
        S2CellId child = face1.child(2);
        S2CellUnion result = cu.Intersection(child);
        cases.push_back({{"label", "union_contains_id"},
                         {"union_ids", cell_ids_json(cu)},
                         {"cell_id", cell_id_str(child)},
                         {"result", cell_ids_json(result)}});
    }

    // Case 2: cell id contains some union cells
    {
        S2CellId face2 = S2CellId::FromFace(2);
        S2CellUnion cu({face2.child(0), face2.child(1), face2.child(2),
                        S2CellId::FromFace(4)});
        S2CellUnion result = cu.Intersection(face2);
        cases.push_back({{"label", "id_contains_some"},
                         {"union_ids", cell_ids_json(cu)},
                         {"cell_id", cell_id_str(face2)},
                         {"result", cell_ids_json(result)}});
    }

    // Case 3: disjoint
    {
        S2CellUnion cu({S2CellId::FromFace(0)});
        S2CellId id = S2CellId::FromFace(5);
        S2CellUnion result = cu.Intersection(id);
        cases.push_back({{"label", "disjoint"},
                         {"union_ids", cell_ids_json(cu)},
                         {"cell_id", cell_id_str(id)},
                         {"result", cell_ids_json(result)}});
    }

    root["intersection_with_cell_id"] = cases;
}

// TEST(S2CellUnion, FromMinMax)
static void add_from_min_max(json &root) {
    json cases = json::array();

    // Face 0 range_min to range_min (single leaf)
    {
        S2CellId face0 = S2CellId::FromFace(0);
        S2CellId mn = face0.range_min();
        S2CellId mx = face0.range_min();
        auto cu = S2CellUnion::FromMinMax(mn, mx);
        cases.push_back(
            {{"label", "single_leaf"},
             {"min_id", cell_id_str(mn)},
             {"max_id", cell_id_str(mx)},
             {"result", cell_ids_json(cu)},
             {"first_range_min",
              cell_id_str(cu.cell_ids().front().range_min())},
             {"last_range_max", cell_id_str(cu.cell_ids().back().range_max())},
             {"is_normalized", cu.IsNormalized()}});
    }

    // Face 0 full range
    {
        S2CellId face0 = S2CellId::FromFace(0);
        S2CellId mn = face0.range_min();
        S2CellId mx = face0.range_max();
        auto cu = S2CellUnion::FromMinMax(mn, mx);
        cases.push_back(
            {{"label", "face0_full"},
             {"min_id", cell_id_str(mn)},
             {"max_id", cell_id_str(mx)},
             {"result", cell_ids_json(cu)},
             {"first_range_min",
              cell_id_str(cu.cell_ids().front().range_min())},
             {"last_range_max", cell_id_str(cu.cell_ids().back().range_max())},
             {"is_normalized", cu.IsNormalized()}});
    }

    // Face 5 full range
    {
        S2CellId face5 = S2CellId::FromFace(5);
        S2CellId mn = face5.range_min();
        S2CellId mx = face5.range_max();
        auto cu = S2CellUnion::FromMinMax(mn, mx);
        cases.push_back(
            {{"label", "face5_full"},
             {"min_id", cell_id_str(mn)},
             {"max_id", cell_id_str(mx)},
             {"result", cell_ids_json(cu)},
             {"first_range_min",
              cell_id_str(cu.cell_ids().front().range_min())},
             {"last_range_max", cell_id_str(cu.cell_ids().back().range_max())},
             {"is_normalized", cu.IsNormalized()}});
    }

    root["from_min_max"] = cases;
}

// TEST(S2CellUnion, FromBeginEnd) - empty cases + full sphere
static void add_from_begin_end(json &root) {
    json cases = json::array();

    // Empty range at begin
    {
        S2CellId id_begin = S2CellId::Begin(S2CellId::kMaxLevel);
        auto cu = S2CellUnion::FromBeginEnd(id_begin, id_begin);
        cases.push_back({{"label", "empty_at_begin"},
                         {"begin", cell_id_str(id_begin)},
                         {"end", cell_id_str(id_begin)},
                         {"result", cell_ids_json(cu)},
                         {"num_cells", cu.num_cells()}});
    }

    // Empty range at end
    {
        S2CellId id_end = S2CellId::End(S2CellId::kMaxLevel);
        auto cu = S2CellUnion::FromBeginEnd(id_end, id_end);
        cases.push_back({{"label", "empty_at_end"},
                         {"begin", cell_id_str(id_end)},
                         {"end", cell_id_str(id_end)},
                         {"result", cell_ids_json(cu)},
                         {"num_cells", cu.num_cells()}});
    }

    // Full sphere
    {
        S2CellId id_begin = S2CellId::Begin(S2CellId::kMaxLevel);
        S2CellId id_end = S2CellId::End(S2CellId::kMaxLevel);
        auto cu = S2CellUnion::FromBeginEnd(id_begin, id_end);
        cases.push_back({{"label", "full_sphere"},
                         {"begin", cell_id_str(id_begin)},
                         {"end", cell_id_str(id_end)},
                         {"result", cell_ids_json(cu)},
                         {"num_cells", cu.num_cells()}});
    }

    root["from_begin_end"] = cases;
}

// TEST(S2CellUnion, Denormalize)
static void add_denormalize(json &root) {
    json cases = json::array();

    // Single face cell denormalized to level 1
    {
        S2CellUnion cu({S2CellId::FromFace(0)});
        std::vector<S2CellId> output;
        cu.Denormalize(1, 1, &output);
        cases.push_back({{"label", "face0_to_level1"},
                         {"input", cell_ids_json(cu)},
                         {"min_level", 1},
                         {"level_mod", 1},
                         {"result", cell_ids_vec_json(output)}});
    }

    // Face cell with level_mod=2
    {
        S2CellUnion cu({S2CellId::FromFace(1)});
        std::vector<S2CellId> output;
        cu.Denormalize(0, 2, &output);
        cases.push_back({{"label", "face1_levelmod2"},
                         {"input", cell_ids_json(cu)},
                         {"min_level", 0},
                         {"level_mod", 2},
                         {"result", cell_ids_vec_json(output)}});
    }

    // Already at sufficient level
    {
        S2CellId cell = S2CellId::FromFace(2).child(1).child(3);
        S2CellUnion cu({cell});
        std::vector<S2CellId> output;
        cu.Denormalize(0, 1, &output);
        cases.push_back({{"label", "already_fine"},
                         {"input", cell_ids_json(cu)},
                         {"min_level", 0},
                         {"level_mod", 1},
                         {"result", cell_ids_vec_json(output)}});
    }

    // Empty
    {
        S2CellUnion cu;
        std::vector<S2CellId> output;
        cu.Denormalize(0, 2, &output);
        cases.push_back({{"label", "empty"},
                         {"input", cell_ids_json(cu)},
                         {"min_level", 0},
                         {"level_mod", 2},
                         {"result", cell_ids_vec_json(output)}});
    }

    root["denormalize"] = cases;
}

// TEST(S2CellUnion, LeafCellsCovered)
static void add_leaf_cells_covered(json &root) {
    json cases = json::array();

    // Empty
    {
        S2CellUnion cu;
        cases.push_back(
            {{"label", "empty"},
             {"cell_ids", cell_ids_json(cu)},
             {"leaf_cells_covered", std::to_string(cu.LeafCellsCovered())}});
    }

    // Single leaf
    {
        S2CellId leaf = S2CellId::FromFace(0).child_begin(S2CellId::kMaxLevel);
        S2CellUnion cu({leaf});
        cases.push_back(
            {{"label", "single_leaf"},
             {"cell_ids", cell_ids_json(cu)},
             {"leaf_cells_covered", std::to_string(cu.LeafCellsCovered())}});
    }

    // Face 0
    {
        S2CellUnion cu({S2CellId::FromFace(0)});
        cases.push_back(
            {{"label", "face0"},
             {"cell_ids", cell_ids_json(cu)},
             {"leaf_cells_covered", std::to_string(cu.LeafCellsCovered())}});
    }

    // Whole sphere
    {
        S2CellUnion cu = S2CellUnion::WholeSphere();
        cases.push_back(
            {{"label", "whole_sphere"},
             {"cell_ids", cell_ids_json(cu)},
             {"leaf_cells_covered", std::to_string(cu.LeafCellsCovered())}});
    }

    // Mixed levels (from C++ test)
    {
        std::vector<S2CellId> ids;
        ids.push_back(S2CellId::FromFace(0));
        ids.push_back(S2CellId::FromFace(1).child_begin(1));
        ids.push_back(S2CellId::FromFace(2).child_begin(2));
        ids.push_back(S2CellId::FromFace(2).child_end(2).prev());
        ids.push_back(S2CellId::FromFace(3).child_begin(14));
        ids.push_back(S2CellId::FromFace(4).child_begin(27));
        ids.push_back(S2CellId::FromFace(4).child_end(15).prev());
        ids.push_back(S2CellId::FromFace(5).child_begin(30));
        S2CellUnion cu(ids);
        cases.push_back(
            {{"label", "mixed_levels"},
             {"cell_ids", cell_ids_json(cu)},
             {"leaf_cells_covered", std::to_string(cu.LeafCellsCovered())}});
    }

    root["leaf_cells_covered"] = cases;
}

// TEST(S2CellUnion, Area)
static void add_area(json &root) {
    json cases = json::array();

    // Single face
    {
        S2CellUnion cu({S2CellId::FromFace(0)});
        cases.push_back({{"label", "face0"},
                         {"cell_ids", cell_ids_json(cu)},
                         {"average_based_area", cu.AverageBasedArea()},
                         {"approx_area", cu.ApproxArea()},
                         {"exact_area", cu.ExactArea()}});
    }

    // Two faces
    {
        S2CellUnion cu({S2CellId::FromFace(0), S2CellId::FromFace(1)});
        cases.push_back({{"label", "two_faces"},
                         {"cell_ids", cell_ids_json(cu)},
                         {"average_based_area", cu.AverageBasedArea()},
                         {"approx_area", cu.ApproxArea()},
                         {"exact_area", cu.ExactArea()}});
    }

    // Whole sphere
    {
        S2CellUnion cu = S2CellUnion::WholeSphere();
        cases.push_back({{"label", "whole_sphere"},
                         {"cell_ids", cell_ids_json(cu)},
                         {"average_based_area", cu.AverageBasedArea()},
                         {"approx_area", cu.ApproxArea()},
                         {"exact_area", cu.ExactArea()}});
    }

    // Empty
    {
        S2CellUnion cu;
        cases.push_back({{"label", "empty"},
                         {"cell_ids", cell_ids_json(cu)},
                         {"average_based_area", cu.AverageBasedArea()},
                         {"approx_area", cu.ApproxArea()},
                         {"exact_area", cu.ExactArea()}});
    }

    // Small region
    {
        S2CellId cell = S2CellId::FromFace(3).child(1).child(2).child(0);
        S2CellUnion cu({cell});
        cases.push_back({{"label", "small_cell"},
                         {"cell_ids", cell_ids_json(cu)},
                         {"average_based_area", cu.AverageBasedArea()},
                         {"approx_area", cu.ApproxArea()},
                         {"exact_area", cu.ExactArea()}});
    }

    root["area"] = cases;
}

// TEST(S2CellUnion, EmptyAndNonEmptyBooleanOps) - partial coverage
static void add_empty_ops(json &root) {
    S2CellId face1 = S2CellId::FromFace(1);
    S2CellUnion empty_cu;
    S2CellUnion face1_cu({face1});

    root["empty_ops"] = {
        {"face1_id", cell_id_str(face1)},
        {"empty_contains_face1_id", empty_cu.Contains(face1)},
        {"face1_contains_face1_id", face1_cu.Contains(face1)},
        {"empty_intersects_face1_id", empty_cu.Intersects(face1)},
        {"face1_intersects_face1_id", face1_cu.Intersects(face1)},
        {"empty_union_empty", cell_ids_json(empty_cu.Union(empty_cu))},
        {"face1_union_empty", cell_ids_json(face1_cu.Union(empty_cu))},
        {"empty_union_face1", cell_ids_json(empty_cu.Union(face1_cu))},
        {"face1_intersection_empty",
         cell_ids_json(face1_cu.Intersection(empty_cu))},
        {"empty_intersection_face1",
         cell_ids_json(empty_cu.Intersection(face1_cu))},
        {"face1_difference_empty",
         cell_ids_json(face1_cu.Difference(empty_cu))},
        {"empty_difference_face1",
         cell_ids_json(empty_cu.Difference(face1_cu))},
        {"face1_difference_face1",
         cell_ids_json(face1_cu.Difference(face1_cu))}};
}

int main() {
    json root;
    add_whole_sphere(root);
    add_is_normalized(root);
    add_normalize(root);
    add_contains_intersects(root);
    add_contains_intersects_union(root);
    add_boolean_ops(root);
    add_intersection_with_cell_id(root);
    add_from_min_max(root);
    add_from_begin_end(root);
    add_denormalize(root);
    add_leaf_cells_covered(root);
    add_area(root);
    add_empty_ops(root);
    std::cout << root.dump(2) << std::endl;
    return 0;
}
