// Golden data generator for S2CellId.
// Mirrors s2geometry/src/s2/s2cell_id_test.cc for all cases emitted here
// (same PRNG seeds and loop bounds as the C++ tests where applicable).
#include <cmath>
#include <iostream>
#include <map>
#include <nlohmann/json.hpp>
#include <random>
#include <string>
#include <vector>

#include "absl/random/random.h"
#include "absl/strings/str_cat.h"
#include "s2/s2cell_id.h"
#include "s2/s2latlng.h"
#include "s2/s2metrics.h"
#include "s2/s2point.h"
#include "s2/s2random.h"

using json = nlohmann::json;

// Match s2/s2testing.cc [MakeTaggedSeedSeq]: absl::StrCat(name,
// FLAGS_s2_random_seed) with default [s2_random_seed] == 1.
static std::seed_seq MakeTaggedSeedSeq(absl::string_view name) {
    const int32_t kDefaultS2RandomSeed = 1;
    std::string seed_str = absl::StrCat(name, kDefaultS2RandomSeed);
    return std::seed_seq(seed_str.begin(), seed_str.end());
}

static json cell_json(S2CellId id) {
    return {{"id", std::to_string(id.id())},
            {"face", id.face()},
            {"level", id.level()},
            {"is_valid", id.is_valid()},
            {"token", id.ToToken()}};
}

// TEST(S2CellId, FaceDefinitions) / FromFace
static void add_faces(json &root) {
    json faces = json::array();
    for (int i = 0; i < 6; ++i) {
        faces.push_back(cell_json(S2CellId::FromFace(i)));
    }
    root["faces"] = faces;
}

// TEST(S2CellId, ParentChildRelationships)
static void add_hierarchy(json &root) {
    json hierarchy = json::array();
    S2CellId c = S2CellId::FromFace(1).child(0).child(3).child(2);
    while (true) {
        json entry = {{"cell", cell_json(c)},
                      {"child0", cell_json(c.child(0))},
                      {"child1", cell_json(c.child(1))},
                      {"child2", cell_json(c.child(2))},
                      {"child3", cell_json(c.child(3))}};
        if (!c.is_face()) {
            entry["parent"] = cell_json(c.parent());
            entry["child_pos"] = c.child_position();
        }
        hierarchy.push_back(entry);
        if (c.is_face())
            break;
        c = c.parent();
    }
    root["hierarchy"] = hierarchy;
}

// TEST(S2CellId, CenterSiTi)
static void add_points(json &root) {
    json points = json::array();
    std::vector<S2Point> test_points = {
        S2Point(1, 0, 0), S2Point(0, 1, 0), S2Point(0, 0, 1),
        S2Point(-1, -1, -1).Normalize(), S2Point(0.1, 0.2, 0.3).Normalize()};
    for (const auto &p : test_points) {
        S2CellId id(p);
        S2Point p2 = id.ToPoint();
        points.push_back({{"p", {p.x(), p.y(), p.z()}},
                          {"id", std::to_string(id.id())},
                          {"to_p", {p2.x(), p2.y(), p2.z()}},
                          {"level", id.level()}});
    }
    root["points"] = points;
}

// TEST(S2CellId, Advance)
static void add_advance_basic(json &root) {
    json advance = json::array();
    S2CellId start = S2CellId::FromFace(0).child(0);
    std::vector<int64_t> steps = {0, 1, 10, 1000, -1, -10, 1000000, -1000000};
    for (int64_t s : steps) {
        advance.push_back(
            {{"id", std::to_string(start.id())},
             {"steps", s},
             {"advance", std::to_string(start.advance(s).id())},
             {"advance_wrap", std::to_string(start.advance_wrap(s).id())}});
    }
    root["advance"] = advance;
}

// TEST(S2CellId, ParentChildRelationships) and related.
static void add_parent_child_relationships(json &root) {
    constexpr int kFace = 3;
    constexpr uint64_t kPos = 0x12345678;
    constexpr int kLevel = S2CellId::kMaxLevel - 4;
    S2CellId id = S2CellId::FromFacePosLevel(kFace, kPos, kLevel);
    S2Point raw = id.ToPointRaw();
    S2Point center = id.ToPoint();
    json o;
    o["construct_face"] = kFace;
    o["construct_pos"] = std::to_string(kPos);
    o["construct_level"] = kLevel;
    o["id"] = std::to_string(id.id());
    o["face"] = id.face();
    o["pos"] = std::to_string(id.pos());
    o["level"] = id.level();
    o["is_leaf"] = id.is_leaf();
    o["child_begin_l2_pos"] =
        std::to_string(id.child_begin(id.level() + 2).pos());
    o["child_begin_pos"] = std::to_string(id.child_begin().pos());
    o["parent_pos"] = std::to_string(id.parent().pos());
    o["parent_level_minus_2_pos"] =
        std::to_string(id.parent(id.level() - 2).pos());
    o["range_min"] = std::to_string(id.range_min().id());
    o["range_max"] = std::to_string(id.range_max().id());
    o["child_end_max"] = std::to_string(id.child_end(S2CellId::kMaxLevel).id());
    o["hilbert_midpoint"] =
        (2 * id.id() == id.range_min().id() + id.range_max().id());
    o["next_child_begin_max"] = std::to_string(
        id.next().child_begin(S2CellId::kMaxLevel).advance(256).id());
    o["child_begin_max_adv"] =
        std::to_string(id.child_begin(S2CellId::kMaxLevel).advance(256).id());
    o["to_point_raw"] = {raw.x(), raw.y(), raw.z()};
    o["to_point"] = {center.x(), center.y(), center.z()};
    root["parent_child"] = o;
}

static void add_sentinel_range(json &root) {
    S2CellId s = S2CellId::Sentinel();
    root["sentinel_range"] = json{{"min", std::to_string(s.range_min().id())},
                                  {"max", std::to_string(s.range_max().id())}};
}

// TEST(S2CellId, Wrapping)
static void add_wrapping(json &root) {
    json arr = json::array();
    auto push_eq = [&](const char *name, uint64_t a, uint64_t b) {
        arr.push_back({{"kind", "eq_pair"},
                       {"name", name},
                       {"a", std::to_string(a)},
                       {"b", std::to_string(b)}});
    };
    push_eq("End0_prev_vs_Begin0_prev_wrap", S2CellId::End(0).prev().id(),
            S2CellId::Begin(0).prev_wrap().id());
    push_eq("BeginMax_prev_wrap_vs_corner_cell",
            S2CellId::FromFacePosLevel(5, ~0ULL >> S2CellId::kFaceBits,
                                       S2CellId::kMaxLevel)
                .id(),
            S2CellId::Begin(S2CellId::kMaxLevel).prev_wrap().id());
    push_eq("BeginMax_advance_wrap_m1_vs_prev_wrap",
            S2CellId::Begin(S2CellId::kMaxLevel).prev_wrap().id(),
            S2CellId::Begin(S2CellId::kMaxLevel).advance_wrap(-1).id());
    push_eq("End4_prev_next_wrap_vs_Begin4", S2CellId::Begin(4).id(),
            S2CellId::End(4).prev().next_wrap().id());
    push_eq("EndMax_prev_next_wrap_vs_face0_leaf_min",
            S2CellId::FromFacePosLevel(0, 0, S2CellId::kMaxLevel).id(),
            S2CellId::End(S2CellId::kMaxLevel).prev().next_wrap().id());
    root["wrapping"] = arr;
}

// TEST(S2CellId, Advance) additional expectations.
static void add_advance_extended(json &root) {
    json arr = json::array();
    S2CellId id =
        S2CellId::FromFacePosLevel(3, 0x12345678, S2CellId::kMaxLevel - 4);

    arr.push_back({{"name", "Begin0_adv_7_to_End0"},
                   {"from_id", std::to_string(S2CellId::Begin(0).id())},
                   {"steps", 7},
                   {"expected", std::to_string(S2CellId::End(0).id())}});
    arr.push_back({{"name", "Begin0_adv_12_to_End0"},
                   {"from_id", std::to_string(S2CellId::Begin(0).id())},
                   {"steps", 12},
                   {"expected", std::to_string(S2CellId::End(0).id())}});
    arr.push_back({{"name", "End0_adv_m7_to_Begin0"},
                   {"from_id", std::to_string(S2CellId::End(0).id())},
                   {"steps", -7},
                   {"expected", std::to_string(S2CellId::Begin(0).id())}});
    arr.push_back({{"name", "End0_adv_large_neg_clamped_Begin0"},
                   {"from_id", std::to_string(S2CellId::End(0).id())},
                   {"steps", -12000000},
                   {"expected", std::to_string(S2CellId::Begin(0).id())}});

    int num_level_5_cells = 6 << (2 * 5);
    arr.push_back(
        {{"name", "End5_adv_wrap_500"},
         {"from_id", std::to_string(S2CellId::End(5).id())},
         {"steps", 500 - num_level_5_cells},
         {"expected", std::to_string(S2CellId::Begin(5).advance(500).id())}});

    arr.push_back(
        {{"name", "adv_wrap_face1_from_Begin0"},
         {"from_id", std::to_string(S2CellId::Begin(0).id())},
         {"steps_wrap", 7},
         {"expected_wrap", std::to_string(S2CellId::FromFace(1).id())}});
    arr.push_back({{"name", "adv_wrap_Begin0_m12000000"},
                   {"from_id", std::to_string(S2CellId::Begin(0).id())},
                   {"steps_wrap", -12000000},
                   {"expected_wrap", std::to_string(S2CellId::Begin(0).id())}});
    arr.push_back(
        {{"name", "adv_wrap_FromFace5_m7"},
         {"from_id", std::to_string(S2CellId::FromFace(5).id())},
         {"steps_wrap", -7},
         {"expected_wrap", std::to_string(S2CellId::FromFace(4).id())}});

    S2CellId face5_leaf = S2CellId::FromFacePosLevel(5, 0, S2CellId::kMaxLevel);
    uint64_t w = uint64_t{4} << (2 * S2CellId::kMaxLevel);
    arr.push_back(
        {{"name", "FromFace5_pos0_max_adv"},
         {"from_id", std::to_string(face5_leaf.id())},
         {"steps", static_cast<int64_t>(w)},
         {"expected",
          std::to_string(face5_leaf.advance(static_cast<int64_t>(w)).id())}});

    uint64_t w2 = uint64_t{2} << (2 * S2CellId::kMaxLevel);
    arr.push_back(
        {{"name", "adv_wrap_cross_from_face5_to_1"},
         {"from_id", std::to_string(face5_leaf.id())},
         {"steps_wrap", static_cast<int64_t>(w2)},
         {"expected_wrap",
          std::to_string(
              face5_leaf.advance_wrap(static_cast<int64_t>(w2)).id())}});

    arr.push_back(
        {{"name", "next_child_begin_max_adv256"},
         {"from_id", std::to_string(id.id())},
         {"expected",
          std::to_string(
              id.next().child_begin(S2CellId::kMaxLevel).advance(256).id())}});
    arr.push_back(
        {{"name", "child_begin_max_adv256"},
         {"from_id", std::to_string(id.id())},
         {"expected",
          std::to_string(
              id.child_begin(S2CellId::kMaxLevel).advance(256).id())}});

    root["advance_extended"] = arr;
}

// TEST(S2CellIdGolden, LatLngFace): lat/lng -> face (leaf id center).
static void add_latlng_face(json &root) {
    struct {
        double lat, lng;
        int face;
    } pts[] = {{0, 0, 0},   {0, 90, 1},  {90, 0, 2},
               {0, 180, 3}, {0, -90, 4}, {-90, 0, 5}};
    json arr = json::array();
    for (const auto &p : pts) {
        S2CellId id(S2LatLng::FromDegrees(p.lat, p.lng));
        arr.push_back({{"lat", p.lat},
                       {"lng", p.lng},
                       {"face", p.face},
                       {"leaf_id", std::to_string(id.id())}});
    }
    root["latlng_face"] = arr;
}

// TEST(S2CellId, Tokens): fixed ids + 10k random round-trips
static void add_tokens(json &root) {
    json known = json::array();
    std::vector<uint64_t> ids = {
        0,
        ~0ULL,
        0x7837423,
        S2CellId::FromFace(7).id(),
        S2CellId::FromFacePosLevel(3, 0x12345678, S2CellId::kMaxLevel - 4).id(),
        S2CellId::FromFacePosLevel(0, 0, 10).id()};
    for (uint64_t x : ids) {
        S2CellId id(x);
        known.push_back({{"id", std::to_string(x)}, {"token", id.ToToken()}});
    }
    absl::BitGen bitgen(MakeTaggedSeedSeq("TOKENS"));
    for (int i = 0; i < 10000; ++i) {
        S2CellId id = s2random::CellId(bitgen);
        known.push_back(
            {{"id", std::to_string(id.id())}, {"token", id.ToToken()}});
    }
    root["tokens"] = known;

    // TEST(S2CellId, LegacyCoderTokenInvalid)
    json bad = json::array();
    for (const char *t : {"876b e99", "876bee99\n", "876[ee99", " 876bee99",
                          "000000000000000404"})
        bad.push_back(t);
    root["token_invalid"] = bad;
}

// TEST(S2CellId, Containment)
static void ExpandCell(S2CellId parent, std::vector<S2CellId> *cells,
                       std::map<uint64_t, uint64_t> *parent_map) {
    cells->push_back(parent);
    constexpr int kMaxExpandLevel = 2;
    if (parent.level() == kMaxExpandLevel)
        return;
    S2CellId child = parent.child_begin();
    for (; child != parent.child_end(); child = child.next()) {
        (*parent_map)[child.id()] = parent.id();
        ExpandCell(child, cells, parent_map);
    }
}

static void add_containment(json &root) {
    std::map<uint64_t, uint64_t> parent_map;
    std::vector<S2CellId> cells;
    for (int face = 0; face < 6; ++face) {
        ExpandCell(S2CellId::FromFace(face), &cells, &parent_map);
    }
    json arr = json::array();
    for (S2CellId end_id : cells) {
        for (S2CellId begin_id : cells) {
            bool contained = true;
            for (S2CellId x = begin_id; x != end_id;) {
                auto it = parent_map.find(x.id());
                if (it == parent_map.end()) {
                    contained = false;
                    break;
                }
                x = S2CellId(it->second);
            }
            bool by_range = (begin_id >= end_id.range_min()
                             && begin_id <= end_id.range_max());
            bool inter = end_id.intersects(begin_id);
            bool inter_exp =
                end_id.contains(begin_id) || begin_id.contains(end_id);
            arr.push_back({{"end", std::to_string(end_id.id())},
                           {"begin", std::to_string(begin_id.id())},
                           {"contains", end_id.contains(begin_id)},
                           {"contains_by_range", by_range},
                           {"intersects", inter},
                           {"intersects_expected", inter_exp}});
        }
    }
    root["containment"] = arr;
}

// TEST(S2CellId, Continuity): full Begin(L)..End(L) walk.
static void add_continuity(json &root) {
    constexpr int L = 8;
    json arr = json::array();
    S2CellId end = S2CellId::End(L);
    for (S2CellId id = S2CellId::Begin(L); id != end; id = id.next()) {
        arr.push_back(
            {{"id", std::to_string(id.id())},
             {"next", std::to_string(id.next().id())},
             {"next_wrap", std::to_string(id.next_wrap().id())},
             {"advance_wrap_1", std::to_string(id.advance_wrap(1).id())}});
    }
    root["continuity_level8"] = arr;
}

// TEST(S2CellId, Continuity)
static void add_continuity_geometry(json &root) {
    constexpr int L = 8;
    double max_edge = S2::kMaxEdge.GetValue(L);
    json arr = json::array();
    S2CellId end = S2CellId::End(L);
    for (S2CellId id = S2CellId::Begin(L); id != end; id = id.next()) {
        double ang = id.ToPointRaw().Angle(id.next_wrap().ToPointRaw());
        arr.push_back({{"id", std::to_string(id.id())},
                       {"angle", ang},
                       {"max_angle", max_edge}});
    }
    root["continuity_geometry"] = arr;
}

// TEST(S2CellId, DistanceFromBegin)
static void add_distance_from_begin(json &root) {
    json arr = json::array();
    auto row = [&](S2CellId id) {
        arr.push_back({{"id", std::to_string(id.id())},
                       {"distance", id.distance_from_begin()}});
    };
    row(S2CellId::End(0));
    row(S2CellId::End(10));
    row(S2CellId::End(S2CellId::kMaxLevel));
    row(S2CellId::Begin(0));
    row(S2CellId::Begin(S2CellId::kMaxLevel));
    S2CellId id =
        S2CellId::FromFacePosLevel(3, 0x12345678, S2CellId::kMaxLevel - 4);
    arr.push_back({{"id", std::to_string(id.id())},
                   {"distance", id.distance_from_begin()},
                   {"roundtrip", true}});
    root["distance_from_begin"] = arr;
}

// TEST(S2CellId, GetCommonAncestorLevel)
static void add_common_ancestor(json &root) {
    json arr = json::array();
    auto row = [&](S2CellId a, S2CellId b, int lev) {
        arr.push_back({{"a", std::to_string(a.id())},
                       {"b", std::to_string(b.id())},
                       {"level", lev}});
    };
    row(S2CellId::FromFace(0), S2CellId::FromFace(0), 0);
    row(S2CellId::FromFace(0).child_begin(30),
        S2CellId::FromFace(0).child_begin(30), 30);
    row(S2CellId::FromFace(0).child_begin(30), S2CellId::FromFace(0), 0);
    row(S2CellId::FromFace(5), S2CellId::FromFace(5).child_end(30).prev(), 0);
    row(S2CellId::FromFace(0), S2CellId::FromFace(5), -1);
    row(S2CellId::FromFace(2).child_begin(30),
        S2CellId::FromFace(3).child_end(20), -1);
    row(S2CellId::FromFace(5).child_begin(9).next().child_begin(15),
        S2CellId::FromFace(5).child_begin(9).child_begin(20), 8);
    row(S2CellId::FromFace(0).child_begin(2).child_begin(30),
        S2CellId::FromFace(0).child_begin(2).next().child_begin(5), 1);
    root["common_ancestor"] = arr;
}

// TEST(S2CellId, MaximumTile): 1000 iters, same cases per iter as
// s2cell_id_test.cc.
static void add_maximum_tile(json &root) {
    absl::BitGen bitgen(MakeTaggedSeedSeq("MAXIMUM_TILE"));
    json arr = json::array();
    auto push3 = [&](S2CellId a, S2CellId b, S2CellId e) {
        arr.push_back({{"tile", std::to_string(a.id())},
                       {"limit", std::to_string(b.id())},
                       {"expect", std::to_string(e.id())}});
    };
    for (int iter = 0; iter < 1000; ++iter) {
        S2CellId id = s2random::CellId(bitgen, 10);
        if (id.is_leaf()) {
            continue;
        }
        push3(id, id, id.maximum_tile(id));
        push3(id.child(0), id, id.child(0).maximum_tile(id));
        push3(id.child(1), id, id.child(1).maximum_tile(id));
        push3(id.next(), id, id.next().maximum_tile(id));
        push3(id, id.child(0), id.maximum_tile(id.child(0)));
        push3(id.child(0), id.next(), id.child(0).maximum_tile(id.next()));
        push3(id.child(0), id.next().child(0),
              id.child(0).maximum_tile(id.next().child(0)));
        push3(id.child(0), id.next().child(1).child(0),
              id.child(0).maximum_tile(id.next().child(1).child(0)));
        push3(id.child(0).child(0), id.next(),
              id.child(0).child(0).maximum_tile(id.next()));
        push3(id.child(0).child(0).child(0), id.next(),
              id.child(0).child(0).child(0).maximum_tile(id.next()));
        push3(id, id.child(0).next(), id.maximum_tile(id.child(0).next()));
        push3(id, id.child(0).next().child(0),
              id.maximum_tile(id.child(0).next().child(0)));
        push3(id, id.child(0).next().child(1),
              id.maximum_tile(id.child(0).next().child(1)));
        push3(id, id.child(0).child(0).next(),
              id.maximum_tile(id.child(0).child(0).next()));
        push3(id, id.child(0).child(0).child(0).next(),
              id.maximum_tile(id.child(0).child(0).child(0).next()));
        push3(id, id.next(), id.maximum_tile(id.next()));
        push3(id, id.next().child(0), id.maximum_tile(id.next().child(0)));
        push3(id, id.next().child(1).child(0),
              id.maximum_tile(id.next().child(1).child(0)));
    }
    root["maximum_tile"] = arr;
}

// Advance_wrap 6644 vs -11788 at level 5 (TEST(S2CellId, Advance)).
static void add_advance_wrap_equiv(json &root) {
    S2CellId b = S2CellId::Begin(5);
    S2CellId a = b.advance_wrap(6644);
    S2CellId c = b.advance_wrap(-11788);
    root["advance_wrap_equiv"] =
        json{{"eq", a == c}, {"id", std::to_string(a.id())}};
}

// TEST(S2CellId, Coverage)
static void add_coverage_sample(json &root) {
    absl::BitGen bitgen(MakeTaggedSeedSeq("COVERAGE"));
    double max_diag = 0.5 * S2::kMaxDiag.GetValue(S2CellId::kMaxLevel);
    json arr = json::array();
    for (int i = 0; i < 1000000; ++i) {
        S2Point p = s2random::Point(bitgen);
        S2Point q = S2CellId(p).ToPointRaw();
        arr.push_back({{"p", {p.x(), p.y(), p.z()}},
                       {"angle", p.Angle(q)},
                       {"max_angle", max_diag}});
    }
    root["coverage_sample"] = arr;
}

int main() {
    json root = json::object();
    add_faces(root);
    add_hierarchy(root);
    add_points(root);
    add_advance_basic(root);
    add_parent_child_relationships(root);
    add_sentinel_range(root);
    add_wrapping(root);
    add_advance_extended(root);
    add_latlng_face(root);
    add_tokens(root);
    add_containment(root);
    add_continuity(root);
    add_continuity_geometry(root);
    add_distance_from_begin(root);
    add_common_ancestor(root);
    add_maximum_tile(root);
    add_advance_wrap_equiv(root);
    add_coverage_sample(root);

    std::cout << root.dump() << std::endl;
    return 0;
}
