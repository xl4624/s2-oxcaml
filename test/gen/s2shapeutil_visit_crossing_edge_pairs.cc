// Golden data generator for s2shapeutil::VisitCrossingEdgePairs and
// s2shapeutil::FindSelfIntersection.
// Mirrors s2geometry/src/s2/s2shapeutil_visit_crossing_edge_pairs_test.cc.

#include "s2/s2shapeutil_visit_crossing_edge_pairs.h"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <utility>
#include <vector>

#include "s2/mutable_s2shape_index.h"
#include "s2/s2edge_vector_shape.h"
#include "s2/s2error.h"
#include "s2/s2latlng.h"
#include "s2/s2lax_polygon_shape.h"
#include "s2/s2point.h"
#include "s2/s2shape.h"
#include "s2/s2shape_index.h"
#include "s2/s2shapeutil_shape_edge.h"
#include "s2/s2shapeutil_shape_edge_id.h"

using json = nlohmann::json;
using s2shapeutil::CrossingType;
using s2shapeutil::ShapeEdge;
using s2shapeutil::ShapeEdgeId;
using s2shapeutil::VisitCrossingEdgePairs;
using std::make_unique;
using std::string;
using std::vector;

static json point_json(const S2Point &p) {
    return json::array({p.x(), p.y(), p.z()});
}

// Serialise a shape as a list of (v0, v1) edges. The OCaml side rebuilds an
// equivalent S2EdgeVectorShape from this representation.
static json edge_vector_shape_json(const vector<S2Shape::Edge> &edges) {
    json out = json::array();
    for (const auto &e : edges) {
        out.push_back(json::array({point_json(e.v0), point_json(e.v1)}));
    }
    return out;
}

// Build a EdgeVectorShape from a list of edges and add it to the index.
static void add_edge_shape(MutableS2ShapeIndex *index,
                           const vector<S2Shape::Edge> &edges) {
    auto shape = make_unique<S2EdgeVectorShape>();
    for (const auto &e : edges)
        shape->Add(e.v0, e.v1);
    index->Add(std::move(shape));
}

using EdgePair = std::pair<ShapeEdgeId, ShapeEdgeId>;
using EdgePairVector = vector<EdgePair>;

// Get crossings in one index (mirrors the helper in the upstream test).
static EdgePairVector GetCrossings(const S2ShapeIndex &index,
                                   CrossingType type) {
    EdgePairVector edge_pairs;
    VisitCrossingEdgePairs(
        index, type,
        [&edge_pairs](const ShapeEdge &a, const ShapeEdge &b, bool) {
            edge_pairs.push_back(std::make_pair(a.id(), b.id()));
            return true;
        });
    if (edge_pairs.size() > 1) {
        std::sort(edge_pairs.begin(), edge_pairs.end());
        edge_pairs.erase(std::unique(edge_pairs.begin(), edge_pairs.end()),
                         edge_pairs.end());
    }
    return edge_pairs;
}

// Get crossings between two indexes.
static EdgePairVector GetCrossings(const S2ShapeIndex &indexA,
                                   const S2ShapeIndex &indexB,
                                   CrossingType type) {
    EdgePairVector edge_pairs;
    VisitCrossingEdgePairs(
        indexA, indexB, type,
        [&edge_pairs](const ShapeEdge &a, const ShapeEdge &b, bool) {
            edge_pairs.push_back(std::make_pair(a.id(), b.id()));
            return true;
        });
    if (edge_pairs.size() > 1) {
        std::sort(edge_pairs.begin(), edge_pairs.end());
        edge_pairs.erase(std::unique(edge_pairs.begin(), edge_pairs.end()),
                         edge_pairs.end());
    }
    return edge_pairs;
}

static json edge_pair_json(const EdgePair &p) {
    return json::array({json::array({p.first.shape_id, p.first.edge_id}),
                        json::array({p.second.shape_id, p.second.edge_id})});
}

static json edge_pairs_json(const EdgePairVector &pairs) {
    json arr = json::array();
    for (const auto &p : pairs)
        arr.push_back(edge_pair_json(p));
    return arr;
}

// Build an EdgeGrid as in the upstream test. There are kGridSize+1 horizontal
// and kGridSize+1 vertical lines on a ~10 deg square. epsilon nudges interior
// lines slightly past the boundary so 'touching' edges register as crossings.
struct GridShapes {
    vector<S2Shape::Edge> horizontal;
    vector<S2Shape::Edge> vertical;
};

static GridShapes BuildEdgeGrid(int grid_size, double epsilon) {
    GridShapes out;
    for (int i = 0; i <= grid_size; ++i) {
        double e = (i == 0 || i == grid_size) ? 0 : epsilon;
        out.horizontal.push_back(
            {S2LatLng::FromDegrees(-e, i).ToPoint(),
             S2LatLng::FromDegrees(grid_size + e, i).ToPoint()});
        out.vertical.push_back(
            {S2LatLng::FromDegrees(i, -e).ToPoint(),
             S2LatLng::FromDegrees(i, grid_size + e).ToPoint()});
    }
    return out;
}

// One-index crossing case: a list of EdgeVectorShapes plus crossings under
// each CrossingType.
static json one_index_case(const string &name,
                           const vector<vector<S2Shape::Edge>> &shapes) {
    MutableS2ShapeIndex index;
    json shapes_json = json::array();
    for (const auto &s : shapes) {
        add_edge_shape(&index, s);
        shapes_json.push_back(edge_vector_shape_json(s));
    }
    EdgePairVector all_crossings = GetCrossings(index, CrossingType::ALL);
    EdgePairVector interior_crossings =
        GetCrossings(index, CrossingType::INTERIOR);
    return json{{"name", name},
                {"shapes", shapes_json},
                {"crossings_all", edge_pairs_json(all_crossings)},
                {"crossings_interior", edge_pairs_json(interior_crossings)}};
}

// Two-index crossing case: two index payloads.
static json two_index_case(const string &name,
                           const vector<vector<S2Shape::Edge>> &a_shapes,
                           const vector<vector<S2Shape::Edge>> &b_shapes) {
    MutableS2ShapeIndex a_index, b_index;
    json a_shapes_json = json::array();
    json b_shapes_json = json::array();
    for (const auto &s : a_shapes) {
        add_edge_shape(&a_index, s);
        a_shapes_json.push_back(edge_vector_shape_json(s));
    }
    for (const auto &s : b_shapes) {
        add_edge_shape(&b_index, s);
        b_shapes_json.push_back(edge_vector_shape_json(s));
    }
    EdgePairVector all_crossings =
        GetCrossings(a_index, b_index, CrossingType::ALL);
    EdgePairVector interior_crossings =
        GetCrossings(a_index, b_index, CrossingType::INTERIOR);
    return json{{"name", name},
                {"a_shapes", a_shapes_json},
                {"b_shapes", b_shapes_json},
                {"crossings_all", edge_pairs_json(all_crossings)},
                {"crossings_interior", edge_pairs_json(interior_crossings)}};
}

// Polygon-style shape encoded as a list of loops, each a list of vertices.
// The OCaml side rebuilds these as S2_lax_polygon.t.
static json loops_json(const vector<vector<S2Point>> &loops) {
    json out = json::array();
    for (const auto &loop : loops) {
        json loop_json = json::array();
        for (const auto &p : loop)
            loop_json.push_back(point_json(p));
        out.push_back(loop_json);
    }
    return out;
}

// Build an S2LaxPolygonShape from loops and add it to the index, then run
// FindSelfIntersection.
static bool RunFindSelfIntersection(const vector<vector<S2Point>> &loops) {
    MutableS2ShapeIndex index;
    index.Add(make_unique<S2LaxPolygonShape>(loops));
    S2Error error;
    return s2shapeutil::FindSelfIntersection(index, &error);
}

// Recursively emit a case for every cyclic permutation of the loop vertices.
// Mirrors TestHasCrossingPermutations from the upstream test.
static void EmitPermutations(const vector<vector<S2Point>> &orig, size_t i,
                             vector<vector<S2Point>> *current,
                             const string &name_prefix, bool expected,
                             json *cases) {
    if (i == orig.size()) {
        bool actual = RunFindSelfIntersection(*current);
        // Emit case; expected_has_intersection mirrors the upstream
        // expectation. actual is also recorded for cross-check.
        json c;
        c["name"] = name_prefix;
        c["loops"] = loops_json(*current);
        c["expected_has_intersection"] = expected;
        c["actual_has_intersection"] = actual;
        cases->push_back(c);
        return;
    }
    const auto &loop = orig[i];
    int n = static_cast<int>(loop.size());
    for (int j = 0; j < n; ++j) {
        vector<S2Point> rotated;
        for (int k = 0; k < n; ++k)
            rotated.push_back(loop[(j + k) % n]);
        (*current)[i] = rotated;
        EmitPermutations(
            orig, i + 1, current,
            name_prefix + "_p" + std::to_string(i) + "_" + std::to_string(j),
            expected, cases);
    }
}

// Parse "lat:lng, lat:lng, ..." into a vector of S2Point. Mirrors the
// convention used by other generators in this directory.
static vector<S2Point> ParseLatLngs(const string &s) {
    vector<S2Point> out;
    size_t i = 0;
    while (i < s.size()) {
        while (i < s.size() && (s[i] == ' ' || s[i] == ','))
            ++i;
        if (i >= s.size())
            break;
        size_t colon = s.find(':', i);
        if (colon == string::npos)
            break;
        size_t comma = s.find(',', colon);
        if (comma == string::npos)
            comma = s.size();
        string lat_str = s.substr(i, colon - i);
        string lng_str = s.substr(colon + 1, comma - colon - 1);
        double lat = std::strtod(lat_str.c_str(), nullptr);
        double lng = std::strtod(lng_str.c_str(), nullptr);
        out.push_back(S2LatLng::FromDegrees(lat, lng).ToPoint());
        i = comma;
    }
    return out;
}

// Parse "loop_str; loop_str; ..." into a list of loops.
static vector<vector<S2Point>> ParseLoops(const string &s) {
    vector<vector<S2Point>> out;
    size_t i = 0;
    while (i < s.size()) {
        size_t semi = s.find(';', i);
        if (semi == string::npos)
            semi = s.size();
        string loop_str = s.substr(i, semi - i);
        out.push_back(ParseLatLngs(loop_str));
        if (semi == s.size())
            break;
        i = semi + 1;
    }
    return out;
}

// Given a polygon-string and an expected-has-intersection bool, emit a case
// for every cyclic permutation of every loop.
static void EmitFindSelfIntersection(json *cases, const string &name_prefix,
                                     const string &polygon_str,
                                     bool expected_has_intersection) {
    vector<vector<S2Point>> orig = ParseLoops(polygon_str);
    vector<vector<S2Point>> current = orig;
    EmitPermutations(orig, 0, &current, name_prefix, expected_has_intersection,
                     cases);
}

int main() {
    json out;

    // TEST(GetCrossingEdgePairs, NoIntersectionsOneIndex)
    json one_index_cases = json::array();
    one_index_cases.push_back(one_index_case("no_intersections", {}));

    // TEST(GetCrossingEdgePairs, EdgeGridOneIndex)
    {
        constexpr int kGridSize = 10;
        const double epsilon = 1e-10;
        GridShapes grid = BuildEdgeGrid(kGridSize, epsilon);
        // Upstream stores both grids in a single shape; replicate that.
        vector<S2Shape::Edge> combined;
        for (const auto &e : grid.horizontal)
            combined.push_back(e);
        for (const auto &e : grid.vertical)
            combined.push_back(e);
        // The upstream test interleaves horizontal and vertical edges
        // (H_i, V_i, H_{i+1}, V_{i+1}, ...). Reproduce that interleaving so
        // shape edge ids match the upstream tests for easier debugging,
        // although the visit_crossing_edge_pairs result is order-invariant.
        vector<S2Shape::Edge> interleaved;
        for (int i = 0; i <= kGridSize; ++i) {
            interleaved.push_back(grid.horizontal[i]);
            interleaved.push_back(grid.vertical[i]);
        }
        one_index_cases.push_back(one_index_case("edge_grid", {interleaved}));
    }

    out["one_index_cases"] = one_index_cases;

    // TEST(GetCrossingEdgePairs, NoIntersectionsTwoIndexes)
    json two_index_cases = json::array();
    two_index_cases.push_back(two_index_case("no_intersections", {}, {}));

    // TEST(GetCrossingEdgePairs, EdgeGridTwoIndexes)
    {
        constexpr int kGridSize = 10;
        const double epsilon = 1e-10;
        GridShapes grid = BuildEdgeGrid(kGridSize, epsilon);
        two_index_cases.push_back(
            two_index_case("edge_grid", {grid.horizontal}, {grid.vertical}));
    }

    out["two_index_cases"] = two_index_cases;

    // TEST(FindSelfIntersection, Basic) - one entry per cyclic permutation of
    // each loop, mirroring TestHasCrossingPermutations.
    json find_self_cases = json::array();
    EmitFindSelfIntersection(&find_self_cases, "no_crossing",
                             "0:0, 0:1, 0:2, 1:2, 1:1, 1:0", false);
    EmitFindSelfIntersection(&find_self_cases, "duplicate_vertex",
                             "0:0, 0:1, 0:2, 1:2, 0:1, 1:0", true);
    EmitFindSelfIntersection(&find_self_cases, "edge_crossing",
                             "0:0, 0:1, 1:0, 1:1", true);
    EmitFindSelfIntersection(&find_self_cases, "duplicate_edge",
                             "0:0, 1:1, 0:1; 0:0, 1:1, 1:0", true);
    EmitFindSelfIntersection(&find_self_cases, "reversed_edge",
                             "0:0, 1:1, 0:1; 1:1, 0:0, 1:0", true);
    EmitFindSelfIntersection(&find_self_cases, "vertex_crossing",
                             "0:0, 0:2, 2:2, 2:0; 1:1, 0:2, 3:1, 2:0", true);

    out["find_self_intersection_cases"] = find_self_cases;

    std::cout << out.dump(2) << std::endl;
    return 0;
}
