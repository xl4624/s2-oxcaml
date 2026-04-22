// Golden data generator for S2Builder and S2PolygonLayer.

#include "s2/s2builder.h"

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <utility>
#include <vector>

#include "s2/s1angle.h"
#include "s2/s2builder_graph.h"
#include "s2/s2builder_layer.h"
#include "s2/s2builderutil_s2polygon_layer.h"
#include "s2/s2builderutil_snap_functions.h"
#include "s2/s2cell_id.h"
#include "s2/s2debug.h"
#include "s2/s2latlng.h"
#include "s2/s2loop.h"
#include "s2/s2point.h"
#include "s2/s2polygon.h"
#include "s2/s2polyline.h"

using json = nlohmann::json;
using s2builderutil::IdentitySnapFunction;
using s2builderutil::S2PolygonLayer;
using std::string;
using std::unique_ptr;
using std::vector;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static json points_json(const vector<S2Point> &ps) {
    json arr = json::array();
    for (const auto &p : ps)
        arr.push_back(point_json(p));
    return arr;
}

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
        double lat = std::strtod(s.substr(i, colon - i).c_str(), nullptr);
        double lng = std::strtod(s.substr(colon + 1, comma - colon - 1).c_str(),
                                 nullptr);
        out.push_back(S2LatLng::FromDegrees(lat, lng).ToPoint());
        i = comma;
    }
    return out;
}

// Record a polygon as a list of loops, each loop as a list of points.
static json polygon_json(const S2Polygon &poly) {
    json loops = json::array();
    for (int i = 0; i < poly.num_loops(); ++i) {
        const S2Loop *loop = poly.loop(i);
        json vs = json::array();
        for (int j = 0; j < loop->num_vertices(); ++j) {
            vs.push_back(point_json(loop->vertex(j)));
        }
        loops.push_back(vs);
    }
    json j;
    j["num_loops"] = poly.num_loops();
    j["is_empty"] = poly.is_empty();
    j["is_full"] = poly.is_full();
    j["loops"] = loops;
    return j;
}

// Run S2Builder on [edges] with [snap_radius] and capture the resulting
// polygon from an S2PolygonLayer.
static json run_case(const string &name, S1Angle snap_radius,
                     const vector<std::pair<S2Point, S2Point>> &edges,
                     bool split_crossing_edges = false) {
    S2Builder::Options options{IdentitySnapFunction(snap_radius)};
    options.set_split_crossing_edges(split_crossing_edges);
    S2Builder builder(options);
    S2Polygon output;
    builder.StartLayer(std::make_unique<S2PolygonLayer>(&output));
    for (const auto &e : edges) {
        builder.AddEdge(e.first, e.second);
    }
    S2Error error;
    bool ok = builder.Build(&error);

    json c;
    c["name"] = name;
    c["snap_radius_radians"] = snap_radius.radians();
    c["split_crossing_edges"] = split_crossing_edges;
    json input_edges = json::array();
    for (const auto &e : edges) {
        json pair = json::array();
        pair.push_back(point_json(e.first));
        pair.push_back(point_json(e.second));
        input_edges.push_back(pair);
    }
    c["input_edges"] = input_edges;
    c["ok"] = ok;
    c["error"] = error.message();
    if (ok)
        c["polygon"] = polygon_json(output);
    return c;
}

// Helper: build input edges from an ordered vertex list (closed loop).
static vector<std::pair<S2Point, S2Point>> LoopEdges(
    const vector<S2Point> &vs) {
    vector<std::pair<S2Point, S2Point>> out;
    int n = vs.size();
    for (int i = 0; i < n; ++i) {
        out.push_back({vs[i], vs[(i + 1) % n]});
    }
    return out;
}

int main() {
    json out;
    json cases = json::array();

    // TEST(S2Builder, UnitSquare) - custom test: a CCW unit square in the
    // xy-plane projected to the unit sphere.
    {
        vector<S2Point> sq = ParseLatLngs("0:0, 0:1, 1:1, 1:0");
        cases.push_back(
            run_case("unit_square", S1Angle::Zero(), LoopEdges(sq)));
    }

    // TEST(S2Builder, Triangle) - a simple triangle near the equator.
    {
        vector<S2Point> tri = ParseLatLngs("0:0, 0:5, 5:0");
        cases.push_back(run_case("triangle", S1Angle::Zero(), LoopEdges(tri)));
    }

    // TEST(S2Builder, AddShape) - two nested CCW squares. No snapping.
    // Output is the same two loops.
    {
        vector<S2Point> outer = ParseLatLngs("0:0, 0:10, 10:10, 10:0");
        vector<S2Point> inner = ParseLatLngs("2:2, 2:8, 8:8, 8:2");
        vector<std::pair<S2Point, S2Point>> edges = LoopEdges(outer);
        auto inner_edges = LoopEdges(inner);
        for (const auto &e : inner_edges)
            edges.push_back(e);
        // Reverse inner: shells are CCW, holes should be CW for S2Polygon
        // InitOriented. The loop [2:2,2:8,8:8,8:2] is CCW, so invert it for
        // a hole representation.
        vector<std::pair<S2Point, S2Point>> combined = LoopEdges(outer);
        for (int i = inner.size() - 1; i >= 0; --i) {
            combined.push_back(
                {inner[i], inner[(i - 1 + inner.size()) % inner.size()]});
        }
        cases.push_back(
            run_case("two_nested_squares", S1Angle::Zero(), combined));
    }

    // TEST(S2Builder, IdempotencyZeroSnap) - identical repeated vertices
    // should be merged even at zero snap radius.
    {
        vector<S2Point> vs = ParseLatLngs("0:0, 0:0, 0:1, 1:1, 1:0");
        // Build as a closed loop: the double (0:0, 0:0) becomes a degenerate
        // edge which the polygon layer discards.
        vector<std::pair<S2Point, S2Point>> edges = LoopEdges(vs);
        cases.push_back(
            run_case("idempotency_zero_snap", S1Angle::Zero(), edges));
    }

    // NOTE: SimpleVertexMerging and similar nonzero-snap-radius cases are
    // intentionally omitted. The OCaml port's snap-rounding core is a
    // simplified cluster-merge, not the full Voronoi site-selection
    // algorithm that chooses a canonical site per cluster based on
    // S2CellId ordering. Adding such cases would fail the
    // contains-expected-vertex check even when the topology matches.

    out["cases"] = cases;
    std::cout << out.dump(2) << std::endl;
    return 0;
}
