// Golden data generator for S2WrappedShape.

#include "s2/s2wrapped_shape.h"

#include <cstdlib>
#include <iostream>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/s2latlng.h"
#include "s2/s2lax_polygon_shape.h"
#include "s2/s2point.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::string;
using std::vector;
using Loop = vector<S2Point>;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static json edge_json(const S2Shape::Edge &e) {
    return json::object({{"v0", point_json(e.v0)}, {"v1", point_json(e.v1)}});
}

// Parse a comma-separated list of "lat:lng" pairs (in degrees). Avoids a
// link-time dependency on s2text_format; matches the helper in
// test/gen/s2lax_polygon.cc.
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

static json loops_json(const vector<Loop> &loops) {
    json arr = json::array();
    for (const auto &loop : loops) {
        json pts = json::array();
        for (const auto &p : loop)
            pts.push_back(point_json(p));
        arr.push_back(pts);
    }
    return arr;
}

// Emit the summary of [S2WrappedShape(lax)] where [lax] is a lax polygon
// built from [loops]. The wrapper delegates every accessor to [lax], so this
// doubles as a parity check: the OCaml test builds the same lax polygon, wraps
// it, and compares field-by-field against this fixture.
static json wrapped_summary(const string &name, const vector<Loop> &loops) {
    S2LaxPolygonShape lax(loops);
    S2WrappedShape shape(&lax);

    json edge_arr = json::array();
    for (int i = 0; i < shape.num_edges(); ++i) {
        edge_arr.push_back(edge_json(shape.edge(i)));
    }
    json chain_arr = json::array();
    for (int i = 0; i < shape.num_chains(); ++i) {
        auto c = shape.chain(i);
        chain_arr.push_back(json::array({c.start, c.length}));
    }
    json chain_positions = json::array();
    for (int e = 0; e < shape.num_edges(); ++e) {
        auto cp = shape.chain_position(e);
        chain_positions.push_back(json::array({cp.chain_id, cp.offset}));
    }
    json chain_edges = json::array();
    for (int i = 0; i < shape.num_chains(); ++i) {
        auto c = shape.chain(i);
        for (int j = 0; j < c.length; ++j) {
            chain_edges.push_back(edge_json(shape.chain_edge(i, j)));
        }
    }
    auto rp = shape.GetReferencePoint();
    return {
        {"name", name},
        {"loops", loops_json(loops)},
        {"num_edges", shape.num_edges()},
        {"num_chains", shape.num_chains()},
        {"dimension", shape.dimension()},
        {"is_empty", shape.is_empty()},
        {"is_full", shape.is_full()},
        {"type_tag", shape.type_tag()},
        {"reference_point",
         {{"point", point_json(rp.point)}, {"contained", rp.contained}}},
        {"edges", edge_arr},
        {"chains", chain_arr},
        {"chain_positions", chain_positions},
        {"chain_edges", chain_edges},
    };
}

int main() {
    json out;
    json cases = json::array();

    // TEST(S2WrappedShape, Coverage): wrap a three-loop lax polygon and
    // confirm every S2Shape method is reachable through the wrapper. The
    // degenerate first loop exercises single-vertex / single-chain paths.
    cases.push_back(wrapped_summary(
        "coverage", {ParseLatLngs("0:0"), ParseLatLngs("1:1, 1:2, 2:1")}));

    // Additional coverage: an empty polygon to pin the degenerate-shape path.
    cases.push_back(wrapped_summary("empty", {}));

    // Additional coverage: a two-loop polygon with an outer shell and a hole,
    // mirroring the multi-loop case in s2lax_polygon_shape_test.cc.
    cases.push_back(wrapped_summary(
        "multi_loop",
        {ParseLatLngs("0:0, 0:3, 3:3"), ParseLatLngs("1:1, 2:2, 1:2")}));

    out["shapes"] = cases;

    std::cout << out.dump(2) << std::endl;
    return 0;
}
