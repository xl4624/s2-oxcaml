// Golden data generator for S2LaxPolygonShape.

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

// Parse a comma-separated list of "lat:lng" pairs (in degrees), matching the
// subset of s2textformat::ParsePointsOrDie that we need. Avoids a link-time
// dependency on s2text_format.
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

static json shape_summary(const std::string &name, const vector<Loop> &loops) {
    S2LaxPolygonShape shape(loops);
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
    json num_loop_vertices = json::array();
    for (int i = 0; i < shape.num_loops(); ++i) {
        num_loop_vertices.push_back(shape.num_loop_vertices(i));
    }
    auto rp = shape.GetReferencePoint();
    return {
        {"name", name},
        {"loops", loops_json(loops)},
        {"num_loops", shape.num_loops()},
        {"num_vertices", shape.num_vertices()},
        {"num_edges", shape.num_edges()},
        {"num_chains", shape.num_chains()},
        {"num_loop_vertices", num_loop_vertices},
        {"dimension", shape.dimension()},
        {"is_empty", shape.is_empty()},
        {"is_full", shape.is_full()},
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

    // TEST(S2LaxPolygonShape, EmptyPolygon)
    // TEST(S2LaxPolygonShape, FullPolygon)
    // TEST(S2LaxPolygonShape, SingleVertexPolygon)
    // TEST(S2LaxPolygonShape, SingleLoopPolygon)
    // TEST(S2LaxPolygonShape, MultiLoopPolygon)
    // TEST(S2LaxPolygonShape, DegenerateLoops)
    // TEST(S2LaxPolygonShape, InvertedLoops)
    {
        json cases = json::array();
        cases.push_back(shape_summary("empty_polygon", {}));
        // A single loop with zero vertices represents the full polygon.
        cases.push_back(shape_summary("full_polygon", {Loop{}}));
        cases.push_back(
            shape_summary("single_vertex_polygon", {ParseLatLngs("0:0")}));
        cases.push_back(shape_summary("single_loop_polygon",
                                      {ParseLatLngs("0:0, 0:1, 1:1, 1:0")}));
        cases.push_back(shape_summary(
            "multi_loop_polygon",
            {ParseLatLngs("0:0, 0:3, 3:3"), ParseLatLngs("1:1, 2:2, 1:2")}));
        cases.push_back(
            shape_summary("degenerate_loops",
                          {ParseLatLngs("1:1, 1:2, 2:2, 1:2, 1:3, 1:2, 1:1"),
                           ParseLatLngs("0:0, 0:3, 0:6, 0:9, 0:6, 0:3, 0:0"),
                           ParseLatLngs("5:5, 6:6")}));
        cases.push_back(shape_summary(
            "inverted_loops",
            {ParseLatLngs("1:2, 1:1, 2:2"), ParseLatLngs("3:4, 3:3, 4:4")}));
        out["shapes"] = cases;
    }

    // Type tag constant.
    out["type_tag"] = S2LaxPolygonShape::kTypeTag;

    std::cout << out.dump(2) << std::endl;
    return 0;
}
