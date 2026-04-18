// Golden data generator for S2LaxLoopShape.

#include <cstdlib>
#include <iostream>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/s2latlng.h"
#include "s2/s2lax_loop_shape.h"
#include "s2/s2point.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::string;
using std::vector;

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

static json shape_summary(const std::string &name,
                          const vector<S2Point> &vertices) {
    S2LaxLoopShape shape(vertices);
    json vertex_arr = json::array();
    for (const auto &p : vertices)
        vertex_arr.push_back(point_json(p));
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
        {"vertices", vertex_arr},
        {"num_vertices", shape.num_vertices()},
        {"num_edges", shape.num_edges()},
        {"num_chains", shape.num_chains()},
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

    // TEST(S2LaxLoopShape, EmptyLoop)
    // TEST(S2LaxLoopShape, NonEmptyLoop)
    {
        json cases = json::array();
        cases.push_back(shape_summary("empty_loop", {}));
        cases.push_back(shape_summary("non_empty_loop",
                                      ParseLatLngs("0:0, 0:1, 1:1, 1:0")));
        // Extra coverage: a single-vertex degenerate loop (one zero-length
        // edge from the vertex to itself).
        cases.push_back(shape_summary("single_vertex", {S2Point(1, 0, 0)}));
        // Extra coverage: a two-vertex degenerate loop with two zero-length
        // edges (a "back-and-forth" loop). All edges are matched, so the
        // shape is empty by convention.
        cases.push_back(shape_summary("two_vertex", ParseLatLngs("0:0, 0:1")));
        // Extra coverage: a triangle loop.
        cases.push_back(
            shape_summary("triangle", ParseLatLngs("0:0, 0:5, 5:0")));
        out["shapes"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
