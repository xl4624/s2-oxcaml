// Golden data generator for S2LaxPolylineShape.

#include <cstdlib>
#include <iostream>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/s2latlng.h"
#include "s2/s2lax_polyline_shape.h"
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
    S2LaxPolylineShape shape(vertices);
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
        {"type_tag", static_cast<int>(shape.type_tag())},
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

    // TEST(S2LaxPolylineShape, NoVertices)
    // TEST(S2LaxPolylineShape, OneVertex)
    // TEST(S2LaxPolylineShape, EdgeAccess)
    {
        json cases = json::array();
        cases.push_back(shape_summary("no_vertices", {}));
        cases.push_back(shape_summary("one_vertex", {S2Point(1, 0, 0)}));
        cases.push_back(
            shape_summary("edge_access", ParseLatLngs("0:0, 0:1, 1:1")));
        // Extra coverage: longer polyline with 4 vertices.
        cases.push_back(
            shape_summary("four_vertices", ParseLatLngs("1:1, 4:4, 2:2, 3:3")));
        // Extra coverage: degenerate polyline with repeated vertex (single
        // zero-length edge). This is explicitly allowed by LaxPolyline.
        cases.push_back(
            shape_summary("degenerate_repeat", ParseLatLngs("5:5, 5:5")));
        // Extra coverage: two antipodal points (allowed by LaxPolyline but
        // not by S2Polyline).
        cases.push_back(
            shape_summary("antipodal", {S2Point(1, 0, 0), S2Point(-1, 0, 0)}));
        out["shapes"] = cases;
    }

    // Type tag constant.
    out["type_tag"] = S2LaxPolylineShape::kTypeTag;

    std::cout << out.dump(2) << std::endl;
    return 0;
}
