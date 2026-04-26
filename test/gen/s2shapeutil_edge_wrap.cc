// Golden data generator for s2shapeutil::NextEdgeWrap and PrevEdgeWrap.
// Mirrors s2geometry/src/s2/s2shapeutil_edge_wrap_test.cc.

#include "s2/s2shapeutil_edge_wrap.h"

#include <cstdlib>
#include <iostream>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/s2latlng.h"
#include "s2/s2lax_polygon_shape.h"
#include "s2/s2point.h"
#include "s2/s2point_vector_shape.h"
#include "s2/s2polyline.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::string;
using std::vector;

static json point_json(const S2Point &p) {
    return json::array({p.x(), p.y(), p.z()});
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

static json vertices_json(const vector<S2Point> &vs) {
    json out = json::array();
    for (const auto &p : vs)
        out.push_back(point_json(p));
    return out;
}

// For every edge id in [shape], record the result of NextEdgeWrap and
// PrevEdgeWrap.
static json walk_edges(const S2Shape &shape) {
    json arr = json::array();
    for (int i = 0; i < shape.num_edges(); ++i) {
        arr.push_back(json{
            {"edge_id", i},
            {"next", s2shapeutil::NextEdgeWrap(shape, i)},
            {"prev", s2shapeutil::PrevEdgeWrap(shape, i)},
        });
    }
    return arr;
}

static json points_case(const string &name, const vector<S2Point> &pts) {
    S2PointVectorShape shape(pts);
    return json{
        {"name", name},
        {"kind", "points"},
        {"vertices", vertices_json(pts)},
        {"edges", walk_edges(shape)},
    };
}

static json polyline_case(const string &name, const vector<S2Point> &vs) {
    S2Polyline polyline(vs);
    S2Polyline::Shape shape(&polyline);
    return json{
        {"name", name},
        {"kind", "polyline"},
        {"vertices", vertices_json(vs)},
        {"edges", walk_edges(shape)},
    };
}

static json polygon_case(const string &name,
                         const vector<vector<S2Point>> &loops) {
    S2LaxPolygonShape shape(loops);
    json loops_json = json::array();
    for (const auto &loop : loops) {
        loops_json.push_back(vertices_json(loop));
    }
    return json{
        {"name", name},
        {"kind", "polygon"},
        {"loops", loops_json},
        {"edges", walk_edges(shape)},
    };
}

int main() {
    json out;
    json cases = json::array();

    // TEST(S2Shape, NextPrevEdgePointDoesNotWrap): "1:1 | 2:2 ##".
    // Each point is its own length-1 chain at dimension 0; both Next and
    // Prev always return -1.
    cases.push_back(points_case("points_no_wrap", ParseLatLngs("1:1, 2:2")));

    // TEST(S2Shape, NextPrevEdgeOpenPolylineDoesNotWrap):
    // "# 1:1, 2:2, 3:3 #". Open polyline (last vertex != first); reaching
    // either chain end returns -1.
    cases.push_back(
        polyline_case("open_polyline_no_wrap", ParseLatLngs("1:1, 2:2, 3:3")));

    // TEST(S2Shape, NextPrevEdgeClosedPolylineWraps):
    // "# 0:0, 1:1, 0:2, -1:1, 0:0 #". The terminal vertex equals the
    // initial one, so the chain is closed and Next/Prev wrap.
    cases.push_back(polyline_case("closed_polyline_wraps",
                                  ParseLatLngs("0:0, 1:1, 0:2, -1:1, 0:0")));

    // TEST(S2Shape, NextPrevEdgePolygonWraps): "## 0:0, 1:1, 0:2, -1:1".
    // Polygon chains always wrap regardless of the underlying loop's
    // explicit closure.
    cases.push_back(
        polygon_case("polygon_wraps", {ParseLatLngs("0:0, 1:1, 0:2, -1:1")}));

    // Extra coverage: a polygon with two loops. Verifies that wrap stays
    // inside the per-chain [start, start + length) window, not across
    // chains.
    cases.push_back(polygon_case(
        "polygon_two_loops_wrap_per_chain",
        {ParseLatLngs("0:0, 0:1, 1:1, 1:0"), ParseLatLngs("5:5, 5:6, 6:6")}));

    // Extra coverage: a single-edge open polyline. Both endpoints must
    // return -1 since neither end is connected to the other.
    cases.push_back(
        polyline_case("single_edge_open_polyline", ParseLatLngs("0:0, 1:1")));

    out["cases"] = cases;
    std::cout << out.dump(2) << std::endl;
    return 0;
}
