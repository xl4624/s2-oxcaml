// Golden data generator for s2shapeutil::GetReferencePoint.
// Mirrors s2geometry/src/s2/s2shapeutil_get_reference_point_test.cc.

#include "s2/s2shapeutil_get_reference_point.h"

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

static json reference_point_json(const S2Shape::ReferencePoint &rp) {
    return json::object({
        {"point", point_json(rp.point)},
        {"contained", rp.contained},
    });
}

static json case_json(const std::string &name, const vector<Loop> &loops) {
    S2LaxPolygonShape shape(loops);
    auto rp = s2shapeutil::GetReferencePoint(shape);
    return json::object({
        {"name", name},
        {"loops", loops_json(loops)},
        {"reference_point", reference_point_json(rp)},
    });
}

int main() {
    json out;
    json cases = json::array();

    // TEST(GetReferencePoint, EmptyPolygon): a polygon with no loops at all
    // has no interior, so the reference point is "not contained".
    cases.push_back(case_json("empty_polygon", vector<Loop>{}));

    // TEST(GetReferencePoint, FullPolygon): a single loop with zero vertices
    // marks the full polygon (every point on the sphere is contained).
    cases.push_back(case_json("full_polygon", vector<Loop>{Loop{}}));

    // TEST(GetReferencePoint, DegenerateLoops): every edge has a sibling
    // pair, so all vertices are balanced and the polygon is empty. None of
    // the chains have length zero, so the answer is "not contained".
    cases.push_back(
        case_json("degenerate_loops",
                  vector<Loop>{
                      ParseLatLngs("1:1, 1:2, 2:2, 1:2, 1:3, 1:2, 1:1"),
                      ParseLatLngs("0:0, 0:3, 0:6, 0:9, 0:6, 0:3, 0:0"),
                      ParseLatLngs("5:5, 6:6"),
                  }));

    // TEST(GetReferencePoint, InvertedLoops): the upstream test only checks
    // ContainsBruteForce(shape, S2::Origin()). Captured here so the OCaml
    // port can pin both that the chosen reference point is the same and
    // that its containment matches C++.
    cases.push_back(
        case_json("inverted_loops", vector<Loop>{
                                        ParseLatLngs("1:2, 1:1, 2:2"),
                                        ParseLatLngs("3:4, 3:3, 4:4"),
                                    }));

    // Extra coverage: a normal CCW triangle. Every vertex is unbalanced,
    // so the algorithm returns at the first vertex it tries (edge(0).v0).
    cases.push_back(
        case_json("ccw_triangle", vector<Loop>{ParseLatLngs("0:0, 0:1, 1:0")}));

    // Extra coverage: a clockwise triangle, oriented so its interior is the
    // complement of the CCW triangle above.
    cases.push_back(
        case_json("cw_triangle", vector<Loop>{ParseLatLngs("0:0, 1:0, 0:1")}));

    out["cases"] = cases;

    std::cout << out.dump(2) << std::endl;
    return 0;
}
