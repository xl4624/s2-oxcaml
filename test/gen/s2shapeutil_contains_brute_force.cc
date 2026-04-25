// Golden data generator for s2shapeutil::ContainsBruteForce.
// Mirrors s2geometry/src/s2/s2shapeutil_contains_brute_force_test.cc.

#include "s2/s2shapeutil_contains_brute_force.h"

#include <cstdlib>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/s1angle.h"
#include "s2/s2latlng.h"
#include "s2/s2lax_polygon_shape.h"
#include "s2/s2lax_polyline_shape.h"
#include "s2/s2loop.h"
#include "s2/s2point.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::make_unique;
using std::string;
using std::vector;

static json point_json(const S2Point &p) {
    return json::array({p.x(), p.y(), p.z()});
}

// Parse a comma-separated list of "lat:lng" pairs (in degrees), matching the
// subset of s2textformat::ParsePointsOrDie that we need.
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

int main() {
    json out;

    // TEST(ContainsBruteForce, NoInterior): a polyline never contains any
    // point because its dimension is 1.
    {
        vector<S2Point> verts = ParseLatLngs("0:0, 0:1, 1:-1, -1:-1, -1e-9:1");
        S2LaxPolylineShape polyline(verts);
        S2Point query = S2LatLng::FromDegrees(0, 0).ToPoint();
        out["no_interior"] = json{
            {"kind", "lax_polyline"},
            {"vertices", vertices_json(verts)},
            {"query", point_json(query)},
            {"expected", s2shapeutil::ContainsBruteForce(polyline, query)},
        };
    }

    // TEST(ContainsBruteForce, ContainsReferencePoint): containment of the
    // shape's own reference point must agree with [GetReferencePoint].
    {
        vector<S2Point> verts = ParseLatLngs("0:0, 0:1, 1:-1, -1:-1, -1e-9:1");
        vector<vector<S2Point>> loops = {verts};
        S2LaxPolygonShape polygon(loops);
        S2Shape::ReferencePoint ref = polygon.GetReferencePoint();
        json cases = json::array();
        cases.push_back(json{
            {"query", point_json(ref.point)},
            {"expected", s2shapeutil::ContainsBruteForce(polygon, ref.point)},
            {"reference_contained", ref.contained},
        });
        out["contains_reference_point"] = json{
            {"kind", "lax_polygon"},
            {"loop_vertices", vertices_json(verts)},
            {"cases", cases},
        };
    }

    // TEST(ContainsBruteForce, ConsistentWithS2Loop): a 100-vertex regular
    // loop centred at "89:-179" with radius 10 degrees. For each vertex,
    // [ContainsBruteForce] must agree with [S2Loop::Contains].
    {
        S2Point center = S2LatLng::FromDegrees(89, -179).ToPoint();
        auto loop = S2Loop::MakeRegularLoop(center, S1Angle::Degrees(10), 100);
        S2Loop::Shape shape(loop.get());
        vector<S2Point> verts;
        for (int i = 0; i < loop->num_vertices(); ++i)
            verts.push_back(loop->vertex(i));
        json cases = json::array();
        for (int i = 0; i < loop->num_vertices(); ++i) {
            S2Point v = loop->vertex(i);
            cases.push_back(json{
                {"query", point_json(v)},
                {"expected", s2shapeutil::ContainsBruteForce(shape, v)},
                {"loop_contains", loop->Contains(v)},
            });
        }
        out["consistent_with_s2loop"] = json{
            {"kind", "loop"},
            {"loop_vertices", vertices_json(verts)},
            {"cases", cases},
        };
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
