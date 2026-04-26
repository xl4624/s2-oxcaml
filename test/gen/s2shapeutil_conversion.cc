// Golden data generator for s2shapeutil::ShapeToS2{Points,Polyline,Polygon}.
// Mirrors s2geometry/src/s2/s2shapeutil_conversion_test.cc.

#include "s2/s2shapeutil_conversion.h"

#include <cstdlib>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/s2latlng.h"
#include "s2/s2lax_polygon_shape.h"
#include "s2/s2lax_polyline_shape.h"
#include "s2/s2loop.h"
#include "s2/s2point.h"
#include "s2/s2point_vector_shape.h"
#include "s2/s2polygon.h"
#include "s2/s2polyline.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::string;
using std::unique_ptr;
using std::vector;
using Loop = vector<S2Point>;

static json point_json(const S2Point &p) {
    return json::array({p.x(), p.y(), p.z()});
}

// Parse a comma-separated list of "lat:lng" pairs (in degrees).
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

static json points_json(const vector<S2Point> &vs) {
    json out = json::array();
    for (const auto &p : vs)
        out.push_back(point_json(p));
    return out;
}

static json loops_json(const vector<Loop> &loops) {
    json arr = json::array();
    for (const auto &loop : loops)
        arr.push_back(points_json(loop));
    return arr;
}

// TEST(S2ShapeConversionUtilTest, PointVectorShapeToPoints)
static json points_case() {
    vector<S2Point> points = ParseLatLngs("11:11, 10:0, 5:5");
    S2PointVectorShape shape(points);
    vector<S2Point> out = s2shapeutil::ShapeToS2Points(shape);
    return json::object({
        {"name", "point_vector_shape_to_points"},
        {"kind", "points"},
        {"input_points", points_json(points)},
        {"expected_points", points_json(out)},
    });
}

// TEST(S2ShapeConversionUtilTest, LineToS2Polyline)
// TEST(S2ShapeConversionUtilTest, ClosedLineToS2Polyline)
static json polyline_case(const string &name, const string &latlngs) {
    vector<S2Point> points = ParseLatLngs(latlngs);
    S2LaxPolylineShape lax(points);
    unique_ptr<S2Polyline> polyline = s2shapeutil::ShapeToS2Polyline(lax);
    vector<S2Point> expected;
    for (int i = 0; i < polyline->num_vertices(); ++i)
        expected.push_back(polyline->vertex(i));
    return json::object({
        {"name", name},
        {"kind", "polyline"},
        {"input_vertices", points_json(points)},
        {"expected_vertices", points_json(expected)},
    });
}

// Polygon cases: emit each input loop as it was given to S2LaxPolygonShape,
// plus the resulting polygon's loops captured via [oriented_vertex] so the
// OCaml port can compare orientation-preserving output. The C++ test pins
// loop.oriented_vertex(j) against the input loops directly.
static json polygon_case(const string &name, const vector<Loop> &input_loops,
                         bool is_full = false) {
    S2LaxPolygonShape lax(input_loops);
    unique_ptr<S2Polygon> polygon = s2shapeutil::ShapeToS2Polygon(lax);

    json out_loops = json::array();
    for (int i = 0; i < polygon->num_loops(); ++i) {
        const S2Loop &loop = *polygon->loop(i);
        json loop_pts = json::array();
        for (int j = 0; j < loop.num_vertices(); ++j)
            loop_pts.push_back(point_json(loop.oriented_vertex(j)));
        out_loops.push_back(loop_pts);
    }
    return json::object({
        {"name", name},
        {"kind", "polygon"},
        {"input_loops", loops_json(input_loops)},
        {"input_is_full", is_full},
        {"expected_num_loops", polygon->num_loops()},
        {"expected_num_vertices", polygon->num_vertices()},
        {"expected_is_full", polygon->is_full()},
        {"expected_oriented_loops", out_loops},
    });
}

int main() {
    json out;
    json cases = json::array();

    cases.push_back(points_case());

    cases.push_back(polyline_case("line_to_polyline", "11:11, 10:0, 5:5"));
    cases.push_back(
        polyline_case("closed_line_to_polyline", "0:0, 0:10, 10:10, 0:0"));

    // TEST(S2ShapeConversionUtilTest, PolygonWithHoleToS2Polygon)
    cases.push_back(polygon_case("polygon_with_hole",
                                 {ParseLatLngs("0:0, 0:10, 10:10, 10:0"),
                                  ParseLatLngs("4:4, 6:4, 6:6, 4:6")}));

    // TEST(S2ShapeConversionUtilTest, MultiPolygonToS2Polygon)
    cases.push_back(polygon_case(
        "multi_polygon",
        {ParseLatLngs("0:0, 0:2, 2:2, 2:0"), ParseLatLngs("0:4, 0:6, 3:6")}));

    // TEST(S2ShapeConversionUtilTest, TwoHolesToS2Polygon)
    cases.push_back(polygon_case(
        "two_holes",
        {ParseLatLngs("0:0, 0:10, 10:10, 10:0"), ParseLatLngs("1:1, 3:3, 1:3"),
         ParseLatLngs("2:6, 4:7, 2:8")}));

    // TEST(S2ShapeConversionUtilTest, FullPolygonToS2Polygon)
    // Full polygon: a single zero-vertex loop marks the full sphere.
    cases.push_back(polygon_case("full_polygon", {Loop{}}, /*is_full=*/true));

    out["cases"] = cases;
    std::cout << out.dump(2) << std::endl;
    return 0;
}
