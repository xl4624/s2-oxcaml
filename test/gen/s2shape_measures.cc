// Golden data generator for S2::GetLength, GetPerimeter, GetArea,
// GetApproxArea, GetCentroid on S2Shape values.

#include "s2/s2shape_measures.h"

#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <utility>
#include <vector>

#include "s2/s1angle.h"
#include "s2/s2edge_vector_shape.h"
#include "s2/s2latlng.h"
#include "s2/s2lax_polygon_shape.h"
#include "s2/s2lax_polyline_shape.h"
#include "s2/s2point.h"
#include "s2/s2point_vector_shape.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::make_unique;
using std::string;
using std::vector;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static S2Point pt(double lat_deg, double lng_deg) {
    return S2LatLng::FromDegrees(lat_deg, lng_deg).ToPoint();
}

static json points_shape_json(const vector<S2Point> &points) {
    json vs = json::array();
    for (const auto &p : points)
        vs.push_back(point_json(p));
    return {{"kind", "points"}, {"points", vs}};
}

static json polyline_shape_json(const vector<S2Point> &vertices) {
    json vs = json::array();
    for (const auto &p : vertices)
        vs.push_back(point_json(p));
    return {{"kind", "polyline"}, {"vertices", vs}};
}

static json polygon_shape_json(const vector<vector<S2Point>> &loops) {
    json arr = json::array();
    for (const auto &loop : loops) {
        json pts = json::array();
        for (const auto &p : loop)
            pts.push_back(point_json(p));
        arr.push_back(pts);
    }
    return {{"kind", "polygon"}, {"loops", arr}};
}

static json edge_vector_shape_json(
    const vector<std::pair<S2Point, S2Point>> &edges) {
    json arr = json::array();
    for (const auto &e : edges) {
        arr.push_back({point_json(e.first), point_json(e.second)});
    }
    return {{"kind", "edge_vector"}, {"edges", arr}};
}

static json measures_for(const S2Shape &shape) {
    json out;
    out["length_radians"] = S2::GetLength(shape).radians();
    out["perimeter_radians"] = S2::GetPerimeter(shape).radians();
    out["area"] = S2::GetArea(shape);
    out["approx_area"] = S2::GetApproxArea(shape);
    out["centroid"] = point_json(S2::GetCentroid(shape));
    return out;
}

static json case_polyline(const string &name, const vector<S2Point> &vertices) {
    S2LaxPolylineShape shape(vertices);
    return {{"name", name},
            {"shape", polyline_shape_json(vertices)},
            {"expected", measures_for(shape)}};
}

static json case_polygon(const string &name,
                         const vector<vector<S2Point>> &loops) {
    S2LaxPolygonShape shape(loops);
    return {{"name", name},
            {"shape", polygon_shape_json(loops)},
            {"expected", measures_for(shape)}};
}

static json case_points(const string &name, const vector<S2Point> &points) {
    S2PointVectorShape shape(points);
    return {{"name", name},
            {"shape", points_shape_json(points)},
            {"expected", measures_for(shape)}};
}

static json case_edge_vector(const string &name,
                             const vector<std::pair<S2Point, S2Point>> &edges,
                             int dimension) {
    S2EdgeVectorShape shape;
    for (const auto &e : edges)
        shape.Add(e.first, e.second);
    shape.set_dimension(dimension);
    json j = {{"name", name},
              {"shape", edge_vector_shape_json(edges)},
              {"dimension", dimension},
              {"expected", measures_for(shape)}};
    return j;
}

int main() {
    json out;
    json cases = json::array();

    // TEST(GetLength, WrongDimension) - shape of dimension 0 or 2 returns
    // length 0.
    cases.push_back(case_points("dim0_point", {pt(0, 0)}));
    cases.push_back(
        case_polygon("dim2_triangle", {{pt(0, 0), pt(0, 1), pt(1, 0)}}));

    // TEST(GetLength, NoPolylines) - empty polyline.
    cases.push_back(case_polyline("empty_polyline", {}));

    // TEST(GetLength, ThreePolylinesInOneShape) - S2EdgeVectorShape with three
    // 1-degree edges; total length = 3 degrees.
    {
        auto p = std::vector<S2Point>{pt(0, 0), pt(1, 0), pt(2, 0), pt(3, 0)};
        cases.push_back(case_edge_vector(
            "three_polylines_one_shape",
            {{p[0], p[1]}, {p[0], p[2]}, {p[0], p[3]}}, /*dimension=*/1));
    }

    // TEST(GetPerimeter, EmptyPolygon) - empty polygon.
    cases.push_back(case_polygon("empty_polygon", {}));

    // TEST(GetPerimeter, FullPolygon) - full polygon (single empty loop).
    cases.push_back(case_polygon("full_polygon", {{}}));

    // TEST(GetPerimeter, TwoLoopPolygon) - two degenerate loops with 6 1-degree
    // edges total.
    cases.push_back(
        case_polygon("two_loop_polygon",
                     {{pt(0, 0), pt(1, 0)}, {pt(0, 1), pt(0, 2), pt(0, 3)}}));

    // TEST(GetArea, TwoTinyShells) - two tiny squares with 1e-10 degree sides.
    {
        double s = 1e-10;
        cases.push_back(case_polygon(
            "two_tiny_shells", {{pt(0, 0), pt(0, s), pt(s, s), pt(s, 0)},
                                {pt(0, 0), pt(0, -s), pt(-s, -s), pt(-s, 0)}}));
    }

    // TEST(GetArea, TinyShellAndHole) - shell with a hole inside.
    {
        double s = 1e-10;
        cases.push_back(case_polygon(
            "tiny_shell_and_hole",
            {{pt(0, 0), pt(0, 2 * s), pt(2 * s, 2 * s), pt(2 * s, 0)},
             {pt(0.5 * s, 0.5 * s), pt(1.5 * s, 0.5 * s), pt(1.5 * s, 1.5 * s),
              pt(0.5 * s, 1.5 * s)}}));
    }

    // TEST(GetApproxArea, LargeShellAndHolePolygon) - large shell with a hole.
    cases.push_back(case_polygon("large_shell_and_hole",
                                 {{pt(0, 0), pt(0, 90), pt(90, 0)},
                                  {pt(0, 22.5), pt(90, 0), pt(0, 67.5)}}));

    // TEST(GetCentroid, Points) - two points; centroid times count = (1, 1, 0).
    cases.push_back(case_points("two_points", {pt(0, 0), pt(0, 90)}));

    // TEST(GetCentroid, Polyline) - single-edge polyline along the equator.
    cases.push_back(case_polyline("centroid_polyline", {pt(0, 0), pt(0, 90)}));

    // TEST(GetCentroid, Polygon) - quarter-sphere triangle.
    cases.push_back(case_polygon("centroid_quarter_triangle",
                                 {{pt(0, 0), pt(0, 90), pt(90, 0)}}));

    out["cases"] = cases;
    std::cout << out.dump(2) << std::endl;
    return 0;
}
