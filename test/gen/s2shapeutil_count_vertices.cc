// Golden data generator for s2shapeutil::CountVertices.

#include "s2/s2shapeutil_count_vertices.h"

#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/mutable_s2shape_index.h"
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

struct Shape {
    enum class Kind { POINTS, POLYLINE, POLYGON };
    Kind kind;
    vector<S2Point> points;                 // POINTS, POLYLINE
    vector<vector<S2Point>> polygon_loops;  // POLYGON
};

struct Case {
    string name;
    vector<Shape> shapes;
};

static json build_case(const Case &c) {
    MutableS2ShapeIndex index;
    json shapes_arr = json::array();
    json per_shape_counts = json::array();
    for (const auto &s : c.shapes) {
        switch (s.kind) {
            case Shape::Kind::POINTS: {
                shapes_arr.push_back(points_shape_json(s.points));
                auto shape = make_unique<S2PointVectorShape>(s.points);
                per_shape_counts.push_back(s2shapeutil::CountVertices(*shape));
                index.Add(std::move(shape));
                break;
            }
            case Shape::Kind::POLYLINE: {
                shapes_arr.push_back(polyline_shape_json(s.points));
                auto shape = make_unique<S2LaxPolylineShape>(s.points);
                per_shape_counts.push_back(s2shapeutil::CountVertices(*shape));
                index.Add(std::move(shape));
                break;
            }
            case Shape::Kind::POLYGON: {
                shapes_arr.push_back(polygon_shape_json(s.polygon_loops));
                auto shape = make_unique<S2LaxPolygonShape>(s.polygon_loops);
                per_shape_counts.push_back(s2shapeutil::CountVertices(*shape));
                index.Add(std::move(shape));
                break;
            }
        }
    }

    return {
        {"name", c.name},
        {"shapes", shapes_arr},
        {"per_shape_counts", per_shape_counts},
        {"index_count", s2shapeutil::CountVertices(index)},
    };
}

int main() {
    json out;
    json cases = json::array();

    // TEST(CountVertices, CountsCorrectly): the five upstream scenarios.

    // "1:1 | 2:2 | 3:3 # #" - three one-point shapes, total 3 vertices.
    {
        Case c;
        c.name = "three_points";
        c.shapes = {
            {Shape::Kind::POINTS, {pt(1, 1)}, {}},
            {Shape::Kind::POINTS, {pt(2, 2)}, {}},
            {Shape::Kind::POINTS, {pt(3, 3)}, {}},
        };
        cases.push_back(build_case(c));
    }

    // "1:1 | 2:2 # 3:3, 4:4, 5:5 #" - two points and a 3-vertex polyline.
    {
        Case c;
        c.name = "points_and_polyline";
        c.shapes = {
            {Shape::Kind::POINTS, {pt(1, 1)}, {}},
            {Shape::Kind::POINTS, {pt(2, 2)}, {}},
            {Shape::Kind::POLYLINE, {pt(3, 3), pt(4, 4), pt(5, 5)}, {}},
        };
        cases.push_back(build_case(c));
    }

    // "1:1 | 2:2 # 3:3, 4:4, 5:5 # 6:6, 7:7, 8:8, 9:9" - also add a 4-vertex
    // polygon.
    {
        Case c;
        c.name = "points_polyline_polygon";
        c.shapes = {
            {Shape::Kind::POINTS, {pt(1, 1)}, {}},
            {Shape::Kind::POINTS, {pt(2, 2)}, {}},
            {Shape::Kind::POLYLINE, {pt(3, 3), pt(4, 4), pt(5, 5)}, {}},
            {Shape::Kind::POLYGON,
             {},
             {{pt(6, 6), pt(7, 7), pt(8, 8), pt(9, 9)}}},
        };
        cases.push_back(build_case(c));
    }

    // "# 3:3, 3:3, 3:3 #" - a degenerate 3-vertex polyline (all coincident).
    {
        Case c;
        c.name = "degenerate_polyline";
        c.shapes = {
            {Shape::Kind::POLYLINE, {pt(3, 3), pt(3, 3), pt(3, 3)}, {}},
        };
        cases.push_back(build_case(c));
    }

    // "# # 4:4, 4:4, 4:4, 4:4" - a degenerate 4-vertex polygon.
    {
        Case c;
        c.name = "degenerate_polygon";
        c.shapes = {
            {Shape::Kind::POLYGON,
             {},
             {{pt(4, 4), pt(4, 4), pt(4, 4), pt(4, 4)}}},
        };
        cases.push_back(build_case(c));
    }

    // Extra coverage: empty index.
    {
        Case c;
        c.name = "empty";
        cases.push_back(build_case(c));
    }

    out["cases"] = cases;
    std::cout << out.dump(2) << std::endl;
    return 0;
}
