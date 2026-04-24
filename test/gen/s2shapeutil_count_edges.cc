// Golden data generator for s2shapeutil::CountEdges / CountEdgesUpTo.

#include "s2/s2shapeutil_count_edges.h"

#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/mutable_s2shape_index.h"
#include "s2/s2latlng.h"
#include "s2/s2point.h"
#include "s2/s2point_vector_shape.h"
#include "s2/s2polyline.h"
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

// Serialise a shape description so OCaml rebuilds an equivalent S2Shape.
// "kind" is either "points" (S2PointVectorShape) or "polyline"
// (S2Polyline).
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

struct Shape {
    enum class Kind { POINTS, POLYLINE };
    Kind kind;
    vector<S2Point> vertices;
};

struct Case {
    string name;
    vector<Shape> shapes;
    vector<int> thresholds;
};

static json build_case(const Case &c) {
    MutableS2ShapeIndex index;
    json shapes_arr = json::array();
    json num_edges_arr = json::array();
    for (const auto &s : c.shapes) {
        if (s.kind == Shape::Kind::POINTS) {
            shapes_arr.push_back(points_shape_json(s.vertices));
            auto shape = make_unique<S2PointVectorShape>(s.vertices);
            num_edges_arr.push_back(shape->num_edges());
            index.Add(std::move(shape));
        } else {
            shapes_arr.push_back(polyline_shape_json(s.vertices));
            auto polyline = make_unique<S2Polyline>(s.vertices);
            auto shape = make_unique<S2Polyline::Shape>(polyline.get());
            num_edges_arr.push_back(shape->num_edges());
            // S2Polyline::Shape doesn't own its polyline; keep the polyline
            // alive by moving it into a shape that owns it. Use
            // S2Polyline::OwningShape instead.
            (void)shape;
            index.Add(
                make_unique<S2Polyline::OwningShape>(std::move(polyline)));
        }
    }

    json up_to = json::array();
    for (int t : c.thresholds) {
        up_to.push_back({{"max_edges", t},
                         {"expected", s2shapeutil::CountEdgesUpTo(index, t)}});
    }

    return {
        {"name", c.name},
        {"shapes", shapes_arr},
        {"shape_num_edges", num_edges_arr},
        {"total_edges", s2shapeutil::CountEdges(index)},
        {"up_to", up_to},
    };
}

int main() {
    json out;
    json cases = json::array();

    // TEST(CountEdgesUpTo, StopsEarly) with the upstream index
    //   "0:0 | 0:1 | 0:2 | 0:3 | 0:4 # 1:0, 1:1 | 1:2, 1:3 | 1:4, 1:5, 1:6 #"
    // Five points (1 shape, 5 edges) and three polylines (1, 1, 2 edges).
    {
        Case c;
        c.name = "stops_early";
        c.shapes = {
            {Shape::Kind::POINTS,
             {pt(0, 0), pt(0, 1), pt(0, 2), pt(0, 3), pt(0, 4)}},
            {Shape::Kind::POLYLINE, {pt(1, 0), pt(1, 1)}},
            {Shape::Kind::POLYLINE, {pt(1, 2), pt(1, 3)}},
            {Shape::Kind::POLYLINE, {pt(1, 4), pt(1, 5), pt(1, 6)}},
        };
        c.thresholds = {1, 5, 6, 8, 100};
        cases.push_back(build_case(c));
    }

    // Extra coverage: empty index.
    {
        Case c;
        c.name = "empty";
        c.thresholds = {0, 1, 100};
        cases.push_back(build_case(c));
    }

    // Extra coverage: threshold = 0 on a non-empty index should return
    // after the first shape (matches the "num_edges >= max_edges" check that
    // fires once any positive count is reached).
    {
        Case c;
        c.name = "threshold_zero";
        c.shapes = {
            {Shape::Kind::POINTS, {pt(0, 0), pt(0, 1), pt(0, 2)}},
            {Shape::Kind::POLYLINE, {pt(1, 0), pt(1, 1)}},
        };
        c.thresholds = {0, 1, 2, 3, 4, 5};
        cases.push_back(build_case(c));
    }

    out["cases"] = cases;
    std::cout << out.dump(2) << std::endl;
    return 0;
}
