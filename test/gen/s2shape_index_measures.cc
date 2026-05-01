// Golden data generator for S2::GetDimension, GetNumPoints, GetLength,
// GetPerimeter, GetArea, GetApproxArea, GetCentroid on S2ShapeIndex values.

#include "s2/s2shape_index_measures.h"

#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <utility>
#include <vector>

#include "s2/mutable_s2shape_index.h"
#include "s2/s1angle.h"
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

static json measures_for(const S2ShapeIndex &index) {
    json out;
    out["dimension"] = S2::GetDimension(index);
    out["num_points"] = S2::GetNumPoints(index);
    out["length_radians"] = S2::GetLength(index).radians();
    out["perimeter_radians"] = S2::GetPerimeter(index).radians();
    out["area"] = S2::GetArea(index);
    out["approx_area"] = S2::GetApproxArea(index);
    out["centroid"] = point_json(S2::GetCentroid(index));
    return out;
}

// Builder for a single test case. Each helper appends a shape description and
// the matching live S2Shape so the index used to compute the expected values
// matches what OCaml will reconstruct from the JSON.
struct CaseBuilder {
    MutableS2ShapeIndex index;
    json shape_descs = json::array();

    void add_points(const vector<S2Point> &points) {
        shape_descs.push_back(points_shape_json(points));
        index.Add(make_unique<S2PointVectorShape>(points));
    }

    void add_polyline(const vector<S2Point> &vertices) {
        shape_descs.push_back(polyline_shape_json(vertices));
        index.Add(make_unique<S2LaxPolylineShape>(vertices));
    }

    void add_polygon(const vector<vector<S2Point>> &loops) {
        shape_descs.push_back(polygon_shape_json(loops));
        index.Add(make_unique<S2LaxPolygonShape>(loops));
    }

    json finish(const string &name) {
        index.ForceBuild();
        return {{"name", name},
                {"shapes", shape_descs},
                {"expected", measures_for(index)}};
    }
};

int main() {
    json out;
    json cases = json::array();

    // TEST(GetDimension, Empty) - "# #" produces an empty index.
    {
        CaseBuilder b;
        cases.push_back(b.finish("empty"));
    }

    // TEST(GetDimension, Points) - single point shape, plus the empty
    // point-set variant.
    {
        CaseBuilder b;
        b.add_points({pt(0, 0)});
        cases.push_back(b.finish("single_point"));
    }
    {
        CaseBuilder b;
        b.add_points({});
        cases.push_back(b.finish("empty_point_set"));
    }

    // TEST(GetDimension, PointsAndLines) - point + 2-vertex polyline, plus
    // a 1-vertex polyline (no edges) so the dimension still reflects the
    // polyline's nominal dimension.
    {
        CaseBuilder b;
        b.add_points({pt(0, 0)});
        b.add_polyline({pt(1, 1), pt(1, 2)});
        cases.push_back(b.finish("point_and_polyline"));
    }
    {
        CaseBuilder b;
        b.add_points({pt(0, 0)});
        b.add_polyline({pt(1, 1)});
        cases.push_back(b.finish("point_and_one_vertex_polyline"));
    }

    // TEST(GetDimension, PointsLinesAndPolygons) - point + polyline +
    // triangle, plus an empty polygon (full-polygon convention).
    {
        CaseBuilder b;
        b.add_points({pt(0, 0)});
        b.add_polyline({pt(1, 1), pt(2, 2)});
        b.add_polygon({{pt(3, 3), pt(3, 4), pt(4, 3)}});
        cases.push_back(b.finish("point_polyline_triangle"));
    }
    {
        CaseBuilder b;
        b.add_polygon({});
        cases.push_back(b.finish("empty_polygon"));
    }

    // TEST(GetNumPoints, TwoPoints) - point shapes contribute their edge
    // counts.
    {
        CaseBuilder b;
        b.add_points({pt(0, 0)});
        b.add_points({pt(1, 0)});
        cases.push_back(b.finish("two_points"));
    }

    // TEST(GetNumPoints, LineAndPolygon) - polyline + polygon contribute 0
    // points.
    {
        CaseBuilder b;
        b.add_polyline({pt(1, 1), pt(1, 2)});
        b.add_polygon({{pt(0, 3), pt(0, 5), pt(2, 5)}});
        cases.push_back(b.finish("line_and_polygon"));
    }

    // TEST(GetLength, TwoLines) - two polylines of total length 2 degrees,
    // plus a point and triangle that contribute 0 to the length.
    {
        CaseBuilder b;
        b.add_points({pt(4, 4)});
        b.add_polyline({pt(0, 0), pt(1, 0)});
        b.add_polyline({pt(1, 0), pt(2, 0)});
        b.add_polygon({{pt(5, 5), pt(5, 6), pt(6, 5)}});
        cases.push_back(b.finish("two_lines_with_other_shapes"));
    }

    // TEST(GetPerimeter, DegeneratePolygon) - degenerate polyline + polygon
    // with perimeter 4 degrees.
    {
        CaseBuilder b;
        b.add_points({pt(4, 4)});
        b.add_polyline({pt(0, 0), pt(1, 0)});
        b.add_polyline({pt(2, 0), pt(3, 0)});
        b.add_polygon({{pt(0, 1), pt(0, 2), pt(0, 3)}});
        cases.push_back(b.finish("degenerate_polygon_perimeter"));
    }

    // TEST(GetArea, TwoFullPolygons) - sum equals 8*pi.
    {
        CaseBuilder b;
        b.add_polygon({{}});
        b.add_polygon({{}});
        cases.push_back(b.finish("two_full_polygons"));
    }

    // TEST(GetCentroid, Points) - two points, centroid * count = (1, 1, 0).
    {
        CaseBuilder b;
        b.add_points({pt(0, 0), pt(0, 90)});
        cases.push_back(b.finish("centroid_two_points"));
    }

    // TEST(GetCentroid, Polyline) - points are ignored when polylines are
    // present.
    {
        CaseBuilder b;
        b.add_points({pt(5, 5), pt(6, 6)});
        b.add_polyline({pt(0, 0), pt(0, 90)});
        cases.push_back(b.finish("centroid_polyline_with_points"));
    }

    // TEST(GetCentroid, Polygon) - points and polylines are ignored when
    // polygons are present.
    {
        CaseBuilder b;
        b.add_points({pt(5, 5)});
        b.add_polyline({pt(6, 6), pt(7, 7)});
        b.add_polygon({{pt(0, 0), pt(0, 90), pt(90, 0)}});
        cases.push_back(b.finish("centroid_polygon_with_others"));
    }

    out["cases"] = cases;
    std::cout << out.dump(2) << std::endl;
    return 0;
}
