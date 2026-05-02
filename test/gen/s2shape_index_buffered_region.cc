// Golden data generator for S2ShapeIndexBufferedRegion (radius accessor,
// cap/rect bounds, conservative cell_union_bound, and sampled
// contains_cell / may_intersect_cell / contains_point queries).

#include "s2/s2shape_index_buffered_region.h"

#include <cstdlib>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/mutable_s2shape_index.h"
#include "s2/s1angle.h"
#include "s2/s1chord_angle.h"
#include "s2/s2cap.h"
#include "s2/s2cell.h"
#include "s2/s2cell_id.h"
#include "s2/s2latlng.h"
#include "s2/s2latlng_rect.h"
#include "s2/s2lax_loop_shape.h"
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

static json cap_json(const S2Cap &c) {
    return {{"center", point_json(c.center())},
            {"height", c.height()},
            {"radius_radians", c.GetRadius().radians()}};
}

static json rect_json(const S2LatLngRect &r) {
    return {{"lat_lo_radians", r.lat_lo().radians()},
            {"lat_hi_radians", r.lat_hi().radians()},
            {"lng_lo_radians", r.lng_lo().radians()},
            {"lng_hi_radians", r.lng_hi().radians()}};
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

static json loop_shape_json(const vector<S2Point> &vertices) {
    json vs = json::array();
    for (const auto &p : vertices)
        vs.push_back(point_json(p));
    return {{"kind", "lax_loop"}, {"vertices", vs}};
}

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

    void add_loop(const vector<S2Point> &vertices) {
        shape_descs.push_back(loop_shape_json(vertices));
        index.Add(make_unique<S2LaxLoopShape>(vertices));
    }
};

static json cell_case(S2CellId id, bool expected) {
    return {{"cell_id", std::to_string(id.id())}, {"expected", expected}};
}

static json point_case(const S2Point &p, bool expected) {
    return {{"point", point_json(p)}, {"expected", expected}};
}

static json bounds_json(S2ShapeIndexBufferedRegion *region) {
    json b;
    b["cap"] = cap_json(region->GetCapBound());
    b["rect"] = rect_json(region->GetRectBound());
    vector<S2CellId> covering;
    region->GetCellUnionBound(&covering);
    json cov = json::array();
    for (auto id : covering)
        cov.push_back(std::to_string(id.id()));
    b["cell_union"] = cov;
    return b;
}

static json finish(const string &name, double radius_radians,
                   S2ShapeIndexBufferedRegion *region, const json &shape_descs,
                   const json &contains_cells, const json &may_intersect_cells,
                   const json &contains_points) {
    return {{"name", name},
            {"radius_radians", radius_radians},
            {"shapes", shape_descs},
            {"bounds", bounds_json(region)},
            {"contains_cell", contains_cells},
            {"may_intersect_cell", may_intersect_cells},
            {"contains_point", contains_points}};
}

int main() {
    json out;
    json cases = json::array();

    // TEST(S2ShapeIndexBufferedRegion, PointZeroRadius): a single point with
    // radius 0. The covering should be a single leaf cell. contains_point on
    // the point itself returns true (less-or-equal semantics).
    {
        CaseBuilder b;
        S2Point p = pt(34, 25);
        b.add_points({p});
        S1ChordAngle radius = S1ChordAngle::Zero();
        S2ShapeIndexBufferedRegion region(&b.index, radius);
        json contains_pts = json::array();
        contains_pts.push_back(point_case(p, true));
        contains_pts.push_back(point_case(pt(34, 26), false));
        cases.push_back(finish("point_zero_radius", 0.0, &region, b.shape_descs,
                               json::array(), json::array(), contains_pts));
    }

    // TEST(S2ShapeIndexBufferedRegion, BufferedPointVsCap): one point with
    // radius 2 degrees. Cross-check contains_point at the centre and at a
    // point just inside the buffer.
    {
        CaseBuilder b;
        S2Point centre = pt(3, 5);
        b.add_points({centre});
        S1Angle radius_angle = S1Angle::Degrees(2);
        S1ChordAngle radius(radius_angle);
        S2ShapeIndexBufferedRegion region(&b.index, radius);
        json contains_pts = json::array();
        // The centre is always inside.
        contains_pts.push_back(point_case(centre, true));
        // A point ~1 degree away (inside the buffer).
        contains_pts.push_back(point_case(pt(3.5, 5.5), true));
        // A point ~5 degrees away (outside the buffer).
        contains_pts.push_back(point_case(pt(8, 10), false));
        cases.push_back(finish("buffered_point", radius_angle.radians(),
                               &region, b.shape_descs, json::array(),
                               json::array(), contains_pts));
    }

    // Exercise contains_cell, may_intersect_cell, and contains_point against
    // a polyline buffered by 2 degrees. We pick two cells: one near the
    // polyline (may-intersect true) and one far away (both predicates false).
    {
        CaseBuilder b;
        b.add_polyline({pt(10, 5), pt(20, 30)});
        S1Angle radius_angle = S1Angle::Degrees(2);
        S1ChordAngle radius(radius_angle);
        S2ShapeIndexBufferedRegion region(&b.index, radius);
        // Two cell candidates whose results we compute from C++.
        S2CellId near_id = S2CellId(pt(10, 5)).parent(20);
        S2CellId far_id = S2CellId(pt(-50, -100)).parent(15);
        S2Cell near_cell(near_id);
        S2Cell far_cell(far_id);
        json contains_cells = json::array();
        contains_cells.push_back(
            cell_case(near_id, region.Contains(near_cell)));
        contains_cells.push_back(cell_case(far_id, region.Contains(far_cell)));
        json may_intersect = json::array();
        may_intersect.push_back(
            cell_case(near_id, region.MayIntersect(near_cell)));
        may_intersect.push_back(
            cell_case(far_id, region.MayIntersect(far_cell)));
        json contains_pts = json::array();
        // A point on the polyline.
        contains_pts.push_back(point_case(pt(10, 5), true));
        // A point ~1 degree off the polyline (inside the buffer).
        contains_pts.push_back(point_case(pt(11, 6), true));
        // A point ~10 degrees away (outside the buffer).
        contains_pts.push_back(point_case(pt(50, 50), false));
        cases.push_back(finish("polyline_buffered", radius_angle.radians(),
                               &region, b.shape_descs, contains_cells,
                               may_intersect, contains_pts));
    }

    // contains_cell true branch: a polyline buffered by 60 degrees. Pick a
    // small cell near the polyline so it sits well inside the buffered
    // region (contains_cell returns true), and a leaf cell far away (false).
    {
        CaseBuilder b;
        b.add_points({pt(0, 0)});
        S1Angle radius_angle = S1Angle::Degrees(60);
        S1ChordAngle radius(radius_angle);
        S2ShapeIndexBufferedRegion region(&b.index, radius);
        S2CellId near_id = S2CellId(pt(1, 1)).parent(20);
        S2CellId far_id = S2CellId(pt(70, 100)).parent(20);
        S2Cell near_cell(near_id);
        S2Cell far_cell(far_id);
        json contains_cells = json::array();
        contains_cells.push_back(
            cell_case(near_id, region.Contains(near_cell)));
        contains_cells.push_back(cell_case(far_id, region.Contains(far_cell)));
        json may_intersect = json::array();
        may_intersect.push_back(
            cell_case(near_id, region.MayIntersect(near_cell)));
        may_intersect.push_back(
            cell_case(far_id, region.MayIntersect(far_cell)));
        cases.push_back(finish("contains_cell_branch", radius_angle.radians(),
                               &region, b.shape_descs, contains_cells,
                               may_intersect, json::array()));
    }

    out["cases"] = cases;
    std::cout << out.dump(2) << std::endl;
    return 0;
}
