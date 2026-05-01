// Golden data generator for S2ShapeIndexRegion (cap/rect/cell-union bounds,
// Contains and MayIntersect against S2Cells, Contains against S2Points).

#include "s2/s2shape_index_region.h"

#include <algorithm>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/mutable_s2shape_index.h"
#include "s2/r2rect.h"
#include "s2/s1angle.h"
#include "s2/s2cap.h"
#include "s2/s2cell.h"
#include "s2/s2cell_id.h"
#include "s2/s2coords.h"
#include "s2/s2edge_clipping.h"
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

// Pad by at least twice the maximum error for reliable results, matching the
// upstream s2shape_index_region_test helpers.
static const double kPadding =
    2 * (S2::kFaceClipErrorUVCoord + S2::kIntersectsRectErrorUVDist);

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

// Reproduce the small loop shape used by the C++ tests: a four-vertex loop
// whose vertices come from the corners of a UV rectangle, optionally
// expanded by padding_uv to make the loop slightly larger or smaller than
// the underlying cell.
static std::unique_ptr<S2LaxLoopShape> NewPaddedCell(S2CellId id,
                                                     double padding_uv) {
    int ij[2], orientation;
    int face = id.ToFaceIJOrientation(&ij[0], &ij[1], &orientation);
    R2Rect uv = S2CellId::IJLevelToBoundUV(ij, id.level()).Expanded(padding_uv);
    vector<S2Point> vertices(4);
    for (int i = 0; i < 4; ++i) {
        vertices[i] = S2::FaceUVtoXYZ(face, uv.GetVertex(i)).Normalize();
    }
    return make_unique<S2LaxLoopShape>(vertices);
}

static json loop_shape_json(S2CellId id, double padding_uv) {
    int ij[2], orientation;
    int face = id.ToFaceIJOrientation(&ij[0], &ij[1], &orientation);
    R2Rect uv = S2CellId::IJLevelToBoundUV(ij, id.level()).Expanded(padding_uv);
    json vs = json::array();
    for (int i = 0; i < 4; ++i) {
        S2Point p = S2::FaceUVtoXYZ(face, uv.GetVertex(i)).Normalize();
        vs.push_back(point_json(p));
    }
    return {{"kind", "lax_loop"}, {"vertices", vs}};
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

// Builder that mirrors the JSON shape encoding (kind + payload) and the live
// MutableS2ShapeIndex used to compute the expected outputs.
struct CaseBuilder {
    MutableS2ShapeIndex index;
    json shape_descs = json::array();

    void add_padded_cell(S2CellId id, double padding_uv) {
        shape_descs.push_back(loop_shape_json(id, padding_uv));
        index.Add(NewPaddedCell(id, padding_uv));
    }

    void add_points(const vector<S2Point> &points) {
        shape_descs.push_back(points_shape_json(points));
        index.Add(make_unique<S2PointVectorShape>(points));
    }

    void add_polyline(const vector<S2Point> &vertices) {
        shape_descs.push_back(polyline_shape_json(vertices));
        index.Add(make_unique<S2LaxPolylineShape>(vertices));
    }

    json finish(const string &name, json bounds, json contains_cell_cases,
                json may_intersect_cases, json contains_point_cases) {
        index.ForceBuild();
        return {{"name", name},
                {"shapes", shape_descs},
                {"bounds", bounds},
                {"contains_cell", contains_cell_cases},
                {"may_intersect_cell", may_intersect_cases},
                {"contains_point", contains_point_cases}};
    }
};

static json bounds_json(MutableS2ShapeIndex *index) {
    S2ShapeIndexRegion<MutableS2ShapeIndex> region(index);
    json b;
    b["cap"] = cap_json(region.GetCapBound());
    b["rect"] = rect_json(region.GetRectBound());
    vector<S2CellId> covering;
    region.GetCellUnionBound(&covering);
    json cov = json::array();
    for (auto id : covering)
        cov.push_back(std::to_string(id.id()));
    b["cell_union"] = cov;
    return b;
}

// Encode one (cell_id, expected_bool) test triple for either the
// contains_cell or may_intersect_cell harness.
static json cell_case(S2CellId id, bool expected) {
    return {{"cell_id", std::to_string(id.id())}, {"expected", expected}};
}

static json point_case(const S2Point &p, bool expected) {
    return {{"point", point_json(p)}, {"expected", expected}};
}

int main() {
    json out;
    json cases = json::array();

    // TEST(S2ShapeIndexRegion, GetCapBound) and GetRectBound: a polygon
    // slightly smaller than a known cell. We assert both bounds via the
    // generic [bounds] block below.
    {
        CaseBuilder b;
        auto id = S2CellId::FromDebugString("3/0123012301230123012301230123");
        b.add_padded_cell(id, -kPadding);
        cases.push_back(b.finish("smaller_polygon_at_cell",
                                 bounds_json(&b.index), json::array(),
                                 json::array(), json::array()));
    }

    // TEST(S2ShapeIndexRegion, GetCellUnionBoundMultipleFaces): two padded
    // cells on different faces. The covering should equal the two ids
    // sorted.
    {
        CaseBuilder b;
        auto id1 = S2CellId::FromDebugString("3/00123");
        auto id2 = S2CellId::FromDebugString("2/11200013");
        b.add_padded_cell(id1, -kPadding);
        b.add_padded_cell(id2, -kPadding);
        cases.push_back(b.finish("cell_union_multiple_faces",
                                 bounds_json(&b.index), json::array(),
                                 json::array(), json::array()));
    }

    // TEST(S2ShapeIndexRegion, GetCellUnionBoundOneFace): three pairs of
    // cells on face 5. Each pair produces one parent cell in the bound.
    {
        CaseBuilder b;
        vector<S2CellId> input = {
            S2CellId::FromDebugString("5/010"),
            S2CellId::FromDebugString("5/0211030"),
            S2CellId::FromDebugString("5/110230123"),
            S2CellId::FromDebugString("5/11023021133"),
            S2CellId::FromDebugString("5/311020003003030303"),
            S2CellId::FromDebugString("5/311020023"),
        };
        for (const auto &id : input) {
            // Add each cell three times so the index decides to subdivide.
            for (int copy = 0; copy < 3; ++copy) {
                b.add_padded_cell(id, -kPadding);
            }
        }
        cases.push_back(b.finish("cell_union_one_face", bounds_json(&b.index),
                                 json::array(), json::array(), json::array()));
    }

    // TEST(S2ShapeIndexRegion, ContainsCellMultipleShapes): a smaller
    // polygon does not contain the cell, but adding a slightly larger
    // polygon makes Contains() true. Verify the cell itself plus its four
    // children are all contained.
    {
        CaseBuilder b;
        auto id = S2CellId::FromDebugString("3/0123012301230123012301230123");
        b.add_padded_cell(id, -kPadding);
        b.add_padded_cell(id, kPadding);
        json contains_cases = json::array();
        contains_cases.push_back(cell_case(id, true));
        for (auto child = id.child_begin(); child != id.child_end();
             child = child.next()) {
            contains_cases.push_back(cell_case(child, true));
        }
        cases.push_back(b.finish("contains_cell_multiple_shapes",
                                 bounds_json(&b.index), contains_cases,
                                 json::array(), json::array()));
    }

    // TEST(S2ShapeIndexRegion, ContainsCellMultipleShapes), false branch:
    // a polygon slightly smaller than the cell does not contain the cell.
    {
        CaseBuilder b;
        auto id = S2CellId::FromDebugString("3/0123012301230123012301230123");
        b.add_padded_cell(id, -kPadding);
        json contains_cases = json::array();
        contains_cases.push_back(cell_case(id, false));
        cases.push_back(b.finish("contains_cell_smaller_polygon",
                                 bounds_json(&b.index), contains_cases,
                                 json::array(), json::array()));
    }

    // TEST(S2ShapeIndexRegion, IntersectsShrunkenCell): a polygon slightly
    // smaller than the cell intersects the cell itself but not its
    // neighbours.
    {
        CaseBuilder b;
        auto target =
            S2CellId::FromDebugString("3/0123012301230123012301230123");
        b.add_padded_cell(target, -kPadding);
        json may_intersect_cases = json::array();
        may_intersect_cases.push_back(cell_case(target, true));
        vector<S2CellId> nbrs;
        target.AppendAllNeighbors(target.level(), &nbrs);
        for (auto id : nbrs) {
            may_intersect_cases.push_back(cell_case(id, false));
        }
        cases.push_back(b.finish("intersects_shrunken_cell",
                                 bounds_json(&b.index), json::array(),
                                 may_intersect_cases, json::array()));
    }

    // TEST(S2ShapeIndexRegion, IntersectsExactCell): polygon following the
    // cell boundary intersects the cell and all its neighbours.
    {
        CaseBuilder b;
        auto target =
            S2CellId::FromDebugString("3/0123012301230123012301230123");
        b.add_padded_cell(target, 0.0);
        json may_intersect_cases = json::array();
        may_intersect_cases.push_back(cell_case(target, true));
        vector<S2CellId> nbrs;
        target.AppendAllNeighbors(target.level(), &nbrs);
        for (auto id : nbrs) {
            may_intersect_cases.push_back(cell_case(id, true));
        }
        cases.push_back(b.finish("intersects_exact_cell", bounds_json(&b.index),
                                 json::array(), may_intersect_cases,
                                 json::array()));
    }

    // Contains(point) sanity cases: a small loop around (0, 0). Inside,
    // outside, and a vertex point all exercise the semi-open path.
    {
        CaseBuilder b;
        S2CellId target = S2CellId::FromDebugString("4/00");
        b.add_padded_cell(target, 0.0);
        // Cell center is inside the polygon; a point far away is outside.
        json contains_pt_cases = json::array();
        contains_pt_cases.push_back(
            point_case(S2Cell(target).GetCenter(), true));
        contains_pt_cases.push_back(point_case(pt(45, 45), false));
        // A dimension-1 polyline at lat=10 should never contain a point under
        // semi-open vertex model, but we still exercise the path.
        b.add_polyline({pt(10, 10), pt(10, 20)});
        cases.push_back(b.finish("contains_point_loop_and_polyline",
                                 bounds_json(&b.index), json::array(),
                                 json::array(), contains_pt_cases));
    }

    out["cases"] = cases;
    std::cout << out.dump(2) << std::endl;
    return 0;
}
