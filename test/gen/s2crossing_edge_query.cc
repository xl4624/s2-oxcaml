// Golden data generator for S2CrossingEdgeQuery.
// Mirrors selected tests from
// s2geometry/src/s2/s2crossing_edge_query_test.cc.

#include "s2/s2crossing_edge_query.h"

#include <algorithm>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <utility>
#include <vector>

#include "s2/mutable_s2shape_index.h"
#include "s2/s1angle.h"
#include "s2/s2cap.h"
#include "s2/s2edge_crossings.h"
#include "s2/s2latlng.h"
#include "s2/s2loop.h"
#include "s2/s2point.h"
#include "s2/s2polyline.h"
#include "s2/s2shape.h"
#include "s2/s2shapeutil_shape_edge.h"
#include "s2/s2shapeutil_shape_edge_id.h"
#include "s2/s2testing.h"

using json = nlohmann::json;
using s2shapeutil::CrossingType;
using s2shapeutil::ShapeEdge;
using s2shapeutil::ShapeEdgeId;
using std::make_unique;
using std::pair;
using std::string;
using std::vector;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static S2Point pt(double lat_deg, double lng_deg) {
    return S2LatLng::FromDegrees(lat_deg, lng_deg).ToPoint();
}

static json ids_json(const vector<ShapeEdgeId> &ids) {
    json arr = json::array();
    for (const auto &id : ids) {
        arr.push_back({{"shape_id", id.shape_id}, {"edge_id", id.edge_id}});
    }
    return arr;
}

static vector<ShapeEdgeId> to_ids(const vector<ShapeEdge> &edges) {
    vector<ShapeEdgeId> ids;
    ids.reserve(edges.size());
    for (const auto &e : edges)
        ids.push_back(e.id());
    return ids;
}

struct QueryEdge {
    S2Point a;
    S2Point b;
};

// Build an index from the shapes and emit, for each query edge, the
// candidate / crossing / interior-crossing results.
static json run_queries(MutableS2ShapeIndex *index,
                        const vector<QueryEdge> &queries) {
    S2CrossingEdgeQuery q(index);
    json arr = json::array();
    for (const auto &qe : queries) {
        json entry;
        entry["a"] = point_json(qe.a);
        entry["b"] = point_json(qe.b);
        entry["candidates_all"] = ids_json(q.GetCandidates(qe.a, qe.b));
        entry["crossings_all"] =
            ids_json(to_ids(q.GetCrossingEdges(qe.a, qe.b, CrossingType::ALL)));
        entry["crossings_interior"] = ids_json(
            to_ids(q.GetCrossingEdges(qe.a, qe.b, CrossingType::INTERIOR)));
        // Per-shape candidate and crossing lists.
        json per_shape = json::array();
        for (int sid = 0; sid < index->num_shape_ids(); ++sid) {
            const S2Shape *shape = index->shape(sid);
            json e;
            e["shape_id"] = sid;
            e["candidates"] =
                ids_json(q.GetCandidates(qe.a, qe.b, sid, *shape));
            e["crossings_all"] = ids_json(to_ids(q.GetCrossingEdges(
                qe.a, qe.b, sid, *shape, CrossingType::ALL)));
            e["crossings_interior"] = ids_json(to_ids(q.GetCrossingEdges(
                qe.a, qe.b, sid, *shape, CrossingType::INTERIOR)));
            per_shape.push_back(e);
        }
        entry["per_shape"] = per_shape;
        arr.push_back(entry);
    }
    return arr;
}

static json loop_json(const vector<S2Point> &vs) {
    json out = json::array();
    for (const auto &p : vs)
        out.push_back(point_json(p));
    return out;
}

static json polyline_json(const vector<S2Point> &vs) {
    json out = json::array();
    for (const auto &p : vs)
        out.push_back(point_json(p));
    return out;
}

// Shapes are described by their raw vertices, tagged with a kind:
//   "polyline" -> S2Polyline::Shape with vertices v[0]..v[n-1]
//   "loop"     -> S2Loop::Shape with the vertices as a closed loop
struct ShapeSpec {
    enum Kind { POLYLINE, LOOP };
    Kind kind;
    vector<S2Point> vertices;
};

static json shape_spec_json(const ShapeSpec &s) {
    json out;
    switch (s.kind) {
        case ShapeSpec::POLYLINE:
            out["kind"] = "polyline";
            out["vertices"] = polyline_json(s.vertices);
            break;
        case ShapeSpec::LOOP:
            out["kind"] = "loop";
            out["vertices"] = loop_json(s.vertices);
            break;
    }
    return out;
}

static int add_shape(MutableS2ShapeIndex *index, const ShapeSpec &s) {
    switch (s.kind) {
        case ShapeSpec::POLYLINE: {
            auto polyline = make_unique<S2Polyline>(s.vertices);
            return index->Add(
                make_unique<S2Polyline::OwningShape>(std::move(polyline)));
        }
        case ShapeSpec::LOOP: {
            auto loop = make_unique<S2Loop>(s.vertices);
            return index->Add(make_unique<S2Loop::Shape>(loop.release()));
        }
    }
    return -1;
}

static vector<S2Point> regular_loop_vertices(const S2Point &center,
                                             S1Angle radius, int n) {
    auto loop = S2Loop::MakeRegularLoop(center, radius, n);
    vector<S2Point> vs;
    for (int i = 0; i < loop->num_vertices(); ++i) {
        vs.push_back(loop->vertex(i));
    }
    return vs;
}

// Construct one case.
struct Case {
    string name;
    vector<ShapeSpec> shapes;
    vector<QueryEdge> queries;
    // If >0, override the index's max_edges_per_cell to force more
    // subdivision (mirrors the C++ test behavior).
    int max_edges_per_cell = 0;
};

static json run_case(const Case &c) {
    MutableS2ShapeIndex::Options options;
    if (c.max_edges_per_cell > 0) {
        options.set_max_edges_per_cell(c.max_edges_per_cell);
    }
    MutableS2ShapeIndex index(options);
    json shapes = json::array();
    for (const auto &spec : c.shapes) {
        add_shape(&index, spec);
        shapes.push_back(shape_spec_json(spec));
    }
    index.ForceBuild();

    json out;
    out["shapes"] = shapes;
    out["num_shape_ids"] = index.num_shape_ids();
    out["queries"] = run_queries(&index, c.queries);
    return out;
}

int main() {
    json out;

    // Empty index: every query returns empty results.
    {
        Case c;
        c.name = "empty";
        c.queries = {
            {pt(0, 0), pt(1, 1)},
            {pt(45, 45), pt(-45, -45)},
        };
        out["empty"] = run_case(c);
    }

    // A single polyline with a handful of segments. Also covers the
    // brute-force threshold since the polyline has <= 27 edges.
    {
        Case c;
        c.name = "zigzag_polyline";
        c.shapes = {{ShapeSpec::POLYLINE,
                     {pt(0, 0), pt(2, 1), pt(0, 2), pt(2, 3), pt(0, 4),
                      pt(2, 5), pt(0, 6)}}};
        c.queries = {
            {pt(1, 0), pt(1, 6)},      // crosses multiple edges
            {pt(1, 2.5), pt(1, 3.5)},  // crosses a single zigzag edge
            {pt(5, 5), pt(6, 6)},      // far away, no crossings
            {pt(0, 0), pt(0, 6)},      // shares endpoints with shape
            {pt(0, 3), pt(2, 3)},      // collinear with an edge
        };
        out["zigzag_polyline"] = run_case(c);
    }

    // Three zigzag polylines near the equator, as in C++
    // GetCrossings.PolylineCrossings.
    {
        Case c;
        c.name = "three_zigzag_polylines";
        c.shapes = {
            {ShapeSpec::POLYLINE,
             {pt(0, 0), pt(2, 1), pt(0, 2), pt(2, 3), pt(0, 4), pt(2, 5),
              pt(0, 6)}},
            {ShapeSpec::POLYLINE,
             {pt(1, 0), pt(3, 1), pt(1, 2), pt(3, 3), pt(1, 4), pt(3, 5),
              pt(1, 6)}},
            {ShapeSpec::POLYLINE,
             {pt(2, 0), pt(4, 1), pt(2, 2), pt(4, 3), pt(2, 4), pt(4, 5),
              pt(2, 6)}},
        };
        c.queries = {
            {pt(1, 0), pt(1, 4)},  // header-file example
            {pt(5, 5), pt(6, 6)},  // no crossings
            {pt(1.5, 0), pt(1.5, 6)},
        };
        out["three_zigzag_polylines"] = run_case(c);
    }

    // A regular 64-gon loop so recursive subdivision is exercised. With the
    // default max_edges_per_cell (10), this index has many cells and many
    // edges per shape.
    {
        Case c;
        c.name = "regular_64gon";
        c.shapes = {{ShapeSpec::LOOP,
                     regular_loop_vertices(pt(0, 0), S1Angle::Degrees(5), 64)}};
        c.queries = {
            {pt(0, 0), pt(0, 30)},     // chord through center exits the loop
            {pt(0, 0), pt(10, 0)},     // meridian chord through center
            {pt(0, 6), pt(0, 10)},     // exits the loop
            {pt(20, 20), pt(30, 30)},  // far outside
            {pt(0, 4), pt(0, 6)},      // crosses boundary near equator
        };
        out["regular_64gon"] = run_case(c);
    }

    // Same 64-gon, but with max_edges_per_cell set to 1 to force deep
    // subdivision. This stresses the recursive cell-descent path.
    {
        Case c;
        c.name = "regular_64gon_fine";
        c.shapes = {{ShapeSpec::LOOP,
                     regular_loop_vertices(pt(0, 0), S1Angle::Degrees(5), 64)}};
        c.max_edges_per_cell = 1;
        c.queries = {
            {pt(0, 0), pt(0, 30)},
            {pt(0, 4), pt(0, 6)},
            {pt(0, -10), pt(0, 10)},  // two crossings
        };
        out["regular_64gon_fine"] = run_case(c);
    }

    // Mixed shapes: a loop plus a polyline, with queries that cross both.
    {
        Case c;
        c.name = "loop_and_polyline";
        c.shapes = {
            {ShapeSpec::LOOP, {pt(-5, -5), pt(-5, 5), pt(5, 5), pt(5, -5)}},
            {ShapeSpec::POLYLINE, {pt(-10, 0), pt(10, 0)}},
        };
        c.queries = {
            {pt(0, -10), pt(0, 10)},   // crosses polyline and the loop sides
            {pt(-10, 3), pt(10, 3)},   // crosses the loop sides only
            {pt(20, 20), pt(30, 30)},  // far outside
            {pt(-5, -5), pt(5, 5)},    // diagonal across the loop
        };
        out["loop_and_polyline"] = run_case(c);
    }

    // Degenerate query: a = b (zero-length edge). Should not error.
    {
        Case c;
        c.name = "degenerate_query";
        c.shapes = {{ShapeSpec::POLYLINE, {pt(0, 0), pt(0, 2), pt(2, 2)}}};
        c.queries = {
            {pt(0, 1), pt(0, 1)},  // on an edge
            {pt(5, 5), pt(5, 5)},  // away from everything
        };
        out["degenerate_query"] = run_case(c);
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
