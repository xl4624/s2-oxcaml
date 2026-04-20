// Golden data generator for MutableS2ShapeIndex and S2ContainsPointQuery.
// Mirrors selected tests from s2geometry/src/s2/mutable_s2shape_index_test.cc
// and s2geometry/src/s2/s2contains_point_query_test.cc.

#include <cmath>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/mutable_s2shape_index.h"
#include "s2/s1angle.h"
#include "s2/s2cell_iterator.h"
#include "s2/s2contains_point_query.h"
#include "s2/s2latlng.h"
#include "s2/s2loop.h"
#include "s2/s2point.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::make_unique;
using std::unique_ptr;
using std::vector;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static S2Point pt(double lat_deg, double lng_deg) {
    return S2LatLng::FromDegrees(lat_deg, lng_deg).ToPoint();
}

// Return the cells of an index as a JSON array ordered by the iterator (which
// is Hilbert-curve / unsigned S2CellId order).
static json cells_json(const MutableS2ShapeIndex &index) {
    json arr = json::array();
    for (MutableS2ShapeIndex::Iterator it(&index,
                                          S2ShapeIndex::InitialPosition::BEGIN);
         !it.done(); it.Next()) {
        json cell;
        cell["cell_id"] = std::to_string(it.id().id());
        json shapes = json::array();
        const S2ShapeIndexCell &c = it.cell();
        for (int i = 0; i < c.num_clipped(); ++i) {
            const S2ClippedShape &cs = c.clipped(i);
            json s;
            s["shape_id"] = cs.shape_id();
            s["contains_center"] = cs.contains_center();
            json edges = json::array();
            for (int j = 0; j < cs.num_edges(); ++j) {
                edges.push_back(cs.edge(j));
            }
            s["edges"] = edges;
            shapes.push_back(s);
        }
        cell["clipped"] = shapes;
        arr.push_back(cell);
    }
    return arr;
}

// Describe a loop by its vertices so OCaml can reconstruct it identically.
static json loop_json(const vector<S2Point> &vs) {
    json out = json::array();
    for (const auto &p : vs)
        out.push_back(point_json(p));
    return out;
}

// Build an index from a list of S2Loop vertex sets, force a build, and dump
// the resulting cells along with the input shapes.
struct LoopIndexCase {
    std::string name;
    vector<vector<S2Point>> loops;
    vector<S2Point> contains_queries;
};

static json build_loop_case(const LoopIndexCase &c) {
    MutableS2ShapeIndex index;
    json shapes = json::array();
    for (const auto &verts : c.loops) {
        auto loop = make_unique<S2Loop>(verts);
        index.Add(make_unique<S2Loop::Shape>(loop.release()));
        shapes.push_back(loop_json(verts));
    }
    index.ForceBuild();

    json out;
    out["shapes"] = shapes;
    out["num_shape_ids"] = index.num_shape_ids();
    out["cells"] = cells_json(index);

    S2ContainsPointQueryOptions opts(S2VertexModel::SEMI_OPEN);
    S2ContainsPointQuery<MutableS2ShapeIndex> q(&index, opts);
    json queries = json::array();
    for (const auto &p : c.contains_queries) {
        json entry;
        entry["point"] = point_json(p);
        entry["contains"] = q.Contains(p);
        json per_shape = json::array();
        for (int sid = 0; sid < index.num_shape_ids(); ++sid) {
            per_shape.push_back(q.ShapeContains(sid, p));
        }
        entry["shape_contains"] = per_shape;
        queries.push_back(entry);
    }
    out["queries"] = queries;
    return out;
}

// Build a regular loop (mirrors S2Loop::MakeRegularLoop with small radius).
static vector<S2Point> regular_loop(const S2Point &center, S1Angle radius,
                                    int num_vertices) {
    auto loop = S2Loop::MakeRegularLoop(center, radius, num_vertices);
    vector<S2Point> out;
    for (int i = 0; i < loop->num_vertices(); ++i) {
        out.push_back(loop->vertex(i));
    }
    return out;
}

int main() {
    json out;

    // Empty index: no shapes, no cells.
    {
        MutableS2ShapeIndex index;
        index.ForceBuild();
        json c;
        c["shapes"] = json::array();
        c["num_shape_ids"] = 0;
        c["cells"] = cells_json(index);
        c["queries"] = json::array();
        out["empty"] = c;
    }

    // Full loop: single vertex, origin_inside = true. Should yield six
    // face-level cells each claiming contains_center for shape 0.
    {
        vector<vector<S2Point>> loops = {{S2Point(0, 0, -1)}};
        LoopIndexCase c{"full",
                        loops,
                        {S2Point(1, 0, 0), S2Point(0, 0, 1), S2Point(-1, 0, 0),
                         S2Point(0, 0, -1)}};
        out["full_loop"] = build_loop_case(c);
    }

    // Single small triangle near (0, 0).
    {
        vector<vector<S2Point>> loops = {
            {pt(0, 0), pt(0, 2), pt(2, 0)},
        };
        vector<S2Point> qs = {
            pt(0.5, 0.5),  // inside
            pt(0, 1),      // on edge (semi-open: excluded)
            pt(-1, -1),    // outside
            pt(2, 2),      // outside
        };
        out["triangle"] = build_loop_case({"triangle", loops, qs});
    }

    // Two nested loops: an outer square and a smaller inner square (hole-like,
    // but here both are CCW so they both contain the same points; the inner
    // just defines a denser region).
    {
        vector<vector<S2Point>> loops = {
            {pt(-5, -5), pt(-5, 5), pt(5, 5), pt(5, -5)},
            {pt(-1, -1), pt(-1, 1), pt(1, 1), pt(1, -1)},
        };
        vector<S2Point> qs = {
            pt(0, 0),    // inside both
            pt(3, 3),    // inside outer only
            pt(10, 10),  // inside neither
            pt(-5, 0),   // on outer edge
        };
        out["nested_squares"] = build_loop_case({"nested_squares", loops, qs});
    }

    // A regular 8-gon around (30, 40) with radius 5 degrees; many query
    // points chosen to span the circle.
    {
        vector<vector<S2Point>> loops = {
            regular_loop(pt(30, 40), S1Angle::Degrees(5), 8),
        };
        vector<S2Point> qs = {
            pt(30, 40),  // center (inside)
            pt(30, 47),  // outside
            pt(35, 40),  // near boundary
            pt(28, 39),  // inside
        };
        out["regular_octagon"] =
            build_loop_case({"regular_octagon", loops, qs});
    }

    // Edge-dense loop (many vertices along a circle) to exercise recursive
    // subdivision and edge clipping.
    {
        vector<vector<S2Point>> loops = {
            regular_loop(pt(-10, 20), S1Angle::Degrees(10), 64),
        };
        vector<S2Point> qs = {
            pt(-10, 20),  // center
            pt(-10, 32),  // outside
            pt(-5, 25),   // inside-ish
            pt(-20, 10),  // outside
        };
        out["regular_64gon"] = build_loop_case({"regular_64gon", loops, qs});
    }

    // Locate tests against a simple index: query each cell_id's relation.
    {
        vector<vector<S2Point>> loops = {
            {pt(0, 0), pt(0, 2), pt(2, 0)},
        };
        MutableS2ShapeIndex index;
        for (const auto &verts : loops) {
            auto loop = make_unique<S2Loop>(verts);
            index.Add(make_unique<S2Loop::Shape>(loop.release()));
        }
        index.ForceBuild();

        json c;
        c["shapes"] = json::array();
        for (const auto &verts : loops)
            c["shapes"].push_back(loop_json(verts));
        c["cells"] = cells_json(index);

        // Sample locate queries: the face cells and a few specific ids.
        vector<S2CellId> targets;
        for (int f = 0; f < 6; ++f)
            targets.push_back(S2CellId::FromFace(f));
        // Include a leaf cell for each triangle vertex.
        targets.push_back(S2CellId(pt(0, 0)));
        targets.push_back(S2CellId(pt(0, 2)));
        targets.push_back(S2CellId(pt(2, 0)));
        targets.push_back(S2CellId(pt(1, 1)));

        json locate = json::array();
        for (const auto &t : targets) {
            MutableS2ShapeIndex::Iterator it(&index);
            S2CellRelation rel = it.Locate(t);
            json entry;
            entry["target"] = std::to_string(t.id());
            switch (rel) {
                case S2CellRelation::INDEXED:
                    entry["relation"] = "indexed";
                    break;
                case S2CellRelation::SUBDIVIDED:
                    entry["relation"] = "subdivided";
                    break;
                case S2CellRelation::DISJOINT:
                    entry["relation"] = "disjoint";
                    break;
            }
            locate.push_back(entry);
        }
        c["locate_cell_id"] = locate;
        out["locate_triangle"] = c;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
