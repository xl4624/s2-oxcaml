// Golden data generator for S2ContainsVertexQuery.
// Mirrors s2geometry/src/s2/s2contains_vertex_query_test.cc.

#include "s2/s2contains_vertex_query.h"

#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>
#include <vector>

#include "s2/s1angle.h"
#include "s2/s2edge_crossings.h"
#include "s2/s2latlng.h"
#include "s2/s2point.h"
#include "s2/s2pointutil.h"
#include "s2/util/math/matrix3x3.h"

using json = nlohmann::json;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

// Parse "lat:lng" in the same way as s2textformat::MakePointOrDie.
static S2Point make_point(double lat_deg, double lng_deg) {
    return S2LatLng::FromDegrees(lat_deg, lng_deg).ToPoint();
}

// A single "add edge" action: add the edge (target, v) with the given
// direction, and record the cumulative ContainsSign / DuplicateEdges results
// afterwards so the OCaml test can replay the exact sequence.
static json add_edge_action(S2ContainsVertexQuery &q, const S2Point &v,
                            int direction) {
    q.AddEdge(v, direction);
    json a = json::object();
    a["v"] = point_json(v);
    a["direction"] = direction;
    a["contains_sign"] = q.ContainsSign();
    a["duplicate_edges"] = q.DuplicateEdges();
    return a;
}

int main() {
    json out;

    // TEST(S2ContainsVertexQuery, Undetermined)
    {
        S2Point target = make_point(1, 2);
        S2ContainsVertexQuery q(target);
        json actions = json::array();
        actions.push_back(add_edge_action(q, make_point(3, 4), 1));
        actions.push_back(add_edge_action(q, make_point(3, 4), -1));
        json c = json::object();
        c["target"] = point_json(target);
        c["actions"] = actions;
        out["undetermined"] = c;
    }

    // TEST(S2ContainsVertexQuery, ContainedWithDuplicates)
    {
        S2Point target = make_point(0, 0);
        S2ContainsVertexQuery q(target);
        json actions = json::array();
        actions.push_back(add_edge_action(q, make_point(3, -3), -1));
        actions.push_back(add_edge_action(q, make_point(1, -5), 1));
        actions.push_back(add_edge_action(q, make_point(2, -4), 1));
        actions.push_back(add_edge_action(q, make_point(1, -5), -1));
        // Incoming/outgoing edges to 1:-5 cancel, so one more is not a
        // duplicate.
        actions.push_back(add_edge_action(q, make_point(1, -5), -1));
        // 3:-3 has only been seen once incoming, another time is a duplicate.
        actions.push_back(add_edge_action(q, make_point(3, -3), -1));
        json c = json::object();
        c["target"] = point_json(target);
        c["actions"] = actions;
        out["contained_with_duplicates"] = c;
    }

    // TEST(S2ContainsVertexQuery, NotContainedWithDuplicates)
    {
        S2Point target = make_point(1, 1);
        S2ContainsVertexQuery q(target);
        json actions = json::array();
        actions.push_back(add_edge_action(q, make_point(1, -5), 1));
        actions.push_back(add_edge_action(q, make_point(2, -4), -1));
        actions.push_back(add_edge_action(q, make_point(3, -3), 1));
        actions.push_back(add_edge_action(q, make_point(1, -5), -1));
        // Incoming/outgoing to 1:-5 cancel, so one more is not a duplicate.
        actions.push_back(add_edge_action(q, make_point(1, -5), -1));
        // 3:-3 has only been seen once outgoing, another time is a duplicate.
        actions.push_back(add_edge_action(q, make_point(3, -3), 1));
        json c = json::object();
        c["target"] = point_json(target);
        c["actions"] = actions;
        out["not_contained_with_duplicates"] = c;
    }

    // TEST(S2ContainsVertexQuery, CompatibleWithAngleContainsVertex)
    //
    // Emit each triple (a, b, c) from a 10-vertex regular loop centered at
    // 89:1 with a 5-degree radius, along with the ContainsSign result. The
    // OCaml test will assert (ContainsSign > 0) == AngleContainsVertex(a, b, c)
    // using the ported S2_edge_crossings.angle_contains_vertex.
    //
    // Mirrors S2Loop::MakeRegularLoop so we do not have to link S2Loop.
    {
        S2Point center = make_point(89, 1);
        const int num_vertices = 10;
        const S1Angle radius = S1Angle::Degrees(5);
        Matrix3x3_d frame;
        S2::GetFrame(center, &frame);
        const double r = std::sin(radius.radians());
        const double z = std::cos(radius.radians());
        const double radian_step = 2 * M_PI / num_vertices;
        std::vector<S2Point> loop_vertices;
        loop_vertices.reserve(num_vertices);
        for (int i = 0; i < num_vertices; ++i) {
            const double ang = i * radian_step;
            const S2Point p(r * std::cos(ang), r * std::sin(ang), z);
            loop_vertices.push_back(S2::FromFrame(frame, p).Normalize());
        }
        json triples = json::array();
        int n = num_vertices;
        for (int i = 0; i < n; ++i) {
            S2Point a = loop_vertices[i];
            S2Point b = loop_vertices[(i + 1) % n];
            S2Point c = loop_vertices[(i + 2) % n];
            S2ContainsVertexQuery q(b);
            q.AddEdge(a, -1);
            q.AddEdge(c, 1);
            json t = json::object();
            t["a"] = point_json(a);
            t["b"] = point_json(b);
            t["c"] = point_json(c);
            t["contains_sign"] = q.ContainsSign();
            t["duplicate_edges"] = q.DuplicateEdges();
            t["angle_contains_vertex"] = S2::AngleContainsVertex(a, b, c);
            triples.push_back(t);
        }
        out["regular_loop_triples"] = triples;
    }

    // TEST(S2ContainsVertexQuery, CompatibleWithAngleContainsVertexDegenerate)
    {
        S2Point a(1, 0, 0);
        S2Point b(0, 1, 0);
        S2ContainsVertexQuery q(b);
        q.AddEdge(a, -1);
        q.AddEdge(a, 1);
        json t = json::object();
        t["a"] = point_json(a);
        t["b"] = point_json(b);
        t["contains_sign"] = q.ContainsSign();
        t["duplicate_edges"] = q.DuplicateEdges();
        t["angle_contains_vertex"] = S2::AngleContainsVertex(a, b, a);
        out["degenerate_triple"] = t;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
