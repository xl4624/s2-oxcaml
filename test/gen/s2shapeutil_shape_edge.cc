// Golden data generator for s2shapeutil::ShapeEdge.
//
// There is no upstream s2shapeutil_shape_edge_test.cc; this generator just
// emits a few canonical (shape_id, edge_id, v0, v1) tuples so the OCaml port
// can verify its constructor and accessors against the C++ struct.

#include "s2/s2shapeutil_shape_edge.h"

#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/s2point.h"
#include "s2/s2shape.h"
#include "s2/s2shapeutil_shape_edge_id.h"

using json = nlohmann::json;
using s2shapeutil::ShapeEdge;
using s2shapeutil::ShapeEdgeId;

static json point_json(const S2Point &p) {
    return json::array({p.x(), p.y(), p.z()});
}

static json case_json(int shape_id, int edge_id, const S2Point &v0,
                      const S2Point &v1) {
    ShapeEdge se(shape_id, edge_id, S2Shape::Edge(v0, v1));
    return json{
        {"shape_id", shape_id},
        {"edge_id", edge_id},
        {"v0_in", point_json(v0)},
        {"v1_in", point_json(v1)},
        {"id_shape_id", se.id().shape_id},
        {"id_edge_id", se.id().edge_id},
        {"v0_out", point_json(se.v0())},
        {"v1_out", point_json(se.v1())},
    };
}

int main() {
    json out;

    json cases = json::array();
    // Generic non-degenerate edge.
    cases.push_back(case_json(0, 0, S2Point(1, 0, 0), S2Point(0, 1, 0)));
    // Distinct shape_id / edge_id.
    cases.push_back(case_json(7, 42, S2Point(0, 0, 1), S2Point(0, 1, 0)));
    // Degenerate edge (v0 == v1) - should round-trip unchanged.
    cases.push_back(case_json(2, 5, S2Point(1, 0, 0), S2Point(1, 0, 0)));
    // Negative-coordinate endpoints.
    cases.push_back(case_json(3, 9, S2Point(-1, 0, 0), S2Point(0, -1, 0)));
    out["cases"] = cases;

    std::cout << out.dump(2) << std::endl;
    return 0;
}
