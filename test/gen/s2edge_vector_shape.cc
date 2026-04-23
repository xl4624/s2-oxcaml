// Golden data generator for S2EdgeVectorShape.

#include "s2/s2edge_vector_shape.h"

#include <iostream>
#include <nlohmann/json.hpp>
#include <utility>
#include <vector>

#include "s2/s2latlng.h"
#include "s2/s2point.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::string;
using std::vector;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static json edge_json(const S2Shape::Edge &e) {
    return json::object({{"v0", point_json(e.v0)}, {"v1", point_json(e.v1)}});
}

static json shape_summary(
    const string &name, const S2EdgeVectorShape &shape,
    const vector<std::pair<S2Point, S2Point>> &input_edges) {
    json input_arr = json::array();
    for (const auto &p : input_edges) {
        input_arr.push_back(edge_json(S2Shape::Edge(p.first, p.second)));
    }
    json edge_arr = json::array();
    for (int i = 0; i < shape.num_edges(); ++i)
        edge_arr.push_back(edge_json(shape.edge(i)));
    json chain_arr = json::array();
    for (int i = 0; i < shape.num_chains(); ++i) {
        auto c = shape.chain(i);
        chain_arr.push_back(json::array({c.start, c.length}));
    }
    json chain_positions = json::array();
    for (int e = 0; e < shape.num_edges(); ++e) {
        auto cp = shape.chain_position(e);
        chain_positions.push_back(json::array({cp.chain_id, cp.offset}));
    }
    json chain_edges = json::array();
    for (int i = 0; i < shape.num_chains(); ++i) {
        auto c = shape.chain(i);
        for (int j = 0; j < c.length; ++j)
            chain_edges.push_back(edge_json(shape.chain_edge(i, j)));
    }
    auto rp = shape.GetReferencePoint();
    return {
        {"name", name},
        {"input_edges", input_arr},
        {"num_edges", shape.num_edges()},
        {"num_chains", shape.num_chains()},
        {"dimension", shape.dimension()},
        {"is_empty", shape.is_empty()},
        {"is_full", shape.is_full()},
        {"reference_point",
         {{"point", point_json(rp.point)}, {"contained", rp.contained}}},
        {"edges", edge_arr},
        {"chains", chain_arr},
        {"chain_positions", chain_positions},
        {"chain_edges", chain_edges},
    };
}

int main() {
    json out;
    json cases = json::array();

    // TEST(S2EdgeVectorShape, Empty)
    {
        S2EdgeVectorShape shape;
        cases.push_back(shape_summary("empty", shape, {}));
    }
    // TEST(S2EdgeVectorShape, SingletonConstructor)
    {
        S2Point a(1, 0, 0), b(0, 1, 0);
        S2EdgeVectorShape shape(a, b);
        cases.push_back(shape_summary("singleton", shape, {{a, b}}));
    }
    // TEST(S2EdgeVectorShape, EdgeAccess)
    // Deterministic triangle instead of 100 random edges so the fixture is
    // reproducible without pulling in absl::BitGen.
    {
        vector<std::pair<S2Point, S2Point>> edges = {
            {S2LatLng::FromDegrees(0, 0).ToPoint(),
             S2LatLng::FromDegrees(0, 10).ToPoint()},
            {S2LatLng::FromDegrees(0, 10).ToPoint(),
             S2LatLng::FromDegrees(10, 10).ToPoint()},
            {S2LatLng::FromDegrees(10, 10).ToPoint(),
             S2LatLng::FromDegrees(0, 0).ToPoint()},
        };
        S2EdgeVectorShape shape;
        for (const auto &p : edges)
            shape.Add(p.first, p.second);
        cases.push_back(shape_summary("three_edges", shape, edges));
    }
    // Extra coverage: set_dimension(0) models a point shape with
    // degenerate edges and exercises the vector-constructor branch.
    {
        vector<std::pair<S2Point, S2Point>> edges = {
            {S2Point(1, 0, 0), S2Point(1, 0, 0)},
            {S2Point(0, 1, 0), S2Point(0, 1, 0)},
        };
        S2EdgeVectorShape shape(edges);
        shape.set_dimension(0);
        cases.push_back(shape_summary("points_dim0", shape, edges));
    }
    // Extra coverage: set_dimension(2) confirms dimension is reported as
    // configured even for a single-edge shape.
    {
        S2Point a(1, 0, 0), b(0, 1, 0);
        S2EdgeVectorShape shape(a, b);
        shape.set_dimension(2);
        cases.push_back(shape_summary("dim2", shape, {{a, b}}));
    }
    out["shapes"] = cases;

    std::cout << out.dump(2) << std::endl;
    return 0;
}
