// Golden data generator for S2PointVectorShape.

#include "s2/s2point_vector_shape.h"

#include <iostream>
#include <nlohmann/json.hpp>
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

static json shape_summary(const string &name, const S2PointVectorShape &shape,
                          const vector<S2Point> &input_points) {
    json pts_arr = json::array();
    for (const auto &p : input_points)
        pts_arr.push_back(point_json(p));

    json points_arr = json::array();
    for (int i = 0; i < shape.num_points(); ++i)
        points_arr.push_back(point_json(shape.point(i)));

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
        {"input_points", pts_arr},
        {"num_points", shape.num_points()},
        {"num_edges", shape.num_edges()},
        {"num_chains", shape.num_chains()},
        {"dimension", shape.dimension()},
        {"is_empty", shape.is_empty()},
        {"is_full", shape.is_full()},
        {"type_tag", shape.type_tag()},
        {"reference_point",
         {{"point", point_json(rp.point)}, {"contained", rp.contained}}},
        {"points", points_arr},
        {"edges", edge_arr},
        {"chains", chain_arr},
        {"chain_positions", chain_positions},
        {"chain_edges", chain_edges},
    };
}

int main() {
    json out;
    json cases = json::array();

    // TEST(S2PointVectorShape, Empty)
    {
        vector<S2Point> points;
        S2PointVectorShape shape(std::move(points));
        cases.push_back(shape_summary("empty", shape, {}));
    }
    // TEST(S2PointVectorShape, ConstructionAndAccess)
    // Deterministic set of points (latlng grid) instead of 100 random points so
    // the fixture is reproducible without pulling in absl::BitGen.
    {
        vector<S2Point> points;
        for (int lat = -60; lat <= 60; lat += 30) {
            for (int lng = -150; lng <= 150; lng += 60) {
                points.push_back(S2LatLng::FromDegrees(lat, lng).ToPoint());
            }
        }
        S2PointVectorShape shape(points);
        cases.push_back(shape_summary("grid", shape, points));
    }
    // TEST(S2PointVectorShape, ChainIteratorWorks) - exercises the
    // three-vertex layout checked by the iterator test. The upstream test uses
    // "0:0, 0:1, 1:1" via s2textformat; we build the same three S2Points
    // directly to avoid linking absl_statusor in generator binaries.
    {
        vector<S2Point> points = {
            S2LatLng::FromDegrees(0, 0).ToPoint(),
            S2LatLng::FromDegrees(0, 1).ToPoint(),
            S2LatLng::FromDegrees(1, 1).ToPoint(),
        };
        S2PointVectorShape shape(points);
        cases.push_back(shape_summary("three_points", shape, points));
    }
    out["shapes"] = cases;

    std::cout << out.dump(2) << std::endl;
    return 0;
}
