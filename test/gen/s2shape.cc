// Golden data generator for S2Shape.
// The upstream C++ library has no standalone s2shape_test.cc because S2Shape is
// an abstract base class. These fixtures exercise the concrete value types
// defined on S2Shape (Edge, Chain, ChainPosition, ReferencePoint) and its
// constants (kNoTypeTag, kNextAvailableTypeTag, kMinUserTypeTag).

#include "s2/s2shape.h"

#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/s2point.h"
#include "s2/s2pointutil.h"

using json = nlohmann::json;

json point_json(const S2Point &p) {
    return json::array({p.x(), p.y(), p.z()});
}

json edge_json(const S2Shape::Edge &e) {
    return json::object({{"v0", point_json(e.v0)}, {"v1", point_json(e.v1)}});
}

int main() {
    json out;

    // Canonical probe points used across cases.
    S2Point px(1, 0, 0);
    S2Point py(0, 1, 0);
    S2Point pz(0, 0, 1);
    S2Point pn(-1, 0, 0);

    // Edge helpers: Reversed, IsDegenerate, Incoming, Outgoing, IncidentOn.
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &a,
                       const S2Point &b, const S2Point &probe) {
            S2Shape::Edge e(a, b);
            cases.push_back({
                {"name", name},
                {"edge", edge_json(e)},
                {"probe", point_json(probe)},
                {"reversed", edge_json(e.Reversed())},
                {"is_degenerate", e.IsDegenerate()},
                {"incoming", e.Incoming(probe)},
                {"outgoing", e.Outgoing(probe)},
                {"incident_on", e.IncidentOn(probe)},
            });
        };
        add("xy_probe_x", px, py, px);
        add("xy_probe_y", px, py, py);
        add("xy_probe_z", px, py, pz);
        add("yz_probe_y", py, pz, py);
        add("yz_probe_z", py, pz, pz);
        add("degenerate_xx", px, px, px);
        add("degenerate_xx_probe_y", px, px, py);
        add("degenerate_origin", S2::Origin(), S2::Origin(), S2::Origin());
        out["edge_helpers"] = cases;
    }

    // Edge comparison: operator< and operator==.
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Shape::Edge &a,
                       const S2Shape::Edge &b) {
            cases.push_back({
                {"name", name},
                {"a", edge_json(a)},
                {"b", edge_json(b)},
                {"equal", a == b},
                {"less", a < b},
            });
        };
        add("same", S2Shape::Edge(px, py), S2Shape::Edge(px, py));
        add("v0_differs_lt", S2Shape::Edge(px, py), S2Shape::Edge(py, py));
        add("v0_differs_gt", S2Shape::Edge(py, px), S2Shape::Edge(px, px));
        add("v1_differs_lt", S2Shape::Edge(px, py), S2Shape::Edge(px, pz));
        add("v1_differs_gt", S2Shape::Edge(px, pz), S2Shape::Edge(px, py));
        add("v0_and_v1_differ", S2Shape::Edge(px, py), S2Shape::Edge(pz, pn));
        out["edge_cmp"] = cases;
    }

    // Chain equality.
    {
        json cases = json::array();
        auto add = [&](const std::string &name, int s1, int l1, int s2,
                       int l2) {
            S2Shape::Chain a(s1, l1);
            S2Shape::Chain b(s2, l2);
            cases.push_back({
                {"name", name},
                {"a", json::array({s1, l1})},
                {"b", json::array({s2, l2})},
                {"equal", a == b},
            });
        };
        add("same", 0, 5, 0, 5);
        add("start_differs", 0, 5, 1, 5);
        add("length_differs", 3, 4, 3, 7);
        add("both_differ", 0, 0, 1, 1);
        add("zero_length_chains", 7, 0, 7, 0);
        out["chain_equal"] = cases;
    }

    // ChainPosition equality.
    {
        json cases = json::array();
        auto add = [&](const std::string &name, int c1, int o1, int c2,
                       int o2) {
            S2Shape::ChainPosition a(c1, o1);
            S2Shape::ChainPosition b(c2, o2);
            cases.push_back({
                {"name", name},
                {"a", json::array({c1, o1})},
                {"b", json::array({c2, o2})},
                {"equal", a == b},
            });
        };
        add("same", 2, 3, 2, 3);
        add("chain_differs", 1, 0, 2, 0);
        add("offset_differs", 4, 1, 4, 9);
        add("both_differ", 0, 0, 5, 5);
        out["chain_position_equal"] = cases;
    }

    // ReferencePoint::Contained(bool) and direct ReferencePoint(point, b).
    {
        json cases = json::array();
        auto add_contained = [&](const std::string &name, bool contained) {
            auto rp = S2Shape::ReferencePoint::Contained(contained);
            cases.push_back({
                {"name", name},
                {"kind", "contained"},
                {"contained_input", contained},
                {"point", point_json(rp.point)},
                {"contained", rp.contained},
            });
        };
        auto add_with_point = [&](const std::string &name, const S2Point &p,
                                  bool contained) {
            S2Shape::ReferencePoint rp(p, contained);
            cases.push_back({
                {"name", name},
                {"kind", "with_point"},
                {"input_point", point_json(p)},
                {"input_contained", contained},
                {"point", point_json(rp.point)},
                {"contained", rp.contained},
                {"equal_to_self", rp == rp},
                {"equal_to_flipped",
                 rp == S2Shape::ReferencePoint(p, !contained)},
            });
        };
        add_contained("contained_true", true);
        add_contained("contained_false", false);
        add_with_point("px_true", px, true);
        add_with_point("py_false", py, false);
        out["reference_point"] = cases;
    }

    // Type tag constants.
    {
        json cases = json::object();
        cases["k_no_type_tag"] = S2Shape::kNoTypeTag;
        cases["k_next_available_type_tag"] = S2Shape::kNextAvailableTypeTag;
        cases["k_min_user_type_tag"] = S2Shape::kMinUserTypeTag;
        out["type_tags"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
