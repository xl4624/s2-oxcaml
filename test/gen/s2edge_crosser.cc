// Golden data generator for S2EdgeCrosser.
// Mirrors s2geometry/src/s2/s2edge_crosser_test.cc.

#include "s2/s2edge_crosser.h"

#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>
#include <vector>

#include "s2/s2edge_crossings.h"
#include "s2/s2edge_distances.h"
#include "s2/s2point.h"

using json = nlohmann::json;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

// One emitted test case for S2EdgeCrosser: fixed edge AB, probe edge CD, and
// the expected values of CrossingSign / EdgeOrVertexCrossing /
// SignedEdgeOrVertexCrossing as returned by C++ for the argument order given.
static json crossing_case(const char *name, S2Point a, S2Point b, S2Point c,
                          S2Point d) {
    a = a.Normalize();
    b = b.Normalize();
    c = c.Normalize();
    d = d.Normalize();
    int cs = S2::CrossingSign(a, b, c, d);
    bool eov = S2::EdgeOrVertexCrossing(a, b, c, d);
    return {{"name", name},
            {"a", point_json(a)},
            {"b", point_json(b)},
            {"c", point_json(c)},
            {"d", point_json(d)},
            {"crossing_sign", cs},
            {"edge_or_vertex_crossing", eov}};
}

// Emit all permutations produced by TestCrossings() in s2edge_crosser_test.cc.
// The upstream helper invokes TestCrossing() for several reorderings that all
// exercise different code paths in EdgeCrosser (including the fast path on
// repeated c vertices and RestartAt); we emit them so the OCaml test can drive
// the stateful crosser over the same sequences.
static json crossings_group(const char *name, S2Point a, S2Point b, S2Point c,
                            S2Point d, int crossing_sign,
                            int signed_crossing_sign) {
    a = a.Normalize();
    b = b.Normalize();
    c = c.Normalize();
    d = d.Normalize();
    json out;
    out["name"] = name;
    out["a"] = point_json(a);
    out["b"] = point_json(b);
    out["c"] = point_json(c);
    out["d"] = point_json(d);
    out["crossing_sign"] = crossing_sign;
    out["signed_crossing_sign"] = signed_crossing_sign;
    return out;
}

int main() {
    json out;

    // TEST(S2, Crossings) -- the 12 cases in s2edge_crosser_test.cc.
    // We emit the raw inputs + expected (crossing_sign, signed_crossing_sign)
    // as in upstream TestCrossings(); the OCaml driver will exercise the
    // various argument orderings itself.
    {
        json cases = json::array();

        // 1. Two regular edges that cross.
        cases.push_back(crossings_group(
            "regular_cross", S2Point(1, 2, 1), S2Point(1, -3, 0.5),
            S2Point(1, -0.5, -3), S2Point(0.1, 0.5, 3), 1, 1));

        // 2. Two regular edges that intersect antipodal points.
        cases.push_back(crossings_group(
            "antipodal_no_cross", S2Point(1, 2, 1), S2Point(1, -3, 0.5),
            S2Point(-1, 0.5, 3), S2Point(-0.1, -0.5, -3), -1, 0));

        // 3. Two edges on the same great circle starting at antipodal points.
        cases.push_back(crossings_group("same_great_circle", S2Point(0, 0, -1),
                                        S2Point(0, 1, 0), S2Point(0, 0, 1),
                                        S2Point(0, 1, 1), -1, 0));

        // 4. Two edges that cross where one vertex is S2::Origin().
        cases.push_back(crossings_group("cross_at_origin", S2Point(1, 0, 0),
                                        S2::Origin(), S2Point(1, -0.1, 1),
                                        S2Point(1, 1, -0.1), 1, 1));

        // 5. Antipodal-origin case.
        cases.push_back(crossings_group("antipodal_origin", S2Point(1, 0, 0),
                                        S2::Origin(), S2Point(-1, 0.1, -1),
                                        S2Point(-1, -1, 0.1), -1, 0));

        // 6. Two edges that share an endpoint.
        cases.push_back(crossings_group("shared_endpoint", S2Point(7, -2, 3),
                                        S2Point(2, 3, 4), S2Point(2, 3, 4),
                                        S2Point(-1, 2, 5), 0, -1));

        // 7. Barely cross near the middle of one edge.
        cases.push_back(crossings_group("barely_cross_middle", S2Point(1, 1, 1),
                                        S2Point(1, nextafter(1.0, 0.0), -1),
                                        S2Point(11, -12, -1),
                                        S2Point(10, 10, 1), 1, -1));

        // 8. Separated by ~1e-15 (no crossing).
        cases.push_back(crossings_group("separated_1e15", S2Point(1, 1, 1),
                                        S2Point(1, nextafter(1.0, 2.0), -1),
                                        S2Point(1, -1, 0), S2Point(1, 1, 0), -1,
                                        0));

        // 9. Barely cross near the end of both edges (needs > double prec).
        cases.push_back(crossings_group(
            "barely_cross_end", S2Point(0, 0, 1), S2Point(2, -1e-323, 1),
            S2Point(1, -1, 1), S2Point(1e-323, 0, 1), 1, -1));

        // 10. Separated by ~1e-640.
        cases.push_back(crossings_group(
            "separated_1e640", S2Point(0, 0, 1), S2Point(2, 1e-323, 1),
            S2Point(1, -1, 1), S2Point(1e-323, 0, 1), -1, 0));

        // 11. Barely cross, > 2000 bits of precision.
        cases.push_back(crossings_group(
            "barely_cross_2000bits", S2Point(1, -1e-323, -1e-323),
            S2Point(1e-323, 1, 1e-323), S2Point(1, -1, 1e-323),
            S2Point(1, 1, 0), 1, 1));

        // 12. Separated by ~1e-640 (variant).
        cases.push_back(crossings_group(
            "separated_1e640_variant", S2Point(1, 1e-323, -1e-323),
            S2Point(-1e-323, 1, 1e-323), S2Point(1, -1, 1e-323),
            S2Point(1, 1, 0), -1, 0));

        out["crossings"] = cases;
    }

    // Chain sequences that drive the stateful crosser over several successive
    // edges.  This exercises the fast path that compares acb to bda as well as
    // the cached tangents and the RestartAt() jump.  We emit each step (c_prev
    // -> d) paired with the expected CrossingSign and EdgeOrVertexCrossing for
    // the chain edge (AB, c_prev, d).
    {
        json groups = json::array();

        auto emit_chain = [&](const char *name, S2Point a, S2Point b,
                              const std::vector<S2Point> &raw_chain) {
            a = a.Normalize();
            b = b.Normalize();
            std::vector<S2Point> chain;
            chain.reserve(raw_chain.size());
            for (const auto &p : raw_chain)
                chain.push_back(p.Normalize());

            S2EdgeCrosser crosser(&a, &b);
            crosser.RestartAt(&chain[0]);
            json steps = json::array();
            for (size_t i = 1; i < chain.size(); ++i) {
                int cs = crosser.CrossingSign(&chain[i]);
                // Recompute EOV / Signed with a separate crosser so the
                // CrossingSign() call above is the only source of state drift
                // on `crosser`.  This mirrors how the OCaml test will iterate.
                S2EdgeCrosser eov_crosser(&a, &b);
                eov_crosser.RestartAt(&chain[i - 1]);
                bool eov = eov_crosser.EdgeOrVertexCrossing(&chain[i]);
                S2EdgeCrosser sgn_crosser(&a, &b);
                sgn_crosser.RestartAt(&chain[i - 1]);
                int sgn = sgn_crosser.SignedEdgeOrVertexCrossing(&chain[i]);
                json step;
                step["c_index"] = static_cast<int>(i - 1);
                step["d_index"] = static_cast<int>(i);
                step["crossing_sign"] = cs;
                step["edge_or_vertex"] = eov;
                step["signed_crossing"] = sgn;
                steps.push_back(step);
            }
            json vs = json::array();
            for (const auto &p : chain)
                vs.push_back(point_json(p));
            json group;
            group["name"] = name;
            group["a"] = point_json(a);
            group["b"] = point_json(b);
            group["chain"] = vs;
            group["steps"] = steps;
            groups.push_back(group);
        };

        // A chain of points sampled along a great circle crossing AB twice.
        emit_chain("great_circle_chain", S2Point(1, 0, 0), S2Point(0, 1, 0),
                   {S2Point(0, 0, 1), S2Point(1, 0, 1), S2Point(1, 1, 1),
                    S2Point(0, 1, -1), S2Point(-1, 0, -1), S2Point(0, 0, -1)});

        // A chain that does not cross AB at all.
        emit_chain("no_cross_chain", S2Point(0, 0, 1), S2Point(1, 1, 1),
                   {S2Point(1, 0, -1), S2Point(1, -1, -1), S2Point(0, -1, -1),
                    S2Point(-1, -1, -1)});

        // A chain where one step shares a vertex with AB (vertex crossing).
        emit_chain("vertex_share_chain", S2Point(1, 0, 0), S2Point(0, 1, 0),
                   {S2Point(0, 0, 1), S2Point(1, 1, 0), S2Point(1, -1, 0)});

        out["chains"] = groups;
    }

    // TEST(S2, CollinearEdgesThatDontTouch) - static deterministic seeds.
    // We cannot easily reproduce the random generator here, so we use a few
    // fixed great-circle interpolations (a = start, d = end, b = 5% in,
    // c = 95% in): crossing_sign must be negative.
    {
        json cases = json::array();
        auto emit = [&](const char *name, S2Point a, S2Point d) {
            a = a.Normalize();
            d = d.Normalize();
            S2Point b = S2::Interpolate(a, d, 0.05);
            S2Point c = S2::Interpolate(a, d, 0.95);
            int cs = S2::CrossingSign(a, b, c, d);
            json g;
            g["name"] = name;
            g["a"] = point_json(a);
            g["b"] = point_json(b);
            g["c"] = point_json(c);
            g["d"] = point_json(d);
            g["crossing_sign"] = cs;
            cases.push_back(g);
        };
        emit("equator", S2Point(1, 0, 0), S2Point(-1, 0.001, 0));
        emit("meridian", S2Point(1, 0, 0), S2Point(0, 0, 1));
        emit("diagonal", S2Point(1, 1, 0), S2Point(0, 1, 1));
        emit("skew", S2Point(2, 1, 3), S2Point(-1, 4, -2));

        out["collinear_no_touch"] = cases;
    }

    (void)crossing_case;  // silence unused warning if reworked later.
    std::cout << out.dump(2) << std::endl;
    return 0;
}
