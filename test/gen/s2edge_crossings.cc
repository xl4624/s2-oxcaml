// Golden data generator for S2EdgeCrossings.
// Mirrors s2geometry/src/s2/s2edge_crossings_test.cc and
// s2geometry/src/s2/s2edge_crosser_test.cc.

#include "s2/s2edge_crossings.h"

#include <cfloat>
#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/s1angle.h"
#include "s2/s2point.h"
#include "s2/s2pointutil.h"
#include "s2/s2predicates.h"

using json = nlohmann::json;

json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

// Emit a single crossing test case (normalized).
json crossing_case(const char *name, S2Point a, S2Point b, S2Point c, S2Point d,
                   int expected_crossing_sign,
                   int expected_signed_crossing_sign) {
    a = a.Normalize();
    b = b.Normalize();
    c = c.Normalize();
    d = d.Normalize();
    // Compute actual values from the library.
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

int main() {
    json out;

    // TEST(S2, Crossings) from s2edge_crosser_test.cc
    {
        json cases = json::array();

        // 1. Two regular edges that cross.
        cases.push_back(crossing_case("regular_cross", S2Point(1, 2, 1),
                                      S2Point(1, -3, 0.5), S2Point(1, -0.5, -3),
                                      S2Point(0.1, 0.5, 3), 1, 1));

        // 2. Two regular edges that intersect antipodal points.
        cases.push_back(crossing_case("antipodal_no_cross", S2Point(1, 2, 1),
                                      S2Point(1, -3, 0.5), S2Point(-1, 0.5, 3),
                                      S2Point(-0.1, -0.5, -3), -1, 0));

        // 3. Two edges on the same great circle that start at antipodal points.
        cases.push_back(crossing_case("same_great_circle", S2Point(0, 0, -1),
                                      S2Point(0, 1, 0), S2Point(0, 0, 1),
                                      S2Point(0, 1, 1), -1, 0));

        // 4. Two edges that cross where one vertex is S2::Origin().
        cases.push_back(crossing_case("cross_at_origin", S2Point(1, 0, 0),
                                      S2::Origin(), S2Point(1, -0.1, 1),
                                      S2Point(1, 1, -0.1), 1, 1));

        // 5. Two edges that intersect antipodal points where one vertex is
        // Origin.
        cases.push_back(crossing_case("antipodal_origin", S2Point(1, 0, 0),
                                      S2::Origin(), S2Point(-1, 0.1, -1),
                                      S2Point(-1, -1, 0.1), -1, 0));

        // 6. Two edges that share an endpoint.
        cases.push_back(crossing_case("shared_endpoint", S2Point(7, -2, 3),
                                      S2Point(2, 3, 4), S2Point(2, 3, 4),
                                      S2Point(-1, 2, 5), 0, -1));

        // 7. Two edges that barely cross near the middle of one edge.
        cases.push_back(crossing_case("barely_cross_middle", S2Point(1, 1, 1),
                                      S2Point(1, nextafter(1.0, 0.0), -1),
                                      S2Point(11, -12, -1), S2Point(10, 10, 1),
                                      1, -1));

        // 8. Separated by about 1e-15.
        cases.push_back(crossing_case("separated_1e15", S2Point(1, 1, 1),
                                      S2Point(1, nextafter(1.0, 2.0), -1),
                                      S2Point(1, -1, 0), S2Point(1, 1, 0), -1,
                                      0));

        // 9. Barely cross near end, requires extended precision.
        cases.push_back(crossing_case("barely_cross_end", S2Point(0, 0, 1),
                                      S2Point(2, -1e-323, 1), S2Point(1, -1, 1),
                                      S2Point(1e-323, 0, 1), 1, -1));

        // 10. Separated by about 1e-640.
        cases.push_back(crossing_case("separated_1e640", S2Point(0, 0, 1),
                                      S2Point(2, 1e-323, 1), S2Point(1, -1, 1),
                                      S2Point(1e-323, 0, 1), -1, 0));

        // 11. Barely cross, requires >2000 bits of precision.
        cases.push_back(
            crossing_case("barely_cross_2000bits", S2Point(1, -1e-323, -1e-323),
                          S2Point(1e-323, 1, 1e-323), S2Point(1, -1, 1e-323),
                          S2Point(1, 1, 0), 1, 1));

        // 12. Separated by about 1e-640 (variant).
        cases.push_back(crossing_case(
            "separated_1e640_variant", S2Point(1, 1e-323, -1e-323),
            S2Point(-1e-323, 1, 1e-323), S2Point(1, -1, 1e-323),
            S2Point(1, 1, 0), -1, 0));

        out["crossings"] = cases;
    }

    // TEST(S2, AngleContainsVertex) from s2edge_crossings_test.cc
    {
        json cases = json::object();
        S2Point a(1, 0, 0), b(0, 1, 0);

        // Degenerate angle ABA.
        cases["degenerate_aba"] = S2::AngleContainsVertex(a, b, a);

        // An angle where A == RefDir(B).
        S2Point ref_b = S2::RefDir(b);
        cases["a_eq_refdir_b"] = S2::AngleContainsVertex(ref_b, b, a);

        // An angle where C == RefDir(B).
        cases["c_eq_refdir_b"] = S2::AngleContainsVertex(a, b, ref_b);

        // Emit the actual points for reconstruction.
        cases["a"] = point_json(a);
        cases["b"] = point_json(b);
        cases["ref_b"] = point_json(ref_b);

        out["angle_contains_vertex"] = cases;
    }

    // TEST(S2, VertexCrossing) - manual cases
    {
        json cases = json::array();

        // Case: A == B (degenerate edge), should return false.
        {
            S2Point a(1, 0, 0), c(0, 1, 0), d(0, 0, 1);
            cases.push_back({{"name", "degenerate_ab"},
                             {"a", point_json(a)},
                             {"b", point_json(a)},
                             {"c", point_json(c)},
                             {"d", point_json(d)},
                             {"expected", S2::VertexCrossing(a, a, c, d)}});
        }

        // Case: C == D (degenerate edge), should return false.
        {
            S2Point a(1, 0, 0), b(0, 1, 0), c(0, 0, 1);
            cases.push_back({{"name", "degenerate_cd"},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"c", point_json(c)},
                             {"d", point_json(c)},
                             {"expected", S2::VertexCrossing(a, b, c, c)}});
        }

        // Case: A == C, B != D
        {
            S2Point a(1, 0, 0), b(0, 1, 0), d(0, 0, 1);
            cases.push_back({{"name", "a_eq_c"},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"c", point_json(a)},
                             {"d", point_json(d)},
                             {"expected", S2::VertexCrossing(a, b, a, d)}});
        }

        // Case: A == C, B == D (same edge)
        {
            S2Point a(1, 0, 0), b(0, 1, 0);
            cases.push_back({{"name", "same_edge"},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"c", point_json(a)},
                             {"d", point_json(b)},
                             {"expected", S2::VertexCrossing(a, b, a, b)}});
        }

        // Case: A == D, B == C (reversed edge)
        {
            S2Point a(1, 0, 0), b(0, 1, 0);
            cases.push_back({{"name", "reversed_edge"},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"c", point_json(b)},
                             {"d", point_json(a)},
                             {"expected", S2::VertexCrossing(a, b, b, a)}});
        }

        // Case: B == D
        {
            S2Point a(1, 0, 0), b(0, 1, 0), c(0, 0, 1);
            cases.push_back({{"name", "b_eq_d"},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"c", point_json(c)},
                             {"d", point_json(b)},
                             {"expected", S2::VertexCrossing(a, b, c, b)}});
        }

        // Case: shared endpoint from crosser_test (case 6).
        {
            S2Point a = S2Point(7, -2, 3).Normalize();
            S2Point b = S2Point(2, 3, 4).Normalize();
            S2Point c = b;  // shared
            S2Point d = S2Point(-1, 2, 5).Normalize();
            cases.push_back({{"name", "shared_endpoint_crosser6"},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"c", point_json(c)},
                             {"d", point_json(d)},
                             {"expected", S2::VertexCrossing(a, b, c, d)}});
        }

        out["vertex_crossing"] = cases;
    }

    // SignedVertexCrossing - same shared-vertex setups, signed result.
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &a,
                       const S2Point &b, const S2Point &c, const S2Point &d) {
            cases.push_back(
                {{"name", name},
                 {"a", point_json(a)},
                 {"b", point_json(b)},
                 {"c", point_json(c)},
                 {"d", point_json(d)},
                 {"expected", S2::SignedVertexCrossing(a, b, c, d)}});
        };

        // Identity (1): SVC(a,a,c,d) == 0.
        {
            S2Point a(1, 0, 0), c(0, 1, 0), d(0, 0, 1);
            add("degenerate_ab", a, a, c, d);
        }
        // Identity (1): SVC(a,b,c,c) == 0.
        {
            S2Point a(1, 0, 0), b(0, 1, 0), c(0, 0, 1);
            add("degenerate_cd", a, b, c, c);
        }
        // Identity (2): SVC(a,b,a,b) == +1.
        {
            S2Point a(1, 0, 0), b(0, 1, 0);
            add("same_edge", a, b, a, b);
        }
        // Identity (3): SVC(a,b,b,a) == -1.
        {
            S2Point a(1, 0, 0), b(0, 1, 0);
            add("reversed_edge", a, b, b, a);
        }
        // a == c, b != d.
        {
            S2Point a(1, 0, 0), b(0, 1, 0), d(0, 0, 1);
            add("a_eq_c", a, b, a, d);
        }
        // b == d.
        {
            S2Point a(1, 0, 0), b(0, 1, 0), c(0, 0, 1);
            add("b_eq_d", a, b, c, b);
        }
        // a == d.
        {
            S2Point a(1, 0, 0), b(0, 1, 0), c(0, 0, 1);
            add("a_eq_d", a, b, c, a);
        }
        // b == c.
        {
            S2Point a(1, 0, 0), b(0, 1, 0), d(0, 0, 1);
            add("b_eq_c", a, b, b, d);
        }
        // Shared endpoint from crosser_test (case 6).
        {
            S2Point a = S2Point(7, -2, 3).Normalize();
            S2Point b = S2Point(2, 3, 4).Normalize();
            S2Point d = S2Point(-1, 2, 5).Normalize();
            add("shared_endpoint_crosser6", a, b, b, d);
        }
        out["signed_vertex_crossing"] = cases;
    }

    // TEST(S2, GetIntersection) - selected deterministic cases
    {
        json cases = json::array();

        // Two regular edges that cross.
        {
            S2Point a = S2Point(1, 2, 1).Normalize();
            S2Point b = S2Point(1, -3, 0.5).Normalize();
            S2Point c = S2Point(1, -0.5, -3).Normalize();
            S2Point d = S2Point(0.1, 0.5, 3).Normalize();
            if (S2::CrossingSign(a, b, c, d) > 0) {
                S2Point x = S2::GetIntersection(a, b, c, d);
                cases.push_back({{"name", "regular_cross"},
                                 {"a", point_json(a)},
                                 {"b", point_json(b)},
                                 {"c", point_json(c)},
                                 {"d", point_json(d)},
                                 {"intersection", point_json(x)}});
            }
        }

        // Cross at origin.
        {
            S2Point a = S2Point(1, 0, 0).Normalize();
            S2Point b = S2::Origin();
            S2Point c = S2Point(1, -0.1, 1).Normalize();
            S2Point d = S2Point(1, 1, -0.1).Normalize();
            if (S2::CrossingSign(a, b, c, d) > 0) {
                S2Point x = S2::GetIntersection(a, b, c, d);
                cases.push_back({{"name", "cross_at_origin"},
                                 {"a", point_json(a)},
                                 {"b", point_json(b)},
                                 {"c", point_json(c)},
                                 {"d", point_json(d)},
                                 {"intersection", point_json(x)}});
            }
        }

        // TEST(S2, ExactIntersectionUnderflow)
        {
            S2Point a0(1, 0, 0), a1(1, 2e-300, 0);
            S2Point b0(1, 1e-300, 0), b1(1, 3e-300, 0);
            S2Point x = S2::GetIntersection(a0, a1, b0, b1);
            cases.push_back({{"name", "exact_underflow"},
                             {"a", point_json(a0)},
                             {"b", point_json(a1)},
                             {"c", point_json(b0)},
                             {"d", point_json(b1)},
                             {"intersection", point_json(x)}});
        }

        // TEST(S2, ExactIntersectionSign)
        {
            S2Point a0(-1, -1.6065916409055676e-10, 0);
            S2Point a1(1, 0, 0);
            S2Point b0(1, -4.7617930898495072e-13, 0);
            S2Point b1(-1, 1.2678623820887328e-09, 0);
            S2Point x = S2::GetIntersection(a0, a1, b0, b1);
            cases.push_back({{"name", "exact_sign"},
                             {"a", point_json(a0)},
                             {"b", point_json(a1)},
                             {"c", point_json(b0)},
                             {"d", point_json(b1)},
                             {"intersection", point_json(x)}});
        }

        out["intersection"] = cases;
    }

    // TEST(S2, RobustSign / Sign) - basic cases
    {
        json cases = json::array();

        auto add_sign = [&](const char *name, S2Point a, S2Point b, S2Point c) {
            int sign = s2pred::Sign(a, b, c);
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"c", point_json(c)},
                             {"sign", sign}});
        };

        // Basic CCW triangle.
        add_sign("ccw_xyz", S2Point(1, 0, 0), S2Point(0, 1, 0),
                 S2Point(0, 0, 1));

        // CW triangle.
        add_sign("cw_xyz", S2Point(0, 1, 0), S2Point(1, 0, 0),
                 S2Point(0, 0, 1));

        // Collinear (on equator).
        add_sign("equator_collinear", S2Point(1, 0, 0), S2Point(0, 1, 0),
                 S2Point(-1, 0, 0));

        // Degenerate: two points same.
        add_sign("a_eq_b", S2Point(1, 0, 0), S2Point(1, 0, 0),
                 S2Point(0, 1, 0));

        // Near-collinear points.
        add_sign("near_collinear", S2Point(1, 0, 0),
                 S2Point(1, 1e-15, 0).Normalize(),
                 S2Point(1, 0, 1e-15).Normalize());

        out["sign"] = cases;
    }

    // TEST(S2, CompareEdgesOrderInvariant)
    {
        json cases = json::object();
        S2Point v0(0, 1, 0), v1(1, 0, 0);
        cases["v0_v1_v0_v1"] = S2::internal::CompareEdges(v0, v1, v0, v1);
        cases["v1_v0_v0_v1"] = S2::internal::CompareEdges(v1, v0, v0, v1);
        cases["v0_v1_v1_v0"] = S2::internal::CompareEdges(v0, v1, v1, v0);
        cases["v1_v0_v1_v0"] = S2::internal::CompareEdges(v1, v0, v1, v0);
        cases["v0"] = point_json(v0);
        cases["v1"] = point_json(v1);
        out["compare_edges"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
