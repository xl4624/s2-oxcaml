// Golden data generator for S2Predicates.

#include "s2/s2predicates.h"

#include <cfloat>
#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/s1angle.h"
#include "s2/s1chord_angle.h"
#include "s2/s2point.h"

using json = nlohmann::json;

json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

S2Point normalized(const S2Point &p) {
    return p.Normalize();
}

json sign_case(const S2Point &a, const S2Point &b, const S2Point &c,
               const char *label) {
    S2Point na = normalized(a), nb = normalized(b), nc = normalized(c);
    return {{"label", label},
            {"a", point_json(na)},
            {"b", point_json(nb)},
            {"c", point_json(nc)},
            {"sign", s2pred::Sign(na, nb, nc)}};
}

// Like sign_case but does NOT normalize (points are already exactly
// representable unit vectors, and normalization would shift bits).
json sign_case_exact(const S2Point &a, const S2Point &b, const S2Point &c,
                     const char *label) {
    return {{"label", label},
            {"a", point_json(a)},
            {"b", point_json(b)},
            {"c", point_json(c)},
            {"sign", s2pred::Sign(a, b, c)}};
}

json compare_distances_case(const S2Point &x, const S2Point &a,
                            const S2Point &b, const char *label) {
    S2Point nx = normalized(x), na = normalized(a), nb = normalized(b);
    return {{"label", label},
            {"x", point_json(nx)},
            {"a", point_json(na)},
            {"b", point_json(nb)},
            {"sign", s2pred::CompareDistances(nx, na, nb)}};
}

json compare_distances_case_exact(const S2Point &x, const S2Point &a,
                                  const S2Point &b, const char *label) {
    return {{"label", label},
            {"x", point_json(x)},
            {"a", point_json(a)},
            {"b", point_json(b)},
            {"sign", s2pred::CompareDistances(x, a, b)}};
}

json compare_distance_case(const S2Point &x, const S2Point &y, double r_rad,
                           const char *label) {
    S2Point nx = normalized(x), ny = normalized(y);
    S1ChordAngle r(S1Angle::Radians(r_rad));
    return {{"label", label},
            {"x", point_json(nx)},
            {"y", point_json(ny)},
            {"r_length2", r.length2()},
            {"sign", s2pred::CompareDistance(nx, ny, r)}};
}

json compare_distance_case_length2(const S2Point &x, const S2Point &y,
                                   double length2, const char *label) {
    S2Point nx = normalized(x), ny = normalized(y);
    S1ChordAngle r = S1ChordAngle::FromLength2(length2);
    return {{"label", label},
            {"x", point_json(nx)},
            {"y", point_json(ny)},
            {"r_length2", r.length2()},
            {"sign", s2pred::CompareDistance(nx, ny, r)}};
}

json sign_dot_prod_case(const S2Point &a, const S2Point &b, const char *label) {
    // No normalization - vectors may intentionally not be unit length.
    return {{"label", label},
            {"a", point_json(a)},
            {"b", point_json(b)},
            {"sign", s2pred::SignDotProd(a, b)}};
}

json ordered_ccw_case(const S2Point &a, const S2Point &b, const S2Point &c,
                      const S2Point &o, const char *label) {
    S2Point na = normalized(a), nb = normalized(b), nc = normalized(c),
            no = normalized(o);
    return {
        {"label", label},      {"a", point_json(na)},
        {"b", point_json(nb)}, {"c", point_json(nc)},
        {"o", point_json(no)}, {"ordered", s2pred::OrderedCCW(na, nb, nc, no)}};
}

int main() {
    json out;

    // TEST(Sign, StableSignUnderflow)
    // Stable-formula sign test: the underflowed y/z components force triage to
    // return 0, so stableSign (used by RobustSign) must handle it.
    {
        json cases = json::array();
        S2Point a(1, 1.9535722048627587e-90, 7.4882501322554515e-80);
        S2Point b(1, 9.6702373087191359e-127, 3.706704857169321e-116);
        S2Point c(1, 3.8163353663361477e-142, 1.4628419538608985e-131);
        cases.push_back(sign_case_exact(a, b, c, "underflow"));
        out["sign_underflow"] = cases;
    }

    // TEST(Sign, CollinearPoints) - exact-arithmetic cases.
    {
        json cases = json::array();

        // Exactly collinear tangent triple: c is the exact midpoint of ab.
        {
            S2Point a(0.72571927877036835, 0.46058825605889098,
                      0.51106749730504852);
            S2Point b(0.7257192746638208, 0.46058826573818168,
                      0.51106749441312738);
            S2Point c(0.72571927671709457, 0.46058826089853633,
                      0.51106749585908795);
            cases.push_back(sign_case_exact(a, b, c, "collinear_tangent"));
        }

        // Exactly proportional x1, x2: (x1, x2, -x1) is a degenerate triple
        // of three distinct points on a line through the origin.
        {
            S2Point x1(0.99999999999999989, 1.4901161193847655e-08, 0);
            S2Point x2(1, 1.4901161193847656e-08, 0);
            cases.push_back(sign_case_exact(x1, x2, -x1, "proportional_x1_x2"));
        }

        // x3 = Normalize(1,1,1), x4 = 0.99999999999999989 * x3.
        {
            S2Point x3 = S2Point(1, 1, 1).Normalize();
            S2Point x4 = 0.99999999999999989 * x3;
            cases.push_back(sign_case_exact(x3, x4, -x3, "proportional_x3_x4"));
        }

        // Normalize is not idempotent: y1 = Normalize((1,1,0)),
        // y2 = Normalize(y1), y1 != y2 but both are normalized.
        {
            S2Point y1 = S2Point(1, 1, 0).Normalize();
            S2Point y2 = y1.Normalize();
            cases.push_back(sign_case_exact(y1, y2, -y1, "proportional_y1_y2"));
        }

        out["sign_collinear"] = cases;
    }

    // TEST(Sign, SymbolicPerturbationCodeCoverage) - each case zeroes the
    // triage + stable determinant and exercises one branch of
    // SymbolicallyPerturbedSign (M1..M13). The inputs are already in
    // lex-sorted order (a < b < c).
    {
        json cases = json::array();
        cases.push_back(sign_case_exact(S2Point(-3, -1, 0), S2Point(-2, 1, 0),
                                        S2Point(1, -2, 0), "m1"));
        cases.push_back(sign_case_exact(S2Point(-6, 3, 3), S2Point(-4, 2, -1),
                                        S2Point(-2, 1, 4), "m2"));
        cases.push_back(sign_case_exact(S2Point(0, -1, -1), S2Point(0, 1, -2),
                                        S2Point(0, 2, 1), "m3"));
        cases.push_back(sign_case_exact(S2Point(-1, 2, 7), S2Point(2, 1, -4),
                                        S2Point(4, 2, -8), "m4"));
        cases.push_back(sign_case_exact(S2Point(-4, -2, 7), S2Point(2, 1, -4),
                                        S2Point(4, 2, -8), "m5"));
        cases.push_back(sign_case_exact(S2Point(0, -5, 7), S2Point(0, -4, 8),
                                        S2Point(0, -2, 4), "m6"));
        cases.push_back(sign_case_exact(S2Point(-5, -2, 7), S2Point(0, 0, -2),
                                        S2Point(0, 0, -1), "m7"));
        cases.push_back(sign_case_exact(S2Point(0, -2, 7), S2Point(0, 0, 1),
                                        S2Point(0, 0, 2), "m8"));
        cases.push_back(sign_case_exact(S2Point(-3, 1, 7), S2Point(-1, -4, 1),
                                        S2Point(0, 0, 0), "m9"));
        cases.push_back(sign_case_exact(S2Point(-6, -4, 7), S2Point(-3, -2, 1),
                                        S2Point(0, 0, 0), "m10"));
        cases.push_back(sign_case_exact(S2Point(0, -4, 7), S2Point(0, -2, 1),
                                        S2Point(0, 0, 0), "m11"));
        cases.push_back(sign_case_exact(S2Point(-1, -4, 5), S2Point(0, 0, -3),
                                        S2Point(0, 0, 0), "m12"));
        cases.push_back(sign_case_exact(S2Point(0, -4, 5), S2Point(0, 0, -5),
                                        S2Point(0, 0, 0), "m13"));
        out["sign_symbolic"] = cases;
    }

    // Standard Sign cases: easy CCW and CW.
    {
        json cases = json::array();
        // Three canonical axes form a CCW triple.
        cases.push_back(sign_case(S2Point(1, 0, 0), S2Point(0, 1, 0),
                                  S2Point(0, 0, 1), "axes_ccw"));
        cases.push_back(sign_case(S2Point(1, 0, 0), S2Point(0, 0, 1),
                                  S2Point(0, 1, 0), "axes_cw"));
        // Identical points are collinear (sign = 0).
        cases.push_back(sign_case(S2Point(1, 0, 0), S2Point(1, 0, 0),
                                  S2Point(0, 1, 0), "degenerate_ab"));
        cases.push_back(sign_case(S2Point(1, 0, 0), S2Point(0, 1, 0),
                                  S2Point(1, 0, 0), "degenerate_ac"));
        // A random CCW triple.
        cases.push_back(sign_case(S2Point(1, 2, 3), S2Point(2, 3, -1),
                                  S2Point(-1, 2, 0), "random_ccw"));

        out["sign_basic"] = cases;
    }

    // TEST(CompareDistances, Coverage) - covers triage, exact, and symbolic
    // code paths. Upstream long-double-only sub-cases are resolved by our
    // exact fallback in this port.
    {
        json cases = json::array();
        // Sin2 triage path
        cases.push_back(
            compare_distances_case(S2Point(1, 1, 1), S2Point(1, 1 - 1e-15, 1),
                                   S2Point(1, 1, 1 + 2e-15), "sin2_triage"));
        cases.push_back(compare_distances_case(
            S2Point(1, 1, 0), S2Point(1, 1 - 1e-15, 1e-21),
            S2Point(1, 1 - 1e-15, 0), "sin2_triage_b"));
        // Sin2 exact path: b has a 1e-100 z-component that only exact
        // arithmetic can distinguish.
        cases.push_back(
            compare_distances_case(S2Point(2, 0, 0), S2Point(2, -1, 0),
                                   S2Point(2, 1, 1e-100), "sin2_exact"));

        // Symbolic cases where a == b (as points after normalization) use Cmp.
        cases.push_back(compare_distances_case_exact(
            S2Point(1, 0, 0), S2Point(1, -1, 0), S2Point(1, 1, 0), "symbolic"));
        cases.push_back(compare_distances_case_exact(
            S2Point(1, 0, 0), S2Point(1, 0, 0), S2Point(1, 0, 0),
            "symbolic_all_equal"));
        // Sin2 symbolic: a == b after normalization.
        cases.push_back(
            compare_distances_case(S2Point(1, 0, 0), S2Point(1, -1, 0),
                                   S2Point(1, 1, 0), "sin2_symbolic"));

        // Cos path (triage)
        cases.push_back(
            compare_distances_case(S2Point(1, 1, 1), S2Point(1, -1, 0),
                                   S2Point(-1, 1, 3e-15), "cos_triage"));
        cases.push_back(
            compare_distances_case(S2Point(1, 0, 0), S2Point(1, 1e-30, 0),
                                   S2Point(-1, 1e-40, 0), "cos_triage_b"));
        // Cos exact: 1e-100 z-component on b.
        cases.push_back(
            compare_distances_case(S2Point(1, 1, 1), S2Point(1, -1, 0),
                                   S2Point(-1, 1, 1e-100), "cos_exact"));
        // Cos symbolic: b has zero z, becomes equal to -a direction.
        cases.push_back(
            compare_distances_case(S2Point(1, 1, 1), S2Point(1, -1, 0),
                                   S2Point(-1, 1, 0), "cos_symbolic"));
        // Cos symbolic where a == b exactly.
        cases.push_back(compare_distances_case(
            S2Point(1, 1, 1), S2Point(1, -1, 0), S2Point(1, -1, 0),
            "cos_symbolic_all_equal"));

        // MinusSin2 (distances greater than 90 degrees).
        cases.push_back(
            compare_distances_case(S2Point(1, 1, 0), S2Point(-1, -1 + 1e-15, 0),
                                   S2Point(-1, -1, 0), "minus_sin2_triage"));
        // MinusSin2 exact.
        cases.push_back(
            compare_distances_case(S2Point(-1, -1, 0), S2Point(2, 1, 0),
                                   S2Point(2, 1, 1e-30), "minus_sin2_exact"));
        // MinusSin2 symbolic.
        cases.push_back(
            compare_distances_case(S2Point(-1, -1, 0), S2Point(2, 1, 0),
                                   S2Point(1, 2, 0), "minus_sin2_symbolic"));

        out["compare_distances"] = cases;
    }

    // TEST(CompareDistance, Coverage) - triage and exact code paths.
    {
        json cases = json::array();
        // Sin2 path
        cases.push_back(compare_distance_case(
            S2Point(1, 1, 1), S2Point(1, 1 - 1e-15, 1), 1e-15, "sin2_triage"));
        cases.push_back(compare_distance_case(S2Point(1, 0, 0),
                                              S2Point(1 + DBL_EPSILON, 0, 0),
                                              0.0, "zero_length"));
        // Sin2 exact paths: the sub-ulp difference in x is below triage
        // tolerance so only exact arithmetic can resolve it.
        cases.push_back(compare_distance_case(
            S2Point(1, 1e-40, 0), S2Point(1 + DBL_EPSILON, 1e-40, 0),
            0.9 * DBL_EPSILON * 1e-40, "sin2_exact_pos"));
        cases.push_back(compare_distance_case(
            S2Point(1, 1e-40, 0), S2Point(1 + DBL_EPSILON, 1e-40, 0),
            1.1 * DBL_EPSILON * 1e-40, "sin2_exact_neg"));
        // Zero length with exact-equal points after normalization.
        cases.push_back(compare_distance_case_length2(
            S2Point(1, 0, 0), S2Point(1 + DBL_EPSILON, 0, 0), 0.0,
            "zero_length_exact"));

        // Cos path
        cases.push_back(compare_distance_case(
            S2Point(1, 0, 0), S2Point(1, 1e-8, 0), 1e-7, "cos_triage"));
        cases.push_back(compare_distance_case(S2Point(1, 0, 0),
                                              S2Point(-1, 1e-8, 0), M_PI - 1e-7,
                                              "cos_triage_near_pi"));
        // 90 degree limits: well-separated case that triage resolves.
        cases.push_back(compare_distance_case(
            S2Point(1, 1, 0), S2Point(1, -1 - 2 * DBL_EPSILON, 0), M_PI_2,
            "cos_right_double"));
        // Right angle resolved via exact: 1e-30 z-component.
        cases.push_back(compare_distance_case_length2(
            S2Point(1, 1, 0), S2Point(1, -1, 1e-30), 2.0, "cos_right_exact"));
        // Exact 60 degrees: chord length^2 = 1, angle = pi/3.
        cases.push_back(compare_distance_case_length2(
            S2Point(1, 1, 0), S2Point(0, 1, 1), 1.0, "cos_60_degrees_exact"));

        out["compare_distance"] = cases;
    }

    // TEST(SignDotProd, Orthogonal / NearlyOrthogonal*) - triage and exact.
    {
        json cases = json::array();
        cases.push_back(sign_dot_prod_case(S2Point(1, 0, 0), S2Point(0, 1, 0),
                                           "orthogonal"));
        // Obviously-positive and obviously-negative cases.
        cases.push_back(
            sign_dot_prod_case(S2Point(1, 0, 0), S2Point(1, 0, 0), "same"));
        cases.push_back(sign_dot_prod_case(S2Point(1, 0, 0), S2Point(-1, 0, 0),
                                           "opposite"));
        // Nearly orthogonal: epsilon and 1e-45 x-components force the exact
        // fallback because the triage error bound is ~3e-16.
        cases.push_back(sign_dot_prod_case(S2Point(1, 0, 0),
                                           S2Point(DBL_EPSILON, 1, 0),
                                           "nearly_orthogonal_eps_pos"));
        cases.push_back(sign_dot_prod_case(S2Point(1, 0, 0),
                                           S2Point(-DBL_EPSILON, 1, 0),
                                           "nearly_orthogonal_eps_neg"));
        cases.push_back(sign_dot_prod_case(S2Point(1, 0, 0),
                                           S2Point(1e-45, 1, 0),
                                           "nearly_orthogonal_exact_pos"));
        cases.push_back(sign_dot_prod_case(S2Point(1, 0, 0),
                                           S2Point(-1e-45, 1, 0),
                                           "nearly_orthogonal_exact_neg"));
        out["sign_dot_prod"] = cases;
    }

    // OrderedCCW - simple coverage.
    {
        json cases = json::array();
        // O is the north pole, A/B/C are equatorial points at 0, 45, 90
        // degrees.
        S2Point o(0, 0, 1);
        S2Point a(1, 0, 0);
        S2Point b = normalized(S2Point(1, 1, 0));
        S2Point c(0, 1, 0);
        cases.push_back(ordered_ccw_case(a, b, c, o, "sorted_ccw"));
        cases.push_back(ordered_ccw_case(a, c, b, o, "sorted_not_ccw"));
        cases.push_back(ordered_ccw_case(a, a, c, o, "a_equals_b"));
        cases.push_back(ordered_ccw_case(a, c, c, o, "b_equals_c"));
        cases.push_back(ordered_ccw_case(a, b, a, o, "a_equals_c_b_between"));
        out["ordered_ccw"] = cases;
    }

    // Note: CircleEdgeIntersectionOrdering is a C++-only predicate (no Go
    // equivalent) and is not ported in this module.

    std::cout << out.dump(2) << std::endl;
    return 0;
}
