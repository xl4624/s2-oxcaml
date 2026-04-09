// Golden data generator for S2EdgeDistances.

#include "s2/s2edge_distances.h"

#include <cmath>
#include <iostream>
#include <limits>
#include <nlohmann/json.hpp>

#include "s2/s1angle.h"
#include "s2/s1chord_angle.h"
#include "s2/s2latlng.h"
#include "s2/s2measures.h"
#include "s2/s2point.h"
#include "s2/s2pointutil.h"

using json = nlohmann::json;

json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

// Emit a distance test case: x, a, b, expected distance (radians), closest
// point.  A zero closest point means "either a or b".
json distance_case(S2Point x, S2Point a, S2Point b, double distance_radians,
                   S2Point expected_closest) {
    x = x.Normalize();
    a = a.Normalize();
    b = b.Normalize();
    expected_closest = expected_closest.Normalize();
    double actual_distance = S2::GetDistance(x, a, b).radians();
    S2Point closest = S2::Project(x, a, b);
    return {
        {"x", point_json(x)},
        {"a", point_json(a)},
        {"b", point_json(b)},
        {"distance_radians", actual_distance},
        {"closest", point_json(closest)},
        {"expected_closest", point_json(expected_closest)},
        {"expected_closest_is_endpoint", expected_closest == S2Point(0, 0, 0)}};
}

json max_distance_case(S2Point x, S2Point a, S2Point b,
                       double distance_radians) {
    x = x.Normalize();
    a = a.Normalize();
    b = b.Normalize();
    S1ChordAngle max_distance = S1ChordAngle::Negative();
    S2::UpdateMaxDistance(x, a, b, &max_distance);
    return {{"x", point_json(x)},
            {"a", point_json(a)},
            {"b", point_json(b)},
            {"distance_radians", max_distance.radians()}};
}

json interpolate_case(S2Point a, S2Point b, double t, S2Point expected) {
    a = a.Normalize();
    b = b.Normalize();
    expected = expected.Normalize();
    S2Point actual = S2::Interpolate(a, b, t);
    return {{"a", point_json(a)},
            {"b", point_json(b)},
            {"t", t},
            {"expected", point_json(expected)},
            {"actual", point_json(actual)}};
}

json edge_pair_min_case(S2Point a0, S2Point a1, S2Point b0, S2Point b1,
                        double distance_radians, S2Point expected_a,
                        S2Point expected_b) {
    a0 = a0.Normalize();
    a1 = a1.Normalize();
    b0 = b0.Normalize();
    b1 = b1.Normalize();
    expected_a = expected_a.Normalize();
    expected_b = expected_b.Normalize();

    S1ChordAngle min_dist = S1ChordAngle::Infinity();
    S2::UpdateEdgePairMinDistance(a0, a1, b0, b1, &min_dist);
    auto closest = S2::GetEdgePairClosestPoints(a0, a1, b0, b1);

    return {{"a0", point_json(a0)},
            {"a1", point_json(a1)},
            {"b0", point_json(b0)},
            {"b1", point_json(b1)},
            {"distance_radians", min_dist.radians()},
            {"closest_a", point_json(closest.first)},
            {"closest_b", point_json(closest.second)},
            {"expected_a", point_json(expected_a)},
            {"expected_b", point_json(expected_b)},
            {"expected_a_is_endpoint", expected_a == S2Point(0, 0, 0)},
            {"expected_b_is_endpoint", expected_b == S2Point(0, 0, 0)}};
}

json edge_pair_max_case(S2Point a0, S2Point a1, S2Point b0, S2Point b1,
                        double distance_radians) {
    a0 = a0.Normalize();
    a1 = a1.Normalize();
    b0 = b0.Normalize();
    b1 = b1.Normalize();
    S1ChordAngle max_dist = S1ChordAngle::Negative();
    S2::UpdateEdgePairMaxDistance(a0, a1, b0, b1, &max_dist);
    return {{"a0", point_json(a0)},
            {"a1", point_json(a1)},
            {"b0", point_json(b0)},
            {"b1", point_json(b1)},
            {"distance_radians", max_dist.radians()}};
}

int main() {
    json out;

    // TEST(S2, GetUpdateMinDistanceMaxError)
    {
        json cases = json::array();
        auto check = [&](double actual, double max_error) {
            S1ChordAngle ca(S1Angle::Radians(actual));
            double err = S2::GetUpdateMinDistanceMaxError(ca);
            S1Angle bound = ca.PlusError(err).ToAngle();
            cases.push_back({{"input_radians", actual},
                             {"max_error", max_error},
                             {"error_bound", err},
                             {"bound_radians", bound.radians()}});
        };
        check(0, 1.5e-15);
        check(1e-8, 1e-15);
        check(1e-5, 1e-15);
        check(0.05, 1e-15);
        check(M_PI_2 - 1e-8, 2e-15);
        check(M_PI_2, 2e-15);
        check(M_PI_2 + 1e-8, 2e-15);
        check(M_PI - 1e-5, 2e-10);
        check(M_PI, 0);
        out["update_min_distance_max_error"] = cases;
    }

    // TEST(S2, Distance)
    {
        json cases = json::array();
        cases.push_back(distance_case(S2Point(1, 0, 0), S2Point(1, 0, 0),
                                      S2Point(0, 1, 0), 0, S2Point(1, 0, 0)));
        cases.push_back(distance_case(S2Point(0, 1, 0), S2Point(1, 0, 0),
                                      S2Point(0, 1, 0), 0, S2Point(0, 1, 0)));
        cases.push_back(distance_case(S2Point(1, 3, 0), S2Point(1, 0, 0),
                                      S2Point(0, 1, 0), 0, S2Point(1, 3, 0)));
        cases.push_back(distance_case(S2Point(0, 0, 1), S2Point(1, 0, 0),
                                      S2Point(0, 1, 0), M_PI_2,
                                      S2Point(1, 0, 0)));
        cases.push_back(distance_case(S2Point(0, 0, -1), S2Point(1, 0, 0),
                                      S2Point(0, 1, 0), M_PI_2,
                                      S2Point(1, 0, 0)));
        cases.push_back(distance_case(S2Point(-1, -1, 0), S2Point(1, 0, 0),
                                      S2Point(0, 1, 0), 0.75 * M_PI,
                                      S2Point(0, 0, 0)));
        cases.push_back(distance_case(S2Point(0, 1, 0), S2Point(1, 0, 0),
                                      S2Point(1, 1, 0), M_PI_4,
                                      S2Point(1, 1, 0)));
        cases.push_back(distance_case(S2Point(0, -1, 0), S2Point(1, 0, 0),
                                      S2Point(1, 1, 0), M_PI_2,
                                      S2Point(1, 0, 0)));
        cases.push_back(distance_case(S2Point(0, -1, 0), S2Point(1, 0, 0),
                                      S2Point(-1, 1, 0), M_PI_2,
                                      S2Point(1, 0, 0)));
        cases.push_back(distance_case(S2Point(-1, -1, 0), S2Point(1, 0, 0),
                                      S2Point(-1, 1, 0), M_PI_2,
                                      S2Point(-1, 1, 0)));
        cases.push_back(distance_case(S2Point(1, 1, 1), S2Point(1, 0, 0),
                                      S2Point(0, 1, 0), asin(sqrt(1. / 3)),
                                      S2Point(1, 1, 0)));
        cases.push_back(distance_case(S2Point(1, 1, -1), S2Point(1, 0, 0),
                                      S2Point(0, 1, 0), asin(sqrt(1. / 3)),
                                      S2Point(1, 1, 0)));
        cases.push_back(distance_case(S2Point(-1, 0, 0), S2Point(1, 1, 0),
                                      S2Point(1, 1, 0), 0.75 * M_PI,
                                      S2Point(1, 1, 0)));
        cases.push_back(distance_case(S2Point(0, 0, -1), S2Point(1, 1, 0),
                                      S2Point(1, 1, 0), M_PI_2,
                                      S2Point(1, 1, 0)));
        cases.push_back(distance_case(S2Point(-1, 0, 0), S2Point(1, 0, 0),
                                      S2Point(1, 0, 0), M_PI,
                                      S2Point(1, 0, 0)));
        out["distance"] = cases;
    }

    // TEST(S2, MaxDistance)
    {
        json cases = json::array();
        cases.push_back(max_distance_case(S2Point(1, 0, 1), S2Point(1, 0, 0),
                                          S2Point(0, 1, 0), M_PI_2));
        cases.push_back(max_distance_case(S2Point(1, 0, -1), S2Point(1, 0, 0),
                                          S2Point(0, 1, 0), M_PI_2));
        cases.push_back(max_distance_case(S2Point(0, 1, 1), S2Point(1, 0, 0),
                                          S2Point(0, 1, 0), M_PI_2));
        cases.push_back(max_distance_case(S2Point(0, 1, -1), S2Point(1, 0, 0),
                                          S2Point(0, 1, 0), M_PI_2));
        cases.push_back(max_distance_case(S2Point(1, 1, 1), S2Point(1, 0, 0),
                                          S2Point(0, 1, 0),
                                          asin(sqrt(2. / 3))));
        cases.push_back(max_distance_case(S2Point(1, 1, -1), S2Point(1, 0, 0),
                                          S2Point(0, 1, 0),
                                          asin(sqrt(2. / 3))));
        cases.push_back(max_distance_case(S2Point(1, 0, 0), S2Point(1, 1, 0),
                                          S2Point(1, -1, 0), M_PI_4));
        cases.push_back(max_distance_case(S2Point(0, 1, 0), S2Point(1, 1, 0),
                                          S2Point(-1, 1, 0), M_PI_4));
        cases.push_back(max_distance_case(S2Point(0, 0, 1), S2Point(0, 1, 1),
                                          S2Point(0, -1, 1), M_PI_4));
        cases.push_back(max_distance_case(S2Point(0, 0, 1), S2Point(1, 0, 0),
                                          S2Point(1, 0, -1), 3 * M_PI_4));
        cases.push_back(max_distance_case(S2Point(0, 0, 1), S2Point(1, 0, 0),
                                          S2Point(1, 1, -M_SQRT2), 3 * M_PI_4));
        cases.push_back(max_distance_case(S2Point(0, 0, 1), S2Point(0, 0, -1),
                                          S2Point(0, 0, -1), M_PI));
        out["max_distance"] = cases;
    }

    // TEST(S2, Interpolate)
    {
        json cases = json::array();
        S2Point p1 = S2Point(0.1, 1e-30, 0.3).Normalize();
        S2Point p2 = S2Point(-0.7, -0.55, -1e30).Normalize();

        // Zero-length edge at endpoints
        cases.push_back(interpolate_case(p1, p1, 0, p1));
        cases.push_back(interpolate_case(p1, p1, 1, p1));

        // Zero-length edges, actually interpolated
        cases.push_back(interpolate_case(S2Point(1, 0, 0), S2Point(1, 0, 0),
                                         0.5, S2Point(1, 0, 0)));
        cases.push_back(interpolate_case(p1, p1, 0.5, p1));

        // Start, end, and middle of a medium-length edge
        cases.push_back(interpolate_case(p1, p2, 0, p1));
        cases.push_back(interpolate_case(p1, p2, 1, p2));
        cases.push_back(interpolate_case(p1, p2, 0.5, 0.5 * (p1 + p2)));

        // Spherical interpolation: 1/3 and 2/3 along 90-degree arc
        cases.push_back(interpolate_case(S2Point(1, 0, 0), S2Point(0, 1, 0),
                                         1. / 3, S2Point(sqrt(3), 1, 0)));
        cases.push_back(interpolate_case(S2Point(1, 0, 0), S2Point(0, 1, 0),
                                         2. / 3, S2Point(1, sqrt(3), 0)));
        out["interpolate"] = cases;
    }

    // TEST(S2, InterpolateCanExtrapolate)
    {
        json cases = json::array();
        const S2Point i(1, 0, 0);
        const S2Point j(0, 1, 0);
        cases.push_back(interpolate_case(i, j, 0, S2Point(1, 0, 0)));
        cases.push_back(interpolate_case(i, j, 1, S2Point(0, 1, 0)));
        cases.push_back(interpolate_case(i, j, 1.5, S2Point(-1, 1, 0)));
        cases.push_back(interpolate_case(i, j, 2, S2Point(-1, 0, 0)));
        cases.push_back(interpolate_case(i, j, 3, S2Point(0, -1, 0)));
        cases.push_back(interpolate_case(i, j, 4, S2Point(1, 0, 0)));
        cases.push_back(interpolate_case(i, j, -1, S2Point(0, -1, 0)));
        cases.push_back(interpolate_case(i, j, -2, S2Point(-1, 0, 0)));
        cases.push_back(interpolate_case(i, j, -3, S2Point(0, 1, 0)));
        cases.push_back(interpolate_case(i, j, -4, S2Point(1, 0, 0)));

        // Initial vectors at 45 degrees
        cases.push_back(
            interpolate_case(i, S2Point(1, 1, 0), 2, S2Point(0, 1, 0)));
        cases.push_back(
            interpolate_case(i, S2Point(1, 1, 0), 3, S2Point(-1, 1, 0)));
        cases.push_back(
            interpolate_case(i, S2Point(1, 1, 0), 4, S2Point(-1, 0, 0)));

        // Initial vectors at 135 degrees
        cases.push_back(
            interpolate_case(i, S2Point(-1, 1, 0), 2, S2Point(0, -1, 0)));
        out["interpolate_extrapolate"] = cases;
    }

    // TEST(S2, DistanceFraction)
    {
        json cases = json::array();
        // Point on edge, fraction should be exact
        S2Point a(1, 0, 0), b(0, 1, 0);
        for (double f = 0.0; f <= 1.0; f += 0.25) {
            S2Point x = S2::Interpolate(a, b, f);
            double frac = S2::GetDistanceFraction(x, a, b);
            cases.push_back({{"a", point_json(a)},
                             {"b", point_json(b)},
                             {"x", point_json(x)},
                             {"expected_fraction", frac}});
        }
        out["distance_fraction"] = cases;
    }

    // TEST(S2, UpdateMinInteriorDistanceLowerBoundOptimizationIsConservative)
    {
        S2Point x(-0.017952729194524016, -0.30232422079175203,
                  0.95303607751077712);
        S2Point a(-0.017894725505830295, -0.30229974986194175,
                  0.95304493075220664);
        S2Point b(-0.017986591360900289, -0.30233851195954353,
                  0.95303090543659963);
        S1ChordAngle min_distance = S1ChordAngle::Infinity();
        S2::UpdateMinDistance(x, a, b, &min_distance);
        out["interior_distance_conservative"] = {
            {"x", point_json(x)},
            {"a", point_json(a)},
            {"b", point_json(b)},
            {"min_distance_length2", min_distance.length2()}};
    }

    // TEST(S2, EdgePairMinDistance)
    {
        json cases = json::array();
        // One edge is degenerate
        cases.push_back(edge_pair_min_case(
            S2Point(1, 0, 1), S2Point(1, 0, 1), S2Point(1, -1, 0),
            S2Point(1, 1, 0), M_PI_4, S2Point(1, 0, 1), S2Point(1, 0, 0)));
        cases.push_back(edge_pair_min_case(
            S2Point(1, -1, 0), S2Point(1, 1, 0), S2Point(1, 0, 1),
            S2Point(1, 0, 1), M_PI_4, S2Point(1, 0, 0), S2Point(1, 0, 1)));
        // Both edges degenerate
        cases.push_back(edge_pair_min_case(
            S2Point(1, 0, 0), S2Point(1, 0, 0), S2Point(0, 1, 0),
            S2Point(0, 1, 0), M_PI_2, S2Point(1, 0, 0), S2Point(0, 1, 0)));
        // Both degenerate and antipodal
        cases.push_back(edge_pair_min_case(
            S2Point(1, 0, 0), S2Point(1, 0, 0), S2Point(-1, 0, 0),
            S2Point(-1, 0, 0), M_PI, S2Point(1, 0, 0), S2Point(-1, 0, 0)));
        // Two identical edges
        cases.push_back(edge_pair_min_case(
            S2Point(1, 0, 0), S2Point(0, 1, 0), S2Point(1, 0, 0),
            S2Point(0, 1, 0), 0, S2Point(0, 0, 0), S2Point(0, 0, 0)));
        // Both degenerate and identical
        cases.push_back(edge_pair_min_case(
            S2Point(1, 0, 0), S2Point(1, 0, 0), S2Point(1, 0, 0),
            S2Point(1, 0, 0), 0, S2Point(1, 0, 0), S2Point(1, 0, 0)));
        // Edges sharing one vertex (4 possibilities)
        cases.push_back(edge_pair_min_case(
            S2Point(1, 0, 0), S2Point(0, 1, 0), S2Point(0, 1, 0),
            S2Point(0, 1, 1), 0, S2Point(0, 1, 0), S2Point(0, 1, 0)));
        cases.push_back(edge_pair_min_case(
            S2Point(0, 1, 0), S2Point(1, 0, 0), S2Point(0, 1, 0),
            S2Point(0, 1, 1), 0, S2Point(0, 1, 0), S2Point(0, 1, 0)));
        cases.push_back(edge_pair_min_case(
            S2Point(1, 0, 0), S2Point(0, 1, 0), S2Point(0, 1, 1),
            S2Point(0, 1, 0), 0, S2Point(0, 1, 0), S2Point(0, 1, 0)));
        cases.push_back(edge_pair_min_case(
            S2Point(0, 1, 0), S2Point(1, 0, 0), S2Point(0, 1, 1),
            S2Point(0, 1, 0), 0, S2Point(0, 1, 0), S2Point(0, 1, 0)));
        // Two edges whose interiors cross
        cases.push_back(edge_pair_min_case(
            S2Point(1, -1, 0), S2Point(1, 1, 0), S2Point(1, 0, -1),
            S2Point(1, 0, 1), 0, S2Point(1, 0, 0), S2Point(1, 0, 0)));
        out["edge_pair_min_distance"] = cases;
    }

    // TEST(S2, EdgePairMaxDistance)
    {
        json cases = json::array();
        cases.push_back(edge_pair_max_case(S2Point(1, 0, 0), S2Point(0, 1, 0),
                                           S2Point(1, 1, 0), S2Point(1, 1, 1),
                                           acos(1 / sqrt(3))));
        cases.push_back(edge_pair_max_case(S2Point(1, 0, 1), S2Point(1, 0, 1),
                                           S2Point(1, -1, 0), S2Point(1, 1, 0),
                                           acos(0.5)));
        cases.push_back(edge_pair_max_case(S2Point(1, -1, 0), S2Point(1, 1, 0),
                                           S2Point(1, 0, 1), S2Point(1, 0, 1),
                                           acos(0.5)));
        cases.push_back(edge_pair_max_case(S2Point(1, 0, 0), S2Point(1, 0, 0),
                                           S2Point(0, 1, 0), S2Point(0, 1, 0),
                                           M_PI_2));
        cases.push_back(edge_pair_max_case(S2Point(1, 0, 0), S2Point(1, 0, 0),
                                           S2Point(-1, 0, 0), S2Point(-1, 0, 0),
                                           M_PI));
        cases.push_back(edge_pair_max_case(S2Point(1, 0, 0), S2Point(0, 1, 0),
                                           S2Point(1, 0, 0), S2Point(0, 1, 0),
                                           M_PI_2));
        cases.push_back(edge_pair_max_case(S2Point(1, 0, 0), S2Point(1, 0, 0),
                                           S2Point(1, 0, 0), S2Point(1, 0, 0),
                                           0));
        cases.push_back(edge_pair_max_case(S2Point(1, 0, 1), S2Point(1, 0, -1),
                                           S2Point(-1, -1, 0),
                                           S2Point(-1, 1, 0), M_PI));
        cases.push_back(edge_pair_max_case(S2Point(1, 0, 1), S2Point(1, 0, 0),
                                           S2Point(-1, -1, 0),
                                           S2Point(-1, 1, 0), M_PI));
        out["edge_pair_max_distance"] = cases;
    }

    // TEST(S2, IsEdgeBNearEdgeA)
    {
        json cases = json::array();
        auto near_case = [&](double a0_lat, double a0_lng, double a1_lat,
                             double a1_lng, double b0_lat, double b0_lng,
                             double b1_lat, double b1_lng,
                             double tolerance_degrees, bool expected) {
            S2Point a0 = S2LatLng::FromDegrees(a0_lat, a0_lng).ToPoint();
            S2Point a1 = S2LatLng::FromDegrees(a1_lat, a1_lng).ToPoint();
            S2Point b0 = S2LatLng::FromDegrees(b0_lat, b0_lng).ToPoint();
            S2Point b1 = S2LatLng::FromDegrees(b1_lat, b1_lng).ToPoint();
            bool result = S2::IsEdgeBNearEdgeA(
                a0, a1, b0, b1, S1Angle::Degrees(tolerance_degrees));
            cases.push_back({{"a0", point_json(a0)},
                             {"a1", point_json(a1)},
                             {"b0", point_json(b0)},
                             {"b1", point_json(b1)},
                             {"tolerance_degrees", tolerance_degrees},
                             {"expected", result}});
        };

        // Edge near itself
        near_case(5, 5, 10, -5, 5, 5, 10, -5, 1e-6, true);
        // Edge near its reverse
        near_case(5, 5, 10, -5, 10, -5, 5, 5, 1e-6, true);
        // Short edge near long edge
        near_case(10, 0, -10, 0, 2, 1, -2, 1, 1.0, true);
        // Long edge not near shorter edge
        near_case(2, 1, -2, 1, 10, 0, -10, 0, 1.0, false);
        // Orthogonal crossing edges not near each other
        near_case(10, 0, -10, 0, 0, 1.5, 0, -1.5, 1.0, false);
        // ... unless all points on B within tolerance
        near_case(10, 0, -10, 0, 0, 1.5, 0, -1.5, 2.0, true);
        // Long edges with close endpoints but far interiors
        near_case(89, 1, -89, 1, 89, 2, -89, 2, 0.5, false);
        near_case(89, 1, -89, 1, 89, 2, -89, 2, 1.5, true);
        // Independent of edge directions
        near_case(89, 1, -89, 1, -89, 2, 89, 2, 1.5, true);
        // Cases at hemisphere boundary
        near_case(0, -100, 0, 100, 5, -80, -5, 80, 70.0, false);
        near_case(0, -100, 0, 100, 1, -35, 10, 35, 70.0, false);
        // Nearly 180-degree arcs
        near_case(0, -179.75, 0, -0.25, 0, 179.75, 0, 0.25, 1.0, false);
        // Equator test
        near_case(40, 0, -5, 0, 39, 0.975, -1, 0.975, 1.0, true);
        // Reversed orientation
        near_case(10, 0, -10, 0, -0.4, 0.975, 0.4, 0.975, 1.0, true);
        // Same great circle, partial overlap
        near_case(0, 0, 1, 0, 0.9, 0, 1.1, 0, 0.25, true);
        // Same great circle, close to endpoint
        near_case(0, 0, 1, 0, 1.1, 0, 1.2, 0, 0.25, true);
        // Reversed B
        near_case(0, 0, 1, 0, 1.2, 0, 1.1, 0, 0.25, true);

        out["is_edge_b_near_edge_a"] = cases;
    }

    // TEST(S2, GetPointToLeft/Right)
    {
        S2Point a = S2LatLng::FromDegrees(0, 0).ToPoint();
        S2Point b = S2LatLng::FromDegrees(0, 5).ToPoint();
        // Use 10 meters as distance
        double kDistanceRadians = 10.0 / 6371010.0;
        S1Angle kDistance = S1Angle::Radians(kDistanceRadians);

        S2Point left = S2::GetPointToLeft(a, b, kDistance);
        S2Point right = S2::GetPointToRight(a, b, kDistance);
        double left_dist = S1Angle(a, left).radians();
        double right_dist = S1Angle(a, right).radians();
        double left_turn = S2::TurnAngle(left, a, b);
        double right_turn = S2::TurnAngle(right, a, b);

        out["point_to_left_right"] = {{"a", point_json(a)},
                                      {"b", point_json(b)},
                                      {"distance_radians", kDistanceRadians},
                                      {"left", point_json(left)},
                                      {"right", point_json(right)},
                                      {"left_dist", left_dist},
                                      {"right_dist", right_dist},
                                      {"left_turn_angle", left_turn},
                                      {"right_turn_angle", right_turn}};
    }

    // Project test cases
    {
        json cases = json::array();
        // Various project cases (same as distance cases, but we care about the
        // projected point)
        auto proj_case = [&](S2Point x, S2Point a, S2Point b) {
            x = x.Normalize();
            a = a.Normalize();
            b = b.Normalize();
            S2Point p = S2::Project(x, a, b);
            double frac = S2::GetDistanceFraction(x, a, b);
            return json{{"x", point_json(x)},
                        {"a", point_json(a)},
                        {"b", point_json(b)},
                        {"projected", point_json(p)},
                        {"fraction", frac}};
        };
        cases.push_back(
            proj_case(S2Point(1, 1, 1), S2Point(1, 0, 0), S2Point(0, 1, 0)));
        cases.push_back(
            proj_case(S2Point(0, 0, 1), S2Point(1, 0, 0), S2Point(0, 1, 0)));
        cases.push_back(
            proj_case(S2Point(1, 0, 0), S2Point(1, 0, 0), S2Point(0, 1, 0)));
        cases.push_back(
            proj_case(S2Point(0, 1, 0), S2Point(1, 0, 0), S2Point(0, 1, 0)));
        out["project"] = cases;
    }

    // GetPointOnLine test cases
    {
        json cases = json::array();
        // Along the equator
        S2Point a = S2LatLng::FromDegrees(0, 0).ToPoint();
        S2Point b = S2LatLng::FromDegrees(0, 90).ToPoint();
        for (double deg = 0; deg <= 90; deg += 15) {
            S1Angle r = S1Angle::Degrees(deg);
            S2Point p = S2::GetPointOnLine(a, b, r);
            cases.push_back({{"a", point_json(a)},
                             {"b", point_json(b)},
                             {"distance_radians", r.radians()},
                             {"result", point_json(p)}});
        }
        out["get_point_on_line"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
