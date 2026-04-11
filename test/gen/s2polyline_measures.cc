// Golden data generator for S2PolylineMeasures.
//
// Mirrors s2geometry/src/s2/s2polyline_measures_test.cc.

#include "s2/s2polyline_measures.h"

#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>
#include <vector>

#include "s2/s1angle.h"
#include "s2/s2point.h"

using json = nlohmann::json;

json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

json polyline_json(const std::vector<S2Point> &line) {
    json arr = json::array();
    for (const auto &p : line)
        arr.push_back(point_json(p));
    return arr;
}

json polyline_case(const std::string &label, const std::vector<S2Point> &line) {
    json tc = json::object();
    tc["label"] = label;
    tc["polyline"] = polyline_json(line);
    tc["length_radians"] = S2::GetLength(line).radians();
    S2Point centroid = S2::GetCentroid(line);
    tc["centroid"] = point_json(centroid);
    tc["centroid_norm"] = centroid.Norm();
    return tc;
}

int main() {
    json out;

    // TEST(GetLengthAndCentroid, GreatCircles) - deterministic analog.
    // The original uses random frames; we substitute a set of deterministic
    // cases that exercise length accumulation and centroid summing.
    {
        json cases = json::array();

        // Empty polyline.
        cases.push_back(polyline_case("empty", {}));

        // Single point (degenerate, length 0, centroid 0).
        cases.push_back(polyline_case("single", {S2Point(1, 0, 0)}));

        // Two identical points (degenerate, length 0, centroid 0).
        {
            S2Point a(1, 0, 0);
            cases.push_back(polyline_case("two_same", {a, a}));
        }

        // Two antipodal points (centroid is zero vector by construction).
        cases.push_back(
            polyline_case("antipodal", {S2Point(1, 0, 0), S2Point(-1, 0, 0)}));

        // Quarter great-circle: (1,0,0) -> (0,1,0), length = pi/2.
        cases.push_back(polyline_case("quarter_equator",
                                      {S2Point(1, 0, 0), S2Point(0, 1, 0)}));

        // Half great circle split into two quarters.
        cases.push_back(polyline_case(
            "half_equator_two_segments",
            {S2Point(1, 0, 0), S2Point(0, 1, 0), S2Point(-1, 0, 0)}));

        // Full great circle split into four quarters; length = 2*pi, centroid
        // norm is ~0 (vanishes because it is a closed great circle).
        cases.push_back(polyline_case(
            "full_equator_four_segments",
            {S2Point(1, 0, 0), S2Point(0, 1, 0), S2Point(-1, 0, 0),
             S2Point(0, -1, 0), S2Point(1, 0, 0)}));

        // Short edge near (1,0,0); length ~1e-8.
        {
            S2Point a(1, 0, 0);
            S2Point b = (a + S2Point(0, 1e-8, 0)).Normalize();
            cases.push_back(polyline_case("short_edge", {a, b}));
        }

        // L-shaped polyline going from equator to north pole.
        {
            S2Point p0(1, 0, 0);
            S2Point p1(0, 1, 0);
            S2Point p2(0, 0, 1);
            cases.push_back(polyline_case("octant_corner", {p0, p1, p2}));
        }

        // Great-circle case exactly like the original random test, but with a
        // deterministic frame. Construct a great circle in the x/y plane with
        // 16 evenly-spaced vertices and close the loop. Expected length is
        // 2*pi; expected centroid is the zero vector.
        {
            std::vector<S2Point> line;
            const int n = 16;
            for (int i = 0; i < n; ++i) {
                double theta = 2 * M_PI * i / n;
                line.push_back(S2Point(std::cos(theta), std::sin(theta), 0));
            }
            line.push_back(line[0]);
            cases.push_back(polyline_case("great_circle_xy", line));
        }

        // Great-circle case in a tilted frame to exercise non-axis-aligned
        // inputs.
        {
            // Orthonormal frame: x' = (1,1,0)/sqrt(2), y' = (0,0,1),
            // z' = (1,-1,0)/sqrt(2).
            S2Point xp = S2Point(1, 1, 0).Normalize();
            S2Point yp(0, 0, 1);
            std::vector<S2Point> line;
            const int n = 32;
            for (int i = 0; i < n; ++i) {
                double theta = 2 * M_PI * i / n;
                line.push_back(std::cos(theta) * xp + std::sin(theta) * yp);
            }
            line.push_back(line[0]);
            cases.push_back(polyline_case("great_circle_tilted", line));
        }

        out["polylines"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
