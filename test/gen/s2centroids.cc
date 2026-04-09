// Golden data generator for S2Centroids.

#include "s2/s2centroids.h"

#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/s2point.h"
#include "s2/s2pointutil.h"

using json = nlohmann::json;

json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

int main() {
    json out;

    // TEST(PlanarCentroid, SemiEquator)
    {
        json c = json::object();
        S2Point a(0, -1, 0), b(1, 0, 0), cv(0, 1, 0);
        S2Point centroid = S2::PlanarCentroid(a, b, cv);
        S2Point normalized = centroid.Normalize();
        c["a"] = point_json(a);
        c["b"] = point_json(b);
        c["c"] = point_json(cv);
        c["centroid"] = point_json(centroid);
        c["centroid_normalized"] = point_json(normalized);
        c["centroid_norm"] = centroid.Norm();
        out["planar_centroid_semi_equator"] = c;
    }

    // TEST(TriangleTrueCentroid, SmallTriangles)
    // We generate a few deterministic small triangles and verify the true
    // centroid.
    {
        json cases = json::array();

        // Use a few hand-picked small triangles
        struct TriCase {
            S2Point a, b, c;
            const char *label;
        };

        // Small triangle near the north pole
        {
            S2Point p(0, 0, 1);
            double d = 1e-6;
            S2Point x(1, 0, 0), y(0, 1, 0);
            S2Point p0 = (p - d * x).Normalize();
            S2Point p1 = (p + d * x).Normalize();
            S2Point p2 = (p + 3 * d * y).Normalize();
            S2Point centroid = S2::TrueCentroid(p0, p1, p2);
            S2Point centroid_normalized = centroid.Normalize();
            S2Point expected = (p + d * y).Normalize();

            json tc = json::object();
            tc["label"] = "small_north_pole";
            tc["a"] = point_json(p0);
            tc["b"] = point_json(p1);
            tc["c"] = point_json(p2);
            tc["centroid"] = point_json(centroid);
            tc["centroid_normalized"] = point_json(centroid_normalized);
            tc["expected_centroid"] = point_json(expected);
            tc["angle_to_expected"] = centroid_normalized.Angle(expected);
            cases.push_back(tc);
        }

        // Right-angle triangle at equator
        {
            S2Point a(1, 0, 0), b(0, 1, 0), c(0, 0, 1);
            S2Point centroid = S2::TrueCentroid(a, b, c);
            json tc = json::object();
            tc["label"] = "octant";
            tc["a"] = point_json(a);
            tc["b"] = point_json(b);
            tc["c"] = point_json(c);
            tc["centroid"] = point_json(centroid);
            cases.push_back(tc);
        }

        // Degenerate triangle (all same point)
        {
            S2Point a(1, 0, 0);
            S2Point centroid = S2::TrueCentroid(a, a, a);
            json tc = json::object();
            tc["label"] = "degenerate_same_point";
            tc["a"] = point_json(a);
            tc["b"] = point_json(a);
            tc["c"] = point_json(a);
            tc["centroid"] = point_json(centroid);
            cases.push_back(tc);
        }

        // Degenerate triangle (collinear points)
        {
            S2Point a(1, 0, 0), b(0, 1, 0);
            S2Point mid = (a + b).Normalize();
            S2Point centroid = S2::TrueCentroid(a, mid, b);
            json tc = json::object();
            tc["label"] = "degenerate_collinear";
            tc["a"] = point_json(a);
            tc["b"] = point_json(mid);
            tc["c"] = point_json(b);
            tc["centroid"] = point_json(centroid);
            cases.push_back(tc);
        }

        out["triangle_true_centroid"] = cases;
    }

    // TEST(EdgeTrueCentroid, SemiEquator)
    {
        json c = json::object();
        S2Point a(0, -1, 0), b(1, 0, 0), cv(0, 1, 0);
        S2Point centroid_ab = S2::TrueCentroid(a, b);
        S2Point centroid_bc = S2::TrueCentroid(b, cv);
        S2Point centroid = centroid_ab + centroid_bc;
        S2Point normalized = centroid.Normalize();
        c["a"] = point_json(a);
        c["b"] = point_json(b);
        c["c"] = point_json(cv);
        c["centroid_ab"] = point_json(centroid_ab);
        c["centroid_bc"] = point_json(centroid_bc);
        c["centroid_sum"] = point_json(centroid);
        c["centroid_sum_normalized"] = point_json(normalized);
        c["centroid_sum_norm"] = centroid.Norm();
        out["edge_true_centroid_semi_equator"] = c;
    }

    // Additional edge true centroid cases
    {
        json cases = json::array();

        // Same point (degenerate)
        {
            S2Point a(1, 0, 0);
            S2Point centroid = S2::TrueCentroid(a, a);
            json tc = json::object();
            tc["label"] = "same_point";
            tc["a"] = point_json(a);
            tc["b"] = point_json(a);
            tc["centroid"] = point_json(centroid);
            cases.push_back(tc);
        }

        // Antipodal points
        {
            S2Point a(1, 0, 0), b(-1, 0, 0);
            S2Point centroid = S2::TrueCentroid(a, b);
            json tc = json::object();
            tc["label"] = "antipodal";
            tc["a"] = point_json(a);
            tc["b"] = point_json(b);
            tc["centroid"] = point_json(centroid);
            cases.push_back(tc);
        }

        // Short edge
        {
            S2Point a(1, 0, 0);
            S2Point b = (a + S2Point(0, 1e-8, 0)).Normalize();
            S2Point centroid = S2::TrueCentroid(a, b);
            json tc = json::object();
            tc["label"] = "short_edge";
            tc["a"] = point_json(a);
            tc["b"] = point_json(b);
            tc["centroid"] = point_json(centroid);
            cases.push_back(tc);
        }

        // 90 degree edge
        {
            S2Point a(1, 0, 0), b(0, 1, 0);
            S2Point centroid = S2::TrueCentroid(a, b);
            json tc = json::object();
            tc["label"] = "ninety_degrees";
            tc["a"] = point_json(a);
            tc["b"] = point_json(b);
            tc["centroid"] = point_json(centroid);
            cases.push_back(tc);
        }

        out["edge_true_centroid_cases"] = cases;
    }

    // Planar centroid additional cases
    {
        json cases = json::array();

        // Unit triangle
        {
            S2Point a(1, 0, 0), b(0, 1, 0), c(0, 0, 1);
            S2Point centroid = S2::PlanarCentroid(a, b, c);
            json tc = json::object();
            tc["label"] = "unit_axes";
            tc["a"] = point_json(a);
            tc["b"] = point_json(b);
            tc["c"] = point_json(c);
            tc["centroid"] = point_json(centroid);
            cases.push_back(tc);
        }

        // All same point
        {
            S2Point a(1, 0, 0);
            S2Point centroid = S2::PlanarCentroid(a, a, a);
            json tc = json::object();
            tc["label"] = "same_point";
            tc["a"] = point_json(a);
            tc["b"] = point_json(a);
            tc["c"] = point_json(a);
            tc["centroid"] = point_json(centroid);
            cases.push_back(tc);
        }

        out["planar_centroid_cases"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
