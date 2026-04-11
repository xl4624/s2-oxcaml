// Golden data generator for S2Measures.

#include "s2/s2measures.h"

#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/s2latlng.h"
#include "s2/s2point.h"

using json = nlohmann::json;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

int main() {
    json out;

    // Common points used by upstream tests.
    const S2Point pz(0, 0, 1);
    const S2Point p000(1, 0, 0);
    const S2Point p045 = S2Point(1, 1, 0).Normalize();
    const S2Point p090(0, 1, 0);
    const S2Point p180(-1, 0, 0);

    // TEST(S2, AngleMethods)
    {
        json cases = json::array();

        auto add_case = [&](const char *label, const S2Point &a,
                            const S2Point &b, const S2Point &c) {
            json tc = json::object();
            tc["label"] = label;
            tc["a"] = point_json(a);
            tc["b"] = point_json(b);
            tc["c"] = point_json(c);
            tc["angle"] = S2::Angle(a, b, c);
            tc["turn_angle"] = S2::TurnAngle(a, b, c);
            cases.push_back(tc);
        };

        add_case("p000_pz_p045", p000, pz, p045);
        add_case("p045_pz_p180", p045, pz, p180);
        add_case("p000_pz_p180", p000, pz, p180);
        add_case("pz_p000_p045", pz, p000, p045);
        // Degenerate a == c: TurnAngle magnitude is Pi.
        add_case("pz_p000_pz", pz, p000, pz);

        out["angle_methods"] = cases;
    }

    // TEST(S2, AreaMethods) - normal triangles
    {
        json cases = json::array();

        auto add_case = [&](const char *label, const S2Point &a,
                            const S2Point &b, const S2Point &c) {
            json tc = json::object();
            tc["label"] = label;
            tc["a"] = point_json(a);
            tc["b"] = point_json(b);
            tc["c"] = point_json(c);
            tc["area"] = S2::Area(a, b, c);
            tc["girard_area"] = S2::GirardArea(a, b, c);
            tc["signed_area"] = S2::SignedArea(a, b, c);
            cases.push_back(tc);
        };

        add_case("octant_p000_p090_pz", p000, p090, pz);
        add_case("three_quarter_p045_pz_p180", p045, pz, p180);

        // Collinear triangle on great circle: zero area.
        add_case("collinear_p000_p045_p090", p000, p045, p090);

        // Very skinny triangle with eps on z-axis.
        const double eps = 1e-10;
        const S2Point p045eps = S2Point(1, 1, eps).Normalize();
        add_case("skinny_p000_p045eps_p090", p000, p045eps, p090);

        // Small triangle near the north pole with relative accuracy.
        const S2Point pepsx = S2Point(eps, 0, 1).Normalize();
        const S2Point pepsy = S2Point(0, eps, 1).Normalize();
        add_case("small_north_pole_pepsx_pepsy_pz", pepsx, pepsy, pz);

        // Degenerate: all three identical.
        const S2Point pr = S2Point(0.257, -0.5723, 0.112).Normalize();
        add_case("degenerate_same_point", pr, pr, pr);

        // Degenerate: a == c, still should be zero area.
        const S2Point pq = S2Point(-0.747, 0.401, 0.2235).Normalize();
        add_case("degenerate_a_eq_c", pr, pq, pr);

        // Reverse orientation of the octant to check signed area flips sign.
        add_case("octant_reversed_pz_p090_p000", pz, p090, p000);

        out["area_methods"] = cases;
    }

    // Regression: TEST(S2, GetAreaRegression_B229644268)
    // Three near-identical points whose area should be zero.
    {
        json tc = json::object();
        const S2Point a(-1.705424004316021258e-01, -8.242696197922716461e-01,
                        5.399026611737816062e-01);
        const S2Point b(-1.706078905422188652e-01, -8.246067119418969416e-01,
                        5.393669607095969987e-01);
        const S2Point c(-1.705800600596222294e-01, -8.244634596153025408e-01,
                        5.395947061167500891e-01);
        tc["label"] = "b229644268_near_collinear";
        tc["a"] = point_json(a);
        tc["b"] = point_json(b);
        tc["c"] = point_json(c);
        tc["area"] = S2::Area(a, b, c);
        tc["girard_area"] = S2::GirardArea(a, b, c);
        out["area_regression"] = tc;
    }

    // TEST(S2, AreaMethods) - quarter sphere sum (near-180 degree edges).
    // Four triangles whose areas sum to Pi using near-180 degree edges.
    {
        json cases = json::array();

        const double eps2 = 1e-14;
        const S2Point p000eps2 = S2Point(1, 0.1 * eps2, eps2).Normalize();
        // Record each of the four triangles and their sum.
        auto add_quarter =
            [&](const char *label,
                const std::vector<std::array<S2Point, 3>> &tris) {
                json group = json::object();
                group["label"] = label;
                json triangles = json::array();
                double sum = 0.0;
                for (const auto &t : tris) {
                    json tc = json::object();
                    tc["a"] = point_json(t[0]);
                    tc["b"] = point_json(t[1]);
                    tc["c"] = point_json(t[2]);
                    double area = S2::Area(t[0], t[1], t[2]);
                    tc["area"] = area;
                    sum += area;
                    triangles.push_back(tc);
                }
                group["triangles"] = triangles;
                group["sum"] = sum;
                cases.push_back(group);
            };

        add_quarter("near_180_quarter_p000eps2", {{{p000eps2, p000, p045},
                                                   {p000eps2, p045, p180},
                                                   {p000eps2, p180, pz},
                                                   {p000eps2, pz, p000}}});

        const S2Point p045eps2 = S2Point(1, 1, eps2).Normalize();
        add_quarter("near_180_quarter_p045eps2", {{{p045eps2, p000, p045},
                                                   {p045eps2, p045, p180},
                                                   {p045eps2, p180, pz},
                                                   {p045eps2, pz, p000}}});

        out["area_quarter_sphere"] = cases;
    }

    // Zero-area case reported by the C++ test: triangle whose three
    // vertices are collinear along a meridian (lat: -45, 0, 45, lng: -170).
    {
        json tc = json::object();
        S2Point a = S2LatLng::FromDegrees(-45, -170).ToPoint();
        S2Point b = S2LatLng::FromDegrees(45, -170).ToPoint();
        S2Point c = S2LatLng::FromDegrees(0, -170).ToPoint();
        tc["label"] = "meridian_zero_area";
        tc["a"] = point_json(a);
        tc["b"] = point_json(b);
        tc["c"] = point_json(c);
        tc["area"] = S2::Area(a, b, c);
        out["area_zero_meridian"] = tc;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
