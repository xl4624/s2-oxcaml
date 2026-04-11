// Golden data generator for S2LatLngRectBounder.

#include "s2/s2latlng_rect_bounder.h"

#include <cfloat>
#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/r1interval.h"
#include "s2/s1angle.h"
#include "s2/s1interval.h"
#include "s2/s2latlng.h"
#include "s2/s2latlng_rect.h"
#include "s2/s2point.h"

using json = nlohmann::json;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static json rect_json(const S2LatLngRect &r) {
    return {{"lat", {r.lat().lo(), r.lat().hi()}},
            {"lng", {r.lng().lo(), r.lng().hi()}}};
}

static S2LatLngRect GetEdgeBound(const S2Point &a, const S2Point &b) {
    S2LatLngRectBounder bounder;
    bounder.AddPoint(a);
    bounder.AddPoint(b);
    return bounder.GetBound();
}

static S2LatLngRect GetEdgeBoundXYZ(double x1, double y1, double z1, double x2,
                                    double y2, double z2) {
    return GetEdgeBound(S2Point(x1, y1, z1).Normalize(),
                        S2Point(x2, y2, z2).Normalize());
}

static json edge_entry(const char *label, double x1, double y1, double z1,
                       double x2, double y2, double z2) {
    S2Point a = S2Point(x1, y1, z1).Normalize();
    S2Point b = S2Point(x2, y2, z2).Normalize();
    S2LatLngRect bound = GetEdgeBound(a, b);
    return {{"label", label},
            {"a", point_json(a)},
            {"b", point_json(b)},
            {"bound", rect_json(bound)}};
}

static S2LatLngRect GetSubregionBound(double x_lat, double x_lng, double y_lat,
                                      double y_lng) {
    S2LatLngRect in =
        S2LatLngRect::FromPointPair(S2LatLng::FromRadians(x_lat, x_lng),
                                    S2LatLng::FromRadians(y_lat, y_lng));
    return S2LatLngRectBounder::ExpandForSubregions(in);
}

int main() {
    json out;

    // S2LatLngRectBounder::MaxErrorForTests()
    {
        S2LatLng err = S2LatLngRectBounder::MaxErrorForTests();
        out["max_error_for_tests"] = {{"lat_rad", err.lat().radians()},
                                      {"lng_rad", err.lng().radians()}};
    }

    // TEST(RectBounder, MaxLatitudeSimple) - vertex cases
    {
        json cases = json::array();
        const double kCubeLat = asin(1.0 / sqrt(3.0));

        // Both (1,1,1)->(1,-1,-1) and (1,-1,1)->(1,1,-1) produce the same
        // bound.
        cases.push_back(edge_entry("cube_111_1m1m1", 1, 1, 1, 1, -1, -1));
        cases.push_back(edge_entry("cube_1m11_11m1", 1, -1, 1, 1, 1, -1));

        // Expected lat cube value for both.
        out["max_latitude_vertex_cube_lat"] = kCubeLat;
        out["max_latitude_vertex_cases"] = cases;
    }

    // TEST(RectBounder, MaxLatitudeSimple) - edge interior CW/CCW
    {
        auto push_interior = [&](json &arr, const char *label, double x1,
                                 double y1, double z1, double x2, double y2,
                                 double z2) {
            S2Point a = S2Point(x1, y1, z1).Normalize();
            S2Point b = S2Point(x2, y2, z2).Normalize();
            S2LatLngRect r = GetEdgeBound(a, b);
            arr.push_back({{"label", label},
                           {"a", point_json(a)},
                           {"b", point_json(b)},
                           {"bound", rect_json(r)},
                           {"lat_hi", r.lat().hi()},
                           {"lat_lo", r.lat().lo()}});
        };
        json cases = json::array();
        // Max latitude, CW edge (1,1,1)->(1,-1,1)
        push_interior(cases, "max_lat_cw", 1, 1, 1, 1, -1, 1);
        // Max latitude, CCW edge (1,-1,1)->(1,1,1)
        push_interior(cases, "max_lat_ccw", 1, -1, 1, 1, 1, 1);
        // Min latitude, CW edge
        push_interior(cases, "min_lat_cw", 1, -1, -1, -1, -1, -1);
        // Min latitude, CCW edge
        push_interior(cases, "min_lat_ccw", -1, 1, -1, -1, -1, -1);
        out["max_latitude_interior_cases"] = cases;
    }

    // TEST(RectBounder, MaxLatitudeSimple) - polar edges
    {
        json polar = json::object();
        S2LatLngRect north = GetEdgeBoundXYZ(0.3, 0.4, 1, -0.3, -0.4, 1);
        S2LatLngRect south = GetEdgeBoundXYZ(0.3, 0.4, -1, -0.3, -0.4, -1);
        polar["north_bound"] = rect_json(north);
        polar["north_lat_hi"] = north.lat().hi();
        polar["south_bound"] = rect_json(south);
        polar["south_lat_lo"] = south.lat().lo();
        out["polar_edges"] = polar;
    }

    // Multi-step accumulation: a chain of edges with final bound.
    {
        // A chain approximating a small loop near the prime meridian.
        std::vector<S2Point> pts = {
            S2LatLng::FromDegrees(10, -10).ToPoint(),
            S2LatLng::FromDegrees(20, -5).ToPoint(),
            S2LatLng::FromDegrees(30, 5).ToPoint(),
            S2LatLng::FromDegrees(20, 15).ToPoint(),
            S2LatLng::FromDegrees(10, 10).ToPoint(),
        };

        S2LatLngRectBounder bounder;
        json steps = json::array();
        for (size_t i = 0; i < pts.size(); ++i) {
            bounder.AddPoint(pts[i]);
            steps.push_back(
                {{"after", (int)i}, {"bound", rect_json(bounder.GetBound())}});
        }
        json chain = json::object();
        json pts_json = json::array();
        for (const auto &p : pts)
            pts_json.push_back(point_json(p));
        chain["points"] = pts_json;
        chain["steps"] = steps;
        chain["final"] = rect_json(bounder.GetBound());
        out["chain_accumulation"] = chain;
    }

    // Single point: empty + one add.
    {
        S2LatLngRectBounder bounder;
        S2Point p = S2LatLng::FromDegrees(42, -71).ToPoint();
        json before = rect_json(bounder.GetBound());
        bounder.AddPoint(p);
        json after = rect_json(bounder.GetBound());
        out["single_point"] = {
            {"bound_before", before},
            {"point", point_json(p)},
            {"bound_after", after},
            {"is_empty_before",
             S2LatLngRect(R1Interval::Empty(), S1Interval::Empty())
                 .is_empty()}};
    }

    // AddLatLng path: should equal AddPoint path for normalized inputs.
    {
        S2LatLng a = S2LatLng::FromDegrees(10, 20);
        S2LatLng b = S2LatLng::FromDegrees(30, 40);

        S2LatLngRectBounder via_point;
        via_point.AddPoint(a.ToPoint());
        via_point.AddPoint(b.ToPoint());

        S2LatLngRectBounder via_latlng;
        via_latlng.AddLatLng(a);
        via_latlng.AddLatLng(b);

        out["add_latlng_vs_point"] = {
            {"via_point", rect_json(via_point.GetBound())},
            {"via_latlng", rect_json(via_latlng.GetBound())}};
    }

    // TEST(RectBounder, NearlyIdenticalOrAntipodalPoints) - one case with
    // exactly identical points and one with antipodal.
    {
        json identical = json::object();
        S2Point a = S2LatLng::FromDegrees(45, 33).ToPoint();
        S2LatLngRect r_identical = GetEdgeBound(a, a);
        identical["a"] = point_json(a);
        identical["bound"] = rect_json(r_identical);

        S2LatLngRect r_antipodal = GetEdgeBound(a, -a);
        identical["antipodal_bound"] = rect_json(r_antipodal);
        identical["antipodal_is_full"] = r_antipodal.is_full();

        out["identical_or_antipodal"] = identical;
    }

    // TEST(RectBounder, ExpandForSubregions)
    {
        json es = json::array();
        auto emit = [&](const char *label, double xl, double xn, double yl,
                        double yn) {
            S2LatLngRect in = S2LatLngRect::FromPointPair(
                S2LatLng::FromRadians(xl, xn), S2LatLng::FromRadians(yl, yn));
            S2LatLngRect out2 = S2LatLngRectBounder::ExpandForSubregions(in);
            es.push_back({{"label", label},
                          {"in", rect_json(in)},
                          {"out", rect_json(out2)},
                          {"is_full", out2.is_full()}});
        };

        emit("near_antip_1_full", 3e-16, 0, 1e-14, M_PI);
        emit("near_antip_1_notfull", 9e-16, 0, 1e-14, M_PI);
        emit("near_antip_2_full", 1e-16, 7e-16, 1e-14, M_PI);
        emit("near_antip_2_notfull", 3e-16, 14e-16, 1e-14, M_PI);
        emit("near_antip_3_full", 1e-100, 14e-16, 1e-14, M_PI);
        emit("near_antip_3_notfull", 1e-100, 22e-16, 1e-14, M_PI);

        emit("pi2_full_1", -M_PI_2, -1e-15, M_PI_2 - 7e-16, 0);
        emit("pi2_notfull_1", -M_PI_2, -1e-15, M_PI_2 - 30e-16, 0);
        emit("pi2_full_2", -M_PI_2 + 4e-16, 0, M_PI_2 - 2e-16, 1e-7);
        emit("pi2_notfull_2", -M_PI_2 + 30e-16, 0, M_PI_2, 1e-7);
        emit("pi2_full_3", -M_PI_2 + 4e-16, 0, M_PI_2 - 4e-16, M_PI_2);
        emit("pi2_notfull_3", -M_PI_2, 0, M_PI_2 - 30e-16, M_PI_2);

        emit("straddle_full_1", -M_PI_2, 0, M_PI_2 - 1e-8, M_PI - 1e-7);
        emit("straddle_notfull_1", -M_PI_2, 0, M_PI_2 - 1e-7, M_PI - 1e-7);
        emit("straddle_full_2", -M_PI_2 + 1e-12, -M_PI + 1e-4, M_PI_2, 0);
        emit("straddle_full_3", -M_PI_2 + 1e-11, -M_PI + 1e-4, M_PI_2, 0);

        // Non-antipodal, but spanning ~180 degrees longitude.
        emit("wide_lng_1", 1.5, -M_PI_2, 1.5, M_PI_2 - 2e-16);
        emit("wide_lng_2", 1.5, -M_PI_2, 1.5, M_PI_2 - 7e-16);

        // Pole expansion cases.
        emit("near_south_pole", -M_PI_2 + 1e-15, 0, -M_PI_2 + 1e-15, 0);
        emit("near_north_pole", M_PI_2 - 1e-15, 0, M_PI_2 - 1e-15, 0);

        out["expand_for_subregions"] = es;
    }

    // ExpandForSubregions full and empty bounds.
    {
        S2LatLngRect full_out =
            S2LatLngRectBounder::ExpandForSubregions(S2LatLngRect::Full());
        S2LatLngRect empty_out =
            S2LatLngRectBounder::ExpandForSubregions(S2LatLngRect::Empty());
        out["expand_full_empty"] = {
            {"full_in_full_out", full_out.is_full()},
            {"empty_in_empty_out", empty_out.is_empty()}};
    }

    // TEST(RectBounder, AccuracyBug)
    {
        S2Point a(-0.99999999999998446, -1.2247195409833338e-16,
                  1.756190424895897e-07);
        S2Point b(7.9020571389665525e-08, -6.6407120842906012e-10,
                  0.99999999999999689);
        S2Point c(0.9999999999999768, -1.2246467991472876e-16,
                  2.1496584824676253e-07);

        S2LatLngRect ac = GetEdgeBound(a, c);
        S2LatLngRect ab = GetEdgeBound(a, b);
        S2LatLngRect bc = GetEdgeBound(b, c);
        S2LatLngRect ac_expanded = S2LatLngRectBounder::ExpandForSubregions(ac);

        out["accuracy_bug"] = {
            {"a", point_json(a)},
            {"b", point_json(b)},
            {"c", point_json(c)},
            {"ac", rect_json(ac)},
            {"ab", rect_json(ab)},
            {"bc", rect_json(bc)},
            {"ac_expanded", rect_json(ac_expanded)},
            {"ac_expanded_ge_ab_hi", ac_expanded.lat().hi() >= ab.lat().hi()},
            {"ac_expanded_ge_ac_hi", ac_expanded.lat().hi() >= ac.lat().hi()}};
    }

    // Suppress unused warning from the helper when compiling on different
    // configurations.
    (void)&GetSubregionBound;

    std::cout << out.dump(2) << std::endl;
    return 0;
}
