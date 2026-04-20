// Golden data generator for S2LatLngRect.

#include "s2/s2latlng_rect.h"

#include <cfloat>
#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/r1interval.h"
#include "s2/s1angle.h"
#include "s2/s1interval.h"
#include "s2/s2cap.h"
#include "s2/s2latlng.h"
#include "s2/s2point.h"

using json = nlohmann::json;

static S2LatLngRect RectFromDegrees(double lat_lo, double lng_lo, double lat_hi,
                                    double lng_hi) {
    return S2LatLngRect(S2LatLng::FromDegrees(lat_lo, lng_lo).Normalized(),
                        S2LatLng::FromDegrees(lat_hi, lng_hi).Normalized());
}

json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

json latlng_json(const S2LatLng &ll) {
    return {ll.lat().radians(), ll.lng().radians()};
}

json rect_json(const S2LatLngRect &r) {
    return {{"lat", {r.lat().lo(), r.lat().hi()}},
            {"lng", {r.lng().lo(), r.lng().hi()}}};
}

int main() {
    json out;

    // TEST(S2LatLngRect, EmptyAndFull)
    {
        json e = json::object();
        S2LatLngRect empty = S2LatLngRect::Empty();
        S2LatLngRect full = S2LatLngRect::Full();
        e["empty"] = rect_json(empty);
        e["full"] = rect_json(full);
        e["empty_valid"] = empty.is_valid();
        e["empty_is_empty"] = empty.is_empty();
        e["empty_is_point"] = empty.is_point();
        e["full_valid"] = full.is_valid();
        e["full_is_full"] = full.is_full();
        e["full_is_point"] = full.is_point();
        out["empty_and_full"] = e;
    }

    // TEST(S2LatLngRect, Accessors)
    {
        json a = json::object();
        S2LatLngRect d1 = RectFromDegrees(-90, 0, -45, 180);
        a["d1"] = rect_json(d1);
        a["d1_lat_lo_deg"] = d1.lat_lo().degrees();
        a["d1_lat_hi_deg"] = d1.lat_hi().degrees();
        a["d1_lng_lo_deg"] = d1.lng_lo().degrees();
        a["d1_lng_hi_deg"] = d1.lng_hi().degrees();
        out["accessors"] = a;
    }

    // TEST(S2LatLngRect, FromCenterSize)
    {
        json cs = json::array();

        // center=(80,170), size=(40,60) -> lat=[60,90], lng=[140,-160]
        {
            S2LatLngRect r = S2LatLngRect::FromCenterSize(
                S2LatLng::FromDegrees(80, 170), S2LatLng::FromDegrees(40, 60));
            S2LatLngRect expected = RectFromDegrees(60, 140, 90, -160);
            cs.push_back({{"result", rect_json(r)},
                          {"expected", rect_json(expected)},
                          {"approx_equals", r.ApproxEquals(expected)}});
        }

        // center=(10,40), size=(210,400) -> full
        {
            S2LatLngRect r = S2LatLngRect::FromCenterSize(
                S2LatLng::FromDegrees(10, 40), S2LatLng::FromDegrees(210, 400));
            cs.push_back({{"result", rect_json(r)}, {"is_full", r.is_full()}});
        }

        // center=(-90,180), size=(20,50) -> lat=[-90,-80], lng=[155,-155]
        {
            S2LatLngRect r = S2LatLngRect::FromCenterSize(
                S2LatLng::FromDegrees(-90, 180), S2LatLng::FromDegrees(20, 50));
            S2LatLngRect expected = RectFromDegrees(-90, 155, -80, -155);
            cs.push_back({{"result", rect_json(r)},
                          {"expected", rect_json(expected)},
                          {"approx_equals", r.ApproxEquals(expected)}});
        }

        out["from_center_size"] = cs;
    }

    // TEST(S2LatLngRect, FromPoint)
    {
        json fp = json::object();
        S2LatLng p = S2LatLng::FromDegrees(23, 47);
        S2LatLngRect r = S2LatLngRect::FromPoint(p);
        fp["p"] = latlng_json(p);
        fp["result"] = rect_json(r);
        fp["is_point"] = r.is_point();
        out["from_point"] = fp;
    }

    // TEST(S2LatLngRect, FromPointPair)
    {
        json fpp = json::array();
        {
            S2LatLngRect r =
                S2LatLngRect::FromPointPair(S2LatLng::FromDegrees(-35, -140),
                                            S2LatLng::FromDegrees(15, 155));
            S2LatLngRect expected = RectFromDegrees(-35, 155, 15, -140);
            fpp.push_back({{"result", rect_json(r)},
                           {"expected", rect_json(expected)},
                           {"equal", r == expected}});
        }
        {
            S2LatLngRect r = S2LatLngRect::FromPointPair(
                S2LatLng::FromDegrees(25, -70), S2LatLng::FromDegrees(-90, 80));
            S2LatLngRect expected = RectFromDegrees(-90, -70, 25, 80);
            fpp.push_back({{"result", rect_json(r)},
                           {"expected", rect_json(expected)},
                           {"equal", r == expected}});
        }
        out["from_point_pair"] = fpp;
    }

    // TEST(S2LatLngRect, GetCenterSize)
    {
        json gs = json::object();
        S2LatLngRect r1(R1Interval(0, M_PI_2), S1Interval(-M_PI, 0));
        gs["r1"] = rect_json(r1);
        gs["center"] = latlng_json(r1.GetCenter());
        gs["size"] = latlng_json(r1.GetSize());

        S2LatLng empty_size = S2LatLngRect::Empty().GetSize();
        gs["empty_size_lat_rad"] = empty_size.lat().radians();
        gs["empty_size_lng_rad"] = empty_size.lng().radians();
        out["get_center_size"] = gs;
    }

    // TEST(S2LatLngRect, GetVertex)
    {
        json gv = json::object();
        S2LatLngRect r1(R1Interval(0, M_PI_2), S1Interval(-M_PI, 0));
        gv["r1"] = rect_json(r1);
        for (int k = 0; k < 4; ++k) {
            gv["v" + std::to_string(k)] = latlng_json(r1.GetVertex(k));
        }
        out["get_vertex"] = gv;
    }

    // TEST(S2LatLngRect, Contains)
    {
        json c = json::object();
        S2LatLng eq_m180 = S2LatLng::FromRadians(0, -M_PI);
        S2LatLng north_pole = S2LatLng::FromRadians(M_PI_2, 0);
        S2LatLngRect r1(eq_m180, north_pole);
        c["r1"] = rect_json(r1);

        c["contains_30_m45"] = r1.Contains(S2LatLng::FromDegrees(30, -45));
        c["interior_contains_30_m45"] =
            r1.InteriorContains(S2LatLng::FromDegrees(30, -45));
        c["not_contains_30_45"] = !r1.Contains(S2LatLng::FromDegrees(30, 45));
        c["not_interior_30_45"] =
            !r1.InteriorContains(S2LatLng::FromDegrees(30, 45));
        c["contains_eq_m180"] = r1.Contains(eq_m180);
        c["not_interior_eq_m180"] = !r1.InteriorContains(eq_m180);
        c["contains_north_pole"] = r1.Contains(north_pole);
        c["not_interior_north_pole"] = !r1.InteriorContains(north_pole);
        c["contains_point_in"] = r1.Contains(S2Point(0.5, -0.3, 0.1));
        c["not_contains_point_out"] = !r1.Contains(S2Point(0.5, 0.2, 0.1));
        out["contains"] = c;
    }

    // TEST(S2LatLngRect, IntervalOps)
    {
        json ops = json::array();
        // A helper to emit one test case.
        auto emit = [&](const S2LatLngRect &x, const S2LatLngRect &y,
                        const std::string &label) {
            json item;
            item["label"] = label;
            item["x"] = rect_json(x);
            item["y"] = rect_json(y);
            item["contains"] = x.Contains(y);
            item["interior_contains"] = x.InteriorContains(y);
            item["intersects"] = x.Intersects(y);
            item["interior_intersects"] = x.InteriorIntersects(y);
            item["union"] = rect_json(x.Union(y));
            item["intersection"] = rect_json(x.Intersection(y));
            ops.push_back(item);
        };

        S2LatLngRect r1 = RectFromDegrees(0, -180, 90, 0);
        emit(r1, RectFromDegrees(45, -90, 45, -90), "r1_mid");
        emit(r1, RectFromDegrees(0, -180, 0, -180), "r1_eq_m180");
        emit(r1, RectFromDegrees(90, 0, 90, 0), "r1_north_pole");
        emit(r1, RectFromDegrees(-10, -1, 1, 20), "r1_overlap1");
        emit(r1, RectFromDegrees(-10, -1, 0, 20), "r1_overlap2");
        emit(r1, RectFromDegrees(-10, 0, 1, 20), "r1_overlap3");
        emit(RectFromDegrees(-15, -160, -15, -150),
             RectFromDegrees(20, 145, 25, 155), "disjoint1");
        emit(RectFromDegrees(70, -10, 90, -140),
             RectFromDegrees(60, 175, 80, 5), "overlap_wrap");
        emit(RectFromDegrees(12, 30, 60, 60), RectFromDegrees(0, 0, 30, 18),
             "lat_overlap_no_lng");
        emit(RectFromDegrees(0, 0, 18, 42), RectFromDegrees(30, 12, 42, 60),
             "lng_overlap_no_lat");
        out["interval_ops"] = ops;
    }

    // TEST(S2LatLngRect, AddPoint)
    {
        json ap = json::object();
        S2LatLngRect p = S2LatLngRect::Empty();
        p.AddPoint(S2LatLng::FromDegrees(0, 0));
        ap["after_first"] = rect_json(p);
        ap["after_first_is_point"] = p.is_point();
        p.AddPoint(S2LatLng::FromRadians(0, -M_PI_2));
        ap["after_second"] = rect_json(p);
        ap["after_second_is_point"] = p.is_point();
        p.AddPoint(S2LatLng::FromRadians(M_PI_4, -M_PI));
        p.AddPoint(S2LatLng(S2Point(0, 0, 1)));
        ap["final"] = rect_json(p);
        S2LatLngRect expected = RectFromDegrees(0, -180, 90, 0);
        ap["expected"] = rect_json(expected);
        ap["final_equals_expected"] = (p == expected);
        out["add_point"] = ap;
    }

    // TEST(S2LatLngRect, Expanded)
    {
        json ex = json::array();
        auto emit_expanded = [&](const S2LatLngRect &r, double lat_margin,
                                 double lng_margin, const std::string &label) {
            S2LatLngRect result =
                r.Expanded(S2LatLng::FromDegrees(lat_margin, lng_margin));
            json item;
            item["label"] = label;
            item["input"] = rect_json(r);
            item["lat_margin_deg"] = lat_margin;
            item["lng_margin_deg"] = lng_margin;
            item["result"] = rect_json(result);
            item["is_empty"] = result.is_empty();
            item["is_full"] = result.is_full();
            ex.push_back(item);
        };

        emit_expanded(RectFromDegrees(70, 150, 80, 170), 20, 30, "basic");
        emit_expanded(S2LatLngRect::Empty(), 20, 30, "empty_expand");
        emit_expanded(S2LatLngRect::Full(), 500, 500, "full_expand");
        emit_expanded(RectFromDegrees(-90, 170, 10, 20), 30, 80, "pole_expand");

        // Negative margins.
        emit_expanded(RectFromDegrees(10, -50, 60, 70), -10, -10, "neg_basic");
        emit_expanded(RectFromDegrees(-20, -180, 20, 180), -10, -10,
                      "neg_full_lng");
        emit_expanded(RectFromDegrees(-20, -180, 20, 180), -30, -30,
                      "neg_to_empty");
        emit_expanded(RectFromDegrees(-90, 10, 90, 11), -10, -10, "neg_thin");
        emit_expanded(RectFromDegrees(-90, 10, 90, 100), -10, -10,
                      "neg_full_lat");

        // Mixed margins.
        emit_expanded(RectFromDegrees(10, -50, 60, 70), -10, 30, "mixed1");
        emit_expanded(RectFromDegrees(-20, -180, 20, 180), 10, -500, "mixed2");

        out["expanded"] = ex;
    }

    // TEST(S2LatLngRect, PolarClosure)
    {
        json pc = json::array();
        auto emit_polar = [&](const S2LatLngRect &r, const std::string &label) {
            json item;
            item["label"] = label;
            item["input"] = rect_json(r);
            item["result"] = rect_json(r.PolarClosure());
            pc.push_back(item);
        };
        emit_polar(RectFromDegrees(-89, 0, 89, 1), "no_pole");
        emit_polar(RectFromDegrees(-90, -30, -45, 100), "south_pole");
        emit_polar(RectFromDegrees(89, 145, 90, 146), "north_pole");
        emit_polar(RectFromDegrees(-90, -145, 90, -144), "both_poles");
        out["polar_closure"] = pc;
    }

    // TEST(S2LatLngRect, Area)
    {
        json ar = json::object();
        ar["empty_area"] = S2LatLngRect::Empty().Area();
        ar["full_area"] = S2LatLngRect::Full().Area();
        ar["quarter_area"] = RectFromDegrees(0, 0, 90, 90).Area();
        out["area"] = ar;
    }

    // TEST(S2LatLngRect, GetCentroid)
    {
        json ct = json::object();
        ct["empty"] = point_json(S2LatLngRect::Empty().GetCentroid());
        ct["full"] = point_json(S2LatLngRect::Full().GetCentroid());

        // A specific rectangle for centroid checking.
        S2LatLngRect r = RectFromDegrees(0, 0, 90, 90);
        ct["quarter"] = point_json(r.GetCentroid());

        // Full lat range rectangle.
        S2LatLngRect full_lat(S2LatLngRect::FullLat(), S1Interval(-1.0, 1.0));
        S2Point c1 = full_lat.GetCentroid();
        ct["full_lat_centroid"] = point_json(c1);
        ct["full_lat_lng_center"] = full_lat.lng().GetCenter();

        out["centroid"] = ct;
    }

    // TEST(S2LatLngRect, ApproxEquals)
    {
        json ae = json::array();
        auto emit_ae = [&](const S2LatLngRect &a, const S2LatLngRect &b,
                           bool expected, const std::string &label) {
            json item;
            item["label"] = label;
            item["a"] = rect_json(a);
            item["b"] = rect_json(b);
            item["expected"] = expected;
            ae.push_back(item);
        };
        emit_ae(S2LatLngRect::Empty(), RectFromDegrees(1, 5, 1, 5), true,
                "empty_vs_point");
        emit_ae(RectFromDegrees(1, 5, 1, 5), S2LatLngRect::Empty(), true,
                "point_vs_empty");
        emit_ae(RectFromDegrees(1, 5, 1, 5), RectFromDegrees(2, 7, 2, 7), false,
                "point_vs_diff_point");
        out["approx_equals"] = ae;
    }

    // TEST(S2LatLngRect, ApproxEquals) with max_error
    {
        json ae2 = json::array();
        auto emit_scalar = [&](const S2LatLngRect &a, const S2LatLngRect &b,
                               double margin_deg, const std::string &label) {
            bool result = a.ApproxEquals(b, S1Angle::Degrees(margin_deg));
            ae2.push_back({{"label", label},
                           {"variant", "scalar"},
                           {"a", rect_json(a)},
                           {"b", rect_json(b)},
                           {"margin_deg", margin_deg},
                           {"result", result}});
        };
        auto emit_latlng = [&](const S2LatLngRect &a, const S2LatLngRect &b,
                               double lat_margin_deg, double lng_margin_deg,
                               const std::string &label) {
            bool result = a.ApproxEquals(
                b, S2LatLng::FromDegrees(lat_margin_deg, lng_margin_deg));
            ae2.push_back({{"label", label},
                           {"variant", "latlng"},
                           {"a", rect_json(a)},
                           {"b", rect_json(b)},
                           {"lat_margin_deg", lat_margin_deg},
                           {"lng_margin_deg", lng_margin_deg},
                           {"result", result}});
        };
        emit_scalar(RectFromDegrees(10, 10, 20, 20),
                    RectFromDegrees(11, 11, 19, 19), 1.001, "within_margin");
        emit_scalar(RectFromDegrees(10, 10, 20, 20),
                    RectFromDegrees(11, 11, 19, 19), 0.999, "outside_margin");
        emit_latlng(RectFromDegrees(0, 10, 20, 30),
                    RectFromDegrees(-1, 8, 21, 32), 1.001, 2.001,
                    "latlng_within");
        emit_latlng(RectFromDegrees(0, 10, 20, 30),
                    RectFromDegrees(-1, 8, 21, 32), 0.999, 1.999,
                    "latlng_outside");
        out["approx_equals_margin"] = ae2;
    }

    // TEST(S2LatLngRect, GetCapBound)
    {
        json cb = json::array();
        auto emit_cap = [&](const S2LatLngRect &r, const std::string &label) {
            S2Cap cap = r.GetCapBound();
            json item;
            item["label"] = label;
            item["rect"] = rect_json(r);
            item["cap_center"] = point_json(cap.center());
            item["cap_height"] = cap.height();
            cb.push_back(item);
        };
        emit_cap(RectFromDegrees(-45, -45, 45, 45), "center_smaller");
        emit_cap(RectFromDegrees(88, -80, 89, 80), "pole_smaller");
        emit_cap(RectFromDegrees(-30, -150, -10, 50), "wide_lng");
        emit_cap(RectFromDegrees(-60, -150, 70, 50), "very_wide");
        out["cap_bound"] = cb;
    }

    // TEST(S2LatLngRect, DistanceToLatLng)
    {
        json dl = json::array();
        auto emit_dist = [&](const S2LatLngRect &r, double lat, double lng,
                             const std::string &label) {
            S2LatLng ll = S2LatLng::FromDegrees(lat, lng).Normalized();
            S1Angle d = r.GetDistance(ll);
            json item;
            item["label"] = label;
            item["rect"] = rect_json(r);
            item["point"] = latlng_json(ll);
            item["distance_rad"] = d.radians();
            dl.push_back(item);
        };

        S2LatLngRect a = RectFromDegrees(-1, -1, 2, 1);
        emit_dist(a, -2, -1, "south_of_a");
        emit_dist(a, 1, 2, "east_of_a");
        emit_dist(a, 0, 0, "inside_a");

        S2LatLngRect b = RectFromDegrees(86, 0, 88, 2);
        emit_dist(b, 87, 3, "near_pole1");
        emit_dist(b, 89, 1, "near_pole2");
        emit_dist(b, 90, 0, "north_pole");

        out["distance_to_latlng"] = dl;
    }

    // TEST(S2LatLngRect, GetDistanceOverlapping)
    {
        json dov = json::object();
        S2LatLngRect a = RectFromDegrees(0, 0, 2, 2);
        dov["self"] = a.GetDistance(a).radians();
        dov["overlap1"] = a.GetDistance(RectFromDegrees(0, 1, 2, 3)).radians();
        dov["overlap2"] = a.GetDistance(RectFromDegrees(0, 2, 2, 4)).radians();
        dov["overlap3"] = a.GetDistance(RectFromDegrees(1, 0, 3, 2)).radians();
        dov["overlap4"] = a.GetDistance(RectFromDegrees(2, 0, 4, 2)).radians();
        dov["overlap5"] = a.GetDistance(RectFromDegrees(1, 1, 3, 3)).radians();
        dov["overlap6"] = a.GetDistance(RectFromDegrees(2, 2, 4, 4)).radians();
        out["distance_overlapping"] = dov;
    }

    // TEST(S2LatLngRect, GetDistanceRectVsRect)
    {
        json drr = json::array();
        auto emit_rr = [&](const S2LatLngRect &a, const S2LatLngRect &b,
                           const std::string &label) {
            json item;
            item["label"] = label;
            item["a"] = rect_json(a);
            item["b"] = rect_json(b);
            item["distance_rad"] = a.GetDistance(b).radians();
            drr.push_back(item);
        };
        S2LatLngRect a = RectFromDegrees(-1, -1, 2, 1);
        emit_rr(a, RectFromDegrees(0, 2, 1, 3), "east");
        emit_rr(a, RectFromDegrees(-2, -3, -1, -2), "south_west");

        S2LatLngRect b = RectFromDegrees(-87, 0, -85, 3);
        emit_rr(b, RectFromDegrees(-89, 1, -88, 2), "south1");
        emit_rr(b, RectFromDegrees(-84, 1, -83, 2), "north1");
        emit_rr(b, RectFromDegrees(-88, 90, -86, 91), "east1");
        emit_rr(b, RectFromDegrees(-84, -91, -83, -90), "west1");
        emit_rr(b, RectFromDegrees(-90, 181, -89, 182), "far_south");
        emit_rr(b, RectFromDegrees(-84, 181, -83, 182), "far_north");
        out["distance_rect_vs_rect"] = drr;
    }

    // TEST(S2LatLngRect, GetDirectedHausdorffDistanceContained)
    {
        json hc = json::object();
        S2LatLngRect a = RectFromDegrees(-10, 20, -5, 90);
        hc["same"] =
            a.GetDirectedHausdorffDistance(RectFromDegrees(-10, 20, -5, 90))
                .radians();
        hc["bigger"] =
            a.GetDirectedHausdorffDistance(RectFromDegrees(-10, 19, -5, 91))
                .radians();
        hc["bigger2"] =
            a.GetDirectedHausdorffDistance(RectFromDegrees(-11, 20, -4, 90))
                .radians();
        hc["bigger3"] =
            a.GetDirectedHausdorffDistance(RectFromDegrees(-11, 19, -4, 91))
                .radians();
        out["hausdorff_contained"] = hc;
    }

    // TEST(S2LatLngRect, GetDirectHausdorffDistancePointToRect)
    {
        json hpr = json::array();
        auto emit_hpr = [&](const S2LatLngRect &a, const S2LatLngRect &b,
                            const std::string &label) {
            json item;
            item["label"] = label;
            item["a"] = rect_json(a);
            item["b"] = rect_json(b);
            item["hausdorff_rad"] = a.GetDirectedHausdorffDistance(b).radians();
            item["distance_rad"] = a.GetDistance(b).radians();
            hpr.push_back(item);
        };

        auto PointRect = [](double lat, double lng) {
            return S2LatLngRect::FromPoint(
                S2LatLng::FromDegrees(lat, lng).Normalized());
        };

        S2LatLngRect b1 = RectFromDegrees(-85, -50, -80, 10);
        emit_hpr(PointRect(5, 8), b1, "p1_b1");
        emit_hpr(PointRect(90, 10), b1, "p2_b1");

        S2LatLngRect b2 = RectFromDegrees(4, -10, 80, 10);
        emit_hpr(PointRect(5, 8), b2, "p1_b2");
        emit_hpr(PointRect(90, 10), b2, "p2_b2");

        S2LatLngRect b3 = RectFromDegrees(70, 170, 80, -170);
        emit_hpr(PointRect(5, 8), b3, "p1_b3");
        emit_hpr(PointRect(90, 10), b3, "p2_b3");

        out["hausdorff_point_to_rect"] = hpr;
    }

    // TEST(S2LatLngRect, GetHausdorffDistance) -- a quick case
    {
        json hd = json::object();
        S2LatLngRect a = RectFromDegrees(-10, 20, -5, 90);
        S2LatLngRect b = RectFromDegrees(-85, -50, -80, 10);
        hd["ab"] = a.GetHausdorffDistance(b).radians();
        hd["ba"] = b.GetHausdorffDistance(a).radians();
        out["hausdorff_distance"] = hd;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
