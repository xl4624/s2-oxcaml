// Golden data generator for S2Cap.

#include "s2/s2cap.h"

#include <cfloat>
#include <cmath>
#include <iostream>
#include <limits>
#include <nlohmann/json.hpp>

#include "absl/random/random.h"
#include "s2/s1angle.h"
#include "s2/s1chord_angle.h"
#include "s2/s2latlng.h"
#include "s2/s2point.h"
#include "s2/s2random.h"
#include "s2/util/coding/coder.h"

using json = nlohmann::json;

static S2Point GetLatLngPoint(double lat_degrees, double lng_degrees) {
    return S2LatLng::FromDegrees(lat_degrees, lng_degrees).ToPoint();
}

json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

json cap_roundtrip_json(const S2Cap &c) {
    return {{"center", point_json(c.center())},
            {"length2", c.radius().length2()}};
}

json rect_json(const S2LatLngRect &r) {
    return {{"lat", {r.lat().lo(), r.lat().hi()}},
            {"lng", {r.lng().lo(), r.lng().hi()}},
            {"lat_empty", r.lat().is_empty()},
            {"lng_full", r.lng().is_full()}};
}

int main() {
    json out;
    static const double kEps = 1e-15;

    // TEST(S2Cap, Basic) — selected predicates and values
    {
        json b = json::object();
        S2Cap empty = S2Cap::Empty();
        S2Cap full = S2Cap::Full();
        b["empty_valid"] = empty.is_valid();
        b["empty_is_empty"] = empty.is_empty();
        b["empty_complement_full"] = empty.Complement().is_full();
        b["full_valid"] = full.is_valid();
        b["full_is_full"] = full.is_full();
        b["full_complement_empty"] = full.Complement().is_empty();
        b["full_height"] = full.height();
        b["full_radius_deg"] = full.GetRadius().degrees();

        b["neg_angle_empty"] =
            S2Cap(S2Point(1, 0, 0), S1Angle::Radians(-20)).is_empty();
        b["large_angle_full"] =
            S2Cap(S2Point(1, 0, 0), S1Angle::Radians(5)).is_full();
        b["infinity_angle_full"] =
            S2Cap(S2Point(1, 0, 0), S1Angle::Infinity()).is_full();

        S2Cap default_empty;
        b["default_equals_empty_center"] =
            (default_empty.center() == empty.center());
        b["default_equals_empty_height"] =
            (default_empty.height() == empty.height());

        b["empty_contains_empty"] = empty.Contains(empty);
        b["full_contains_empty"] = full.Contains(empty);
        b["full_contains_full"] = full.Contains(full);
        b["empty_interior_intersects_empty"] = empty.InteriorIntersects(empty);
        b["full_interior_intersects_full"] = full.InteriorIntersects(full);
        b["full_interior_intersects_empty"] = full.InteriorIntersects(empty);

        S2Cap xaxis = S2Cap::FromPoint(S2Point(1, 0, 0));
        S2Cap yaxis = S2Cap::FromPoint(S2Point(0, 1, 0));
        b["xaxis_contains_axis"] = xaxis.Contains(S2Point(1, 0, 0));
        {
            S2Point xaxis_near = S2Point(1, 1e-20, 0).Normalize();
            b["xaxis_near_point"] = point_json(xaxis_near);
            b["xaxis_not_contains_near"] = xaxis.Contains(xaxis_near);
        }
        b["xaxis_radius_rad"] = xaxis.GetRadius().radians();
        b["yaxis_not_contains_xcenter"] = yaxis.Contains(xaxis.center());
        b["xaxis_height"] = xaxis.height();

        S2Cap xcomp = xaxis.Complement();
        b["xcomp_full"] = xcomp.is_full();
        b["xcomp_contains_center"] = xcomp.Contains(xaxis.center());
        b["xcompcomp_empty"] = xcomp.Complement().is_empty();
        b["xcompcomp_not_contains"] =
            !xcomp.Complement().Contains(xaxis.center());

        static const double kTinyRad = 1e-10;
        S2Cap tiny(S2Point(1, 2, 3).Normalize(), S1Angle::Radians(kTinyRad));
        Vector3_d tangent =
            tiny.center().CrossProd(S2Point(3, 2, 1)).Normalize();
        b["tiny_contains_inner"] = tiny.Contains(
            (tiny.center() + 0.99 * kTinyRad * tangent).Normalize());
        b["tiny_not_contains_outer"] = !tiny.Contains(
            (tiny.center() + 1.01 * kTinyRad * tangent).Normalize());

        S2Cap hemi = S2Cap::FromCenterHeight(S2Point(1, 0, 1).Normalize(), 1);
        b["hemi_complement_center_neg"] =
            (hemi.Complement().center() == -hemi.center());
        b["hemi_complement_height"] = hemi.Complement().height();
        b["hemi_contains_x"] = hemi.Contains(S2Point(1, 0, 0));
        b["hemi_comp_not_x"] = !hemi.Complement().Contains(S2Point(1, 0, 0));
        b["hemi_contains_near"] =
            hemi.Contains(S2Point(1, 0, -(1 - kEps)).Normalize());
        b["hemi_not_interior"] =
            !hemi.InteriorContains(S2Point(1, 0, -(1 + kEps)).Normalize());

        S2Point center = GetLatLngPoint(80, 10);
        S1ChordAngle radius(S1Angle::Degrees(150));
        double max_error =
            (radius.GetS2PointConstructorMaxError()
             + radius.GetS1AngleConstructorMaxError() + 3 * DBL_EPSILON);
        S2Cap concave(center, radius);
        S2Cap concave_min(center, radius.PlusError(-max_error));
        S2Cap concave_max(center, radius.PlusError(max_error));
        b["concave_max_contains_a"] =
            concave_max.Contains(GetLatLngPoint(-70, 10));
        b["concave_min_not_a"] = !concave_min.Contains(GetLatLngPoint(-70, 10));
        b["concave_max_contains_b"] =
            concave_max.Contains(GetLatLngPoint(-50, -170));
        b["concave_min_not_b"] =
            !concave_min.Contains(GetLatLngPoint(-50, -170));

        b["empty_not_contains_xaxis"] = !empty.Contains(xaxis);
        b["empty_not_interior_xaxis"] = !empty.InteriorIntersects(xaxis);
        b["full_contains_xaxis"] = full.Contains(xaxis);
        b["full_interior_xaxis"] = full.InteriorIntersects(xaxis);
        b["xaxis_not_contains_full"] = !xaxis.Contains(full);
        b["xaxis_not_interior_full"] = !xaxis.InteriorIntersects(full);
        b["xaxis_contains_self"] = xaxis.Contains(xaxis);
        b["xaxis_not_interior_self"] = !xaxis.InteriorIntersects(xaxis);
        b["xaxis_contains_empty"] = xaxis.Contains(empty);
        b["xaxis_not_interior_empty"] = !xaxis.InteriorIntersects(empty);
        b["hemi_contains_tiny"] = hemi.Contains(tiny);
        b["hemi_contains_small_cap"] = hemi.Contains(
            S2Cap(S2Point(1, 0, 0), S1Angle::Radians(M_PI_4 - kEps)));
        b["hemi_not_contains_larger"] = !hemi.Contains(
            S2Cap(S2Point(1, 0, 0), S1Angle::Radians(M_PI_4 + kEps)));
        b["concave_contains_hemi"] = concave.Contains(hemi);
        b["concave_interior_intersects_hemi_comp"] =
            concave.InteriorIntersects(hemi.Complement());
        b["concave_not_contains_antipode_cap"] =
            !concave.Contains(S2Cap::FromCenterHeight(-concave.center(), 0.1));

        out["basic"] = b;
    }

    // TEST(S2Cap, AddEmptyCapToNonEmptyCap) / AddNonEmptyCapToEmptyCap
    {
        json a = json::object();
        S2Cap non_empty(S2Point(1, 0, 0), S1Angle::Degrees(10));
        double initial_area = non_empty.GetArea();
        non_empty.AddCap(S2Cap::Empty());
        a["add_empty_area"] = non_empty.GetArea();
        a["expected_area"] = initial_area;

        S2Cap empty = S2Cap::Empty();
        empty.AddCap(non_empty);
        a["empty_after_add_area"] = empty.GetArea();
        a["empty_after_add_expected"] = non_empty.GetArea();
        out["add_cap"] = a;
    }

    // TEST(S2Cap, GetRectBound)
    {
        json arr = json::array();
        arr.push_back({{"name", "empty"},
                       {"rect", rect_json(S2Cap::Empty().GetRectBound())}});
        arr.push_back({{"name", "full"},
                       {"rect", rect_json(S2Cap::Full().GetRectBound())}});

        arr.push_back({{"name", "south_pole_cap"},
                       {"rect", rect_json(S2Cap(GetLatLngPoint(-45, 57),
                                                S1Angle::Degrees(50))
                                              .GetRectBound())}});
        arr.push_back(
            {{"name", "north_tangent"},
             {"rect", rect_json(S2Cap(S2Point(1, 0, 1).Normalize(),
                                      S1Angle::Radians(M_PI_4 + 1e-16))
                                    .GetRectBound())}});
        arr.push_back({{"name", "north_45_eps"},
                       {"rect", rect_json(S2Cap(S2Point(1, 0, 1).Normalize(),
                                                S1Angle::Degrees(45 + 5e-15))
                                              .GetRectBound())}});
        arr.push_back(
            {{"name", "eastern_hemi"},
             {"rect", rect_json(S2Cap(S2Point(0, 1, 0),
                                      S1Angle::Radians(M_PI_2 + 2e-16))
                                    .GetRectBound())}});
        arr.push_back({{"name", "equator_50"},
                       {"rect", rect_json(S2Cap(GetLatLngPoint(0, 50),
                                                S1Angle::Degrees(20))
                                              .GetRectBound())}});
        arr.push_back({{"name", "north_pole_cap"},
                       {"rect", rect_json(S2Cap(GetLatLngPoint(90, 123),
                                                S1Angle::Degrees(10))
                                              .GetRectBound())}});
        out["rect_bound"] = arr;
    }

    // TEST(S2Cap, Expanded)
    {
        json e = json::object();
        e["empty_stays_empty"] =
            S2Cap::Empty().Expanded(S1Angle::Radians(2)).is_empty();
        e["full_stays_full"] =
            S2Cap::Full().Expanded(S1Angle::Radians(2)).is_full();
        S2Cap cap50(S2Point(1, 0, 0), S1Angle::Degrees(50));
        S2Cap cap51(S2Point(1, 0, 0), S1Angle::Degrees(51));
        e["cap50_exp0_len2"] =
            cap50.Expanded(S1Angle::Radians(0)).radius().length2();
        e["cap51_len2"] = cap51.radius().length2();
        e["cap50_exp1_len2"] =
            cap50.Expanded(S1Angle::Degrees(1)).radius().length2();
        e["exp12999_not_full"] =
            !cap50.Expanded(S1Angle::Degrees(129.99)).is_full();
        e["exp13001_full"] = cap50.Expanded(S1Angle::Degrees(130.01)).is_full();
        out["expanded"] = e;
    }

    // TEST(S2Cap, GetCentroid)
    {
        json c = json::object();
        c["empty_centroid"] = point_json(S2Cap::Empty().GetCentroid());
        c["full_centroid_norm"] = S2Cap::Full().GetCentroid().Norm();

        json samples = json::array();
        absl::BitGen bitgen(absl::SeedSeq({1U, 2U, 3U, 4U, 5U}));
        for (int i = 0; i < 100; ++i) {
            S2Point ctr = s2random::Point(bitgen);
            double height = absl::Uniform(bitgen, 0.0, 2.0);
            S2Cap cap = S2Cap::FromCenterHeight(ctr, height);
            S2Point centroid = cap.GetCentroid();
            S2Point expected = ctr * (1.0 - height / 2.0) * cap.GetArea();
            samples.push_back({{"center", point_json(ctr)},
                               {"height", height},
                               {"centroid", point_json(centroid)},
                               {"expected", point_json(expected)}});
        }
        c["samples"] = samples;
        out["centroid"] = c;
    }

    // TEST(S2Cap, Union)
    {
        json u = json::array();

        S2Cap a0 = S2Cap(GetLatLngPoint(50.0, 10.0), S1Angle::Degrees(0.2));
        S2Cap b0 = S2Cap(GetLatLngPoint(50.0, 10.0), S1Angle::Degrees(0.3));
        S2Cap u0 = a0.Union(b0);
        u.push_back({{"name", "same_center_larger"},
                     {"b_contains_a", b0.Contains(a0)},
                     {"eq", (u0 == b0)}});

        u.push_back({{"name", "with_full"},
                     {"is_full", a0.Union(S2Cap::Full()).is_full()}});

        u.push_back({{"name", "with_empty"},
                     {"area_eq",
                      (a0.Union(S2Cap::Empty()).GetArea() == a0.GetArea())}});

        S2Cap c0 = S2Cap(GetLatLngPoint(51.0, 11.0), S1Angle::Degrees(1.5));
        S2Cap u1 = a0.Union(c0);
        u.push_back({{"name", "encompasses"},
                     {"contains", c0.Contains(a0)},
                     {"center_eq", (u1.center() == c0.center())},
                     {"radius_eq", (u1.GetRadius() == c0.GetRadius())}});

        S2Cap d0 = S2Cap(GetLatLngPoint(51.0, 11.0), S1Angle::Degrees(0.1));
        S2Cap ud = a0.Union(d0);
        S2LatLng udl(ud.center());
        u.push_back({{"name", "disjoint"},
                     {"not_contains", !d0.Contains(a0)},
                     {"not_intersects", !d0.Intersects(a0)},
                     {"lat_deg", udl.lat().degrees()},
                     {"lng_deg", udl.lng().degrees()},
                     {"radius_deg", ud.GetRadius().degrees()}});

        S2Cap e0 = S2Cap(GetLatLngPoint(50.3, 10.3), S1Angle::Degrees(0.2));
        S2Cap ue = a0.Union(e0);
        S2LatLng uel(ue.center());
        u.push_back({{"name", "overlap"},
                     {"intersects", e0.Intersects(a0)},
                     {"lat_deg", uel.lat().degrees()},
                     {"lng_deg", uel.lng().degrees()},
                     {"radius_deg", ue.GetRadius().degrees()}});

        S2Cap f0 = S2Cap(S2Point(0, 0, 1).Normalize(), S1Angle::Degrees(150));
        S2Cap g0 = S2Cap(S2Point(0, 1, 0).Normalize(), S1Angle::Degrees(150));
        u.push_back(
            {{"name", "two_large"}, {"is_full", f0.Union(g0).is_full()}});

        S2Cap hemi = S2Cap::FromCenterHeight(S2Point(0, 0, 1).Normalize(), 1);
        u.push_back({{"name", "hemi_union_complement"},
                     {"is_full", hemi.Union(hemi.Complement()).is_full()}});

        out["union"] = u;
    }

    // TEST(S2Cap, EncodeDecode)
    {
        S2Cap cap = S2Cap::FromCenterHeight(S2Point(3, 2, 1).Normalize(), 1);
        Encoder encoder;
        cap.Encode(&encoder);
        out["encode_decode"] = {{"roundtrip", cap_roundtrip_json(cap)},
                                {"encoded",
                                 {cap.center().x(), cap.center().y(),
                                  cap.center().z(), cap.radius().length2()}}};
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
