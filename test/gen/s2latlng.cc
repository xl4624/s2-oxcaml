// Golden data generator for S2LatLng.
// Mirrors s2geometry/src/s2/s2latlng_test.cc.

#include "s2/s2latlng.h"

#include <cmath>
#include <iostream>
#include <limits>
#include <nlohmann/json.hpp>

#include "s2/s1angle.h"
#include "s2/s2point.h"
#include "s2/s2pointutil.h"

using json = nlohmann::json;

json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

json latlng_json(const S2LatLng &ll) {
    return {ll.lat().radians(), ll.lng().radians()};
}

int main() {
    json out;

    // TEST(S2LatLng, TestBasic) - constructors and basic properties
    {
        json cases = json::array();

        // FromRadians
        {
            S2LatLng ll = S2LatLng::FromRadians(M_PI_4, M_PI_2);
            cases.push_back({
                {"op", "from_radians"},
                {"lat_rad", M_PI_4},
                {"lng_rad", M_PI_2},
                {"lat", ll.lat().radians()},
                {"lng", ll.lng().radians()},
                {"is_valid", ll.is_valid()},
            });
        }

        // FromDegrees
        {
            S2LatLng ll = S2LatLng::FromDegrees(45, 90);
            cases.push_back({
                {"op", "from_degrees"},
                {"lat_deg", 45.0},
                {"lng_deg", 90.0},
                {"lat", ll.lat().radians()},
                {"lng", ll.lng().radians()},
                {"is_valid", ll.is_valid()},
            });
        }

        // Default constructor
        {
            S2LatLng ll;
            cases.push_back({
                {"op", "default"},
                {"lat", ll.lat().radians()},
                {"lng", ll.lng().radians()},
                {"is_valid", ll.is_valid()},
            });
        }

        // Invalid
        {
            S2LatLng ll = S2LatLng::Invalid();
            cases.push_back({
                {"op", "invalid"},
                {"lat", ll.lat().radians()},
                {"lng", ll.lng().radians()},
                {"is_valid", ll.is_valid()},
            });
        }

        out["constructors"] = cases;
    }

    // TEST(S2LatLng, TestBasic) - is_valid
    {
        json cases = json::array();
        auto test = [&](const std::string &name, double lat_deg,
                        double lng_deg) {
            S2LatLng ll = S2LatLng::FromDegrees(lat_deg, lng_deg);
            cases.push_back({
                {"name", name},
                {"lat_deg", lat_deg},
                {"lng_deg", lng_deg},
                {"is_valid", ll.is_valid()},
            });
        };
        test("valid_45_90", 45, 90);
        test("invalid_lat_neg91", -91, 0);
        test("invalid_lng_181", 0, 181);
        test("valid_90_180", 90, 180);
        test("valid_neg90_neg180", -90, -180);
        test("invalid_120_200", 120, 200);
        out["is_valid"] = cases;
    }

    // TEST(S2LatLng, TestBasic) - normalized
    {
        json cases = json::array();
        auto test = [&](const std::string &name, double lat_deg,
                        double lng_deg) {
            S2LatLng ll = S2LatLng::FromDegrees(lat_deg, lng_deg);
            S2LatLng n = ll.Normalized();
            cases.push_back({
                {"name", name},
                {"lat_deg", lat_deg},
                {"lng_deg", lng_deg},
                {"result_lat", n.lat().radians()},
                {"result_lng", n.lng().radians()},
                {"result_is_valid", n.is_valid()},
            });
        };
        test("over_90_200", 120, 200);
        test("under_neg90_neg360", -100, -360);
        test("normal_45_90", 45, 90);
        test("zero", 0, 0);
        test("exactly_90_180", 90, 180);
        test("exactly_neg90_neg180", -90, -180);
        test("large_lng", 0, 540);
        out["normalized"] = cases;
    }

    // TEST(S2LatLng, TestBasic) - arithmetic
    {
        json cases = json::array();

        // add
        {
            S2LatLng a = S2LatLng::FromDegrees(10, 20);
            S2LatLng b = S2LatLng::FromDegrees(20, 30);
            S2LatLng r = a + b;
            cases.push_back({
                {"op", "add"},
                {"a", latlng_json(a)},
                {"b", latlng_json(b)},
                {"result", latlng_json(r)},
            });
        }

        // sub
        {
            S2LatLng a = S2LatLng::FromDegrees(10, 20);
            S2LatLng b = S2LatLng::FromDegrees(20, 30);
            S2LatLng r = a - b;
            cases.push_back({
                {"op", "sub"},
                {"a", latlng_json(a)},
                {"b", latlng_json(b)},
                {"result", latlng_json(r)},
            });
        }

        // mul
        {
            S2LatLng a = S2LatLng::FromDegrees(10, 20);
            S2LatLng r = 0.5 * a;
            cases.push_back({
                {"op", "mul"},
                {"a", latlng_json(a)},
                {"scalar", 0.5},
                {"result", latlng_json(r)},
            });
        }

        out["arithmetic"] = cases;
    }

    // TEST(S2LatLng, TestConversion) - point conversion round-trips
    {
        json cases = json::array();
        auto test = [&](const std::string &name, double lat_deg,
                        double lng_deg) {
            S2LatLng ll = S2LatLng::FromDegrees(lat_deg, lng_deg);
            S2Point p = ll.ToPoint();
            S2LatLng back(p);
            cases.push_back({
                {"name", name},
                {"lat_deg", lat_deg},
                {"lng_deg", lng_deg},
                {"point", point_json(p)},
                {"back_lat", back.lat().radians()},
                {"back_lng", back.lng().radians()},
            });
        };
        test("north_pole", 90.0, 65.0);
        test("south_pole_rad", -90.0, 57.29577951308232);
        test("date_line", 12.2, 180.0);
        test("neg_date_line_rad", 5.72957795130823, -180.0);
        test("equator_0", 0.0, 0.0);
        test("mid", 37.0, -122.0);
        test("neg_lat", -45.0, 90.0);
        out["conversion"] = cases;
    }

    // TEST(S2LatLng, TestConversion) - Latitude/Longitude static methods
    {
        json cases = json::array();
        auto test = [&](const std::string &name, const S2Point &p) {
            cases.push_back({
                {"name", name},
                {"point", point_json(p)},
                {"lat", S2LatLng::Latitude(p).radians()},
                {"lng", S2LatLng::Longitude(p).radians()},
            });
        };
        test("x_axis", S2Point(1, 0, 0));
        test("y_axis", S2Point(0, 1, 0));
        test("z_axis", S2Point(0, 0, 1));
        test("neg_x", S2Point(-1, 0, 0));
        test("neg_z", S2Point(0, 0, -1));
        test("diagonal", S2Point(1, 1, 1).Normalize());
        test("origin", S2::Origin());
        out["lat_lng_from_point"] = cases;
    }

    // TEST(S2LatLng, NegativeZeros) - ensure +0.0 for negative zeros
    {
        json cases = json::array();
        auto test = [&](const std::string &name, const S2Point &p,
                        const std::string &which) {
            double val;
            if (which == "lat") {
                val = S2LatLng::Latitude(p).radians();
            } else {
                val = S2LatLng::Longitude(p).radians();
            }
            cases.push_back({
                {"name", name},
                {"point", point_json(p)},
                {"which", which},
                {"value", val},
                {"is_negative_zero", (val == 0.0 && std::signbit(val))},
            });
        };
        test("lat_neg_zero_z", S2Point(1., 0., -0.), "lat");
        test("lng_neg_zero_y", S2Point(1., -0., 0.), "lng");
        test("lng_neg_x_neg_y", S2Point(-1., -0., 0.), "lng");
        test("lng_neg_zero_x_pos_y", S2Point(-0., 0., 1.), "lng");
        test("lng_neg_zero_x_neg_y", S2Point(-0., -0., 1.), "lng");
        out["negative_zeros"] = cases;
    }

    // TEST(S2LatLng, TestDistance) - haversine distances
    {
        json cases = json::array();
        auto test = [&](const std::string &name, double lat1, double lng1,
                        double lat2, double lng2) {
            S2LatLng a = S2LatLng::FromDegrees(lat1, lng1);
            S2LatLng b = S2LatLng::FromDegrees(lat2, lng2);
            S1Angle d = a.GetDistance(b);
            cases.push_back({
                {"name", name},
                {"a_lat_deg", lat1},
                {"a_lng_deg", lng1},
                {"b_lat_deg", lat2},
                {"b_lng_deg", lng2},
                {"distance_radians", d.radians()},
                {"distance_degrees", d.degrees()},
            });
        };
        test("same_pole", 90, 0, 90, 0);
        test("cross_globe", -37, 25, -66, -155);
        test("equator_span", 0, 165, 0, -80);
        test("antipodal_approx", 47, -127, -47, 53);
        test("same_point", 37, -122, 37, -122);
        test("equator_quarter", 0, 0, 0, 90);
        out["distance"] = cases;
    }

    // TEST(S2LatLng, TestBasic) - approx_equal
    {
        json cases = json::array();
        auto test = [&](const std::string &name, double lat1, double lng1,
                        double lat2, double lng2) {
            S2LatLng a = S2LatLng::FromDegrees(lat1, lng1);
            S2LatLng b = S2LatLng::FromDegrees(lat2, lng2);
            cases.push_back({
                {"name", name},
                {"a", latlng_json(a)},
                {"b", latlng_json(b)},
                {"approx_equal", a.ApproxEquals(b)},
            });
        };
        test("same", 10.0, 20.0, 10.0, 20.0);
        test("different", 10.0, 20.0, 11.0, 21.0);
        test("tiny_diff", 0.0, 0.0, 1e-16, 1e-16);
        test("at_limit", 0.0, 0.0, 1e-15, 0.0);
        test("over_limit", 0.0, 0.0, 2e-15, 0.0);
        out["approx_equal"] = cases;
    }

    // FromE5/E6/E7 constructors
    {
        json cases = json::array();

        // E5
        {
            S2LatLng ll = S2LatLng::FromE5(4500000, 9000000);
            cases.push_back({
                {"op", "from_e5"},
                {"lat_e5", 4500000},
                {"lng_e5", 9000000},
                {"lat", ll.lat().radians()},
                {"lng", ll.lng().radians()},
            });
        }

        // E6
        {
            S2LatLng ll = S2LatLng::FromE6(45000000, 90000000);
            cases.push_back({
                {"op", "from_e6"},
                {"lat_e6", 45000000},
                {"lng_e6", 90000000},
                {"lat", ll.lat().radians()},
                {"lng", ll.lng().radians()},
            });
        }

        // E7
        {
            S2LatLng ll = S2LatLng::FromE7(450000000, 900000000);
            cases.push_back({
                {"op", "from_e7"},
                {"lat_e7", 450000000},
                {"lng_e7", 900000000},
                {"lat", ll.lat().radians()},
                {"lng", ll.lng().radians()},
            });
        }

        out["e_constructors"] = cases;
    }

    // to_point - detailed conversion cases
    {
        json cases = json::array();
        auto test = [&](const std::string &name, double lat_deg,
                        double lng_deg) {
            S2LatLng ll = S2LatLng::FromDegrees(lat_deg, lng_deg);
            S2Point p = ll.ToPoint();
            cases.push_back({
                {"name", name},
                {"lat_deg", lat_deg},
                {"lng_deg", lng_deg},
                {"point", point_json(p)},
            });
        };
        test("origin", 0, 0);
        test("north_pole", 90, 0);
        test("south_pole", -90, 0);
        test("equator_90", 0, 90);
        test("equator_180", 0, 180);
        test("equator_neg90", 0, -90);
        test("mid_45_45", 45, 45);
        test("sf", 37.7749, -122.4194);
        out["to_point"] = cases;
    }

    // TEST(S2LatLng, TestToString) - ToStringInDegrees produces "lat,lng"
    // formatted with %f (default 6 decimals of precision) after normalizing
    // out-of-range inputs.
    {
        json cases = json::array();
        auto test = [&](const std::string &name, double lat_deg,
                        double lng_deg) {
            S2LatLng ll = S2LatLng::FromDegrees(lat_deg, lng_deg);
            cases.push_back({
                {"name", name},
                {"lat_deg", lat_deg},
                {"lng_deg", lng_deg},
                {"expected", ll.ToStringInDegrees()},
            });
        };
        test("zero", 0, 0);
        test("simple", 1.5, 91.7);
        test("negative", 9.9, -0.31);
        test("irrational", std::sqrt(2.0), -std::sqrt(5.0));
        test("over_lat_91", 91.3, 190.4);
        test("under_lat_neg100", -100, -710);
        out["to_string_in_degrees"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
