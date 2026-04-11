// Golden data generator for Earth.
//
// There is no direct C++ equivalent of the Go earth package, but S2Earth in
// s2earth.h provides the same conversions and uses the same mean radius
// (6371010.0 meters). This generator uses Go's earth_test.go cases as the
// primary reference and computes expected values via S2Earth / S2Point /
// S2LatLng for full numerical parity.

#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/s1angle.h"
#include "s2/s2earth.h"
#include "s2/s2latlng.h"
#include "s2/s2point.h"

using json = nlohmann::json;

static json point_xyz(double x, double y, double z) {
    S2Point p = S2Point(x, y, z).Normalize();
    return {p.x(), p.y(), p.z()};
}

int main() {
    json out;

    // Radius / altitude constants.
    //
    // Go earth.go: Radius = 6371.01 * unit.Kilometer, which equals
    // 6371010.0 meters. Go: LowestAltitude = -10898 m, HighestAltitude =
    // 8848 m. (The C++ S2Earth header reports 8846 m for the highest; we
    // follow the Go source for this port.)
    {
        json c = json::object();
        c["radius_meters"] = S2Earth::RadiusMeters();
        c["radius_km"] = S2Earth::RadiusKm();
        c["lowest_altitude_meters"] = -10898.0;
        c["highest_altitude_meters"] = 8848.0;
        out["constants"] = c;
    }

    // Go: TestAngleFromLength / TestLengthFromAngle
    //
    // Go's table [degreesToMeters] exercises the round trip between an angle
    // in degrees and a length in meters on the Earth's surface. We emit the
    // same inputs and ask S2Earth to compute both directions.
    {
        json cases = json::array();
        auto add = [&](double degrees, double meters) {
            S1Angle angle = S1Angle::Degrees(degrees);
            cases.push_back({
                {"degrees", degrees},
                {"meters", meters},
                {"angle_from_length_radians",
                 S2Earth::MetersToAngle(meters).radians()},
                {"length_from_angle_meters", S2Earth::ToMeters(angle)},
            });
        };
        add(-89.93201943346866, -1e7);
        add(-30.0, -3335853.035324518);
        add(0.0, 0.0);
        add(30.0, 3335853.035324518);
        add(89.93201943346866, 1e7);
        add(90.0, 10007559.105973555);
        add(179.86403886693734, 2e7);
        add(180.0, 20015118.21194711);
        add(359.72807773387467, 4e7);
        add(360.0, 40030236.42389422);
        add(899.3201943346867, 1e8);
        out["angle_length_roundtrip"] = cases;
    }

    // Go: TestLengthFromPoints
    {
        json cases = json::array();
        auto add = [&](double x1, double y1, double z1, double x2, double y2,
                       double z2, double meters) {
            S2Point a = S2Point(x1, y1, z1).Normalize();
            S2Point b = S2Point(x2, y2, z2).Normalize();
            cases.push_back({
                {"a", point_xyz(x1, y1, z1)},
                {"b", point_xyz(x2, y2, z2)},
                {"reference_meters", meters},
                {"length_meters", S2Earth::GetDistanceMeters(a, b)},
            });
        };
        add(1, 0, 0, 1, 0, 0, 0.0);
        add(1, 0, 0, 0, 1, 0, 10007559.105973555);
        add(1, 0, 0, 0, 1, 1, 10007559.105973555);
        add(1, 0, 0, -1, 0, 0, 20015118.21194711);
        add(1, 2, 3, 2, 3, -1, 7680820.247060414);
        out["length_from_points"] = cases;
    }

    // Go: TestLengthFromLatLngs
    {
        json cases = json::array();
        auto add = [&](double lat1, double lng1, double lat2, double lng2,
                       double reference_meters) {
            S2LatLng a = S2LatLng::FromDegrees(lat1, lng1);
            S2LatLng b = S2LatLng::FromDegrees(lat2, lng2);
            cases.push_back({
                {"lat1_degrees", lat1},
                {"lng1_degrees", lng1},
                {"lat2_degrees", lat2},
                {"lng2_degrees", lng2},
                {"reference_meters", reference_meters},
                {"length_meters", S2Earth::GetDistanceMeters(a, b)},
            });
        };
        add(90, 0, 90, 0, 0.0);
        add(-37, 25, -66, -155, 8562022.790666264);
        add(0, 165, 0, -80, 12787436.635410652);
        add(47, -127, -47, 53, 20015118.077688109);
        add(51.961951, -180.227156, 51.782383, 181.126878, 95078.3566198074);
        out["length_from_latlngs"] = cases;
    }

    // Go: TestAreaFromSteradians / TestSteradiansFromArea
    //
    // Go's table uses several polygon-computed areas. Here we simply emit
    // matching (steradians, square meters) pairs derived from S2Earth so the
    // OCaml side can check both directions. The "earth_area" case mirrors
    // 4*pi steradians = total Earth surface area, and a few representative
    // steradian samples cover the range.
    {
        json cases = json::array();
        auto add = [&](double steradians) {
            double area_m2 = S2Earth::SteradiansToSquareMeters(steradians);
            cases.push_back({
                {"steradians", steradians},
                {"square_meters", area_m2},
            });
        };
        add(1.0);
        add(4.0 * M_PI);
        add(M_PI);
        add(M_PI / 2.0);
        add(0.0);
        // Matches Go's degreesToMeters / average-area levels.
        add(81.07281893380302 * 1e6
            / (S2Earth::RadiusMeters() * S2Earth::RadiusMeters()));
        out["steradian_area_roundtrip"] = cases;
    }

    // Go: TestInitialBearingFromLatLngs
    //
    // The Go table tests named scenarios plus two long-distance routes
    // (Spain -> Japan, Japan -> Spain) at degree precision. Go's assertion
    // uses a 0.01 degree tolerance. We emit C++-computed bearings so OCaml
    // can verify exact numerical parity with the Go haversine formulation.
    {
        json cases = json::array();
        auto add = [&](const std::string &name, double lat1, double lng1,
                       double lat2, double lng2, double want_degrees) {
            S2LatLng a = S2LatLng::FromDegrees(lat1, lng1);
            S2LatLng b = S2LatLng::FromDegrees(lat2, lng2);
            S1Angle bearing = S2Earth::GetInitialBearing(a, b);
            cases.push_back({
                {"name", name},
                {"a_lat_degrees", lat1},
                {"a_lng_degrees", lng1},
                {"b_lat_degrees", lat2},
                {"b_lng_degrees", lng2},
                {"want_degrees", want_degrees},
                {"bearing_radians", bearing.radians()},
                {"bearing_degrees", bearing.degrees()},
            });
        };
        add("Westward on equator", 0, 50, 0, 100, 90);
        add("Eastward on equator", 0, 50, 0, 0, -90);
        add("Northward on meridian", 16, 28, 81, 28, 0);
        add("Southward on meridian", 24, 64, -27, 64, 180);
        add("Towards north pole", 12, 76, 90, 50, 0);
        add("Towards south pole", -35, 105, -90, -120, 180);
        add("Spain to Japan", 40.4379332, -3.749576, 35.6733227, 139.6403486,
            29.2);
        add("Japan to Spain", 35.6733227, 139.6403486, 40.4379332, -3.749576,
            -27.2);
        out["initial_bearing"] = cases;
    }

    // Go: TestInitialBearingFromLatLngsUndefinedResultDoesNotCrash
    //
    // Degenerate inputs. We emit the pairs so OCaml can call the function
    // and verify the result is finite (value itself is undefined).
    {
        json cases = json::array();
        auto add = [&](const std::string &name, double lat1, double lng1,
                       double lat2, double lng2) {
            cases.push_back({
                {"name", name},
                {"a_lat_degrees", lat1},
                {"a_lng_degrees", lng1},
                {"b_lat_degrees", lat2},
                {"b_lng_degrees", lng2},
            });
        };
        add("North pole prime meridian to Null Island", 90, 0, 0, 0);
        add("North pole facing east to Guatemala", 90, 90, 15, -90);
        add("South pole facing west to McMurdo", -90, -90, -78, 166);
        add("South pole anti-prime meridian to Null Island", -90, -180, 0, 0);
        add("Jakarta and antipode", -6.109, 106.668, 6.109, -180 + 106.668);
        add("Alert and antipode", 82.499, -62.350, -82.499, 180 - 62.350);
        out["initial_bearing_degenerate"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
