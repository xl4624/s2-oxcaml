// Golden data generator for S2Projections.

#include "s2/s2projections.h"

#include <cmath>
#include <iostream>
#include <limits>
#include <nlohmann/json.hpp>

#include "s2/r2.h"
#include "s2/s2latlng.h"
#include "s2/s2point.h"

using json = nlohmann::json;
using S2::MercatorProjection;
using S2::PlateCarreeProjection;
using S2::Projection;

namespace {

    // Encode a double that may be +/-inf. nlohmann::json encodes non-finite
    // doubles as JSON null, which loses the sign. Represent infinities as the
    // tagged strings "inf"/"-inf" and finite values as JSON numbers.
    json encode_double(double v) {
        if (std::isinf(v))
            return v > 0 ? json("inf") : json("-inf");
        return json(v);
    }

    json r2_to_json(const R2Point &p) {
        return json::array({encode_double(p.x()), encode_double(p.y())});
    }
    json s2_to_json(const S2Point &p) {
        return json::array(
            {encode_double(p.x()), encode_double(p.y()), encode_double(p.z())});
    }
    json latlng_to_json(const S2LatLng &ll) {
        return json::array({encode_double(ll.lat().radians()),
                            encode_double(ll.lng().radians())});
    }

    json make_project_case(const Projection &proj, const R2Point &px,
                           const S2Point &x) {
        return {
            {"px", r2_to_json(px)},
            {"x", s2_to_json(x)},
            {"project", r2_to_json(proj.Project(x))},
            {"unproject", s2_to_json(proj.Unproject(px))},
            {"from_latlng", r2_to_json(proj.FromLatLng(S2LatLng(x)))},
            {"to_latlng", latlng_to_json(proj.ToLatLng(px))},
        };
    }

    json make_interpolate_case(const Projection &proj, double f,
                               const R2Point &a, const R2Point &b) {
        return {
            {"f", f},
            {"a", r2_to_json(a)},
            {"b", r2_to_json(b)},
            {"result", r2_to_json(proj.Interpolate(f, a, b))},
        };
    }

    json make_wrap_case(const Projection &proj, const R2Point &a,
                        const R2Point &b) {
        return {
            {"a", r2_to_json(a)},
            {"b", r2_to_json(b)},
            {"result", r2_to_json(proj.WrapDestination(a, b))},
        };
    }

}  // namespace

int main() {
    json out;

    const double inf = std::numeric_limits<double>::infinity();

    // TEST(PlateCarreeProjection, Interpolate)
    {
        PlateCarreeProjection proj(180);
        json cases = json::array();

        cases.push_back(
            make_interpolate_case(proj, 0.25, R2Point(1, 5), R2Point(3, 9)));
        cases.push_back(
            make_interpolate_case(proj, -2, R2Point(1, 0), R2Point(3, 0)));

        R2Point a(1.234, -5.456e-20), b(2.1234e-20, 7.456);
        cases.push_back(make_interpolate_case(proj, 0, a, b));
        cases.push_back(make_interpolate_case(proj, 1, a, b));
        // A few additional fractions to cover the middle of the range.
        cases.push_back(
            make_interpolate_case(proj, 0.5, R2Point(0, 0), R2Point(10, 20)));

        out["plate_carree_interpolate"] = cases;
    }

    // TEST(PlateCarreeProjection, ProjectUnproject)
    {
        PlateCarreeProjection proj(180);
        json cases = json::array();
        cases.push_back(
            make_project_case(proj, R2Point(0, 0), S2Point(1, 0, 0)));
        cases.push_back(
            make_project_case(proj, R2Point(180, 0), S2Point(-1, 0, 0)));
        cases.push_back(
            make_project_case(proj, R2Point(90, 0), S2Point(0, 1, 0)));
        cases.push_back(
            make_project_case(proj, R2Point(-90, 0), S2Point(0, -1, 0)));
        cases.push_back(
            make_project_case(proj, R2Point(0, 90), S2Point(0, 0, 1)));
        cases.push_back(
            make_project_case(proj, R2Point(0, -90), S2Point(0, 0, -1)));

        // Extra: an off-axis, mid-latitude point.
        S2LatLng ll = S2LatLng::FromDegrees(30, 45);
        cases.push_back(make_project_case(proj, R2Point(45, 30), ll.ToPoint()));

        out["plate_carree_project_unproject"] = cases;
    }

    // TEST(MercatorProjection, ProjectUnproject)
    {
        MercatorProjection proj(180);
        json cases = json::array();
        cases.push_back(
            make_project_case(proj, R2Point(0, 0), S2Point(1, 0, 0)));
        cases.push_back(
            make_project_case(proj, R2Point(180, 0), S2Point(-1, 0, 0)));
        cases.push_back(
            make_project_case(proj, R2Point(90, 0), S2Point(0, 1, 0)));
        cases.push_back(
            make_project_case(proj, R2Point(-90, 0), S2Point(0, -1, 0)));
        cases.push_back(
            make_project_case(proj, R2Point(0, inf), S2Point(0, 0, 1)));
        cases.push_back(
            make_project_case(proj, R2Point(0, -inf), S2Point(0, 0, -1)));

        // Arbitrary sanity point from the C++ test.
        cases.push_back(
            make_project_case(proj, R2Point(0, 70.255578967830246),
                              S2LatLng::FromRadians(1, 0).ToPoint()));

        // Extra: a mid-latitude off-axis point.
        cases.push_back(make_project_case(
            proj, proj.FromLatLng(S2LatLng::FromDegrees(30, 45)),
            S2LatLng::FromDegrees(30, 45).ToPoint()));

        out["mercator_project_unproject"] = cases;
    }

    // wrap_distance exposes the projection period along each axis.
    {
        PlateCarreeProjection plate(180);
        MercatorProjection merc(180);
        out["wrap_distance"] = {
            {"plate_carree", r2_to_json(plate.wrap_distance())},
            {"mercator", r2_to_json(merc.wrap_distance())},
        };
    }

    // WrapDestination: exercise the helper in both projections.
    {
        PlateCarreeProjection plate(180);
        MercatorProjection merc(180);
        json plate_cases = json::array();
        json merc_cases = json::array();

        // Shortest edge should wrap the "b" side of the antimeridian.
        plate_cases.push_back(
            make_wrap_case(plate, R2Point(170, 20), R2Point(-170, 20)));
        plate_cases.push_back(
            make_wrap_case(plate, R2Point(-170, 20), R2Point(170, 20)));
        // No wrap needed.
        plate_cases.push_back(
            make_wrap_case(plate, R2Point(10, 5), R2Point(20, -5)));
        // Boundary-exact half wrap: |x-a.x| exactly 0.5*wrap (no wrap applied).
        plate_cases.push_back(
            make_wrap_case(plate, R2Point(0, 0), R2Point(180, 0)));
        // Y axis does not wrap even if y differs by more than any wrap.
        plate_cases.push_back(
            make_wrap_case(plate, R2Point(0, -80), R2Point(0, 80)));

        merc_cases.push_back(
            make_wrap_case(merc, R2Point(170, 20), R2Point(-170, 20)));
        merc_cases.push_back(
            make_wrap_case(merc, R2Point(-170, 20), R2Point(170, 20)));
        merc_cases.push_back(
            make_wrap_case(merc, R2Point(10, 5), R2Point(20, -5)));

        out["plate_carree_wrap_destination"] = plate_cases;
        out["mercator_wrap_destination"] = merc_cases;
    }

    // Non-default x_scale: exercise the scale factor on both projections.
    {
        PlateCarreeProjection plate(M_PI);  // radians mode
        MercatorProjection merc(1.0);       // unit-scale
        json cases = json::array();

        S2Point p = S2LatLng::FromDegrees(15, -30).ToPoint();
        cases.push_back({
            {"kind", "plate_carree"},
            {"x_scale", M_PI},
            {"input", s2_to_json(p)},
            {"projected", r2_to_json(plate.Project(p))},
            {"from_latlng",
             r2_to_json(plate.FromLatLng(S2LatLng::FromDegrees(15, -30)))},
            {"wrap_distance", r2_to_json(plate.wrap_distance())},
        });
        cases.push_back({
            {"kind", "mercator"},
            {"x_scale", 1.0},
            {"input", s2_to_json(p)},
            {"projected", r2_to_json(merc.Project(p))},
            {"from_latlng",
             r2_to_json(merc.FromLatLng(S2LatLng::FromDegrees(15, -30)))},
            {"wrap_distance", r2_to_json(merc.wrap_distance())},
        });
        out["alternate_scales"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
