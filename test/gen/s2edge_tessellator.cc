// Golden data generator for S2EdgeTessellator.

#include "s2/s2edge_tessellator.h"

#include <cmath>
#include <cstdint>
#include <iostream>
#include <nlohmann/json.hpp>
#include <vector>

#include "s2/r2.h"
#include "s2/s1angle.h"
#include "s2/s2latlng.h"
#include "s2/s2point.h"
#include "s2/s2projections.h"

using json = nlohmann::json;
using S2::MercatorProjection;
using S2::PlateCarreeProjection;
using S2::Projection;
using std::vector;

namespace {

    json r2_to_json(const R2Point &p) {
        return json::array({p.x(), p.y()});
    }

    json s2_to_json(const S2Point &p) {
        return json::array({p.x(), p.y(), p.z()});
    }

    json s2_points_to_json(const vector<S2Point> &pts) {
        json out = json::array();
        for (const auto &p : pts)
            out.push_back(s2_to_json(p));
        return out;
    }

    json r2_points_to_json(const vector<R2Point> &pts) {
        json out = json::array();
        for (const auto &p : pts)
            out.push_back(r2_to_json(p));
        return out;
    }

    // "plate_carree" or "mercator" + x_scale. Returns the projection + a JSON
    // stub describing it.
    struct ProjSpec {
        std::string kind;
        double x_scale;
    };

    json proj_spec_to_json(const ProjSpec &s) {
        return {{"kind", s.kind}, {"x_scale", s.x_scale}};
    }

    // Build a projection pointer from a spec. Caller owns.
    Projection *make_projection(const ProjSpec &s) {
        if (s.kind == "plate_carree")
            return new PlateCarreeProjection(s.x_scale);
        if (s.kind == "mercator")
            return new MercatorProjection(s.x_scale);
        std::cerr << "unknown projection kind: " << s.kind << std::endl;
        std::exit(1);
    }

    // Run AppendProjected over a chain of S2Points and return the full
    // planar tessellation.
    json run_projected_chain(const ProjSpec &ps, double tolerance_radians,
                             const vector<S2Point> &chain) {
        Projection *proj = make_projection(ps);
        S2EdgeTessellator tess(proj, S1Angle::Radians(tolerance_radians));
        vector<R2Point> out;
        for (size_t i = 0; i + 1 < chain.size(); ++i) {
            tess.AppendProjected(chain[i], chain[i + 1], &out);
        }
        if (chain.size() == 1) {
            // Mirror what the OCaml chain function will do for a single-vertex
            // input: just project the point.
            out.push_back(proj->Project(chain.front()));
        }
        json j = {
            {"projection", proj_spec_to_json(ps)},
            {"tolerance_radians", tolerance_radians},
            {"input", s2_points_to_json(chain)},
            {"output", r2_points_to_json(out)},
        };
        delete proj;
        return j;
    }

    // Run AppendUnprojected over a chain of R2Points and return the full
    // spherical tessellation.
    json run_unprojected_chain(const ProjSpec &ps, double tolerance_radians,
                               const vector<R2Point> &chain) {
        Projection *proj = make_projection(ps);
        S2EdgeTessellator tess(proj, S1Angle::Radians(tolerance_radians));
        vector<S2Point> out;
        for (size_t i = 0; i + 1 < chain.size(); ++i) {
            tess.AppendUnprojected(chain[i], chain[i + 1], &out);
        }
        if (chain.size() == 1) {
            out.push_back(proj->Unproject(chain.front()));
        }
        json j = {
            {"projection", proj_spec_to_json(ps)},
            {"tolerance_radians", tolerance_radians},
            {"input", r2_points_to_json(chain)},
            {"output", s2_points_to_json(out)},
        };
        delete proj;
        return j;
    }

    S2Point ll(double lat_deg, double lng_deg) {
        return S2LatLng::FromDegrees(lat_deg, lng_deg).ToPoint();
    }

    // A small helper: returns min_longitude, max_longitude of a chain of
    // unprojected points.  Used by the wrapping tests.
    struct LngRange {
        double min_lng;
        double max_lng;
    };

    LngRange s2_lng_range(const vector<S2Point> &pts) {
        double lo = std::numeric_limits<double>::infinity();
        double hi = -std::numeric_limits<double>::infinity();
        for (const auto &p : pts) {
            double d = std::fabs(S2LatLng::Longitude(p).degrees());
            lo = std::min(lo, d);
            hi = std::max(hi, d);
        }
        return {lo, hi};
    }

}  // namespace

int main() {
    json out;

    const double kDeg001 = S1Angle::Degrees(0.01).radians();
    const double kE7_1 = S1Angle::E7(1).radians();
    const double kOneMicron = 1e-6 / 6371.0;  // 1 micron on Earth.

    // TEST(S2EdgeTessellator, ProjectedNoTessellation)
    // TEST(S2EdgeTessellator, UnprojectedNoTessellation)
    {
        json cases = json::array();

        // Projected, no tessellation: (1,0,0) -> (0,1,0) with 0.01 deg
        // tolerance should produce exactly 2 vertices.
        {
            vector<S2Point> chain = {S2Point(1, 0, 0), S2Point(0, 1, 0)};
            json c =
                run_projected_chain({"plate_carree", 180.0}, kDeg001, chain);
            c["kind"] = "projected";
            c["expected_size"] = 2;
            cases.push_back(c);
        }

        // Unprojected, no tessellation: (0,30) -> (0,50) with 0.01 deg
        // tolerance should produce exactly 2 vertices.
        {
            vector<R2Point> chain = {R2Point(0, 30), R2Point(0, 50)};
            json c =
                run_unprojected_chain({"plate_carree", 180.0}, kDeg001, chain);
            c["kind"] = "unprojected";
            c["expected_size"] = 2;
            cases.push_back(c);
        }

        out["no_tessellation"] = cases;
    }

    // TEST(S2EdgeTessellator, UnprojectedWrapping)
    // TEST(S2EdgeTessellator, ProjectedWrapping)
    {
        json cases = json::array();

        {
            // Unprojected: (-170, 0) -> (170, 80) - crosses antimeridian,
            // every vertex should have |longitude| >= 170 degrees.
            vector<R2Point> chain = {R2Point(-170, 0), R2Point(170, 80)};
            json c =
                run_unprojected_chain({"plate_carree", 180.0}, kDeg001, chain);
            c["kind"] = "unprojected";
            // Post-condition: for every output point, |lng_deg| >= threshold.
            c["abs_lng_min_deg"] = 170.0;
            cases.push_back(c);
        }

        {
            // Projected geodesic from 0:-170 to 0:170; the x coordinate
            // should be <= -170 (non-canonical wrapping).
            vector<S2Point> chain = {ll(0, -170), ll(0, 170)};
            json c =
                run_projected_chain({"plate_carree", 180.0}, kDeg001, chain);
            c["kind"] = "projected";
            c["x_max"] = -170.0;
            cases.push_back(c);
        }

        out["wrapping"] = cases;
    }

    // TEST(S2EdgeTessellator, UnprojectedWrappingMultipleCrossings)
    {
        vector<R2Point> chain;
        chain.push_back(R2Point(180 - 0.03, 1));
        for (int lat = 1; lat <= 60; ++lat) {
            double a = 180 - 0.03 * lat;
            double b = -180 + 0.07 * lat;
            double c = 180 - 0.03 * (lat + 1);
            chain.push_back(R2Point(b, lat));
            chain.push_back(R2Point(c, lat + 1));
            (void)a;
        }
        json c = run_unprojected_chain({"plate_carree", 180.0}, kDeg001, chain);
        c["kind"] = "unprojected";
        c["abs_lng_min_deg"] = 175.0;
        out["unprojected_multi_crossing"] = c;
    }

    // TEST(S2EdgeTessellator, ProjectedWrappingMultipleCrossings)
    {
        // Loop from the C++ test: "0:160, 0:-40, 0:120, 0:-80, 10:120,
        // 10:-40, 0:160"
        vector<S2Point> chain = {
            ll(0, 160),  ll(0, -40),  ll(0, 120), ll(0, -80),
            ll(10, 120), ll(10, -40), ll(0, 160),
        };
        json c = run_projected_chain({"plate_carree", 180.0}, kE7_1, chain);
        c["kind"] = "projected";
        c["min_x"] = 160.0;
        c["max_x"] = 640.0;
        c["first_equals_last"] = true;
        out["projected_multi_crossing"] = c;
    }

    // TEST(S2EdgeTessellator, InfiniteRecursionBug)
    {
        vector<S2Point> chain = {ll(3, 21), ll(1, -159)};
        json c =
            run_projected_chain({"plate_carree", 180.0}, kOneMicron, chain);
        c["kind"] = "projected";
        c["expected_size"] = 36;
        out["infinite_recursion_bug"] = c;
    }

    // A few additional simple chain checks: a degenerate case (a==b) for
    // both directions under both projections.  This exercises the early-out
    // path in the recursion when the two endpoints coincide.
    {
        json cases = json::array();
        {
            S2Point p(1, 0, 0);
            vector<S2Point> chain = {p, p};
            json c =
                run_projected_chain({"plate_carree", 180.0}, kDeg001, chain);
            c["kind"] = "projected_degenerate";
            c["expected_size"] = 2;
            cases.push_back(c);
        }
        {
            R2Point p(30, 40);
            vector<R2Point> chain = {p, p};
            json c =
                run_unprojected_chain({"plate_carree", 180.0}, kDeg001, chain);
            c["kind"] = "unprojected_degenerate";
            c["expected_size"] = 2;
            cases.push_back(c);
        }
        out["degenerate"] = cases;
    }

    // Mercator projection, a Seattle -> New York like edge. Sanity check
    // that we exercise the Mercator path.
    {
        S2Point a = S2LatLng::FromDegrees(47.6062, -122.3321).ToPoint();
        S2Point b = S2LatLng::FromDegrees(40.7128, -74.0059).ToPoint();
        vector<S2Point> chain = {a, b};
        double tol = S1Angle::Radians(1.0 / 6371.0).radians();  // ~1 km
        json c = run_projected_chain({"mercator", 180.0}, tol, chain);
        c["kind"] = "mercator_seattle_ny";
        out["mercator_seattle_ny"] = c;
    }

    // Alternate scale: radians mode.
    {
        S2Point a = S2LatLng::FromRadians(0.1, 0.2).ToPoint();
        S2Point b = S2LatLng::FromRadians(0.3, 0.4).ToPoint();
        vector<S2Point> chain = {a, b};
        json c = run_projected_chain({"plate_carree", M_PI}, kDeg001, chain);
        c["kind"] = "plate_radians";
        out["plate_radians"] = c;
    }

    // min_tolerance value.
    {
        out["min_tolerance_radians"] =
            S2EdgeTessellator::kMinTolerance().radians();
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
