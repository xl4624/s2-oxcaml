// Golden data generator for S2PointUtil.
// Mirrors s2geometry/src/s2/s2pointutil_test.cc.

#include "s2/s2pointutil.h"

#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/s1angle.h"
#include "s2/s2point.h"
#include "s2/util/math/matrix3x3.h"

using json = nlohmann::json;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

int main() {
    json out;

    // TEST(S2, OriginTest) - origin constant and its invariants.
    {
        json c = json::object();
        S2Point o = S2::Origin();
        c["origin"] = point_json(o);
        c["is_unit_length"] = S2::IsUnitLength(o);
        c["norm"] = o.Norm();
        c["norm2"] = o.Norm2();
        // Distance from the north pole in radians: acos(o.z()).
        c["distance_from_north_pole_radians"] = std::acos(o.z());
        out["origin"] = c;
    }

    // TEST(S2PointUtil, IsUnitLength) - representative points.
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &p) {
            cases.push_back({{"name", name},
                             {"p", point_json(p)},
                             {"expected", S2::IsUnitLength(p)}});
        };
        add("unit_x", S2Point(1, 0, 0));
        add("unit_y", S2Point(0, 1, 0));
        add("unit_z", S2Point(0, 0, 1));
        add("neg_unit_x", S2Point(-1, 0, 0));
        add("origin", S2::Origin());
        add("normalized_diag", S2Point(1, 1, 1).Normalize());
        add("unnormalized_diag", S2Point(1, 1, 1));
        add("zero", S2Point(0, 0, 0));
        add("tiny", S2Point(1e-200, 0, 0));
        add("large", S2Point(2, 0, 0));
        add("nearly_unit_below",
            S2Point(1 - 1e-16, 0, 0));  // still within tolerance
        add("nearly_unit_above", S2Point(1 + 1e-16, 0, 0));
        out["is_unit_length"] = cases;
    }

    // TEST(S2PointUtil, ApproxEquals) - various angle tolerances.
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &a,
                       const S2Point &b, double max_error_radians) {
            bool expected =
                S2::ApproxEquals(a, b, S1Angle::Radians(max_error_radians));
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"max_error_radians", max_error_radians},
                             {"expected", expected}});
        };
        S2Point px(1, 0, 0);
        S2Point py(0, 1, 0);
        // Default tolerance is 1e-15 in C++.
        add("same_default", px, px, 1e-15);
        add("orthogonal_default", px, py, 1e-15);
        add("orthogonal_halfpi", px, py, M_PI_2 + 1e-15);
        add("antipodal_below_pi", px, S2Point(-1, 0, 0), M_PI - 1e-15);
        add("antipodal_above_pi", px, S2Point(-1, 0, 0), M_PI + 1e-15);

        S2Point nearly_px = S2Point(1, 1e-10, 0).Normalize();
        add("nearly_px_loose", px, nearly_px, 1e-9);
        add("nearly_px_tight", px, nearly_px, 1e-11);
        out["approx_equals"] = cases;
    }

    // TEST(S2PointUtil, Ortho) - orthogonality and unit length.
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &a) {
            S2Point ortho = S2::Ortho(a);
            S2Point neg_ortho = S2::Ortho(-a);
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"ortho", point_json(ortho)},
                             {"neg_a_ortho", point_json(neg_ortho)},
                             {"dot", a.DotProd(ortho)},
                             {"is_unit", S2::IsUnitLength(ortho)}});
        };
        add("x", S2Point(1, 0, 0));
        add("y", S2Point(0, 1, 0));
        add("z", S2Point(0, 0, 1));
        add("neg_x", S2Point(-1, 0, 0));
        add("neg_y", S2Point(0, -1, 0));
        add("neg_z", S2Point(0, 0, -1));
        add("diag", S2Point(1, 1, 1).Normalize());
        add("off_axis", S2Point(0.2, 0.5, -3.3).Normalize());
        out["ortho"] = cases;
    }

    // RefDir currently aliases S2::Ortho in C++. We cover it separately so
    // the OCaml module can expose both names.
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &a) {
            S2Point ref = S2::RefDir(a);
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"ref_dir", point_json(ref)},
                             {"is_unit", S2::IsUnitLength(ref)}});
        };
        add("x", S2Point(1, 0, 0));
        add("diag", S2Point(1, 1, 1).Normalize());
        add("off_axis", S2Point(0.2, 0.5, -3.3).Normalize());
        out["ref_dir"] = cases;
    }

    // TEST(S2, Rotate) - deterministic cases.
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &p,
                       const S2Point &axis, double angle_radians) {
            S2Point result =
                S2::Rotate(p, axis, S1Angle::Radians(angle_radians));
            cases.push_back({{"name", name},
                             {"p", point_json(p)},
                             {"axis", point_json(axis)},
                             {"angle_radians", angle_radians},
                             {"result", point_json(result)},
                             {"is_unit", S2::IsUnitLength(result)}});
        };
        S2Point px(1, 0, 0);
        S2Point py(0, 1, 0);
        S2Point pz(0, 0, 1);
        add("x_around_z_90", px, pz, M_PI_2);
        add("x_around_z_180", px, pz, M_PI);
        add("x_around_z_neg_90", px, pz, -M_PI_2);
        add("y_around_x_90", py, px, M_PI_2);
        add("z_around_y_90", pz, py, M_PI_2);
        add("x_around_x_zero", px, px, 0.0);
        add("x_around_x_45", px, px, M_PI_4);
        add("diag_around_z", S2Point(1, 1, 0).Normalize(), pz, M_PI_4);
        add("full_turn", px, pz, 2.0 * M_PI);
        out["rotate"] = cases;
    }

    // TEST(S2, Frames) - GetFrame/ToFrame/FromFrame round-trips.
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &z_in) {
            S2Point z = z_in.Normalize();
            Matrix3x3_d m = S2::GetFrame(z);
            S2Point col0 = m.Col(0);
            S2Point col1 = m.Col(1);
            S2Point col2 = m.Col(2);

            S2Point tf0 = S2::ToFrame(m, col0);
            S2Point tf1 = S2::ToFrame(m, col1);
            S2Point tf2 = S2::ToFrame(m, col2);

            S2Point ff0 = S2::FromFrame(m, S2Point(1, 0, 0));
            S2Point ff1 = S2::FromFrame(m, S2Point(0, 1, 0));
            S2Point ff2 = S2::FromFrame(m, S2Point(0, 0, 1));

            // A generic off-axis point and its round-trip through the frame.
            S2Point q(0.3, -0.7, 0.5);
            S2Point q_in_frame = S2::ToFrame(m, q);
            S2Point q_back = S2::FromFrame(m, q_in_frame);

            cases.push_back({{"name", name},
                             {"z", point_json(z)},
                             {"col0", point_json(col0)},
                             {"col1", point_json(col1)},
                             {"col2", point_json(col2)},
                             {"det", m.Det()},
                             {"to_frame_col0", point_json(tf0)},
                             {"to_frame_col1", point_json(tf1)},
                             {"to_frame_col2", point_json(tf2)},
                             {"from_frame_e0", point_json(ff0)},
                             {"from_frame_e1", point_json(ff1)},
                             {"from_frame_e2", point_json(ff2)},
                             {"q", point_json(q)},
                             {"q_in_frame", point_json(q_in_frame)},
                             {"q_back", point_json(q_back)}});
        };
        add("unit_x", S2Point(1, 0, 0));
        add("unit_y", S2Point(0, 1, 0));
        add("unit_z", S2Point(0, 0, 1));
        add("upstream_off_axis", S2Point(0.2, 0.5, -3.3));
        add("diagonal", S2Point(1, 1, 1));
        out["frames"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
