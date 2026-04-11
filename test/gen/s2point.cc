// Golden data generator for S2Point.
// Mirrors s2geometry/src/s2/s2point_test.cc and s2pointutil_test.cc.

#include "s2/s2point.h"

#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/s1angle.h"
#include "s2/s2edge_crossings.h"
#include "s2/s2pointutil.h"
#include "s2/util/math/matrix3x3.h"

using json = nlohmann::json;

json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

int main() {
    json out;

    // TEST(S2, Origin) - origin
    {
        json cases = json::object();
        S2Point o = S2::Origin();
        cases["origin"] = point_json(o);
        cases["is_unit_length"] = S2::IsUnitLength(o);
        out["origin"] = cases;
    }

    // TEST(S2PointUtil, IsUnitLength) - is_unit_length
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &p,
                       bool expected) {
            cases.push_back(
                {{"name", name}, {"p", point_json(p)}, {"expected", expected}});
        };
        add("unit_x", S2Point(1, 0, 0), S2::IsUnitLength(S2Point(1, 0, 0)));
        add("unit_y", S2Point(0, 1, 0), S2::IsUnitLength(S2Point(0, 1, 0)));
        add("unit_z", S2Point(0, 0, 1), S2::IsUnitLength(S2Point(0, 0, 1)));
        add("origin", S2::Origin(), S2::IsUnitLength(S2::Origin()));
        add("zero", S2Point(0, 0, 0), S2::IsUnitLength(S2Point(0, 0, 0)));
        add("unnorm", S2Point(1, 1, 1), S2::IsUnitLength(S2Point(1, 1, 1)));
        add("nearly_unit", S2Point(1, 1e-16, 0),
            S2::IsUnitLength(S2Point(1, 1e-16, 0)));
        add("diag_norm", S2Point(1, 1, 1).Normalize(),
            S2::IsUnitLength(S2Point(1, 1, 1).Normalize()));
        out["is_unit_length"] = cases;
    }

    // TEST(S2PointUtil, ApproxEquals) - approx_equal
    {
        json cases = json::array();
        auto add = [&](const S2Point &a, const S2Point &b, double max_error,
                       bool expected) {
            cases.push_back({{"a", point_json(a)},
                             {"b", point_json(b)},
                             {"max_error", max_error},
                             {"expected", expected}});
        };
        S2Point px(1, 0, 0);
        S2Point py(0, 1, 0);
        S2Point pz(0, 0, 1);

        add(px, px, 1e-15, true);
        add(px, py, 1e-15, false);
        add(px, py, M_PI_2 + 1e-15, true);
        add(px, S2Point(-1, 0, 0), M_PI - 1e-15, false);
        add(px, S2Point(-1, 0, 0), M_PI + 1e-15, true);

        S2Point nearly_px = S2Point(1, 1e-10, 0).Normalize();
        add(px, nearly_px, 1e-9, true);
        add(px, nearly_px, 1e-11, false);

        out["approx_equal"] = cases;
    }

    // TEST(S2PointUtil, Ortho) - ortho
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &a) {
            S2Point ortho = S2::Ortho(a);
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"ortho", point_json(ortho)},
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

    // TEST(S2Point, PointCross / RobustCrossProd) - robust_cross_prod
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &a,
                       const S2Point &b) {
            S2Point result = S2::RobustCrossProd(a, b);
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"result", point_json(result)}});
        };
        S2Point px(1, 0, 0);
        S2Point py(0, 1, 0);
        S2Point pz(0, 0, 1);
        add("x_cross_y", px, py);
        add("y_cross_z", py, pz);
        add("z_cross_x", pz, px);
        add("same_point", px, px);
        add("antipodal", px, S2Point(-1, 0, 0));
        add("nearly_parallel", px, S2Point(1, 1e-20, 0).Normalize());
        add("general", S2Point(1, 2, 3).Normalize(),
            S2Point(4, 5, 6).Normalize());
        out["robust_cross_prod"] = cases;
    }

    // TEST(S2, Frames) - frames
    {
        json frame_cases = json::array();
        auto add_frame = [&](const std::string &name, const S2Point &z) {
            Matrix3x3_d m = S2::GetFrame(z);
            S2Point col0 = m.Col(0);
            S2Point col1 = m.Col(1);
            S2Point col2 = m.Col(2);

            // to_frame: basis vectors should map to standard basis
            S2Point tf0 = S2::ToFrame(m, col0);
            S2Point tf1 = S2::ToFrame(m, col1);
            S2Point tf2 = S2::ToFrame(m, col2);

            // from_frame: standard basis should map back to columns
            S2Point ff0 = S2::FromFrame(m, S2Point(1, 0, 0));
            S2Point ff1 = S2::FromFrame(m, S2Point(0, 1, 0));
            S2Point ff2 = S2::FromFrame(m, S2Point(0, 0, 1));

            frame_cases.push_back({{"name", name},
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
                                   {"from_frame_e2", point_json(ff2)}});
        };
        add_frame("unit_x", S2Point(1, 0, 0));
        add_frame("unit_y", S2Point(0, 1, 0));
        add_frame("unit_z", S2Point(0, 0, 1));
        add_frame("diagonal", S2Point(1, 1, 1).Normalize());
        add_frame("off_axis", S2Point(0.2, 0.5, -3.3).Normalize());
        out["frames"] = frame_cases;
    }

    // TEST(S2, Rotate) - rotate
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &p,
                       const S2Point &axis, double angle_rad) {
            S2Point result = S2::Rotate(p, axis, S1Angle::Radians(angle_rad));
            cases.push_back({{"name", name},
                             {"p", point_json(p)},
                             {"axis", point_json(axis)},
                             {"angle_radians", angle_rad},
                             {"result", point_json(result)},
                             {"is_unit", S2::IsUnitLength(result)}});
        };
        S2Point px(1, 0, 0);
        S2Point py(0, 1, 0);
        S2Point pz(0, 0, 1);

        add("x_around_z_90", px, pz, M_PI_2);
        add("x_around_z_180", px, pz, M_PI);
        add("x_around_z_270", px, pz, 3.0 * M_PI_2);
        add("y_around_x_90", py, px, M_PI_2);
        add("z_around_y_90", pz, py, M_PI_2);
        add("x_around_x_45", px, px, M_PI_4);
        add("diag_around_z", S2Point(1, 1, 0).Normalize(), pz, M_PI_4);
        add("zero_angle", px, pz, 0.0);
        add("full_turn", px, pz, 2.0 * M_PI);
        add("negative_angle", px, pz, -M_PI_2);
        out["rotate"] = cases;
    }

    // distance (as angle) - distance
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &a,
                       const S2Point &b) {
            double angle = S1Angle(a, b).radians();
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"angle_radians", angle}});
        };
        S2Point px(1, 0, 0);
        S2Point py(0, 1, 0);
        S2Point pz(0, 0, 1);

        add("same", px, px);
        add("orthogonal", px, py);
        add("antipodal", px, S2Point(-1, 0, 0));
        add("general", S2Point(1, 2, 3).Normalize(),
            S2Point(4, 5, 6).Normalize());
        add("close", px, S2Point(1, 1e-10, 0).Normalize());
        out["distance"] = cases;
    }

    // point_from_coords (Go-style: normalizes input) - from_coords
    {
        json cases = json::array();
        auto add = [&](double x, double y, double z) {
            S2Point raw(x, y, z);
            S2Point result;
            if (x == 0 && y == 0 && z == 0) {
                result = S2::Origin();
            } else {
                result = raw.Normalize();
            }
            cases.push_back(
                {{"x", x}, {"y", y}, {"z", z}, {"result", point_json(result)}});
        };
        add(1, 0, 0);
        add(0, 1, 0);
        add(0, 0, 1);
        add(1, 1, 1);
        add(-1, -1, -1);
        add(3, 4, 0);
        add(0, 0, 0);
        add(1e-100, 1e-100, 1e-100);
        out["from_coords"] = cases;
    }

    // chord_angle_between_points
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &a,
                       const S2Point &b) {
            double len2 = std::min(4.0, (a - b).Norm2());
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"length2", len2}});
        };
        S2Point px(1, 0, 0);
        S2Point py(0, 1, 0);
        S2Point pz(0, 0, 1);
        add("same", px, px);
        add("orthogonal", px, py);
        add("antipodal", px, S2Point(-1, 0, 0));
        add("general", S2Point(1, 2, 3).Normalize(),
            S2Point(4, 5, 6).Normalize());
        out["chord_angle_between"] = cases;
    }

    // stable_angle (Kahan formula) - stable_angle
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S2Point &a,
                       const S2Point &b) {
            double angle = 2 * atan2((a - b).Norm(), (a + b).Norm());
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"angle_radians", angle}});
        };
        S2Point px(1, 0, 0);
        S2Point py(0, 1, 0);
        add("same", px, px);
        add("orthogonal", px, py);
        add("antipodal", px, S2Point(-1, 0, 0));
        add("nearly_same", px, S2Point(1, 1e-15, 0).Normalize());
        add("nearly_antipodal", px, S2Point(-1, 1e-15, 0).Normalize());
        out["stable_angle"] = cases;
    }

    // TEST(S2Point, SubtractionWorks) / operator+ / operator- / operator* /
    // operator/ / unary operator-
    {
        json cases = json::array();
        auto add_case = [&](const std::string &name, const S2Point &a,
                            const S2Point &b, double k) {
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"k", k},
                             {"add", point_json(a + b)},
                             {"sub", point_json(a - b)},
                             {"mul", point_json(a * k)},
                             {"neg", point_json(-a)}});
        };
        add_case("basic", S2Point(1, 2, 3), S2Point(4, -1, 2), 2.5);
        add_case("zeros", S2Point(0, 0, 0), S2Point(1, 1, 1), -3.0);
        add_case("mixed_signs", S2Point(-1.5, 2.5, -3.5),
                 S2Point(0.5, -1.0, 2.0), 0.25);
        add_case("unit_like", S2Point(1, 1, 1).Normalize(),
                 S2Point(1, -1, 0).Normalize(), 4.0);
        out["arith"] = cases;
    }

    // TEST(S2Point, ElementWiseDivisionWorks) - mul_components and
    // div_components
    {
        json cases = json::array();
        auto add_case = [&](const std::string &name, const S2Point &a,
                            const S2Point &b) {
            cases.push_back(
                {{"name", name},
                 {"a", point_json(a)},
                 {"b", point_json(b)},
                 {"mul_components", point_json(a.MulComponents(b))},
                 {"div_components", point_json(a.DivComponents(b))}});
        };
        add_case("basic", S2Point(2, 4, 6), S2Point(1, 2, 3));
        add_case("fractions", S2Point(0.5, -0.25, 1.5), S2Point(2, -4, 0.5));
        add_case("mixed", S2Point(-3, 5, -7), S2Point(2, -2, 4));
        out["components"] = cases;
    }

    // Component-wise Max and Min - max / min
    {
        json cases = json::array();
        auto add_case = [&](const std::string &name, const S2Point &a,
                            const S2Point &b) {
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"max", point_json(Max(a, b))},
                             {"min", point_json(Min(a, b))}});
        };
        add_case("basic", S2Point(1, 5, -3), S2Point(2, 3, -1));
        add_case("equal", S2Point(1, 2, 3), S2Point(1, 2, 3));
        add_case("negatives", S2Point(-2, -5, -1), S2Point(-3, -4, 0));
        add_case("crossed", S2Point(1, -2, 3), S2Point(-1, 2, -3));
        out["minmax"] = cases;
    }

    // TEST(S2Point, SqrtWorks) - sqrt
    {
        json cases = json::array();
        auto add_case = [&](const std::string &name, const S2Point &a) {
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"sqrt", point_json(a.Sqrt())}});
        };
        add_case("basic", S2Point(4, 9, 16));
        add_case("fractions", S2Point(0.25, 2.0, 1e-4));
        add_case("zeros", S2Point(0, 1, 0));
        out["sqrt"] = cases;
    }

    // TEST(S2Point, FloorWorks) - floor
    {
        json cases = json::array();
        auto add_case = [&](const std::string &name, const S2Point &a) {
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"floor", point_json(a.Floor())}});
        };
        add_case("basic", S2Point(1.2, 3.9, -0.5));
        add_case("integer", S2Point(2.0, -5.0, 0.0));
        add_case("negative", S2Point(-1.1, -2.5, -3.9));
        out["floor"] = cases;
    }

    // TEST(S2Point, CeilWorks) - ceil
    {
        json cases = json::array();
        auto add_case = [&](const std::string &name, const S2Point &a) {
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"ceil", point_json(a.Ceil())}});
        };
        add_case("basic", S2Point(1.2, 3.9, -0.5));
        add_case("integer", S2Point(2.0, -5.0, 0.0));
        add_case("negative", S2Point(-1.1, -2.5, -3.9));
        out["ceil"] = cases;
    }

    // TEST(S2Point, FRoundWorks) - fround (rint, banker's rounding)
    {
        json cases = json::array();
        auto add_case = [&](const std::string &name, const S2Point &a) {
            cases.push_back({{"name", name},
                             {"a", point_json(a)},
                             {"fround", point_json(a.FRound())}});
        };
        add_case("basic", S2Point(1.2, 3.7, -0.6));
        add_case("halves", S2Point(0.5, 1.5, 2.5));
        add_case("negatives", S2Point(-0.5, -1.5, -2.5));
        add_case("integer", S2Point(2.0, -3.0, 0.0));
        out["fround"] = cases;
    }

    // NaN construction - nan / is_nan
    {
        json cases = json::object();
        S2Point n = S2Point::NaN();
        cases["nan"] = point_json(n);
        cases["nan_is_nan"] =
            std::isnan(n.x()) && std::isnan(n.y()) && std::isnan(n.z());
        cases["finite_is_nan"] = false;
        cases["finite"] = point_json(S2Point(1, 2, 3));
        out["nan"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
