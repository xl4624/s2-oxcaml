// Golden data generator for S1ChordAngle.
// Mirrors s2geometry/src/s2/s1chord_angle_test.cc

#include <cfloat>
#include <cmath>
#include <iostream>
#include <limits>
#include <nlohmann/json.hpp>

#include "s2/s1angle.h"
#include "s2/s1chord_angle.h"

using json = nlohmann::json;

int main() {
    json out;

    // TEST(S1ChordAngle, ToFromS1Angle), TEST(S1ChordAngle, FromLength2),
    // TEST(S1ChordAngle, Zero), TEST(S1ChordAngle, Right),
    // TEST(S1ChordAngle, Straight), TEST(S1ChordAngle, Infinity),
    // TEST(S1ChordAngle, Negative)
    {
        json cases = json::array();

        // from_angle
        auto add_from_angle = [&](double radians) {
            S1ChordAngle c(S1Angle::Radians(radians));
            cases.push_back({{"op", "from_angle"},
                             {"input_radians", radians},
                             {"length2", c.length2()},
                             {"radians", c.radians()},
                             {"degrees", c.degrees()}});
        };
        add_from_angle(0.0);
        add_from_angle(M_PI);
        add_from_angle(M_PI_2);
        add_from_angle(1.0);
        add_from_angle(-1.0);
        add_from_angle(M_PI + 0.01);

        // infinity angle
        {
            S1ChordAngle c(S1Angle::Infinity());
            cases.push_back({{"op", "from_angle"},
                             {"input_radians", "infinity"},
                             {"length2", c.length2()},
                             {"is_infinity", c.is_infinity()}});
        }

        // from_length2
        auto add_from_length2 = [&](double len2) {
            S1ChordAngle c = S1ChordAngle::FromLength2(len2);
            cases.push_back({{"op", "from_length2"},
                             {"input", len2},
                             {"length2", c.length2()},
                             {"degrees", c.degrees()}});
        };
        add_from_length2(0.0);
        add_from_length2(1.0);
        add_from_length2(2.0);
        add_from_length2(4.0);
        add_from_length2(5.0);

        // special values
        {
            S1ChordAngle z = S1ChordAngle::Zero();
            cases.push_back({{"op", "zero"},
                             {"length2", z.length2()},
                             {"radians", z.radians()},
                             {"degrees", z.degrees()}});
        }
        {
            S1ChordAngle r = S1ChordAngle::Right();
            cases.push_back({{"op", "right"},
                             {"length2", r.length2()},
                             {"degrees", r.degrees()}});
        }
        {
            S1ChordAngle s = S1ChordAngle::Straight();
            cases.push_back({{"op", "straight"},
                             {"length2", s.length2()},
                             {"degrees", s.degrees()}});
        }

        out["constructors"] = cases;
    }

    // TEST(S1ChordAngle, Predicates)
    {
        json cases = json::array();
        auto add = [&](const std::string &name, S1ChordAngle c) {
            cases.push_back({{"name", name},
                             {"length2", c.length2()},
                             {"is_zero", c.is_zero()},
                             {"is_negative", c.is_negative()},
                             {"is_infinity", c.is_infinity()},
                             {"is_special", c.is_special()},
                             {"is_valid", c.is_valid()}});
        };
        add("zero", S1ChordAngle::Zero());
        add("right", S1ChordAngle::Right());
        add("straight", S1ChordAngle::Straight());
        add("negative", S1ChordAngle::Negative());
        add("infinity", S1ChordAngle::Infinity());
        add("degrees_45", S1ChordAngle::Degrees(45));
        out["predicates"] = cases;
    }

    // Comparison (implicit in C++ via operator==/</>)
    {
        json cases = json::array();
        auto add = [&](const std::string &a_name, S1ChordAngle a,
                       const std::string &b_name, S1ChordAngle b) {
            cases.push_back({
                {"a", a_name},
                {"b", b_name},
                {"a_length2", a.length2()},
                {"b_length2", b.length2()},
                {"compare", (a < b) ? -1 : ((a > b) ? 1 : 0)},
                {"equal", a == b},
            });
        };
        add("negative", S1ChordAngle::Negative(), "zero", S1ChordAngle::Zero());
        add("zero", S1ChordAngle::Zero(), "zero", S1ChordAngle::Zero());
        add("zero", S1ChordAngle::Zero(), "right", S1ChordAngle::Right());
        add("right", S1ChordAngle::Right(), "right", S1ChordAngle::Right());
        add("straight", S1ChordAngle::Straight(), "infinity",
            S1ChordAngle::Infinity());
        add("infinity", S1ChordAngle::Infinity(), "straight",
            S1ChordAngle::Straight());
        add("d30", S1ChordAngle::Degrees(30), "d60", S1ChordAngle::Degrees(60));
        add("d60", S1ChordAngle::Degrees(60), "d30", S1ChordAngle::Degrees(30));
        out["comparison"] = cases;
    }

    // TEST(S1ChordAngle, ToFromS1Angle)
    {
        json cases = json::array();
        auto add = [&](const std::string &name, S1ChordAngle c) {
            S1Angle a = c.ToAngle();
            cases.push_back({{"name", name},
                             {"length2", c.length2()},
                             {"radians", a.radians()},
                             {"degrees", a.degrees()},
                             {"is_inf", a == S1Angle::Infinity()}});
        };
        add("zero", S1ChordAngle::Zero());
        add("right", S1ChordAngle::Right());
        add("straight", S1ChordAngle::Straight());
        add("negative", S1ChordAngle::Negative());
        add("infinity", S1ChordAngle::Infinity());
        add("from_radians_1", S1ChordAngle(S1Angle::Radians(1.0)));
        out["to_angle"] = cases;
    }

    // TEST(S1ChordAngle, Successor), TEST(S1ChordAngle, Predecessor)
    {
        json cases = json::array();
        auto add = [&](const std::string &name, S1ChordAngle c) {
            S1ChordAngle s = c.Successor();
            S1ChordAngle p = c.Predecessor();
            cases.push_back({{"name", name},
                             {"length2", c.length2()},
                             {"successor_length2", s.length2()},
                             {"predecessor_length2", p.length2()}});
        };
        add("negative", S1ChordAngle::Negative());
        add("zero", S1ChordAngle::Zero());
        add("straight", S1ChordAngle::Straight());
        add("infinity", S1ChordAngle::Infinity());
        add("right", S1ChordAngle::Right());
        add("degrees_1", S1ChordAngle::Degrees(1));

        // Chain: 10 successors from negative
        {
            json chain = json::array();
            S1ChordAngle x = S1ChordAngle::Negative();
            for (int i = 0; i < 10; ++i) {
                S1ChordAngle next = x.Successor();
                chain.push_back({{"step", i},
                                 {"length2", x.length2()},
                                 {"successor_length2", next.length2()}});
                x = next;
            }
            out["successor_chain"] = chain;
        }

        // Chain: 10 predecessors from infinity
        {
            json chain = json::array();
            S1ChordAngle x = S1ChordAngle::Infinity();
            for (int i = 0; i < 10; ++i) {
                S1ChordAngle prev = x.Predecessor();
                chain.push_back({{"step", i},
                                 {"length2", x.length2()},
                                 {"predecessor_length2", prev.length2()}});
                x = prev;
            }
            out["predecessor_chain"] = chain;
        }

        out["successor_predecessor"] = cases;
    }

    // TEST(S1ChordAngle, Arithmetic)
    {
        json cases = json::array();
        auto add_case = [&](const std::string &a_name, S1ChordAngle a,
                            const std::string &b_name, S1ChordAngle b) {
            S1ChordAngle sum = a + b;
            S1ChordAngle diff = a - b;
            cases.push_back({{"a", a_name},
                             {"b", b_name},
                             {"a_length2", a.length2()},
                             {"b_length2", b.length2()},
                             {"sum_length2", sum.length2()},
                             {"sum_degrees", sum.degrees()},
                             {"diff_length2", diff.length2()},
                             {"diff_degrees", diff.degrees()}});
        };

        S1ChordAngle zero = S1ChordAngle::Zero();
        S1ChordAngle d30 = S1ChordAngle::Degrees(30);
        S1ChordAngle d60 = S1ChordAngle::Degrees(60);
        S1ChordAngle d90 = S1ChordAngle::Degrees(90);
        S1ChordAngle d120 = S1ChordAngle::Degrees(120);
        S1ChordAngle d180 = S1ChordAngle::Straight();

        add_case("zero", zero, "zero", zero);
        add_case("d60", d60, "zero", zero);
        add_case("zero", zero, "d60", d60);
        add_case("d30", d30, "d60", d60);
        add_case("d60", d60, "d30", d30);
        add_case("d90", d90, "d30", d30);
        add_case("d90", d90, "d60", d60);
        add_case("d90", d90, "d90", d90);
        add_case("d120", d120, "d90", d90);
        add_case("d120", d120, "d120", d120);
        add_case("d30", d30, "d180", d180);
        add_case("d180", d180, "d180", d180);
        add_case("d180", d180, "zero", zero);
        add_case("d60", d60, "d60", d60);
        add_case("d30", d30, "d90", d90);
        out["arithmetic"] = cases;
    }

    // TEST(S1ChordAngle, Trigonometry)
    {
        json cases = json::array();
        static constexpr int kIters = 20;
        for (int iter = 0; iter <= kIters; ++iter) {
            double radians = M_PI * iter / kIters;
            S1ChordAngle angle(S1Angle::Radians(radians));
            cases.push_back({{"radians", radians},
                             {"length2", angle.length2()},
                             {"sin", sin(angle)},
                             {"cos", cos(angle)},
                             {"tan", tan(angle)},
                             {"sin2", sin2(angle)}});
        }

        // Exact cases: 90 and 180
        {
            S1ChordAngle a90 = S1ChordAngle::FromLength2(2);
            S1ChordAngle a180 = S1ChordAngle::FromLength2(4);
            cases.push_back({{"radians", M_PI_2},
                             {"length2", a90.length2()},
                             {"sin", sin(a90)},
                             {"cos", cos(a90)},
                             {"tan", tan(a90)},
                             {"sin2", sin2(a90)},
                             {"exact", true}});
            cases.push_back({{"radians", M_PI},
                             {"length2", a180.length2()},
                             {"sin", sin(a180)},
                             {"cos", cos(a180)},
                             {"tan", tan(a180)},
                             {"sin2", sin2(a180)},
                             {"exact", true}});
        }
        out["trigonometry"] = cases;
    }

    // TEST(S1ChordAngle, PlusError)
    {
        json cases = json::array();
        auto add = [&](const std::string &name, S1ChordAngle c, double error) {
            S1ChordAngle result = c.PlusError(error);
            cases.push_back({{"name", name},
                             {"length2", c.length2()},
                             {"error", error},
                             {"result_length2", result.length2()}});
        };
        add("negative+5", S1ChordAngle::Negative(), 5);
        add("infinity-5", S1ChordAngle::Infinity(), -5);
        add("straight+5", S1ChordAngle::Straight(), 5);
        add("zero-5", S1ChordAngle::Zero(), -5);
        add("len1+0.25", S1ChordAngle::FromLength2(1), 0.25);
        add("len1-0.25", S1ChordAngle::FromLength2(1), -0.25);
        out["plus_error"] = cases;
    }

    // TEST(S1ChordAngle, GetS2PointConstructorMaxError)
    {
        json cases = json::array();
        auto add = [&](double len2) {
            S1ChordAngle c = S1ChordAngle::FromLength2(len2);
            cases.push_back(
                {{"length2", len2},
                 {"max_point_error", c.GetS2PointConstructorMaxError()},
                 {"max_angle_error", c.GetS1AngleConstructorMaxError()}});
        };
        add(0.0);
        add(1.0);
        add(2.0);
        add(4.0);
        add(0.5);
        add(3.75);
        out["error_bounds"] = cases;
    }

    // Convenience constructors (S1ChordAngle::Radians, Degrees, E5, E6, E7)
    {
        json cases = json::array();
        auto add_rad = [&](double r) {
            S1ChordAngle c = S1ChordAngle::Radians(r);
            cases.push_back(
                {{"op", "radians"}, {"input", r}, {"length2", c.length2()}});
        };
        add_rad(0.0);
        add_rad(1.0);
        add_rad(M_PI_2);
        add_rad(M_PI);

        auto add_deg = [&](double d) {
            S1ChordAngle c = S1ChordAngle::Degrees(d);
            cases.push_back(
                {{"op", "degrees"}, {"input", d}, {"length2", c.length2()}});
        };
        add_deg(0.0);
        add_deg(45.0);
        add_deg(90.0);
        add_deg(180.0);

        auto add_e5 = [&](int32_t e) {
            S1ChordAngle c = S1ChordAngle::E5(e);
            cases.push_back(
                {{"op", "e5"}, {"input", e}, {"length2", c.length2()}});
        };
        add_e5(0);
        add_e5(9000000);

        auto add_e6 = [&](int32_t e) {
            S1ChordAngle c = S1ChordAngle::E6(e);
            cases.push_back(
                {{"op", "e6"}, {"input", e}, {"length2", c.length2()}});
        };
        add_e6(0);
        add_e6(90000000);

        auto add_e7 = [&](int32_t e) {
            S1ChordAngle c = S1ChordAngle::E7(e);
            cases.push_back(
                {{"op", "e7"}, {"input", e}, {"length2", c.length2()}});
        };
        add_e7(0);
        add_e7(900000000);

        out["convenience"] = cases;
    }

    // FastUpperBoundFrom - cheap chord-angle bound from an S1Angle. Result is
    // a valid upper bound on the equivalent chord through the sphere; loose
    // for large angles but very fast (no sin).
    {
        json cases = json::array();
        auto add = [&](double radians) {
            S1Angle a = S1Angle::Radians(radians);
            S1ChordAngle c = S1ChordAngle::FastUpperBoundFrom(a);
            cases.push_back({{"radians", radians}, {"length2", c.length2()}});
        };
        add(0.0);
        add(1e-6);
        add(0.1);
        add(0.5);
        add(1.0);
        add(M_PI_2);
        add(M_PI);
        out["fast_upper_bound_from"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
