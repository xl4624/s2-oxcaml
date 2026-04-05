// Golden data generator for S1Angle.

#include "s2/s1angle.h"

#include <cmath>
#include <cstdint>
#include <iostream>
#include <nlohmann/json.hpp>
#include <random>

using json = nlohmann::json;

int main() {
    json out;

    // TEST(S1Angle, DefaultConstructor) … E5E6E7Representations,
    // PiRadiansExactly180Degrees, E6E7RepresentationsUnsigned - constructors
    {
        json cases = json::array();

        // TEST(S1Angle, DefaultConstructor)
        {
            S1Angle a;
            cases.push_back({{"op", "default"},
                             {"radians", a.radians()},
                             {"degrees", a.degrees()}});
        }

        // Radians
        auto add_rad = [&](double r) {
            S1Angle a = S1Angle::Radians(r);
            cases.push_back({
                {"op", "radians"},
                {"input", r},
                {"radians", a.radians()},
                {"degrees", a.degrees()},
            });
        };
        add_rad(M_PI);
        add_rad(M_PI_2);
        add_rad(-M_PI_4);
        add_rad(0.0);
        add_rad(1.0);
        add_rad(-1.0);

        // Degrees
        auto add_deg = [&](double d) {
            S1Angle a = S1Angle::Degrees(d);
            cases.push_back({
                {"op", "degrees"},
                {"input", d},
                {"radians", a.radians()},
                {"degrees", a.degrees()},
            });
        };
        add_deg(180.0);
        add_deg(90.0);
        add_deg(-45.0);
        add_deg(0.0);
        add_deg(60.0);
        add_deg(-90.0);

        // E5
        auto add_e5 = [&](int32_t e) {
            S1Angle a = S1Angle::E5(e);
            cases.push_back({
                {"op", "e5"},
                {"input", e},
                {"radians", a.radians()},
                {"degrees", a.degrees()},
            });
        };
        add_e5(-4500000);
        add_e5(18000000);
        add_e5(0);

        // E6
        auto add_e6 = [&](int32_t e) {
            S1Angle a = S1Angle::E6(e);
            cases.push_back({
                {"op", "e6"},
                {"input", e},
                {"radians", a.radians()},
                {"degrees", a.degrees()},
            });
        };
        add_e6(-60000000);
        add_e6(180000000);
        add_e6(0);

        // E7
        auto add_e7 = [&](int32_t e) {
            S1Angle a = S1Angle::E7(e);
            cases.push_back({
                {"op", "e7"},
                {"input", e},
                {"radians", a.radians()},
                {"degrees", a.degrees()},
            });
        };
        add_e7(750000000);
        add_e7(-1800000000);
        add_e7(0);

        // TEST(S1Angle, E6E7RepresentationsUnsigned)
        auto add_u6 = [&](uint32_t u) {
            S1Angle a = S1Angle::UnsignedE6(u);
            cases.push_back({
                {"op", "unsigned_e6"},
                {"unsigned", u},
                {"radians", a.radians()},
                {"degrees", a.degrees()},
            });
        };
        add_u6(static_cast<uint32_t>(60000000));
        add_u6(static_cast<uint32_t>(static_cast<int32_t>(-60000000)));

        auto add_u7 = [&](uint32_t u) {
            S1Angle a = S1Angle::UnsignedE7(u);
            cases.push_back({
                {"op", "unsigned_e7"},
                {"unsigned", u},
                {"radians", a.radians()},
                {"degrees", a.degrees()},
            });
        };
        add_u7(static_cast<uint32_t>(750000000));
        add_u7(static_cast<uint32_t>(static_cast<int32_t>(-750000000)));

        out["constructors"] = cases;
    }

    // TEST(S1Angle, E5E6E7Representations) - to_e5_e6_e7
    {
        json cases = json::array();
        auto test = [&](double deg) {
            S1Angle a = S1Angle::Degrees(deg);
            cases.push_back({
                {"degrees", deg},
                {"e5", a.e5()},
                {"e6", a.e6()},
                {"e7", a.e7()},
            });
        };
        test(-172.56123);
        test(12.345678);
        test(-12.3456789);
        test(0.0);
        test(180.0);
        test(-180.0);
        test(45.0);
        test(-90.0);
        test(1.0);
        out["to_e5_e6_e7"] = cases;
    }

    // TEST(S1Angle, Zero) / TEST(S1Angle, Infinity) - special_values
    {
        json cases = json::object();
        cases["zero_radians"] = S1Angle::Zero().radians();
        cases["infinity_radians"] = S1Angle::Infinity().radians();
        out["special_values"] = cases;
    }

    // TEST(S1Angle, ArithmeticOperationsOnAngles) - arithmetic
    {
        json cases = json::array();

        // abs
        {
            S1Angle a = S1Angle::Radians(-0.3);
            cases.push_back({
                {"op", "abs"},
                {"input", a.radians()},
                {"result", a.abs().radians()},
            });
        }

        // negate
        {
            S1Angle a = S1Angle::Radians(0.1);
            cases.push_back({
                {"op", "negate"},
                {"input", a.radians()},
                {"result", (-a).radians()},
            });
        }

        // add
        {
            S1Angle a = S1Angle::Radians(0.1);
            S1Angle b = S1Angle::Radians(0.3);
            cases.push_back({
                {"op", "add"},
                {"a", a.radians()},
                {"b", b.radians()},
                {"result", (a + b).radians()},
            });
        }

        // sub
        {
            S1Angle a = S1Angle::Radians(0.1);
            S1Angle b = S1Angle::Radians(0.3);
            cases.push_back({
                {"op", "sub"},
                {"a", a.radians()},
                {"b", b.radians()},
                {"result", (a - b).radians()},
            });
        }

        // mul
        {
            S1Angle a = S1Angle::Radians(0.3);
            double m = 2.0;
            cases.push_back({
                {"op", "mul"},
                {"a", a.radians()},
                {"m", m},
                {"result", (a * m).radians()},
            });
        }

        // div by scalar
        {
            S1Angle a = S1Angle::Radians(0.3);
            double m = 2.0;
            cases.push_back({
                {"op", "div"},
                {"a", a.radians()},
                {"m", m},
                {"result", (a / m).radians()},
            });
        }

        // div by angle
        {
            S1Angle a = S1Angle::Radians(0.3);
            S1Angle b = S1Angle::Radians(0.6);
            cases.push_back({
                {"op", "div_angle"},
                {"a", a.radians()},
                {"b", b.radians()},
                {"result", a / b},
            });
        }

        out["arithmetic"] = cases;
    }

    // TEST(S1Angle, NormalizeCorrectlyCanonicalizesAngles) - normalized
    {
        json cases = json::array();
        auto test = [&](double deg) {
            S1Angle a = S1Angle::Degrees(deg);
            cases.push_back({
                {"input_degrees", deg},
                {"result_degrees", a.Normalized().degrees()},
                {"result_radians", a.Normalized().radians()},
            });
        };
        test(360.0);
        test(-90.0);
        test(-180.0);
        test(180.0);
        test(540.0);
        test(-270.0);
        test(0.0);
        test(45.0);
        test(-360.0);
        out["normalized"] = cases;
    }

    // TEST(S1Angle, Trigonometry) - trigonometry
    {
        json cases = json::array();
        auto test = [&](double deg) {
            S1Angle a = S1Angle::Degrees(deg);
            cases.push_back({
                {"degrees", deg},
                {"sin", sin(a)},
                {"cos", cos(a)},
                {"tan", tan(a)},
            });
        };
        test(0.0);
        test(30.0);
        test(45.0);
        test(60.0);
        test(90.0);
        test(180.0);
        test(-45.0);
        test(-90.0);
        out["trigonometry"] = cases;
    }

    // TEST(S1Angle, DegreesVsE6) - degrees_vs_e6 (exact: Degrees(n) ==
    // E6(1000000*n))
    {
        json cases = json::array();
        for (int i = 0; i <= 180; ++i) {
            S1Angle from_deg = S1Angle::Degrees(i);
            S1Angle from_e6 = S1Angle::E6(1000000 * i);
            cases.push_back({
                {"n", i},
                {"deg_radians", from_deg.radians()},
                {"e6_radians", from_e6.radians()},
                {"equal", from_deg == from_e6},
            });
        }
        out["degrees_vs_e6"] = cases;
    }

    // TEST(S1Angle, DegreesVsE7) - degrees_vs_e7 (exact: Degrees(n) ==
    // E7(10000000*n))
    {
        json cases = json::array();
        for (int i = 0; i <= 180; ++i) {
            S1Angle from_deg = S1Angle::Degrees(i);
            S1Angle from_e7 = S1Angle::E7(10000000 * i);
            cases.push_back({
                {"n", i},
                {"deg_radians", from_deg.radians()},
                {"e7_radians", from_e7.radians()},
                {"equal", from_deg == from_e7},
            });
        }
        out["degrees_vs_e7"] = cases;
    }

    // TEST(S1Angle, DegreesVsRadians) - degrees_vs_radians
    {
        json cases = json::array();
        for (int k = -8; k <= 8; ++k) {
            S1Angle from_deg = S1Angle::Degrees(45 * k);
            S1Angle from_rad = S1Angle::Radians(k * M_PI / 4);
            cases.push_back({
                {"k", k},
                {"deg_radians", from_deg.radians()},
                {"rad_radians", from_rad.radians()},
                {"equal", from_deg == from_rad},
                {"degrees_exact", from_deg.degrees()},
                {"expected_degrees", 45.0 * k},
            });
        }
        out["degrees_vs_radians"] = cases;
    }

    // TEST(S1Angle, Trigonometry) - sin_cos (SinCos vs sin/cos per degree)
    {
        json cases = json::array();
        for (int k = -1000; k <= 1000; ++k) {
            S1Angle a = S1Angle::Degrees(k);
            auto sc = a.SinCos();
            cases.push_back({{"degrees", k}, {"sin", sc.sin}, {"cos", sc.cos}});
        }
        out["sin_cos"] = cases;
    }

    // TEST(S1Angle, E6VsE7) - e6_vs_e7
    {
        std::mt19937 gen(12345);
        std::uniform_int_distribution<int> dist(0, 179999999);
        json cases = json::array();
        for (int iter = 0; iter < 1000; ++iter) {
            int i = dist(gen);
            cases.push_back({{"e6_input", i},
                             {"equal", S1Angle::E6(i) == S1Angle::E7(10 * i)}});
        }
        out["e6_vs_e7"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
