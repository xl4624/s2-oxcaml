// Golden data generator for R2Point.

#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/r2.h"

using json = nlohmann::json;

json point_json(const R2Point &p) {
    return json::array({p.x(), p.y()});
}

int main() {
    json out;

    // TEST(R2PointGolden, GoldenConstructors) - constructors (no dedicated
    // GTest)
    {
        json cases = json::array();
        cases.push_back({{"op", "zero"}, {"point", point_json(R2Point())}});
        cases.push_back({{"op", "create"},
                         {"x", 1.5},
                         {"y", -2.25},
                         {"point", point_json(R2Point(1.5, -2.25))}});
        cases.push_back({{"op", "create"},
                         {"x", 0.0},
                         {"y", 0.0},
                         {"point", point_json(R2Point(0.0, 0.0))}});
        // Distinct bit pattern: negative zero still compares equal in C++.
        cases.push_back(
            {{"op", "create"},
             {"x", std::copysign(0.0, -1.0)},
             {"y", 1.0},
             {"point", point_json(R2Point(std::copysign(0.0, -1.0), 1.0))}});
        out["constructors"] = cases;
    }

    // TEST(Go_r2_rect_test, TestDot) / TestCross - arithmetic
    {
        json cases = json::array();
        auto test = [&](const R2Point &a, const R2Point &b) {
            cases.push_back({{"p1", point_json(a)},
                             {"p2", point_json(b)},
                             {"add", point_json(a + b)},
                             {"sub", point_json(a - b)},
                             {"dot", a.DotProd(b)},
                             {"cross", a.CrossProd(b)}});
        };
        test(R2Point(1, 0), R2Point(0, 1));
        test(R2Point(1, 2), R2Point(3, 4));
        test(R2Point(-1.5, 2.5), R2Point(3.0, -4.0));
        test(R2Point(0, 0), R2Point(0, 0));
        test(R2Point(1e10, -1e10), R2Point(-1e10, 1e10));
        test(R2Point(0.1, 0.2), R2Point(0.3, 0.4));
        // Parallel / anti-parallel (cross/dot edge cases)
        test(R2Point(2, 3), R2Point(4, 6));
        test(R2Point(1, 1), R2Point(-2, -2));
        test(R2Point(3, 4), R2Point(-4, 3));
        test(R2Point(1.234, -5.456e-20), R2Point(2.1234e-20, 7.456));
        out["arithmetic"] = cases;
    }

    // TEST(R2PointGolden, GoldenMul) - mul
    {
        json cases = json::array();
        auto test = [&](const R2Point &p, double k) {
            cases.push_back({{"point", point_json(p)},
                             {"scalar", k},
                             {"result", point_json(p * k)}});
        };
        test(R2Point(1, 2), 3.0);
        test(R2Point(-1, 4), 0.5);
        test(R2Point(1, 1), 0.0);
        test(R2Point(3, -2), -1.0);
        test(R2Point(1e-15, 1e-15), 1e15);
        test(R2Point(2.5, -4.0), -0.25);
        test(R2Point(0, 0), 100.0);
        out["mul"] = cases;
    }

    // TEST(R2PointGolden, GoldenDiv) - div
    {
        json cases = json::array();
        auto test = [&](const R2Point &p, double k) {
            cases.push_back({{"point", point_json(p)},
                             {"scalar", k},
                             {"result", point_json(p / k)}});
        };
        test(R2Point(4, 6), 2.0);
        test(R2Point(-9, 15), 3.0);
        test(R2Point(1, 1), 4.0);
        test(R2Point(0, 0), 10.0);
        test(R2Point(1e-10, 2e-10), 1e-5);
        out["div"] = cases;
    }

    // TEST(R2PointGolden, GoldenNeg) - neg
    {
        json cases = json::array();
        auto test = [&](const R2Point &p) {
            cases.push_back(
                {{"point", point_json(p)}, {"result", point_json(-p)}});
        };
        test(R2Point(1, 2));
        test(R2Point(-3, 4));
        test(R2Point(0, 0));
        test(R2Point(1e-100, -1e100));
        out["neg"] = cases;
    }

    // TEST(Go_r2_rect_test, TestOrtho) - ortho
    {
        json cases = json::array();
        auto test = [&](const R2Point &p) {
            cases.push_back(
                {{"point", point_json(p)}, {"ortho", point_json(p.Ortho())}});
        };
        test(R2Point(1, 0));
        test(R2Point(0, 1));
        test(R2Point(1, 2));
        test(R2Point(-3, 4));
        test(R2Point(0, 0));
        test(R2Point(1e-50, -2e-50));
        out["ortho"] = cases;
    }

    // TEST(Go_r2_rect_test, TestNorm) - norm
    {
        json cases = json::array();
        auto test = [&](const R2Point &p) {
            cases.push_back({{"point", point_json(p)},
                             {"norm", p.Norm()},
                             {"norm2", p.Norm2()}});
        };
        test(R2Point(0, 0));
        test(R2Point(1, 0));
        test(R2Point(0, 1));
        test(R2Point(3, 4));
        test(R2Point(-3, 4));
        test(R2Point(1, 1));
        test(R2Point(1e-100, 0));
        test(R2Point(1e100, 0));
        test(R2Point(1e-200, 1e-200));
        test(R2Point(5, 12));
        out["norm"] = cases;
    }

    // TEST(Go_r2_rect_test, TestNormalize) - normalize
    {
        json cases = json::array();
        auto test = [&](const R2Point &p) {
            cases.push_back({{"point", point_json(p)},
                             {"normalized", point_json(p.Normalize())}});
        };
        test(R2Point(1, 0));
        test(R2Point(0, 1));
        test(R2Point(3, 4));
        test(R2Point(-3, 4));
        test(R2Point(1, 1));
        test(R2Point(0, 0));
        test(R2Point(1e-200, 1e-200));
        test(R2Point(-1e100, 0));
        test(R2Point(1, 1e-150));
        out["normalize"] = cases;
    }

    // TEST(R2PointGolden, GoldenAngle) - angle (Vector2::Angle)
    {
        json cases = json::array();
        auto test = [&](const R2Point &a, const R2Point &b) {
            cases.push_back({{"a", point_json(a)},
                             {"b", point_json(b)},
                             {"angle", a.Angle(b)}});
        };
        test(R2Point(1, 0), R2Point(0, 1));
        test(R2Point(1, 0), R2Point(1, 0));
        test(R2Point(1, 0), R2Point(-1, 0));
        test(R2Point(2, 0), R2Point(0, 2));
        test(R2Point(1, 1), R2Point(-1, 1));
        test(R2Point(3, 4), R2Point(3, 4));
        test(R2Point(0, 0), R2Point(1, 0));
        test(R2Point(1, 0), R2Point(0, 0));
        test(R2Point(1, 2), R2Point(4, 8));
        out["angle"] = cases;
    }

    // TEST(R2PointGolden, GoldenFabs) - fabs
    {
        json cases = json::array();
        auto test = [&](const R2Point &p) {
            cases.push_back(
                {{"point", point_json(p)}, {"fabs", point_json(p.Fabs())}});
        };
        test(R2Point(-3, -4));
        test(R2Point(-1, 2));
        test(R2Point(5, -6));
        test(R2Point(0, 0));
        test(R2Point(-0.0, 1.0));
        test(R2Point(-1e-100, 2e100));
        out["fabs"] = cases;
    }

    // TEST(R2PointGolden, GoldenEquality) - equality
    {
        json cases = json::array();
        auto test = [&](const R2Point &a, const R2Point &b) {
            cases.push_back({{"p1", point_json(a)},
                             {"p2", point_json(b)},
                             {"equal", a == b}});
        };
        test(R2Point(1, 2), R2Point(1, 2));
        test(R2Point(1, 2), R2Point(1, 3));
        test(R2Point(1, 2), R2Point(3, 2));
        test(R2Point(0, 0), R2Point(0, 0));
        test(R2Point(std::copysign(0.0, -1.0), 1.0), R2Point(0.0, 1.0));
        out["equality"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
