// Golden data generator for R1Interval.

#include "s2/r1interval.h"

#include <cfloat>
#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

json interval_json(const R1Interval &i) {
    return json::array({i.lo(), i.hi()});
}

int main() {
    // Named intervals used throughout
    R1Interval empty = R1Interval::Empty();
    R1Interval unit(0, 1);
    R1Interval negunit(-1, 0);
    R1Interval half(0.5, 0.5);

    json out;

    // TEST(R1Interval, TestBasic) - constructors
    {
        json cases = json::array();
        // FromPoint
        auto fp = R1Interval::FromPoint(3.0);
        cases.push_back({{"op", "from_point"},
                         {"input", 3.0},
                         {"expected", interval_json(fp)}});

        // FromPointPair
        auto pp1 = R1Interval::FromPointPair(4, 4);
        auto pp2 = R1Interval::FromPointPair(-1, -2);
        auto pp3 = R1Interval::FromPointPair(-5, 3);
        cases.push_back({{"op", "from_point_pair"},
                         {"p1", 4},
                         {"p2", 4},
                         {"expected", interval_json(pp1)}});
        cases.push_back({{"op", "from_point_pair"},
                         {"p1", -1},
                         {"p2", -2},
                         {"expected", interval_json(pp2)}});
        cases.push_back({{"op", "from_point_pair"},
                         {"p1", -5},
                         {"p2", 3},
                         {"expected", interval_json(pp3)}});

        // R1Interval(lo, hi)
        cases.push_back({{"op", "create"},
                         {"lo", 0.0},
                         {"hi", 10.0},
                         {"expected", interval_json(R1Interval(0, 10))}});
        cases.push_back({{"op", "create"},
                         {"lo", -10.0},
                         {"hi", 10.0},
                         {"expected", interval_json(R1Interval(-10, 10))}});
        out["constructors"] = cases;
    }

    // TEST(R1Interval, TestBasic) - accessors
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const R1Interval &i) {
            cases.push_back({
                {"interval", name},
                {"lo", i.lo()},
                {"hi", i.hi()},
                {"is_empty", i.is_empty()},
                {"center", i.GetCenter()},
                {"length", i.GetLength()},
            });
        };
        add("unit", unit);
        add("negunit", negunit);
        add("half", half);
        add("empty", empty);
        out["accessors"] = cases;
    }

    // TEST(R1Interval, TestBasic) - contains_point
    {
        json cases = json::array();
        auto test = [&](const std::string &name, const R1Interval &i,
                        double p) {
            cases.push_back({
                {"interval", name},
                {"point", p},
                {"contains", i.Contains(p)},
                {"interior_contains", i.InteriorContains(p)},
            });
        };
        test("unit", unit, 0.5);
        test("unit", unit, 0.0);
        test("unit", unit, 1.0);
        test("unit", unit, -0.1);
        test("unit", unit, 1.1);
        test("half", half, 0.5);
        test("half", half, 0.0);
        test("empty", empty, 0.0);
        out["contains_point"] = cases;
    }

    // TEST(R1Interval, TestBasic) - interval_ops (contains, interior_contains,
    // intersects, interior_intersects)
    {
        json cases = json::array();
        auto test = [&](const std::string &xn, const R1Interval &x,
                        const std::string &yn, const R1Interval &y) {
            cases.push_back({
                {"x", xn},
                {"x_val", interval_json(x)},
                {"y", yn},
                {"y_val", interval_json(y)},
                {"contains", x.Contains(y)},
                {"interior_contains", x.InteriorContains(y)},
                {"intersects", x.Intersects(y)},
                {"interior_intersects", x.InteriorIntersects(y)},
                {"union", interval_json(x.Union(y))},
                {"intersection", interval_json(x.Intersection(y))},
            });
        };
        test("empty", empty, "empty", empty);
        test("empty", empty, "unit", unit);
        test("unit", unit, "half", half);
        test("unit", unit, "unit", unit);
        test("unit", unit, "empty", empty);
        test("unit", unit, "negunit", negunit);
        test("unit", unit, "[0,0.5]", R1Interval(0, 0.5));
        test("half", half, "[0,0.5]", R1Interval(0, 0.5));
        test("negunit", negunit, "unit", unit);
        test("negunit", negunit, "half", half);
        test("[0,10]", R1Interval(0, 10), "[-5,5]", R1Interval(-5, 5));
        test("[99,100]", R1Interval(99, 100), "empty", empty);
        test("empty", empty, "[99,100]", R1Interval(99, 100));
        test("(5,3)", R1Interval(5, 3), "(0,-2)", R1Interval(0, -2));
        test("(0,-2)", R1Interval(0, -2), "(5,3)", R1Interval(5, 3));
        test("half", half, "unit", unit);
        out["interval_ops"] = cases;
    }

    // TEST(R1Interval, TestBasic) - add_point
    {
        json cases = json::array();
        R1Interval r = empty;
        r.AddPoint(5);
        cases.push_back({{"after_adding", 5}, {"lo", r.lo()}, {"hi", r.hi()}});
        r.AddPoint(-1);
        cases.push_back({{"after_adding", -1}, {"lo", r.lo()}, {"hi", r.hi()}});
        r.AddPoint(0);
        cases.push_back({{"after_adding", 0}, {"lo", r.lo()}, {"hi", r.hi()}});
        r.AddPoint(10);
        cases.push_back({{"after_adding", 10}, {"lo", r.lo()}, {"hi", r.hi()}});
        out["add_point"] = cases;
    }

    // TEST(R1Interval, TestBasic) - project
    {
        json cases = json::array();
        auto test = [&](double lo, double hi, double p) {
            R1Interval i(lo, hi);
            cases.push_back({
                {"interval", interval_json(i)},
                {"point", p},
                {"projected", i.Project(p)},
            });
        };
        test(0.1, 0.4, 0.3);
        test(0.1, 0.4, -7.0);
        test(0.1, 0.4, 0.6);
        test(-1.0, 1.0, 0.0);
        test(-1.0, 1.0, 5.0);
        test(-1.0, 1.0, -5.0);
        out["project"] = cases;
    }

    // TEST(R1Interval, TestBasic) - expanded
    {
        json cases = json::array();
        auto test = [&](const std::string &name, const R1Interval &i,
                        double margin) {
            auto e = i.Expanded(margin);
            cases.push_back({
                {"interval", name},
                {"interval_val", interval_json(i)},
                {"margin", margin},
                {"result", interval_json(e)},
                {"result_is_empty", e.is_empty()},
            });
        };
        test("empty", empty, 0.45);
        test("unit", unit, 0.5);
        test("unit", unit, -0.5);
        test("unit", unit, -0.51);
        test("[1,3]", R1Interval(1, 3), 1.0);
        out["expanded"] = cases;
    }

    // TEST(R1Interval, TestBasic) - expanded_chain (shrink then expand stays
    // empty)
    {
        json chain_cases = json::array();
        R1Interval u(0, 1);
        R1Interval e1 = u.Expanded(-0.51);
        R1Interval e2 = e1.Expanded(0.51);
        chain_cases.push_back({
            {"first_margin", -0.51},
            {"second_margin", 0.51},
            {"after_first", interval_json(e1)},
            {"after_first_empty", e1.is_empty()},
            {"final", interval_json(e2)},
            {"final_empty", e2.is_empty()},
        });
        out["expanded_chain"] = chain_cases;
    }

    // TEST(R1Interval, TestBasic) - equal (operator== / operator!= semantics)
    {
        json cases = json::array();
        auto add = [&](const R1Interval &x, const R1Interval &y) {
            cases.push_back({{"x", interval_json(x)},
                             {"y", interval_json(y)},
                             {"equal", x == y}});
        };
        add(empty, empty);
        add(unit, unit);
        add(unit, empty);
        add(R1Interval(1, 2), R1Interval(1, 3));
        R1Interval default_empty;
        add(empty, default_empty);
        add(R1Interval(5, 3), R1Interval(0, -2));
        out["equal"] = cases;
    }

    // TEST(R1IntervalGolden, DirectedHausdorff) - extension (not in
    // r1interval_test.cc)
    {
        json cases = json::array();
        auto test = [&](const R1Interval &x, const R1Interval &y) {
            cases.push_back({
                {"x", interval_json(x)},
                {"y", interval_json(y)},
                {"distance", x.GetDirectedHausdorffDistance(y)},
            });
        };
        test(empty, empty);
        test(empty, unit);
        test(unit, empty);
        test(unit, unit);
        test(unit, negunit);
        test(R1Interval(0, 4), R1Interval(1, 2));
        test(R1Interval(1, 2), R1Interval(0, 4));
        out["directed_hausdorff"] = cases;
    }

    // TEST(R1Interval, ApproxEquals) - approx_equal
    {
        double kLo = 4 * DBL_EPSILON;
        double kHi = 6 * DBL_EPSILON;
        json cases = json::array();
        auto test = [&](const R1Interval &x, const R1Interval &y) {
            cases.push_back({
                {"x", interval_json(x)},
                {"y", interval_json(y)},
                {"approx_equal", x.ApproxEquals(y)},
            });
        };
        test(empty, empty);
        test(R1Interval(0, 0), empty);
        test(empty, R1Interval(0, 0));
        test(R1Interval(1, 1), empty);
        test(empty, R1Interval(1, 1));
        test(empty, R1Interval(0, 1));
        test(empty, R1Interval(1, 1 + 2 * kLo));
        test(empty, R1Interval(1, 1 + 2 * kHi));
        test(R1Interval(1, 1), R1Interval(1, 1));
        test(R1Interval(1, 1), R1Interval(1 - kLo, 1 - kLo));
        test(R1Interval(1, 1), R1Interval(1 + kLo, 1 + kLo));
        test(R1Interval(1, 1), R1Interval(1 - kHi, 1));
        test(R1Interval(1, 1), R1Interval(1, 1 + kHi));
        test(R1Interval(1, 1), R1Interval(1 - kLo, 1 + kLo));
        test(R1Interval(0, 0), R1Interval(1, 1));
        test(R1Interval(1 - kLo, 2 + kLo), R1Interval(1, 2));
        test(R1Interval(1 + kLo, 2 - kLo), R1Interval(1, 2));
        test(R1Interval(1 - kHi, 2 + kLo), R1Interval(1, 2));
        test(R1Interval(1 + kHi, 2 - kLo), R1Interval(1, 2));
        test(R1Interval(1 - kLo, 2 + kHi), R1Interval(1, 2));
        test(R1Interval(1 + kLo, 2 - kHi), R1Interval(1, 2));
        out["approx_equal"] = cases;
    }

    // TEST(R1IntervalGolden, ApproxEqualsCustom) - ApproxEquals(y, max_error)
    {
        json cases = json::array();
        auto test = [&](const R1Interval &x, const R1Interval &y, double me) {
            cases.push_back({{"x", interval_json(x)},
                             {"y", interval_json(y)},
                             {"max_error", me},
                             {"approx_equal", x.ApproxEquals(y, me)}});
        };
        R1Interval s(1, 1);
        R1Interval wobble(1, 1 + 2 * 6 * DBL_EPSILON);
        test(empty, R1Interval(0, 2e-16), 1e-15);
        test(empty, R1Interval(0, 2e-16), 1e-20);
        test(s, wobble, 1e-15);
        test(s, wobble, 2e-14);
        out["approx_equal_custom"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
