// Golden data generator for S1Interval.

#include "s2/s1interval.h"

#include <cfloat>
#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>
#include <vector>

using json = nlohmann::json;

json interval_json(const S1Interval &i) {
    return json::array({i.lo(), i.hi()});
}

int main() {
    // Standard intervals from S1IntervalTestBase
    S1Interval empty = S1Interval::Empty();
    S1Interval full = S1Interval::Full();
    S1Interval zero(0, 0);
    S1Interval pi2(M_PI_2, M_PI_2);
    S1Interval pi(M_PI, M_PI);
    S1Interval mipi(-M_PI, -M_PI);
    S1Interval mipi2(-M_PI_2, -M_PI_2);
    S1Interval quad1(0, M_PI_2);
    S1Interval quad2(M_PI_2, -M_PI);
    S1Interval quad3(M_PI, -M_PI_2);
    S1Interval quad4(-M_PI_2, 0);
    S1Interval quad12(0, -M_PI);
    S1Interval quad23(M_PI_2, -M_PI_2);
    S1Interval quad34(M_PI, 0);
    S1Interval quad41(-M_PI_2, M_PI_2);
    S1Interval quad123(0, -M_PI_2);
    S1Interval quad234(M_PI_2, 0);
    S1Interval quad341(M_PI, M_PI_2);
    S1Interval quad412(-M_PI_2, -M_PI);
    S1Interval mid12(M_PI_2 - 0.01, M_PI_2 + 0.02);
    S1Interval mid23(M_PI - 0.01, -M_PI + 0.02);
    S1Interval mid34(-M_PI_2 - 0.01, -M_PI_2 + 0.02);
    S1Interval mid41(-0.01, 0.02);

    json out;

    // TEST_F(S1IntervalTestBase, ConstructorsAndAccessors) - constructors (+
    // FromPointPair cases)
    {
        json cases = json::array();
        cases.push_back(
            {{"op", "default"}, {"expected", interval_json(S1Interval())}});
        cases.push_back({{"op", "empty"},
                         {"expected", interval_json(S1Interval::Empty())}});
        cases.push_back(
            {{"op", "full"}, {"expected", interval_json(S1Interval::Full())}});
        cases.push_back(
            {{"op", "from_point"},
             {"p", 0.0},
             {"expected", interval_json(S1Interval::FromPoint(0.0))}});
        cases.push_back(
            {{"op", "from_point"},
             {"p", M_PI},
             {"expected", interval_json(S1Interval::FromPoint(M_PI))}});
        cases.push_back(
            {{"op", "from_point"},
             {"p", -M_PI},
             {"expected", interval_json(S1Interval::FromPoint(-M_PI))}});

        cases.push_back({{"op", "from_point_pair"},
                         {"p1", -M_PI},
                         {"p2", M_PI},
                         {"expected", interval_json(S1Interval::FromPointPair(
                                          -M_PI, M_PI))}});
        cases.push_back({{"op", "from_point_pair"},
                         {"p1", M_PI},
                         {"p2", -M_PI},
                         {"expected", interval_json(S1Interval::FromPointPair(
                                          M_PI, -M_PI))}});
        cases.push_back({{"op", "from_point_pair"},
                         {"p1", mid34.hi()},
                         {"p2", mid34.lo()},
                         {"expected", interval_json(S1Interval::FromPointPair(
                                          mid34.hi(), mid34.lo()))}});
        cases.push_back({{"op", "from_point_pair"},
                         {"p1", mid23.lo()},
                         {"p2", mid23.hi()},
                         {"expected", interval_json(S1Interval::FromPointPair(
                                          mid23.lo(), mid23.hi()))}});

        out["constructors"] = cases;
    }

    // TEST_F(S1IntervalTestBase, SimplePredicates) / GetCenter, GetLength -
    // accessors
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S1Interval &i) {
            cases.push_back({
                {"name", name},
                {"lo", i.lo()},
                {"hi", i.hi()},
                {"is_valid", i.is_valid()},
                {"is_empty", i.is_empty()},
                {"is_full", i.is_full()},
                {"is_inverted", i.is_inverted()},
                {"center", i.GetCenter()},
                {"length", i.GetLength()},
            });
        };
        add("empty", empty);
        add("full", full);
        add("zero", zero);
        add("pi", pi);
        add("mipi", mipi);
        add("quad12", quad12);
        add("quad23", quad23);
        add("quad123", quad123);
        out["accessors"] = cases;
    }

    // TEST_F(S1IntervalTestBase, Complement) - complement
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const S1Interval &i) {
            cases.push_back({
                {"name", name},
                {"interval", interval_json(i)},
                {"complement", interval_json(i.Complement())},
                {"complement_center", i.GetComplementCenter()},
            });
        };
        add("empty", empty);
        add("full", full);
        add("pi", pi);
        add("zero", zero);
        add("quad12", quad12);
        add("quad34", quad34);
        add("quad123", quad123);
        out["complement"] = cases;
    }

    // TEST_F(S1IntervalTestBase, Contains) - contains_point
    {
        json cases = json::array();
        auto test = [&](const std::string &name, const S1Interval &i,
                        double p) {
            cases.push_back({
                {"name", name},
                {"interval", interval_json(i)},
                {"p", p},
                {"contains", i.Contains(p)},
                {"interior_contains", i.InteriorContains(p)},
            });
        };
        std::vector<double> points = {0, M_PI_2, M_PI, -M_PI, -M_PI_2};
        for (double p : points) {
            test("empty", empty, p);
            test("full", full, p);
            test("quad12", quad12, p);
            test("quad23", quad23, p);
            test("pi", pi, p);
            test("zero", zero, p);
        }
        out["contains_point"] = cases;
    }

    // TEST_F(S1IntervalTestBase, IntervalOps) - interval_ops
    {
        json cases = json::array();
        auto test = [&](const std::string &xn, const S1Interval &x,
                        const std::string &yn, const S1Interval &y) {
            cases.push_back({
                {"x_name", xn},
                {"x", interval_json(x)},
                {"y_name", yn},
                {"y", interval_json(y)},
                {"contains", x.Contains(y)},
                {"interior_contains", x.InteriorContains(y)},
                {"intersects", x.Intersects(y)},
                {"interior_intersects", x.InteriorIntersects(y)},
                {"union", interval_json(x.Union(y))},
                {"intersection", interval_json(x.Intersection(y))},
            });
        };

        std::vector<std::pair<std::string, S1Interval>> intervals = {
            {"empty", empty}, {"full", full},     {"zero", zero},
            {"pi", pi},       {"pi2", pi2},       {"mipi", mipi},
            {"mipi2", mipi2}, {"quad1", quad1},   {"quad2", quad2},
            {"quad3", quad3}, {"quad12", quad12}, {"quad23", quad23}};

        for (const auto &ix : intervals) {
            for (const auto &iy : intervals) {
                test(ix.first, ix.second, iy.first, iy.second);
            }
        }
        out["interval_ops"] = cases;
    }

    // TEST_F(S1IntervalTestBase, AddPoint) - add_point
    {
        json cases = json::array();
        auto test = [&](const S1Interval &i, double p) {
            S1Interval r = i;
            r.AddPoint(p);
            cases.push_back({{"interval", interval_json(i)},
                             {"p", p},
                             {"expected", interval_json(r)}});
        };
        test(empty, 0);
        test(empty, M_PI);
        test(empty, -M_PI);

        S1Interval r = empty;
        r.AddPoint(M_PI);
        r.AddPoint(-M_PI);
        cases.push_back(
            {{"op", "add_pi_then_mipi"}, {"expected", interval_json(r)}});

        r = empty;
        r.AddPoint(-M_PI);
        r.AddPoint(M_PI);
        cases.push_back(
            {{"op", "add_mipi_then_pi"}, {"expected", interval_json(r)}});

        test(quad1, -0.9 * M_PI);
        test(S1Interval(0, M_PI_2), -M_PI_2);
        test(full, 0);
        out["add_point"] = cases;
    }

    // TEST_F(S1IntervalTestBase, Project) - project
    {
        json cases = json::array();
        auto test = [&](const S1Interval &i, double p) {
            cases.push_back({{"interval", interval_json(i)},
                             {"p", p},
                             {"projected", i.Project(p)}});
        };
        test(S1Interval(-M_PI, -M_PI), -M_PI);
        test(S1Interval(-M_PI, -M_PI), 0);
        test(S1Interval(0, M_PI), 0.1);
        test(S1Interval(0, M_PI), -M_PI_2 + 1e-15);
        test(S1Interval(0, M_PI), -M_PI_2 - 1e-15);
        test(S1Interval(M_PI - 0.1, -M_PI + 0.1), M_PI);
        test(S1Interval(M_PI - 0.1, -M_PI + 0.1), 1e-15);
        test(S1Interval(M_PI - 0.1, -M_PI + 0.1), -1e-15);
        test(full, 0);
        test(full, M_PI);
        test(full, -M_PI);
        out["project"] = cases;
    }

    // TEST_F(S1IntervalTestBase, Expanded) - expanded
    {
        json cases = json::array();
        auto test = [&](const S1Interval &i, double margin) {
            cases.push_back({{"interval", interval_json(i)},
                             {"margin", margin},
                             {"expected", interval_json(i.Expanded(margin))}});
        };
        test(empty, 1);
        test(full, 1);
        test(zero, 1);
        test(mipi, 0.01);
        test(pi, 27);
        test(pi, M_PI_2);
        test(pi2, M_PI_2);
        test(mipi2, M_PI_2);
        test(empty, -1);
        test(full, -1);
        test(quad123, -27);
        test(quad123, -M_PI_2);
        out["expanded"] = cases;
    }

    // TEST_F(S1IntervalTestBase, ApproxEquals) - approx_equal
    {
        double kLo = 4 * DBL_EPSILON;
        double kHi = 6 * DBL_EPSILON;
        json cases = json::array();
        auto test = [&](const S1Interval &x, const S1Interval &y) {
            cases.push_back({{"x", interval_json(x)},
                             {"y", interval_json(y)},
                             {"approx_equal", x.ApproxEquals(y)}});
        };
        test(empty, empty);
        test(zero, empty);
        test(pi, empty);
        test(mipi, empty);
        test(empty, full);
        test(empty, S1Interval(1, 1 + 2 * kLo));
        test(empty, S1Interval(1, 1 + 2 * kHi));
        test(full, full);
        test(full, empty);
        test(full, S1Interval(kLo, -kLo));
        test(full, S1Interval(2 * kHi, 0));
        test(pi, pi);
        test(mipi, pi);
        test(S1Interval(1 - kLo, 2 + kLo), S1Interval(1, 2));
        test(S1Interval(1 - kHi, 2 + kLo), S1Interval(1, 2));
        out["approx_equal"] = cases;
    }

    // TEST_F(S1IntervalTestBase, GetDirectedHausdorffDistance) -
    // directed_hausdorff
    {
        json cases = json::array();
        auto test = [&](const S1Interval &x, const S1Interval &y) {
            cases.push_back({{"x", interval_json(x)},
                             {"y", interval_json(y)},
                             {"distance", x.GetDirectedHausdorffDistance(y)}});
        };
        test(empty, empty);
        test(empty, mid12);
        test(mid12, empty);
        test(quad12, quad123);
        S1Interval in(3.0, -3.0);
        test(S1Interval(-0.1, 0.2), in);
        test(S1Interval(0.1, 0.2), in);
        test(S1Interval(-0.2, -0.1), in);
        out["directed_hausdorff"] = cases;
    }

    // TEST(S1IntervalTest, IsValidPoint) - is_valid_point
    {
        json cases = json::array();
        auto add = [&](double p) {
            cases.push_back({{"p", p}, {"valid", S1Interval::IsValidPoint(p)}});
        };
        add(0);
        add(M_PI);
        add(-M_PI);
        add(M_PI + 1e-15);
        add(-M_PI - 1e-15);
        add(4.0);
        add(-4.0);
        out["is_valid_point"] = cases;
    }

    // TEST_F(S1IntervalTestBase, AlmostEmptyOrFull) - almost_empty_full
    {
        const double kAlmostPi = M_PI - 2 * DBL_EPSILON;
        json cases = json::array();
        auto row = [&](const S1Interval &i) {
            cases.push_back({{"interval", interval_json(i)},
                             {"is_full", i.is_full()},
                             {"is_empty", i.is_empty()}});
        };
        row(S1Interval(-kAlmostPi, M_PI));
        row(S1Interval(-M_PI, kAlmostPi));
        row(S1Interval(M_PI, -kAlmostPi));
        row(S1Interval(kAlmostPi, -M_PI));
        out["almost_empty_full"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
