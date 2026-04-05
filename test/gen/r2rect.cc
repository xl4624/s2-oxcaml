// Golden data generator for R2Rect.

#include "s2/r2rect.h"

#include <iostream>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/r1interval.h"
#include "s2/r2.h"

using json = nlohmann::json;

json point_json(const R2Point &p) {
    return json::array({p.x(), p.y()});
}

json interval_json(const R1Interval &i) {
    return json::array({i.lo(), i.hi()});
}

json rect_json(const R2Rect &r) {
    return {{"x", interval_json(r.x())}, {"y", interval_json(r.y())}};
}

// Mirrors C++ TestIntervalOps - tests Contains, InteriorContains, Intersects,
// InteriorIntersects, Union, Intersection, AddRect, and AddPoint (for
// zero-size y).
json interval_ops_case(const R2Rect &x, const R2Rect &y) {
    R2Rect u = x.Union(y);
    R2Rect inter = x.Intersection(y);
    R2Rect added = x;
    added.AddRect(y);
    json c = {{"x", rect_json(x)},
              {"y", rect_json(y)},
              {"contains", x.Contains(y)},
              {"interior_contains", x.InteriorContains(y)},
              {"intersects", x.Intersects(y)},
              {"interior_intersects", x.InteriorIntersects(y)},
              {"union", rect_json(u)},
              {"intersection", rect_json(inter)},
              {"add_rect", rect_json(added)}};
    if (y.GetSize() == R2Point(0, 0)) {
        R2Rect added_pt = x;
        added_pt.AddPoint(y.lo());
        c["add_point_union"] = rect_json(added_pt);
    }
    return c;
}

int main() {
    R2Rect empty = R2Rect::Empty();

    json out;

    // TEST(R2Rect, EmptyRectangles) - empty_rect
    {
        json c;
        c["is_valid"] = empty.is_valid();
        c["is_empty"] = empty.is_empty();
        c["equal_self"] = (empty == empty);
        out["empty_rect"] = c;
    }

    // TEST(R2Rect, ConstructorsAndAccessors) / FromCenterSize / FromPoint /
    // FromPointPair - constructors
    {
        json cases = json::array();

        // C++ ConstructorsAndAccessors: R2Rect(R2Point(0.1, 0), R2Point(0.25,
        // 1))
        R2Rect ca(R2Point(0.1, 0), R2Point(0.25, 1));
        cases.push_back({{"op", "create"},
                         {"lo", point_json(R2Point(0.1, 0))},
                         {"hi", point_json(R2Point(0.25, 1))},
                         {"expected", rect_json(ca)},
                         {"x_lo", ca.x().lo()},
                         {"x_hi", ca.x().hi()},
                         {"y_lo", ca.y().lo()},
                         {"y_hi", ca.y().hi()}});

        // C++ FromCenterSize case 1
        cases.push_back(
            {{"op", "from_center_size"},
             {"center", point_json(R2Point(0.3, 0.5))},
             {"size", point_json(R2Point(0.2, 0.4))},
             {"expected", rect_json(R2Rect::FromCenterSize(
                              R2Point(0.3, 0.5), R2Point(0.2, 0.4)))}});

        // C++ FromCenterSize case 2
        cases.push_back({{"op", "from_center_size"},
                         {"center", point_json(R2Point(1, 0.1))},
                         {"size", point_json(R2Point(0, 2))},
                         {"expected", rect_json(R2Rect::FromCenterSize(
                                          R2Point(1, 0.1), R2Point(0, 2)))}});

        // C++ FromPoint
        R2Rect d1(R2Point(0.1, 0), R2Point(0.25, 1));
        cases.push_back({{"op", "from_point"},
                         {"p", point_json(d1.lo())},
                         {"expected", rect_json(R2Rect::FromPoint(d1.lo()))}});

        // C++ FromPointPair case 1
        cases.push_back(
            {{"op", "from_point_pair"},
             {"p1", point_json(R2Point(0.15, 0.9))},
             {"p2", point_json(R2Point(0.35, 0.3))},
             {"expected", rect_json(R2Rect::FromPointPair(R2Point(0.15, 0.9),
                                                          R2Point(0.35, 0.3)))}

            });

        // C++ FromPointPair case 2
        cases.push_back(
            {{"op", "from_point_pair"},
             {"p1", point_json(R2Point(0.83, 0))},
             {"p2", point_json(R2Point(0.12, 0.5))},
             {"expected", rect_json(R2Rect::FromPointPair(R2Point(0.83, 0),
                                                          R2Point(0.12, 0.5)))}

            });

        out["constructors"] = cases;
    }

    // TEST(R2Rect, ConstructorsAndAccessors) / SimplePredicates - accessors
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const R2Rect &r) {
            cases.push_back({{"name", name},
                             {"rect", rect_json(r)},
                             {"lo", point_json(r.lo())},
                             {"hi", point_json(r.hi())},
                             {"center", point_json(r.GetCenter())},
                             {"size", point_json(r.GetSize())},
                             {"is_empty", r.is_empty()},
                             {"is_valid", r.is_valid()},
                             {"v0", point_json(r.GetVertex(0))},
                             {"v1", point_json(r.GetVertex(1))},
                             {"v2", point_json(r.GetVertex(2))},
                             {"v3", point_json(r.GetVertex(3))}});
        };
        add("empty", empty);
        // C++ SimplePredicates rect
        R2Rect sp(R2Point(0, 0.25), R2Point(0.5, 0.75));
        add("sp", sp);
        // Other useful rects
        R2Rect unit(R2Point(0, 0), R2Point(1, 1));
        add("unit", unit);
        R2Rect rect1(R2Point(0.1, 0.2), R2Point(0.3, 0.4));
        add("rect1", rect1);
        out["accessors"] = cases;
    }

    // TEST(R2Rect, SimplePredicates) - contains_point / interior_contains_point
    {
        json cases = json::array();
        R2Rect r1(R2Point(0, 0.25), R2Point(0.5, 0.75));
        R2Point sw1 = r1.lo();
        R2Point ne1 = r1.hi();

        auto test = [&](const R2Rect &r, const R2Point &p) {
            cases.push_back({{"rect", rect_json(r)},
                             {"p", point_json(p)},
                             {"contains", r.Contains(p)},
                             {"interior_contains", r.InteriorContains(p)}});
        };

        // C++ SimplePredicates cases
        test(r1, R2Point(0.2, 0.4));
        test(r1, R2Point(0.2, 0.8));
        test(r1, R2Point(-0.1, 0.4));
        test(r1, R2Point(0.6, 0.1));
        test(r1, sw1);
        test(r1, ne1);

        // Extra: empty rect
        test(empty, R2Point(0, 0));

        out["contains_point"] = cases;
    }

    // TEST(R2Rect, SimplePredicates) - ccw_vertices (CCW order invariant)
    {
        R2Rect r1(R2Point(0, 0.25), R2Point(0.5, 0.75));
        json cases = json::array();
        for (int k = 0; k < 4; ++k) {
            R2Point a = r1.GetVertex(k - 1);
            R2Point b = r1.GetVertex(k);
            R2Point c = r1.GetVertex(k + 1);
            double cross = (b - a).Ortho().DotProd(c - a);
            cases.push_back({{"k", k},
                             {"a", point_json(a)},
                             {"b", point_json(b)},
                             {"c", point_json(c)},
                             {"cross_positive", cross > 0}});
        }
        out["ccw_vertices"] = cases;
    }

    // TEST(R2Rect, IntervalOperations) - interval_ops
    {
        json cases = json::array();
        R2Point sw1(0, 0.25);
        R2Point ne1(0.5, 0.75);
        R2Rect r1(sw1, ne1);
        R2Rect r1_mid(R2Point(0.25, 0.5), R2Point(0.25, 0.5));
        R2Rect r_sw1(sw1, sw1);
        R2Rect r_ne1(ne1, ne1);

        cases.push_back(interval_ops_case(r1, r1_mid));
        cases.push_back(interval_ops_case(r1, r_sw1));
        cases.push_back(interval_ops_case(r1, r_ne1));
        cases.push_back(interval_ops_case(
            r1, R2Rect(R2Point(0.45, 0.1), R2Point(0.75, 0.3))));
        cases.push_back(interval_ops_case(
            r1, R2Rect(R2Point(0.5, 0.1), R2Point(0.7, 0.3))));
        cases.push_back(interval_ops_case(
            r1, R2Rect(R2Point(0.45, 0.1), R2Point(0.7, 0.25))));
        // Disjoint
        cases.push_back(
            interval_ops_case(R2Rect(R2Point(0.1, 0.2), R2Point(0.1, 0.3)),
                              R2Rect(R2Point(0.15, 0.7), R2Point(0.2, 0.8))));
        // Overlap in x but not y
        cases.push_back(
            interval_ops_case(R2Rect(R2Point(0.1, 0.2), R2Point(0.4, 0.5)),
                              R2Rect(R2Point(0, 0), R2Point(0.2, 0.1))));
        // Overlap in y but not x
        cases.push_back(
            interval_ops_case(R2Rect(R2Point(0, 0), R2Point(0.1, 0.3)),
                              R2Rect(R2Point(0.2, 0.1), R2Point(0.3, 0.4))));

        out["interval_ops"] = cases;
    }

    // TEST(R2Rect, AddPoint) - add_point
    {
        json cases = json::array();
        R2Point sw1(0, 0.25);
        R2Point ne1(0.5, 0.75);
        R2Rect target(sw1, ne1);

        R2Rect r = empty;
        r.AddPoint(R2Point(0, 0.25));
        cases.push_back(
            {{"p", point_json(R2Point(0, 0.25))}, {"after", rect_json(r)}});
        r.AddPoint(R2Point(0.5, 0.25));
        cases.push_back(
            {{"p", point_json(R2Point(0.5, 0.25))}, {"after", rect_json(r)}});
        r.AddPoint(R2Point(0, 0.75));
        cases.push_back(
            {{"p", point_json(R2Point(0, 0.75))}, {"after", rect_json(r)}});
        r.AddPoint(R2Point(0.1, 0.4));
        cases.push_back(
            {{"p", point_json(R2Point(0.1, 0.4))}, {"after", rect_json(r)}});

        // Final rect should equal target
        cases.push_back({{"target_equal", r == target}});

        out["add_point"] = cases;
    }

    // TEST(R2Rect, Project) - project
    {
        json cases = json::array();
        R2Rect r1(R1Interval(0, 0.5), R1Interval(0.25, 0.75));
        auto test = [&](const R2Point &p) {
            cases.push_back({{"rect", rect_json(r1)},
                             {"p", point_json(p)},
                             {"projected", point_json(r1.Project(p))}});
        };
        test(R2Point(-0.01, 0.24));
        test(R2Point(-5.0, 0.48));
        test(R2Point(-5.0, 2.48));
        test(R2Point(0.19, 2.48));
        test(R2Point(6.19, 2.48));
        test(R2Point(6.19, 0.53));
        test(R2Point(6.19, -2.53));
        test(R2Point(0.33, -2.53));
        test(R2Point(0.33, 0.37));
        out["project"] = cases;
    }

    // TEST(R2Rect, Expanded) - expanded
    {
        json cases = json::array();

        // Empty expanded stays empty
        cases.push_back(
            {{"rect", rect_json(empty)},
             {"margin", point_json(R2Point(0.1, 0.3))},
             {"expected", rect_json(empty.Expanded(R2Point(0.1, 0.3)))},
             {"expected_empty", true}});
        cases.push_back(
            {{"rect", rect_json(empty)},
             {"margin", point_json(R2Point(-0.1, -0.3))},
             {"expected", rect_json(empty.Expanded(R2Point(-0.1, -0.3)))},
             {"expected_empty", true}});

        R2Rect r(R2Point(0.2, 0.4), R2Point(0.3, 0.7));

        // Normal expansion
        auto er1 = r.Expanded(R2Point(0.1, 0.3));
        cases.push_back({{"rect", rect_json(r)},
                         {"margin", point_json(R2Point(0.1, 0.3))},
                         {"expected", rect_json(er1)},
                         {"expected_empty", er1.is_empty()}});

        // Shrink x below zero -> empty
        auto er2 = r.Expanded(R2Point(-0.1, 0.3));
        cases.push_back({{"rect", rect_json(r)},
                         {"margin", point_json(R2Point(-0.1, 0.3))},
                         {"expected", rect_json(er2)},
                         {"expected_empty", er2.is_empty()}});

        // Shrink y below zero -> empty
        auto er3 = r.Expanded(R2Point(0.1, -0.2));
        cases.push_back({{"rect", rect_json(r)},
                         {"margin", point_json(R2Point(0.1, -0.2))},
                         {"expected", rect_json(er3)},
                         {"expected_empty", er3.is_empty()}});

        // Shrink y slightly (still valid)
        auto er4 = r.Expanded(R2Point(0.1, -0.1));
        cases.push_back({{"rect", rect_json(r)},
                         {"margin", point_json(R2Point(0.1, -0.1))},
                         {"expected", rect_json(er4)},
                         {"expected_empty", er4.is_empty()}});

        // Scalar expansion
        auto er5 = r.Expanded(0.1);
        cases.push_back({{"rect", rect_json(r)},
                         {"margin_scalar", 0.1},
                         {"expected", rect_json(er5)},
                         {"expected_empty", er5.is_empty()}});

        out["expanded"] = cases;
    }

    // TEST(R2RectGolden, ApproxEquals) - approx_equal (R2Rect::ApproxEquals)
    {
        json cases = json::array();
        R2Rect unit(R2Point(0, 0), R2Point(1, 1));
        cases.push_back({{"r1", rect_json(unit)},
                         {"r2", rect_json(unit)},
                         {"expected", unit.ApproxEquals(unit)}});
        R2Rect unit_eps(R2Point(-1e-16, -1e-16), R2Point(1 + 1e-16, 1 + 1e-16));
        cases.push_back({{"r1", rect_json(unit)},
                         {"r2", rect_json(unit_eps)},
                         {"expected", unit.ApproxEquals(unit_eps)}});
        // Non-equal
        R2Rect far(R2Point(0, 0), R2Point(2, 2));
        cases.push_back({{"r1", rect_json(unit)},
                         {"r2", rect_json(far)},
                         {"expected", unit.ApproxEquals(far)}});
        out["approx_equal"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
