// Golden data generator for R3Vector.

#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>
#include <vector>

#include "s2/s2point.h"
#include "s2/util/math/vector.h"

using json = nlohmann::json;

typedef Vector3_d R3Vector;

json vector_json(const R3Vector &v) {
    return {v.x(), v.y(), v.z()};
}

// Matches OCaml [smallest_component] / lexicographic tie-break on abs
// components.
int SmallestAbsComponent(const R3Vector &v) {
    R3Vector a = v.Abs();
    if (a.x() < a.y()) {
        return a.x() < a.z() ? 0 : 2;
    }
    return a.y() < a.z() ? 1 : 2;
}

int main() {
    json out;

    // Named vectors used throughout
    R3Vector v1(1, 2, 3);
    R3Vector v2(4, 5, 6);
    R3Vector v_zero(0, 0, 0);
    R3Vector v_unit_x(1, 0, 0);
    R3Vector v_unit_y(0, 1, 0);
    R3Vector v_unit_z(0, 0, 1);

    // TEST(Vector3Golden, Constructors) - constructors
    {
        json cases = json::array();
        cases.push_back(
            {{"op", "default"}, {"expected", vector_json(R3Vector())}});
        cases.push_back(
            {{"op", "zero"}, {"expected", vector_json(R3Vector())}});
        cases.push_back({{"op", "init"},
                         {"x", 1.5},
                         {"y", -2.0},
                         {"z", 3.1},
                         {"expected", vector_json(R3Vector(1.5, -2.0, 3.1))}});
        out["constructors"] = cases;
    }

    // TEST(Vector3Golden, Accessors) - accessors
    {
        json cases = json::array();
        auto add = [&](const std::string &name, const R3Vector &v) {
            cases.push_back({
                {"name", name},
                {"val", vector_json(v)},
                {"x", v.x()},
                {"y", v.y()},
                {"z", v.z()},
                {"norm2", v.Norm2()},
                {"norm", v.Norm()},
                {"largest_abs_component", v.LargestAbsComponent()},
                {"smallest_abs_component", SmallestAbsComponent(v)},
            });
        };
        add("v1", v1);
        add("v2", v2);
        add("v_zero", v_zero);
        add("v_neg", R3Vector(-1, -5, -2));
        out["accessors"] = cases;
    }

    // TEST(Vector3Golden, Arithmetic) - arithmetic
    {
        json cases = json::array();
        auto test = [&](const R3Vector &a, const R3Vector &b) {
            cases.push_back({
                {"a", vector_json(a)},
                {"b", vector_json(b)},
                {"add", vector_json(a + b)},
                {"sub", vector_json(a - b)},
                {"mul_scalar", vector_json(a * 2.5)},
                {"div_scalar", vector_json(a / 2.0)},
                {"neg", vector_json(-a)},
                {"dot", a.DotProd(b)},
                {"cross", vector_json(a.CrossProd(b))},
                {"dist", (a - b).Norm()},
                {"angle", a.Angle(b)},
            });
        };
        test(v1, v2);
        test(v_unit_x, v_unit_y);
        test(v1, v_zero);
        test(R3Vector(1, 0, 0), R3Vector(-1, 0, 0));  // Angle should be pi
        out["arithmetic"] = cases;
    }

    // TEST(Vector3Golden, ComponentWise) - component_wise
    {
        json cases = json::array();
        auto test = [&](const R3Vector &a, const R3Vector &b) {
            cases.push_back({
                {"a", vector_json(a)},
                {"b", vector_json(b)},
                {"mul", vector_json(a.MulComponents(b))},
                {"div", vector_json(a.DivComponents(b))},
                {"min", vector_json(Min(a, b))},
                {"max", vector_json(Max(a, b))},
                {"abs", vector_json(a.Abs())},
            });
        };
        test(v1, v2);
        test(R3Vector(-1, 2, -3), R3Vector(5, -6, 7));
        out["component_wise"] = cases;
    }

    // TEST(Vector3Golden, Normalization) - normalization
    {
        json cases = json::array();
        auto test = [&](const R3Vector &v) {
            R3Vector normalized = v.Normalize();
            cases.push_back({
                {"v", vector_json(v)},
                {"normalized", vector_json(normalized)},
                {"is_unit", normalized.Norm() > 0
                                ? std::abs(normalized.Norm() - 1.0) < 1e-15
                                : false},
            });
        };
        test(v1);
        test(R3Vector(10, 0, 0));
        test(v_zero);
        out["normalization"] = cases;
    }

    // TEST(Vector3Golden, Ortho) ortho
    {
        json cases = json::array();
        auto test = [&](const R3Vector &v) {
            R3Vector ortho = v.Ortho();
            cases.push_back({
                {"v", vector_json(v)},
                {"ortho", vector_json(ortho)},
                {"dot_product", v.DotProd(ortho)},
                {"ortho_norm", ortho.Norm()},
            });
        };
        test(v_unit_x);
        test(v_unit_y);
        test(v_unit_z);
        test(v1);
        test(R3Vector(0.1, 0.2, 0.3));
        out["ortho"] = cases;
    }

    // TEST(Vector3Golden, ApproxEquals) approx_equal
    {
        json cases = json::array();
        auto test = [&](const R3Vector &a, const R3Vector &b, double margin) {
            cases.push_back({
                {"a", vector_json(a)},
                {"b", vector_json(b)},
                {"margin", margin},
                {"aequal", a.aequal(b, margin)},
            });
        };
        test(v1, v1, 1e-15);
        test(v1, v1 + R3Vector(1e-10, 0, 0), 1e-9);
        test(v1, v1 + R3Vector(1e-10, 0, 0), 1e-11);
        out["approx_equal"] = cases;
    }

    // TEST(Vector3Golden, CompareEqual) compare_equal
    {
        json cases = json::array();
        auto add = [&](const R3Vector &a, const R3Vector &b) {
            int cmp = 0;
            if (a < b) {
                cmp = -1;
            } else if (b < a) {
                cmp = 1;
            }
            cases.push_back({{"a", vector_json(a)},
                             {"b", vector_json(b)},
                             {"compare", cmp},
                             {"equal", a == b}});
        };
        add(v1, v2);
        add(v2, v1);
        add(v1, v1);
        add(v_zero, v1);
        add(R3Vector(), R3Vector(1e-200, 1e-100, 1e-50));
        out["compare_equal"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
