// Golden data generator for S2 metrics.

#include "s2/s2metrics.h"

#include <algorithm>
#include <cmath>
#include <iostream>
#include <limits>
#include <nlohmann/json.hpp>

#include "s2/s2cell_id.h"

using json = nlohmann::json;

template <int dim>
static json metric_json(const S2::Metric<dim> &m, const char *name) {
    json j;
    j["name"] = name;
    j["dim"] = dim;
    j["deriv"] = m.deriv();
    json values = json::array();
    for (int level = 0; level <= S2::kMaxCellLevel; ++level) {
        values.push_back(m.GetValue(level));
    }
    j["values"] = values;
    return j;
}

int main() {
    json out;

    out["max_cell_level"] = S2::kMaxCellLevel;

    // All metric constants with their deriv and GetValue at every level.
    // This covers the raw constant values used throughout S2.
    {
        json metrics = json::object();
        metrics["min_angle_span"] =
            metric_json(S2::kMinAngleSpan, "kMinAngleSpan");
        metrics["max_angle_span"] =
            metric_json(S2::kMaxAngleSpan, "kMaxAngleSpan");
        metrics["avg_angle_span"] =
            metric_json(S2::kAvgAngleSpan, "kAvgAngleSpan");
        metrics["min_width"] = metric_json(S2::kMinWidth, "kMinWidth");
        metrics["max_width"] = metric_json(S2::kMaxWidth, "kMaxWidth");
        metrics["avg_width"] = metric_json(S2::kAvgWidth, "kAvgWidth");
        metrics["min_edge"] = metric_json(S2::kMinEdge, "kMinEdge");
        metrics["max_edge"] = metric_json(S2::kMaxEdge, "kMaxEdge");
        metrics["avg_edge"] = metric_json(S2::kAvgEdge, "kAvgEdge");
        metrics["min_diag"] = metric_json(S2::kMinDiag, "kMinDiag");
        metrics["max_diag"] = metric_json(S2::kMaxDiag, "kMaxDiag");
        metrics["avg_diag"] = metric_json(S2::kAvgDiag, "kAvgDiag");
        metrics["min_area"] = metric_json(S2::kMinArea, "kMinArea");
        metrics["max_area"] = metric_json(S2::kMaxArea, "kMaxArea");
        metrics["avg_area"] = metric_json(S2::kAvgArea, "kAvgArea");
        out["metrics"] = metrics;
    }

    out["max_edge_aspect"] = S2::kMaxEdgeAspect;
    out["max_diag_aspect"] = S2::kMaxDiagAspect;

    // TEST(S2, Metrics) - boundary cases for GetLevelForMaxValue / MinValue on
    // kMinWidth and kMaxWidth.
    {
        json cases = json::array();

        auto add = [&](const char *metric, const char *op, double input,
                       int expected) {
            cases.push_back({
                {"metric", metric},
                {"op", op},
                {"input", input},
                {"expected_level", expected},
            });
        };

        add("min_width", "level_for_max_value", -1.0,
            S2::kMinWidth.GetLevelForMaxValue(-1.0));
        add("max_width", "level_for_max_value", -1.0,
            S2::kMaxWidth.GetLevelForMaxValue(-1.0));
        add("min_width", "level_for_max_value", 0.0,
            S2::kMinWidth.GetLevelForMaxValue(0.0));
        add("max_width", "level_for_max_value", 0.0,
            S2::kMaxWidth.GetLevelForMaxValue(0.0));

        add("min_width", "level_for_min_value", 4.0,
            S2::kMinWidth.GetLevelForMinValue(4.0));
        add("max_width", "level_for_min_value", 4.0,
            S2::kMaxWidth.GetLevelForMinValue(4.0));

        constexpr double kInf = std::numeric_limits<double>::infinity();
        add("min_width", "level_for_min_value", kInf,
            S2::kMinWidth.GetLevelForMinValue(kInf));
        add("max_width", "level_for_min_value", kInf,
            S2::kMaxWidth.GetLevelForMinValue(kInf));

        out["boundary_level_cases"] = cases;
    }

    // TEST(S2, Metrics) - loop from level -2 to kMaxCellLevel + 3 exercising
    // boundary widths and areas. Each entry records the (value, expected_level)
    // pairs that the upstream test verifies for GetLevelForMaxValue,
    // GetLevelForMinValue, and GetClosestLevel.
    {
        json width_cases = json::array();
        json area_cases = json::array();

        for (int level = -2; level <= S2::kMaxCellLevel + 3; ++level) {
            double width = S2::kMinWidth.deriv() * std::pow(2, -level);
            if (level >= S2::kMaxCellLevel + 3)
                width = 0;
            int expected_level = std::clamp(level, 0, S2::kMaxCellLevel);

            json w;
            w["level"] = level;
            w["expected_level"] = expected_level;
            w["width"] = width;
            w["width_1_2"] = 1.2 * width;
            w["width_0_8"] = 0.8 * width;
            width_cases.push_back(w);

            double area = S2::kMinArea.deriv() * std::pow(4, -level);
            if (level <= -3)
                area = 0;
            // The C++ test uses the same expected_level for area cases.
            json a;
            a["level"] = level;
            a["expected_level"] = expected_level;
            a["area"] = area;
            a["area_1_2"] = 1.2 * area;
            a["area_0_8"] = 0.8 * area;
            area_cases.push_back(a);
        }

        out["width_loop"] = width_cases;
        out["area_loop"] = area_cases;
    }

    // Sanity-check GetClosestLevel for a handful of hand-picked values against
    // several metrics. This catches mistakes in the dim=1 vs dim=2 branching.
    {
        json cases = json::array();

        auto add_closest = [&](const char *metric_name, int dim, double deriv,
                               double value, int expected) {
            cases.push_back({
                {"metric", metric_name},
                {"dim", dim},
                {"deriv", deriv},
                {"value", value},
                {"expected_level", expected},
            });
        };

        add_closest("min_width", 1, S2::kMinWidth.deriv(), 0.1,
                    S2::kMinWidth.GetClosestLevel(0.1));
        add_closest("max_width", 1, S2::kMaxWidth.deriv(), 0.1,
                    S2::kMaxWidth.GetClosestLevel(0.1));
        add_closest("avg_edge", 1, S2::kAvgEdge.deriv(), 0.01,
                    S2::kAvgEdge.GetClosestLevel(0.01));
        add_closest("min_area", 2, S2::kMinArea.deriv(), 0.001,
                    S2::kMinArea.GetClosestLevel(0.001));
        add_closest("avg_area", 2, S2::kAvgArea.deriv(), 1e-6,
                    S2::kAvgArea.GetClosestLevel(1e-6));
        add_closest("max_area", 2, S2::kMaxArea.deriv(), 1.0,
                    S2::kMaxArea.GetClosestLevel(1.0));

        out["closest_level_cases"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
