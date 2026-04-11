// Golden data generator for S2LoopMeasures.

#include "s2/s2loop_measures.h"

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/s1angle.h"
#include "s2/s2latlng.h"
#include "s2/s2point.h"
#include "s2/s2point_span.h"

using json = nlohmann::json;
using std::string;
using std::vector;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static json loop_json(const vector<S2Point> &loop) {
    json arr = json::array();
    for (const auto &p : loop) {
        arr.push_back(point_json(p));
    }
    return arr;
}

// Parse a comma-separated list of "lat:lng" pairs (in degrees), matching the
// subset of s2textformat::ParsePointsOrDie that we need. Avoids a link-time
// dependency on s2text_format.
static vector<S2Point> ParseLatLngs(const string &s) {
    vector<S2Point> out;
    size_t i = 0;
    while (i < s.size()) {
        while (i < s.size() && (s[i] == ' ' || s[i] == ','))
            ++i;
        if (i >= s.size())
            break;
        size_t colon = s.find(':', i);
        if (colon == string::npos)
            break;
        size_t comma = s.find(',', colon);
        if (comma == string::npos)
            comma = s.size();
        // Use strtod so denormals like 1e-320 parse without throwing.
        string lat_str = s.substr(i, colon - i);
        string lng_str = s.substr(colon + 1, comma - colon - 1);
        double lat = std::strtod(lat_str.c_str(), nullptr);
        double lng = std::strtod(lng_str.c_str(), nullptr);
        out.push_back(S2LatLng::FromDegrees(lat, lng).ToPoint());
        i = comma;
    }
    return out;
}

// Build a loop out of one-dimensional "character" points; these are NOT
// unit-length and are only suitable for structural tests (PruneDegeneracies
// and GetCanonicalLoopOrder), matching the C++ test file's MakeTestLoop.
static vector<S2Point> MakeTestLoop(const string &s) {
    vector<S2Point> loop;
    for (char ch : s) {
        loop.push_back(S2Point(ch, 0, 0));
    }
    return loop;
}

static json prune_case(const string &input) {
    vector<S2Point> loop = MakeTestLoop(input);
    vector<S2Point> new_vertices;
    S2PointLoopSpan result = S2::PruneDegeneracies(loop, &new_vertices);
    string actual;
    for (const S2Point &p : result) {
        actual.push_back(static_cast<char>(p[0]));
    }
    json tc = json::object();
    tc["input"] = input;
    tc["pruned"] = actual;
    return tc;
}

static json canonical_case(const string &input) {
    vector<S2Point> loop = MakeTestLoop(input);
    S2::LoopOrder order = S2::GetCanonicalLoopOrder(loop);
    json tc = json::object();
    tc["input"] = input;
    tc["first"] = order.first;
    tc["dir"] = order.dir;
    return tc;
}

static json measures_json(const vector<S2Point> &loop) {
    json tc = json::object();
    tc["vertices"] = loop_json(loop);
    tc["perimeter"] = S2::GetPerimeter(loop).radians();
    tc["area"] = S2::GetArea(loop);
    tc["signed_area"] = S2::GetSignedArea(loop);
    tc["approx_area"] = S2::GetApproxArea(loop);
    tc["curvature"] = S2::GetCurvature(loop);
    tc["curvature_max_error"] = S2::GetCurvatureMaxError(loop);
    tc["centroid"] = point_json(S2::GetCentroid(loop));
    tc["is_normalized"] = S2::IsNormalized(loop);
    S2::LoopOrder order = S2::GetCanonicalLoopOrder(loop);
    tc["canonical_first"] = order.first;
    tc["canonical_dir"] = order.dir;
    return tc;
}

int main() {
    json out;

    // TEST(PruneDegeneracies, CompletelyDegenerate) and
    // TEST(PruneDegeneracies, PartiallyDegenerate).
    {
        json cases = json::array();
        const vector<string> inputs = {
            // CompletelyDegenerate
            "", "a", "aaaaa", "ab", "abb", "aab", "aba", "abba", "abcb",
            "abcba", "abcdcdedefedcbcdcb",
            // PartiallyDegenerate
            "abc", "abca", "abcc", "abccaa", "aabbcc", "abcdedca",
            "abcbabcbcdc", "xyzabcazy", "xxyyzzaabbccaazzyyxx", "abcdb",
            "abcdecb", "abcdefdcb", "abcad", "abcdbae", "abcdecbaf"};
        for (const auto &s : inputs) {
            cases.push_back(prune_case(s));
        }
        out["prune_degeneracies"] = cases;
    }

    // TEST(GetCanonicalLoopOrder, AllDegeneracies).
    {
        json cases = json::array();
        const vector<string> inputs = {"",    "a",    "aaaaa",  "ba",
                                       "bab", "cbab", "bacbcab"};
        for (const auto &s : inputs) {
            cases.push_back(canonical_case(s));
        }
        out["canonical_loop_order"] = cases;
    }

    // TEST(GetPerimeter, Empty) / TEST(GetPerimeter, Octant) /
    // TEST(GetPerimeter, MoreThanTwoPi).
    {
        json cases = json::array();

        auto add = [&](const char *label, const vector<S2Point> &loop) {
            json tc = json::object();
            tc["label"] = label;
            tc["vertices"] = loop_json(loop);
            tc["perimeter"] = S2::GetPerimeter(loop).radians();
            cases.push_back(tc);
        };

        add("empty", vector<S2Point>{});
        add("octant", ParseLatLngs("0:0, 0:90, 90:0"));
        add("more_than_two_pi", ParseLatLngs("0:0, 0:90, 0:180, 90:0, 0:-90"));

        out["perimeter"] = cases;
    }

    // TEST(GetSignedArea, Underflow).
    {
        json tc = json::object();
        auto loop = ParseLatLngs("0:0, 0:1e-88, 1e-88:1e-88, 1e-88:0");
        tc["vertices"] = loop_json(loop);
        tc["signed_area"] = S2::GetSignedArea(loop);
        out["signed_area_underflow"] = tc;
    }

    // Standard loops used by LoopTestBase.
    // Used by TEST_F(LoopTestBase, GetAreaConsistentWithCurvature),
    // TEST_F(LoopTestBase, GetAreaAndCentroid) (subset),
    // TEST_F(LoopTestBase, GetCurvature) (subset).
    {
        json cases = json::array();

        auto add = [&](const char *label, const vector<S2Point> &loop) {
            json tc = measures_json(loop);
            tc["label"] = label;
            cases.push_back(tc);
        };

        add("full", vector<S2Point>{});
        add("v_loop", ParseLatLngs("5:1, 0:2, 5:3, 0:2"));
        add("north_hemi", ParseLatLngs("0:-180, 0:-90, 0:0, 0:90"));
        add("north_hemi3", ParseLatLngs("0:-180, 0:-60, 0:60"));
        add("west_hemi", ParseLatLngs("0:-180, -90:0, 0:0, 90:0"));
        add("east_hemi", ParseLatLngs("90:0, 0:0, -90:0, 0:-180"));
        add("candy_cane",
            ParseLatLngs("-20:150, -20:-70, 0:70, 10:-150, 10:70, -10:-70"));
        add("line_triangle", ParseLatLngs("0:1, 0:2, 0:3"));
        add("skinny_chevron",
            ParseLatLngs("0:0, -1e-320:80, 0:1e-320, 1e-320:80"));
        add("three_leaf_clover",
            ParseLatLngs("0:0, -3:3, 3:3, 0:0, 3:0, 3:-3, 0:0, -3:-3, -3:0"));
        add("tessellated_loop",
            ParseLatLngs(
                "10:34, 5:34, 0:34, -10:34, -10:36, -5:36, 0:36, 10:36"));

        out["standard_loops"] = cases;
    }

    // TEST_F(LoopTestBase, GetSurfaceIntegralGreaterThan4Pi).
    // Loop for which GetSurfaceIntegral(SignedArea) > 4*Pi but
    // GetSignedArea() still returns a sensible answer.
    {
        S2Point p0(1, 0, 0);
        S2Point p1 = S2Point(0, 1, 1e-150).Normalize();
        S2Point p2 = S2Point(-1, -2, 0).Normalize();
        S2Point p3 = S2Point(-1, 0, 1e-50).Normalize();
        S2Point p4(0, 0, 1);
        vector<S2Point> loop{p0, p1, p2, p3, p4};
        json tc = measures_json(loop);
        tc["label"] = "surface_integral_gt_4pi";
        out["surface_integral_gt_4pi"] = tc;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
