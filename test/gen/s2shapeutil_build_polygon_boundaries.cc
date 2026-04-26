// Golden data generator for s2shapeutil::BuildPolygonBoundaries.
// Mirrors s2geometry/src/s2/s2shapeutil_build_polygon_boundaries_test.cc.

#include "s2/s2shapeutil_build_polygon_boundaries.h"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/s2latlng.h"
#include "s2/s2lax_loop_shape.h"
#include "s2/s2point.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::map;
using std::string;
using std::vector;

static json point_json(const S2Point &p) {
    return json::array({p.x(), p.y(), p.z()});
}

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
        string lat_str = s.substr(i, colon - i);
        string lng_str = s.substr(colon + 1, comma - colon - 1);
        double lat = std::strtod(lat_str.c_str(), nullptr);
        double lng = std::strtod(lng_str.c_str(), nullptr);
        out.push_back(S2LatLng::FromDegrees(lat, lng).ToPoint());
        i = comma;
    }
    return out;
}

// One loop: a list of vertices.
using Loop = vector<S2Point>;
// Components: list of components, each a list of loops.
using Components = vector<vector<Loop>>;

static json loop_json(const Loop &loop) {
    json arr = json::array();
    for (const auto &p : loop)
        arr.push_back(point_json(p));
    return arr;
}

static json components_json(const Components &components) {
    json arr = json::array();
    for (const auto &comp : components) {
        json comp_json = json::array();
        for (const auto &loop : comp)
            comp_json.push_back(loop_json(loop));
        arr.push_back(comp_json);
    }
    return arr;
}

// Build LaxLoop shapes, run BuildPolygonBoundaries, then translate the shape*
// faces back into face id lists matching the OCaml flat-id assignment (loops
// numbered in input order: components[0][0]=0, components[0][1]=1, ...).
static json case_json(const string &name, const Components &components) {
    vector<std::unique_ptr<S2LaxLoopShape>> owned;
    map<S2Shape *, int> shape_to_id;
    vector<vector<S2Shape *>> raw;
    int next_id = 0;
    for (const auto &comp : components) {
        vector<S2Shape *> raw_comp;
        for (const auto &loop : comp) {
            auto loop_shape = std::make_unique<S2LaxLoopShape>(loop);
            S2Shape *p = loop_shape.get();
            shape_to_id[p] = next_id++;
            raw_comp.push_back(p);
            owned.push_back(std::move(loop_shape));
        }
        raw.push_back(raw_comp);
    }
    vector<vector<S2Shape *>> faces;
    s2shapeutil::BuildPolygonBoundaries(raw, &faces);

    // Convert pointer-faces to id-faces. Sort each inner face for stability,
    // and sort the faces themselves; the OCaml side compares with the same
    // canonicalisation.
    vector<vector<int>> id_faces;
    for (const auto &face : faces) {
        vector<int> ids;
        for (S2Shape *s : face)
            ids.push_back(shape_to_id.at(s));
        std::sort(ids.begin(), ids.end());
        id_faces.push_back(std::move(ids));
    }
    std::sort(id_faces.begin(), id_faces.end());

    json out_faces = json::array();
    for (const auto &face : id_faces) {
        json face_arr = json::array();
        for (int id : face)
            face_arr.push_back(id);
        out_faces.push_back(face_arr);
    }

    return json::object({
        {"name", name},
        {"components", components_json(components)},
        {"expected_faces", out_faces},
    });
}

int main() {
    json out;
    json cases = json::array();

    // TEST(BuildPolygonBoundaries, NoComponents)
    cases.push_back(case_json("no_components", Components{}));

    // TEST(BuildPolygonBoundaries, OneLoop)
    cases.push_back(case_json("one_loop", Components{
                                              {ParseLatLngs("0:0, 1:0, 0:1"),
                                               ParseLatLngs("0:0, 0:1, 1:0")},
                                          }));

    // TEST(BuildPolygonBoundaries, TwoLoopsSameComponent)
    cases.push_back(case_json(
        "two_loops_same_component",
        Components{
            {ParseLatLngs("0:0, 1:0, 0:1"), ParseLatLngs("0:0, 0:1, 1:0"),
             ParseLatLngs("1:0, 0:1, 1:1")},
        }));

    // TEST(BuildPolygonBoundaries, TwoNestedLoops)
    cases.push_back(case_json(
        "two_nested_loops",
        Components{
            {ParseLatLngs("0:0, 3:0, 0:3"), ParseLatLngs("0:0, 0:3, 3:0")},
            {ParseLatLngs("1:1, 2:0, 0:2"), ParseLatLngs("1:1, 0:2, 2:0")},
        }));

    // TEST(BuildPolygonBoundaries, TwoLoopsDifferentComponents)
    cases.push_back(case_json(
        "two_loops_different_components",
        Components{
            {ParseLatLngs("0:0, 1:0, 0:1"), ParseLatLngs("0:0, 0:1, 1:0")},
            {ParseLatLngs("0:2, 1:2, 0:3"), ParseLatLngs("0:2, 0:3, 1:2")},
        }));

    // TEST(BuildPolygonBoundaries, OneDegenerateLoop)
    cases.push_back(
        case_json("one_degenerate_loop", Components{
                                             {ParseLatLngs("0:0, 1:0, 0:0")},
                                         }));

    // TEST(BuildPolygonBoundaries, TwoDegenerateLoops)
    cases.push_back(
        case_json("two_degenerate_loops", Components{
                                              {ParseLatLngs("0:0, 1:0, 0:0")},
                                              {ParseLatLngs("2:0, 3:0, 2:0")},
                                          }));

    out["cases"] = cases;
    std::cout << out.dump(2) << std::endl;
    return 0;
}
