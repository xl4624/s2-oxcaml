// Golden data generator for s2shapeutil::EdgeIterator.
// Mirrors selected tests from
// s2geometry/src/s2/s2shapeutil_edge_iterator_test.cc.

#include "s2/s2shapeutil_edge_iterator.h"

#include <cstdlib>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <utility>
#include <vector>

#include "s2/mutable_s2shape_index.h"
#include "s2/s2latlng.h"
#include "s2/s2loop.h"
#include "s2/s2point.h"
#include "s2/s2point_vector_shape.h"
#include "s2/s2polyline.h"
#include "s2/s2shape.h"
#include "s2/s2shapeutil_shape_edge_id.h"

using json = nlohmann::json;
using s2shapeutil::EdgeIterator;
using std::make_unique;
using std::string;
using std::vector;

static json point_json(const S2Point &p) {
    return json::array({p.x(), p.y(), p.z()});
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
        string lat_str = s.substr(i, colon - i);
        string lng_str = s.substr(colon + 1, comma - colon - 1);
        double lat = std::strtod(lat_str.c_str(), nullptr);
        double lng = std::strtod(lng_str.c_str(), nullptr);
        out.push_back(S2LatLng::FromDegrees(lat, lng).ToPoint());
        i = comma;
    }
    return out;
}

enum class Kind { POINTS, POLYLINE, LOOP };

struct ShapeSpec {
    Kind kind;
    vector<S2Point> vertices;
};

static json vertices_json(const vector<S2Point> &vs) {
    json out = json::array();
    for (const auto &p : vs)
        out.push_back(point_json(p));
    return out;
}

static json shape_spec_json(const ShapeSpec &s) {
    json out;
    switch (s.kind) {
        case Kind::POINTS: out["kind"] = "points"; break;
        case Kind::POLYLINE: out["kind"] = "polyline"; break;
        case Kind::LOOP: out["kind"] = "loop"; break;
    }
    out["vertices"] = vertices_json(s.vertices);
    return out;
}

static int add_shape(MutableS2ShapeIndex *index, const ShapeSpec &s) {
    switch (s.kind) {
        case Kind::POINTS:
            return index->Add(make_unique<S2PointVectorShape>(s.vertices));
        case Kind::POLYLINE: {
            auto polyline = make_unique<S2Polyline>(s.vertices);
            return index->Add(
                make_unique<S2Polyline::OwningShape>(std::move(polyline)));
        }
        case Kind::LOOP: {
            auto loop = make_unique<S2Loop>(s.vertices);
            return index->Add(make_unique<S2Loop::Shape>(loop.release()));
        }
    }
    return -1;
}

// Run the EdgeIterator over [index] and dump every (shape_id, edge_id, v0, v1)
// it visits.
static json run_iterator(const MutableS2ShapeIndex &index) {
    json arr = json::array();
    for (EdgeIterator it(&index); !it.Done(); it.Next()) {
        S2Shape::Edge e = it.edge();
        arr.push_back(json{
            {"shape_id", it.shape_id()},
            {"edge_id", it.edge_id()},
            {"v0", point_json(e.v0)},
            {"v1", point_json(e.v1)},
        });
    }
    return arr;
}

static json run_case(const string &name, const vector<ShapeSpec> &shapes) {
    MutableS2ShapeIndex index;
    json shapes_json = json::array();
    for (const auto &s : shapes) {
        add_shape(&index, s);
        shapes_json.push_back(shape_spec_json(s));
    }
    return json{
        {"name", name},
        {"shapes", shapes_json},
        {"edges", run_iterator(index)},
    };
}

int main() {
    json out;
    json cases = json::array();

    // TEST(S2ShapeutilEdgeIteratorTest, Empty)
    cases.push_back(run_case("empty", {}));

    // TEST(S2ShapeutilEdgeIteratorTest, Points): "0:0|1:1##"
    cases.push_back(
        run_case("points", {{Kind::POINTS, ParseLatLngs("0:0, 1:1")}}));

    // TEST(S2ShapeutilEdgeIteratorTest, Lines):
    //   "#0:0,10:10|5:5,5:10|1:2,2:1#"
    cases.push_back(
        run_case("lines", {
                              {Kind::POLYLINE, ParseLatLngs("0:0, 10:10")},
                              {Kind::POLYLINE, ParseLatLngs("5:5, 5:10")},
                              {Kind::POLYLINE, ParseLatLngs("1:2, 2:1")},
                          }));

    // TEST(S2ShapeutilEdgeIteratorTest, Polygons):
    //   "##10:10,10:0,0:0|-10:-10,-10:0,0:0,0:-10"
    cases.push_back(
        run_case("polygons",
                 {
                     {Kind::LOOP, ParseLatLngs("10:10, 10:0, 0:0")},
                     {Kind::LOOP, ParseLatLngs("-10:-10, -10:0, 0:0, 0:-10")},
                 }));

    // TEST(S2ShapeutilEdgeIteratorTest, Collection):
    //   "1:1|7:2 # 1:1,2:2,3:3|2:2,1:7 #
    //    10:10,10:0,0:0; 20:20,20:10,10:10 | 15:15,15:0,0:0"
    // We split the multi-shape components into separate ShapeSpecs in the
    // shape-id order MutableS2ShapeIndex assigns: points first, then
    // polylines, then polygons (loops). The two-loop polygon group becomes
    // two distinct loop shapes; iterator order is unaffected because the
    // upstream test treats every loop as its own shape via S2Loop::Shape.
    cases.push_back(run_case(
        "collection", {
                          {Kind::POINTS, ParseLatLngs("1:1, 7:2")},
                          {Kind::POLYLINE, ParseLatLngs("1:1, 2:2, 3:3")},
                          {Kind::POLYLINE, ParseLatLngs("2:2, 1:7")},
                          {Kind::LOOP, ParseLatLngs("10:10, 10:0, 0:0")},
                          {Kind::LOOP, ParseLatLngs("20:20, 20:10, 10:10")},
                          {Kind::LOOP, ParseLatLngs("15:15, 15:0, 0:0")},
                      }));

    out["cases"] = cases;
    std::cout << out.dump(2) << std::endl;
    return 0;
}
