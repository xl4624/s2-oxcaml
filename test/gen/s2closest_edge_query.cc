// Golden data generator for S2ClosestEdgeQuery.
// Mirrors selected tests from s2closest_edge_query_test.cc and exercises
// all four target types (Point, Edge, Cell, ShapeIndex), as well as the
// various predicate helpers (IsDistanceLess, IsDistanceLessOrEqual,
// IsConservativeDistanceLessOrEqual).

#include "s2/s2closest_edge_query.h"

#include <algorithm>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <utility>
#include <vector>

#include "s2/mutable_s2shape_index.h"
#include "s2/s1angle.h"
#include "s2/s1chord_angle.h"
#include "s2/s2cell.h"
#include "s2/s2cell_id.h"
#include "s2/s2latlng.h"
#include "s2/s2loop.h"
#include "s2/s2point.h"
#include "s2/s2point_vector_shape.h"
#include "s2/s2polyline.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::make_unique;
using std::pair;
using std::string;
using std::vector;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static S2Point pt(double lat, double lng) {
    return S2LatLng::FromDegrees(lat, lng).ToPoint();
}

// Shapes are encoded by a "kind" tag and a vertex list.
//   "points"   -> S2PointVectorShape with the listed vertices.
//   "polyline" -> S2Polyline::OwningShape.
//   "loop"     -> S2Loop::Shape.
struct ShapeSpec {
    enum Kind { POINTS, POLYLINE, LOOP };
    Kind kind;
    vector<S2Point> vertices;
};

static json verts_json(const vector<S2Point> &vs) {
    json a = json::array();
    for (const auto &p : vs)
        a.push_back(point_json(p));
    return a;
}

static json shape_spec_json(const ShapeSpec &s) {
    json out;
    switch (s.kind) {
        case ShapeSpec::POINTS: out["kind"] = "points"; break;
        case ShapeSpec::POLYLINE: out["kind"] = "polyline"; break;
        case ShapeSpec::LOOP: out["kind"] = "loop"; break;
    }
    out["vertices"] = verts_json(s.vertices);
    return out;
}

static int add_shape(MutableS2ShapeIndex *index, const ShapeSpec &s) {
    switch (s.kind) {
        case ShapeSpec::POINTS:
            return index->Add(make_unique<S2PointVectorShape>(s.vertices));
        case ShapeSpec::POLYLINE: {
            auto polyline = make_unique<S2Polyline>(s.vertices);
            return index->Add(
                make_unique<S2Polyline::OwningShape>(std::move(polyline)));
        }
        case ShapeSpec::LOOP: {
            auto loop = make_unique<S2Loop>(s.vertices);
            return index->Add(make_unique<S2Loop::Shape>(loop.release()));
        }
    }
    return -1;
}

static vector<S2Point> regular_loop_vertices(const S2Point &center,
                                             S1Angle radius, int n) {
    auto loop = S2Loop::MakeRegularLoop(center, radius, n);
    vector<S2Point> vs;
    for (int i = 0; i < loop->num_vertices(); ++i) {
        vs.push_back(loop->vertex(i));
    }
    return vs;
}

// Targets are serialized as a kind tag + payload. For Shape_index targets,
// we embed the shapes so the OCaml test can reconstruct the index.
struct TargetSpec {
    enum Kind { POINT, EDGE, CELL, SHAPE_INDEX };
    Kind kind;
    S2Point a;
    S2Point b;
    S2CellId cell_id = S2CellId();
    vector<ShapeSpec> shapes;
    bool include_interiors = true;
};

static json target_spec_json(const TargetSpec &t) {
    json out;
    switch (t.kind) {
        case TargetSpec::POINT:
            out["kind"] = "point";
            out["p"] = point_json(t.a);
            break;
        case TargetSpec::EDGE:
            out["kind"] = "edge";
            out["a"] = point_json(t.a);
            out["b"] = point_json(t.b);
            break;
        case TargetSpec::CELL:
            out["kind"] = "cell";
            out["cell_id"] = t.cell_id.ToToken();
            break;
        case TargetSpec::SHAPE_INDEX: {
            out["kind"] = "shape_index";
            json shapes = json::array();
            for (const auto &s : t.shapes)
                shapes.push_back(shape_spec_json(s));
            out["shapes"] = shapes;
            out["include_interiors"] = t.include_interiors;
            break;
        }
    }
    return out;
}

struct Options {
    int max_results = std::numeric_limits<int>::max();
    double max_distance_deg = -1.0;  // <0 => infinity
    double max_error_deg = 0.0;
    bool include_interiors = true;
    bool use_brute_force = false;
};

static json options_json(const Options &o) {
    json out;
    out["max_results"] = o.max_results;
    if (o.max_distance_deg < 0) {
        out["max_distance_deg"] = nullptr;
    } else {
        out["max_distance_deg"] = o.max_distance_deg;
    }
    out["max_error_deg"] = o.max_error_deg;
    out["include_interiors"] = o.include_interiors;
    out["use_brute_force"] = o.use_brute_force;
    return out;
}

static void apply_options(S2ClosestEdgeQuery::Options *opts, const Options &o) {
    opts->set_max_results(o.max_results);
    if (o.max_distance_deg >= 0) {
        opts->set_max_distance(S1Angle::Degrees(o.max_distance_deg));
    }
    opts->set_max_error(S1ChordAngle(S1Angle::Degrees(o.max_error_deg)));
    opts->set_include_interiors(o.include_interiors);
    opts->set_use_brute_force(o.use_brute_force);
}

static std::unique_ptr<S2ClosestEdgeQuery::Target> build_target(
    const TargetSpec &ts, std::unique_ptr<MutableS2ShapeIndex> &out_sub_index) {
    switch (ts.kind) {
        case TargetSpec::POINT:
            return make_unique<S2ClosestEdgeQuery::PointTarget>(ts.a);
        case TargetSpec::EDGE:
            return make_unique<S2ClosestEdgeQuery::EdgeTarget>(ts.a, ts.b);
        case TargetSpec::CELL:
            return make_unique<S2ClosestEdgeQuery::CellTarget>(
                S2Cell(ts.cell_id));
        case TargetSpec::SHAPE_INDEX: {
            auto sub = make_unique<MutableS2ShapeIndex>();
            for (const auto &s : ts.shapes)
                add_shape(sub.get(), s);
            sub->ForceBuild();
            auto target =
                make_unique<S2ClosestEdgeQuery::ShapeIndexTarget>(sub.get());
            target->set_include_interiors(ts.include_interiors);
            out_sub_index = std::move(sub);
            return target;
        }
    }
    return nullptr;
}

static json results_json(const vector<S2ClosestEdgeQuery::Result> &rs) {
    json arr = json::array();
    for (const auto &r : rs) {
        json x;
        x["distance"] = r.distance().length2();
        x["shape_id"] = r.shape_id();
        x["edge_id"] = r.edge_id();
        arr.push_back(x);
    }
    return arr;
}

struct Case {
    string name;
    vector<ShapeSpec> shapes;
    vector<pair<TargetSpec, Options>> queries;
    // Extra predicate probes: each is a (target, limit_deg).
    vector<pair<TargetSpec, double>> predicate_probes;
};

static json run_case(const Case &c) {
    MutableS2ShapeIndex index;
    json shapes = json::array();
    for (const auto &s : c.shapes) {
        add_shape(&index, s);
        shapes.push_back(shape_spec_json(s));
    }
    index.ForceBuild();

    json qs = json::array();
    for (const auto &q : c.queries) {
        const auto &ts = q.first;
        const auto &opts = q.second;
        std::unique_ptr<MutableS2ShapeIndex> sub_index;
        auto target = build_target(ts, sub_index);

        // We always run two flavors of each query: optimized and brute-force,
        // and expect them to agree.
        S2ClosestEdgeQuery::Options optimized_opts, brute_opts;
        apply_options(&optimized_opts, opts);
        apply_options(&brute_opts, opts);
        brute_opts.set_use_brute_force(true);

        S2ClosestEdgeQuery q_opt(&index, optimized_opts);
        S2ClosestEdgeQuery q_brute(&index, brute_opts);
        auto rs_opt = q_opt.FindClosestEdges(target.get());
        auto rs_brute = q_brute.FindClosestEdges(target.get());

        S2ClosestEdgeQuery q_single(&index, optimized_opts);
        auto single = q_single.FindClosestEdge(target.get());
        auto dist = q_single.GetDistance(target.get());

        json entry;
        entry["target"] = target_spec_json(ts);
        entry["options"] = options_json(opts);
        entry["results_optimized"] = results_json(rs_opt);
        entry["results_brute_force"] = results_json(rs_brute);
        json sing;
        sing["distance"] = single.distance().length2();
        sing["shape_id"] = single.shape_id();
        sing["edge_id"] = single.edge_id();
        entry["closest_edge"] = sing;
        entry["distance_length2"] = dist.length2();
        qs.push_back(entry);
    }

    json ps = json::array();
    for (const auto &p : c.predicate_probes) {
        std::unique_ptr<MutableS2ShapeIndex> sub_index;
        auto target = build_target(p.first, sub_index);
        S2ClosestEdgeQuery q(&index);
        S1ChordAngle limit(S1Angle::Degrees(p.second));
        json entry;
        entry["target"] = target_spec_json(p.first);
        entry["limit_deg"] = p.second;
        entry["is_distance_less"] = q.IsDistanceLess(target.get(), limit);
        entry["is_distance_less_or_equal"] =
            q.IsDistanceLessOrEqual(target.get(), limit);
        entry["is_conservative_distance_less_or_equal"] =
            q.IsConservativeDistanceLessOrEqual(target.get(), limit);
        ps.push_back(entry);
    }

    json out;
    out["shapes"] = shapes;
    out["queries"] = qs;
    out["predicates"] = ps;
    return out;
}

int main() {
    json root;

    // Empty index: every query returns infinity / is_empty.
    {
        Case c;
        c.name = "empty";
        c.queries = {
            {{TargetSpec::POINT, pt(0, 0)}, {1, -1.0, 0.0, true, false}},
            {{TargetSpec::EDGE, pt(0, 0), pt(0, 1)},
             {1, -1.0, 0.0, true, false}},
        };
        root["empty"] = run_case(c);
    }

    // Small set of points: brute-force only path. FindClosestEdge without any
    // distance limit, and FindClosestEdges up to a distance.
    {
        Case c;
        c.name = "small_points";
        c.shapes = {
            {ShapeSpec::POINTS, {pt(1, 1), pt(1, 2), pt(1, 3), pt(2, 2)}}};
        TargetSpec probe{TargetSpec::POINT, pt(2, 2)};
        Options o1{1};  // closest edge only
        Options o_all{};
        o_all.max_distance_deg = 1.5;
        o_all.include_interiors = false;
        Options o_top2{2};
        c.queries = {
            {probe, o1},
            {probe, o_all},
            {probe, o_top2},
        };
        c.predicate_probes = {
            {probe, 0.0},  // distance is zero
            {probe, 0.5},  // less than 1 deg distance to (1,2)
            {probe, 5.0},  // generous bound
        };
        root["small_points"] = run_case(c);
    }

    // Polygon (loop) target scenarios: include_interiors matters when target
    // is inside a polygon.
    {
        Case c;
        c.name = "polygon_interior";
        c.shapes = {
            {ShapeSpec::LOOP, {pt(0, 0), pt(0, 5), pt(5, 5), pt(5, 0)}},
        };
        TargetSpec inside{TargetSpec::POINT, pt(2, 2)};
        TargetSpec outside{TargetSpec::POINT, pt(10, 10)};
        Options o_with{1, -1.0, 0.0, true, false};
        Options o_without{1, -1.0, 0.0, false, false};
        c.queries = {
            {inside, o_with},
            {inside, o_without},
            {outside, o_with},
        };
        c.predicate_probes = {
            {inside, 0.5},
            {outside, 20.0},
        };
        root["polygon_interior"] = run_case(c);
    }

    // Edge target against a polyline index.
    {
        Case c;
        c.name = "edge_target_polyline";
        c.shapes = {
            {ShapeSpec::POLYLINE,
             {pt(0, 0), pt(2, 1), pt(0, 2), pt(2, 3), pt(0, 4)}},
        };
        TargetSpec edge_probe{TargetSpec::EDGE, pt(1, 0), pt(1, 4)};
        Options o_all{};
        c.queries = {
            {edge_probe, Options{1}},
            {edge_probe, o_all},
        };
        c.predicate_probes = {
            {edge_probe, 0.5},
            {edge_probe, 2.0},
        };
        root["edge_target_polyline"] = run_case(c);
    }

    // Cell target against a loop index.
    {
        Case c;
        c.name = "cell_target";
        c.shapes = {
            {ShapeSpec::LOOP, {pt(0, 0), pt(0, 5), pt(5, 5), pt(5, 0)}},
        };
        TargetSpec inside_cell{
            TargetSpec::CELL, S2Point(), S2Point(),
            S2CellId(S2LatLng::FromDegrees(2.5, 2.5).ToPoint()).parent(15)};
        TargetSpec outside_cell{
            TargetSpec::CELL, S2Point(), S2Point(),
            S2CellId(S2LatLng::FromDegrees(40, 40).ToPoint()).parent(15)};
        Options o_default{1};
        c.queries = {
            {inside_cell, o_default},
            {outside_cell, o_default},
        };
        c.predicate_probes = {
            {inside_cell, 0.5},
            {outside_cell, 10.0},
        };
        root["cell_target"] = run_case(c);
    }

    // Shape-index target.
    {
        Case c;
        c.name = "shape_index_target";
        c.shapes = {
            {ShapeSpec::POLYLINE, {pt(0, 0), pt(0, 5), pt(0, 10)}},
        };
        TargetSpec tgt{TargetSpec::SHAPE_INDEX};
        tgt.shapes = {
            {ShapeSpec::POINTS, {pt(0, 3), pt(0, 7)}},
        };
        TargetSpec tgt_away{TargetSpec::SHAPE_INDEX};
        tgt_away.shapes = {
            {ShapeSpec::POINTS, {pt(20, 20)}},
        };
        Options o_one{1};
        c.queries = {
            {tgt, o_one},
            {tgt_away, o_one},
        };
        c.predicate_probes = {
            {tgt, 0.5},
            {tgt_away, 10.0},
        };
        root["shape_index_target"] = run_case(c);
    }

    // Regular loop (64-gon) to exercise the optimized cell-descent path.
    {
        Case c;
        c.name = "regular_loop";
        c.shapes = {
            {ShapeSpec::LOOP,
             regular_loop_vertices(pt(0, 0), S1Angle::Degrees(5), 64)},
        };
        TargetSpec center{TargetSpec::POINT, pt(0, 0)};
        TargetSpec far{TargetSpec::POINT, pt(30, 30)};
        Options o3{3};
        c.queries = {
            {center, Options{1}},
            {center, o3},
            {far, Options{1}},
        };
        c.predicate_probes = {
            {far, 15.0},
            {far, 40.0},
        };
        root["regular_loop"] = run_case(c);
    }

    std::cout << root.dump(2) << std::endl;
    return 0;
}
