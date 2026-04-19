// Golden data generator for S2Polyline.

#include "s2/s2polyline.h"

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/s1angle.h"
#include "s2/s2cap.h"
#include "s2/s2cell.h"
#include "s2/s2cell_id.h"
#include "s2/s2latlng.h"
#include "s2/s2latlng_rect.h"
#include "s2/s2point.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::string;
using std::vector;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static json edge_json(const S2Shape::Edge &e) {
    return json::object({{"v0", point_json(e.v0)}, {"v1", point_json(e.v1)}});
}

static json points_json(const vector<S2Point> &ps) {
    json arr = json::array();
    for (const auto &p : ps)
        arr.push_back(point_json(p));
    return arr;
}

static json rect_json(const S2LatLngRect &r) {
    return {{"lat", {r.lat().lo(), r.lat().hi()}},
            {"lng", {r.lng().lo(), r.lng().hi()}},
            {"is_empty", r.is_empty()}};
}

static json cap_json(const S2Cap &c) {
    return {{"center", point_json(c.center())},
            {"length2", c.radius().length2()}};
}

// Parse a comma-separated list of "lat:lng" pairs (in degrees).
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

static json polyline_summary(const string &name,
                             const vector<S2Point> &vertices) {
    S2Polyline line(vertices, S2Debug::DISABLE);
    return {
        {"name", name},
        {"vertices", points_json(vertices)},
        {"num_vertices", line.num_vertices()},
        {"length_radians", line.GetLength().radians()},
        {"centroid", point_json(line.GetCentroid())},
        {"rect_bound", rect_json(line.GetRectBound())},
        {"cap_bound", cap_json(line.GetCapBound())},
        {"is_valid", line.IsValid()},
    };
}

int main() {
    json out;

    // TEST(S2Polyline, Basic)
    // TEST(S2Polyline, NoData)
    {
        json cases = json::array();
        cases.push_back(polyline_summary("empty", {}));
        cases.push_back(
            polyline_summary("semi_equator", ParseLatLngs("0:0, 0:90, 0:180")));
        cases.push_back(polyline_summary("single_vertex", {S2Point(1, 0, 0)}));
        cases.push_back(polyline_summary(
            "four_vertices", {S2Point(1, 0, 0), S2Point(0, 1, 0),
                              S2Point(0, 1, 1).Normalize(), S2Point(0, 0, 1)}));
        cases.push_back(
            polyline_summary("two_vertices", ParseLatLngs("1:1, 4:4")));
        cases.push_back(polyline_summary(
            "longish", ParseLatLngs("0:0, 0:10, 10:20, 20:30")));
        out["polylines"] = cases;
    }

    // Reverse: result of reversing semi_equator and four_vertices.
    {
        json cases = json::array();
        auto reverse_case = [&](const string &name,
                                const vector<S2Point> &verts) {
            S2Polyline line(verts, S2Debug::DISABLE);
            line.Reverse();
            vector<S2Point> reversed;
            reversed.reserve(line.num_vertices());
            for (int i = 0; i < line.num_vertices(); ++i)
                reversed.push_back(line.vertex(i));
            cases.push_back({{"name", name},
                             {"original", points_json(verts)},
                             {"reversed", points_json(reversed)}});
        };
        reverse_case("empty", {});
        reverse_case("semi_equator", ParseLatLngs("0:0, 0:90, 0:180"));
        reverse_case("four_vertices",
                     {S2Point(1, 0, 0), S2Point(0, 1, 0),
                      S2Point(0, 1, 1).Normalize(), S2Point(0, 0, 1)});
        out["reverse"] = cases;
    }

    // TEST(S2Polyline, Interpolate)
    // Stores fractions and expected (point, next_vertex) pairs.
    {
        const vector<S2Point> verts = {S2Point(1, 0, 0), S2Point(0, 1, 0),
                                       S2Point(0, 1, 1).Normalize(),
                                       S2Point(0, 0, 1)};
        S2Polyline line(verts, S2Debug::DISABLE);

        json cases = json::array();
        const vector<double> fractions = {-0.1, 0.0,  0.1, 0.25,
                                          0.5,  0.75, 1.0, 1.1};
        for (double f : fractions) {
            int next_vertex;
            S2Point p = line.GetSuffix(f, &next_vertex);
            cases.push_back({{"fraction", f},
                             {"point", point_json(p)},
                             {"next_vertex", next_vertex}});
        }
        json group = {{"vertices", points_json(verts)}, {"cases", cases}};

        // Edge-case polyline where the interpolated point coincides with
        // the last vertex (rounding).
        const vector<S2Point> short_verts = {
            S2Point(1, 1, 1).Normalize(), S2Point(1, 1, 1 + 1e-15).Normalize(),
            S2Point(1, 1, 1 + 2e-15).Normalize()};
        S2Polyline short_line(short_verts, S2Debug::DISABLE);
        int short_next;
        S2Point short_p = short_line.GetSuffix(1.0 - 2e-16, &short_next);
        json short_group = {
            {"vertices", points_json(short_verts)},
            {"cases", json::array({{{"fraction", 1.0 - 2e-16},
                                    {"point", point_json(short_p)},
                                    {"next_vertex", short_next}}})}};

        out["interpolate"] = json::array({group, short_group});
    }

    // TEST(S2Polyline, UnInterpolate)
    {
        json cases = json::array();
        // Single-vertex polyline always returns 0.
        {
            S2Polyline single({S2Point(1, 0, 0)}, S2Debug::DISABLE);
            cases.push_back({
                {"vertices", points_json({S2Point(1, 0, 0)})},
                {"point", point_json(S2Point(0, 1, 0))},
                {"next_vertex", 1},
                {"expected", single.UnInterpolate(S2Point(0, 1, 0), 1)},
            });
        }
        const vector<S2Point> verts = {S2Point(1, 0, 0), S2Point(0, 1, 0),
                                       S2Point(0, 1, 1).Normalize(),
                                       S2Point(0, 0, 1)};
        S2Polyline line(verts, S2Debug::DISABLE);
        const vector<double> fractions = {-0.1, 0.0, 0.5, 0.75, 1.1};
        for (double f : fractions) {
            int nv;
            S2Point p = line.GetSuffix(f, &nv);
            cases.push_back({{"vertices", points_json(verts)},
                             {"point", point_json(p)},
                             {"next_vertex", nv},
                             {"expected", line.UnInterpolate(p, nv)}});
        }
        // Clamp to 1.0 case.
        cases.push_back({
            {"vertices", points_json(verts)},
            {"point", point_json(S2Point(0, 1, 0))},
            {"next_vertex", static_cast<int>(verts.size())},
            {"expected", line.UnInterpolate(S2Point(0, 1, 0), verts.size())},
        });
        out["uninterpolate"] = cases;
    }

    // TEST(S2Polyline, Project)
    {
        const vector<S2Point> verts = ParseLatLngs("0:0, 0:1, 0:2, 1:2");
        S2Polyline line(verts, S2Debug::DISABLE);

        json cases = json::array();
        auto add = [&](double lat, double lng) {
            S2Point query = S2LatLng::FromDegrees(lat, lng).ToPoint();
            int nv;
            S2Point p = line.Project(query, &nv);
            cases.push_back({{"vertices", points_json(verts)},
                             {"query", point_json(query)},
                             {"point", point_json(p)},
                             {"next_vertex", nv}});
        };
        add(0.5, -0.5);
        add(0.5, 0.5);
        add(0.5, 1.0);
        add(-0.5, 2.5);
        add(2.0, 2.0);

        // Single-vertex polyline projects to its vertex.
        const vector<S2Point> single = {S2LatLng::FromDegrees(1, 1).ToPoint()};
        S2Polyline single_line(single, S2Debug::DISABLE);
        auto add_single = [&](double lat, double lng) {
            S2Point query = S2LatLng::FromDegrees(lat, lng).ToPoint();
            int nv;
            S2Point p = single_line.Project(query, &nv);
            cases.push_back({{"vertices", points_json(single)},
                             {"query", point_json(query)},
                             {"point", point_json(p)},
                             {"next_vertex", nv}});
        };
        add_single(2.0, 2.0);
        add_single(-1.0, 0.0);

        out["project"] = cases;
    }

    // TEST(S2Polyline, IsOnRight)
    {
        json cases = json::array();
        const vector<S2Point> verts = ParseLatLngs("0:0, 0:1, 0:2, 1:2");
        const vector<std::pair<double, double>> queries = {
            {-0.5, 0.5}, {0.5, -0.5}, {0.5, 0.5},
            {0.5, 1.0},  {-0.5, 2.5}, {1.5, 2.5}};
        S2Polyline line(verts, S2Debug::DISABLE);
        for (auto [lat, lng] : queries) {
            S2Point q = S2LatLng::FromDegrees(lat, lng).ToPoint();
            cases.push_back({{"vertices", points_json(verts)},
                             {"query", point_json(q)},
                             {"on_right", line.IsOnRight(q)}});
        }
        // Interior vertex case.
        const vector<S2Point> verts2 = ParseLatLngs("0:0, 0:1, -1:0");
        S2Polyline line2(verts2, S2Debug::DISABLE);
        for (auto [lat, lng] :
             vector<std::pair<double, double>>{{-0.5, 5.0}, {5.5, 5.0}}) {
            S2Point q = S2LatLng::FromDegrees(lat, lng).ToPoint();
            cases.push_back({{"vertices", points_json(verts2)},
                             {"query", point_json(q)},
                             {"on_right", line2.IsOnRight(q)}});
        }
        out["is_on_right"] = cases;
    }

    // TEST(S2Polyline, Intersects*)
    {
        json cases = json::array();
        auto add = [&](const string &name, const string &a_str,
                       const string &b_str) {
            vector<S2Point> a = ParseLatLngs(a_str);
            vector<S2Point> b = ParseLatLngs(b_str);
            S2Polyline la(a, S2Debug::DISABLE);
            S2Polyline lb(b, S2Debug::DISABLE);
            cases.push_back({{"name", name},
                             {"a", points_json(a)},
                             {"b", points_json(b)},
                             {"intersects", la.Intersects(lb)}});
        };
        add("empty_vs_line", "", "1:1, 4:4");
        add("line_vs_empty", "1:1, 4:4", "");
        add("line_vs_single", "1:1, 4:4", "1:1");
        add("small_crossing", "1:1, 4:4", "1:2, 2:1");
        add("small_noncrossing", "1:1, 4:4", "1:2, 2:3");
        add("big_crossing", "1:1, 4:4", "1:2, 2:3, 4:3");
        add("share_vertex_a", "1:1, 4:4, 4:6", "1:1, 1:2");
        add("share_vertex_b", "1:1, 4:4, 4:6", "5:1, 4:4, 2:2");
        add("vertex_on_edge_left_to_right_bottom_to_top", "0:1, 0:3",
            "-1:2, 0:2, 1:2");
        add("vertex_on_edge_left_to_right_top_to_bottom", "0:1, 0:3",
            "1:2, 0:2, -1:2");
        add("vertex_on_edge_right_to_left_bottom_to_top", "0:3, 0:1",
            "-1:2, 0:2, 1:2");
        add("vertex_on_edge_right_to_left_top_to_bottom", "0:3, 0:1",
            "1:2, 0:2, -1:2");
        out["intersects"] = cases;
    }

    // TEST(S2Polyline, MayIntersect)
    {
        const vector<S2Point> verts = {S2Point(1, -1.1, 0.8).Normalize(),
                                       S2Point(1, -0.8, 1.1).Normalize()};
        S2Polyline line(verts, S2Debug::DISABLE);
        json faces = json::array();
        for (int face = 0; face < 6; ++face) {
            S2Cell cell = S2Cell::FromFace(face);
            faces.push_back(
                {{"face", face}, {"may_intersect", line.MayIntersect(cell)}});
        }
        out["may_intersect"] = {{"vertices", points_json(verts)},
                                {"faces", faces}};
    }

    // TEST(S2Polyline, SubsampleVertices*)
    {
        json cases = json::array();
        auto add = [&](const string &name, const string &poly_str,
                       double tol_deg) {
            vector<S2Point> verts = ParseLatLngs(poly_str);
            S2Polyline line(verts, S2Debug::DISABLE);
            vector<int> indices;
            line.SubsampleVertices(S1Angle::Degrees(tol_deg), &indices);
            cases.push_back({{"name", name},
                             {"vertices", points_json(verts)},
                             {"tolerance_degrees", tol_deg},
                             {"indices", indices}});
        };
        // Trivial inputs.
        add("empty_t1", "", 1.0);
        add("one_vertex", "0:1", 1.0);
        add("two_vertices", "10:10, 11:11", 5.0);
        add("collinear_eps", "-1:0, 0:0, 1:0", 1e-15);
        add("zero_tol_curve", "-1:0, 0:0, 1:1", 0.0);
        add("negative_tol", "-1:0, 0:0, 1:1", -1.0);
        add("straight_line", "0:1, 0:2, 0:3, 0:4, 0:5", 1.0);
        add("duplicates", "0:1, 0:1, 0:1, 0:2", 0.0);
        // Simple example.
        const string poly =
            "0:0, 0:1, -1:2, 0:3, 0:4, 1:4, 2:4.5, 3:4, 3.5:4, 4:4";
        add("simple_3", poly, 3.0);
        add("simple_2", poly, 2.0);
        add("simple_0_9", poly, 0.9);
        add("simple_0_4", poly, 0.4);
        add("simple_0", poly, 0.0);
        // Guarantees.
        add("dup_collapse", "10:10, 12:12, 10:10", 5.0);
        add("dup_collapse_2", "0:0, 1:1, 0:0, 0:120, 0:130", 5.0);
        add("long_edges",
            "90:0, 50:180, 20:180, -20:180, -50:180, -90:0, 30:0, 90:0", 5.0);
        add("backtrack_1", "10:10, 10:20, 10:30, 10:15, 10:40", 5.0);
        add("backtrack_2", "10:10, 10:20, 10:30, 10:10, 10:30, 10:40", 5.0);
        add("backtrack_3", "10:10, 12:12, 9:9, 10:20, 10:30", 5.0);
        out["subsample"] = cases;
    }

    // TEST(S2Polyline, ApproxEquals)
    {
        json cases = json::array();
        auto add = [&](const string &name, const string &a_str,
                       const string &b_str, double max_err_deg) {
            vector<S2Point> a = ParseLatLngs(a_str);
            vector<S2Point> b = ParseLatLngs(b_str);
            S2Polyline la(a, S2Debug::DISABLE);
            S2Polyline lb(b, S2Debug::DISABLE);
            cases.push_back(
                {{"name", name},
                 {"a", points_json(a)},
                 {"b", points_json(b)},
                 {"max_error_degrees", max_err_deg},
                 {"approx_equal",
                  la.ApproxEquals(lb, S1Angle::Degrees(max_err_deg))}});
        };
        add("close_within", "0:0, 0:10, 5:5", "0:0.1, -0.1:9.9, 5:5.2", 0.5);
        add("close_outside", "0:0, 0:10, 5:5", "0:0.1, -0.1:9.9, 5:5.2", 0.01);
        add("different_lengths", "0:0, 0:10, 0:20", "0:0, 0:20", 0.1);
        add("different_order", "0:0, 5:5, 0:10", "5:5, 0:10, 0:0", 0.1);
        out["approx_equal"] = cases;
    }

    // TEST(S2Polyline, Equals)
    {
        json cases = json::array();
        auto add = [&](const string &name, const string &a_str,
                       const string &b_str) {
            vector<S2Point> a = ParseLatLngs(a_str);
            vector<S2Point> b = ParseLatLngs(b_str);
            S2Polyline la(a, S2Debug::DISABLE);
            S2Polyline lb(b, S2Debug::DISABLE);
            cases.push_back({{"name", name},
                             {"a", points_json(a)},
                             {"b", points_json(b)},
                             {"equal", la.Equals(lb)}});
        };
        add("identical", "0:0, 1:1, 2:2", "0:0, 1:1, 2:2");
        add("different_lengths", "0:0, 1:1", "0:0, 1:1, 2:2");
        add("different_vertices", "0:0, 1:1, 2:2", "0:0, 1:1, 3:3");
        out["equals"] = cases;
    }

    // TEST(S2Polyline, FindValidationError)
    {
        json cases = json::array();
        // Each case: a vertex array (possibly invalid) and expected error.
        // We probe FindValidationError by calling it.
        auto add = [&](const string &name, const vector<S2Point> &verts) {
            S2Polyline line(verts, S2Debug::DISABLE);
            S2Error error;
            bool has_error = line.FindValidationError(&error);
            cases.push_back({
                {"name", name},
                {"vertices", points_json(verts)},
                {"is_valid", line.IsValid()},
                {"has_error", has_error},
            });
        };
        add("empty_valid", {});
        add("single_valid", {S2Point(1, 0, 0)});
        add("normal_valid", ParseLatLngs("0:0, 0:1, 0:2"));
        add("not_unit_length", {S2Point(1, 0, 0), S2Point(2, 0, 0)});
        add("identical_neighbors", {S2Point(1, 0, 0), S2Point(1, 0, 0)});
        add("antipodal_neighbors", {S2Point(1, 0, 0), S2Point(-1, 0, 0)});
        out["validation"] = cases;
    }

    // S2Polyline::Shape: TEST(S2PolylineShape, Basic) /
    // TEST(S2PolylineShape, EmptyPolyline)
    {
        json cases = json::array();
        auto add = [&](const string &name, const vector<S2Point> &verts) {
            S2Polyline line(verts, S2Debug::DISABLE);
            S2Polyline::Shape shape(&line);
            json edges = json::array();
            for (int i = 0; i < shape.num_edges(); ++i)
                edges.push_back(edge_json(shape.edge(i)));
            json chains = json::array();
            for (int i = 0; i < shape.num_chains(); ++i) {
                auto c = shape.chain(i);
                chains.push_back({c.start, c.length});
            }
            json chain_edges = json::array();
            for (int i = 0; i < shape.num_chains(); ++i) {
                auto c = shape.chain(i);
                for (int j = 0; j < c.length; ++j)
                    chain_edges.push_back(edge_json(shape.chain_edge(i, j)));
            }
            json chain_positions = json::array();
            for (int i = 0; i < shape.num_edges(); ++i) {
                auto cp = shape.chain_position(i);
                chain_positions.push_back({cp.chain_id, cp.offset});
            }
            auto rp = shape.GetReferencePoint();
            cases.push_back({
                {"name", name},
                {"vertices", points_json(verts)},
                {"num_edges", shape.num_edges()},
                {"num_chains", shape.num_chains()},
                {"dimension", shape.dimension()},
                {"is_empty", shape.is_empty()},
                {"is_full", shape.is_full()},
                {"type_tag", static_cast<int>(shape.type_tag())},
                {"reference_point",
                 {{"point", point_json(rp.point)},
                  {"contained", rp.contained}}},
                {"edges", edges},
                {"chains", chains},
                {"chain_edges", chain_edges},
                {"chain_positions", chain_positions},
            });
        };
        add("basic", ParseLatLngs("0:0, 1:0, 1:1, 2:1"));
        add("empty", {});
        out["shapes"] = cases;
    }

    out["type_tag"] = static_cast<int>(S2Polyline::Shape::kTypeTag);

    std::cout << out.dump(2) << std::endl;
    return 0;
}
