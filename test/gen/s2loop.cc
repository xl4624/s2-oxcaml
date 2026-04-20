// Golden data generator for S2Loop.

#include "s2/s2loop.h"

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>

#include "s2/s1angle.h"
#include "s2/s2cap.h"
#include "s2/s2cell.h"
#include "s2/s2cell_id.h"
#include "s2/s2debug.h"
#include "s2/s2latlng.h"
#include "s2/s2latlng_rect.h"
#include "s2/s2point.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::string;
using std::unique_ptr;
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
            {"is_empty", r.is_empty()},
            {"is_full", r.is_full()}};
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

static unique_ptr<S2Loop> MakeLoop(const vector<S2Point> &v) {
    return std::make_unique<S2Loop>(v, S2Debug::DISABLE);
}

// Catalog of named loops. The OCaml side reconstructs each loop from the
// stored vertex array, so the same vertices flow into both ends.
struct NamedLoop {
    string name;
    vector<S2Point> vertices;
};

static vector<NamedLoop> Catalog() {
    return {
        {"empty", {S2Point(0, 0, 1)}},
        {"full", {S2Point(0, 0, -1)}},
        {"north_hemi", ParseLatLngs("0:-180, 0:-90, 0:0, 0:90")},
        {"south_hemi", ParseLatLngs("0:90, 0:0, 0:-90, 0:-180")},
        {"west_hemi", ParseLatLngs("0:-180, -90:0, 0:0, 90:0")},
        {"east_hemi", ParseLatLngs("90:0, 0:0, -90:0, 0:-180")},
        {"near_hemi", ParseLatLngs("0:-90, -90:0, 0:90, 90:0")},
        {"far_hemi", ParseLatLngs("90:0, 0:90, -90:0, 0:-90")},
        {"candy_cane", ParseLatLngs("-20:150, -20:-70, 0:70, 10:-150, 10:70, "
                                    "-10:-70")},
        {"small_ne_cw", ParseLatLngs("35:20, 45:20, 40:25")},
        {"arctic_80", ParseLatLngs("80:-150, 80:-30, 80:90")},
        {"antarctic_80", ParseLatLngs("-80:120, -80:0, -80:-120")},
        {"line_triangle", ParseLatLngs("0:1, 0:2, 0:3")},
        {"loop_a", ParseLatLngs("0:178, -1:180, 0:-179, 1:-180")},
        {"square", ParseLatLngs("-10:-10, -10:10, 10:10, 10:-10")},
    };
}

static json loop_summary(const NamedLoop &nl) {
    auto loop = MakeLoop(nl.vertices);
    S2Loop::Shape shape(loop.get());
    auto rp = shape.GetReferencePoint();
    return {
        {"name", nl.name},
        {"vertices", points_json(nl.vertices)},
        {"num_vertices", loop->num_vertices()},
        {"num_edges", shape.num_edges()},
        {"num_chains", shape.num_chains()},
        {"dimension", shape.dimension()},
        {"is_empty", loop->is_empty()},
        {"is_full", loop->is_full()},
        {"is_empty_or_full", loop->is_empty_or_full()},
        {"is_valid", loop->IsValid()},
        {"is_normalized", loop->IsNormalized()},
        {"contains_origin", loop->Contains(S2::Origin())},
        {"area", loop->GetArea()},
        {"centroid", point_json(loop->GetCentroid())},
        {"curvature", loop->GetCurvature()},
        {"curvature_max_error", loop->GetCurvatureMaxError()},
        {"rect_bound", rect_json(loop->GetRectBound())},
        {"cap_bound", cap_json(loop->GetCapBound())},
        {"reference_point",
         {{"point", point_json(rp.point)}, {"contained", rp.contained}}},
    };
}

int main() {
    json out;
    auto catalog = Catalog();

    // TEST(S2LoopTestBase, GetRectBound)
    // TEST(S2LoopTestBase, AreaConsistentWithCurvature)
    // TEST(S2LoopTestBase, GetAreaAndCentroid)
    // TEST(S2LoopTestBase, GetCurvature)
    {
        json arr = json::array();
        for (const auto &nl : catalog)
            arr.push_back(loop_summary(nl));
        out["loops"] = arr;
    }

    // Vertex wrap-around behavior: vertex(i) for i in [0, 2n).
    {
        json arr = json::array();
        for (const auto &nl : catalog) {
            if (nl.vertices.size() < 2)
                continue;
            auto loop = MakeLoop(nl.vertices);
            int n = loop->num_vertices();
            json wrap = json::array();
            for (int i = 0; i < 2 * n; ++i)
                wrap.push_back(point_json(loop->vertex(i)));
            arr.push_back({{"name", nl.name},
                           {"vertices", points_json(nl.vertices)},
                           {"wrapped", wrap}});
        }
        out["vertex_wrap"] = arr;
    }

    // TEST(S2Loop, LoopFromCell)
    {
        json arr = json::array();
        const vector<string> debug_strs = {"1/03",       "2/12",      "4/210",
                                           "5/00112233", "1/0123",    "3/2",
                                           "4/00000000", "5/33333333"};
        for (const auto &s : debug_strs) {
            S2CellId id = S2CellId::FromDebugString(s);
            S2Cell cell(id);
            S2Loop loop(cell);
            vector<S2Point> verts;
            for (int i = 0; i < loop.num_vertices(); ++i)
                verts.push_back(loop.vertex(i));
            arr.push_back({{"cell_id_token", id.ToToken()},
                           {"vertices", points_json(verts)},
                           {"area", loop.GetArea()},
                           {"is_valid", loop.IsValid()}});
        }
        out["from_cell"] = arr;
    }

    // TEST(S2Loop, Invert)
    {
        json arr = json::array();
        for (const auto &nl : catalog) {
            auto loop = MakeLoop(nl.vertices);
            loop->Invert();
            vector<S2Point> after;
            for (int i = 0; i < loop->num_vertices(); ++i)
                after.push_back(loop->vertex(i));
            arr.push_back({{"name", nl.name},
                           {"before", points_json(nl.vertices)},
                           {"after", points_json(after)},
                           {"is_full_after", loop->is_full()},
                           {"is_empty_after", loop->is_empty()}});
        }
        out["invert"] = arr;
    }

    // TEST(S2Loop, Normalize) - we verify that after Normalize, IsNormalized is
    // true and the area is at most 2*pi.
    {
        json arr = json::array();
        for (const auto &nl : catalog) {
            auto loop = MakeLoop(nl.vertices);
            loop->Normalize();
            vector<S2Point> after;
            for (int i = 0; i < loop->num_vertices(); ++i)
                after.push_back(loop->vertex(i));
            arr.push_back({{"name", nl.name},
                           {"before", points_json(nl.vertices)},
                           {"after", points_json(after)},
                           {"area_after", loop->GetArea()}});
        }
        out["normalize"] = arr;
    }

    // TEST(S2Loop, ContainsPoint*) - probe each loop with a few sample points.
    {
        json arr = json::array();
        const vector<S2Point> probes = {
            S2Point(1, 0, 0),
            S2Point(0, 1, 0),
            S2Point(0, 0, 1),
            S2Point(-1, 0, 0),
            S2Point(0, -1, 0),
            S2Point(0, 0, -1),
            S2LatLng::FromDegrees(45, 45).ToPoint(),
            S2LatLng::FromDegrees(-45, -45).ToPoint(),
            S2LatLng::FromDegrees(0, 0).ToPoint(),
            S2LatLng::FromDegrees(40, 22).ToPoint(),
        };
        for (const auto &nl : catalog) {
            auto loop = MakeLoop(nl.vertices);
            json cases = json::array();
            for (const auto &p : probes)
                cases.push_back({{"point", point_json(p)},
                                 {"contains", loop->Contains(p)}});
            arr.push_back({{"name", nl.name},
                           {"vertices", points_json(nl.vertices)},
                           {"cases", cases}});
        }
        out["contains_point"] = arr;
    }

    // TEST(S2Loop, ContainsCell, MayIntersect) - probe each loop with the six
    // face cells and a few small cells.
    {
        json arr = json::array();
        vector<S2CellId> probes;
        for (int f = 0; f < 6; ++f)
            probes.push_back(S2CellId::FromFace(f));
        // A handful of smaller cells to exercise the bounded checks.
        for (const string &s : {"1/0", "1/03", "3/22", "4/00", "5/33"})
            probes.push_back(S2CellId::FromDebugString(s));
        for (const auto &nl : catalog) {
            auto loop = MakeLoop(nl.vertices);
            json cases = json::array();
            for (const auto &id : probes) {
                S2Cell cell(id);
                cases.push_back({{"cell_id_token", id.ToToken()},
                                 {"contains_cell", loop->Contains(cell)},
                                 {"may_intersect", loop->MayIntersect(cell)}});
            }
            arr.push_back({{"name", nl.name},
                           {"vertices", points_json(nl.vertices)},
                           {"cases", cases}});
        }
        out["cell_relations"] = arr;
    }

    // TEST(S2Loop, Equals, BoundaryEquals, BoundaryApproxEquals)
    {
        // Pairs of (a_name, b_name, b_vertices) - if b_vertices is empty, use
        // the catalog entry by name.
        struct Case {
            string name;
            vector<S2Point> a;
            vector<S2Point> b;
        };
        vector<Case> cases;
        // Identical
        cases.push_back({"north_eq_north",
                         ParseLatLngs("0:-180, 0:-90, 0:0, 0:90"),
                         ParseLatLngs("0:-180, 0:-90, 0:0, 0:90")});
        // Cyclically rotated (BoundaryEquals true, Equal false)
        cases.push_back({"north_rot1", ParseLatLngs("0:-180, 0:-90, 0:0, 0:90"),
                         ParseLatLngs("0:-90, 0:0, 0:90, 0:-180")});
        // Reversed (BoundaryEquals false)
        cases.push_back({"north_vs_south",
                         ParseLatLngs("0:-180, 0:-90, 0:0, 0:90"),
                         ParseLatLngs("0:90, 0:0, 0:-90, 0:-180")});
        // Different size
        cases.push_back({"north_vs_triangle",
                         ParseLatLngs("0:-180, 0:-90, 0:0, 0:90"),
                         ParseLatLngs("0:-180, 0:-60, 0:60")});
        // Empty vs full
        cases.push_back(
            {"empty_vs_full", {S2Point(0, 0, 1)}, {S2Point(0, 0, -1)}});
        // Empty vs empty
        cases.push_back(
            {"empty_vs_empty", {S2Point(0, 0, 1)}, {S2Point(0, 0, 1)}});
        json arr = json::array();
        for (const auto &c : cases) {
            auto la = MakeLoop(c.a);
            auto lb = MakeLoop(c.b);
            arr.push_back(
                {{"name", c.name},
                 {"a", points_json(c.a)},
                 {"b", points_json(c.b)},
                 {"equal", la->Equals(*lb)},
                 {"boundary_equals", la->BoundaryEquals(*lb)},
                 {"boundary_approx_equals_default",
                  la->BoundaryApproxEquals(*lb, S1Angle::Radians(1e-15))}});
        }
        out["equality"] = arr;
    }

    // Approximate-boundary equality with perturbed vertices.
    {
        const auto base = ParseLatLngs("0:0, 0:90, 90:0");
        vector<S2Point> perturbed;
        for (const auto &p : base) {
            S2Point q = (p + S2Point(1e-10, 0, 0)).Normalize();
            perturbed.push_back(q);
        }
        auto la = MakeLoop(base);
        auto lb = MakeLoop(perturbed);
        out["approx_equality_perturbed"] = {
            {"a", points_json(base)},
            {"b", points_json(perturbed)},
            {"approx_equal_loose",
             la->BoundaryApproxEquals(*lb, S1Angle::Radians(1e-5))},
            {"approx_equal_tight",
             la->BoundaryApproxEquals(*lb, S1Angle::Radians(1e-15))}};
    }

    // Shape interface: emit edges/chains/chain_positions for each loop.
    {
        json arr = json::array();
        for (const auto &nl : catalog) {
            auto loop = MakeLoop(nl.vertices);
            S2Loop::Shape shape(loop.get());
            json edge_arr = json::array();
            for (int i = 0; i < shape.num_edges(); ++i)
                edge_arr.push_back(edge_json(shape.edge(i)));
            json chain_arr = json::array();
            for (int i = 0; i < shape.num_chains(); ++i) {
                auto c = shape.chain(i);
                chain_arr.push_back(json::array({c.start, c.length}));
            }
            json chain_positions = json::array();
            for (int e = 0; e < shape.num_edges(); ++e) {
                auto cp = shape.chain_position(e);
                chain_positions.push_back(
                    json::array({cp.chain_id, cp.offset}));
            }
            arr.push_back({{"name", nl.name},
                           {"vertices", points_json(nl.vertices)},
                           {"num_edges", shape.num_edges()},
                           {"num_chains", shape.num_chains()},
                           {"edges", edge_arr},
                           {"chains", chain_arr},
                           {"chain_positions", chain_positions}});
        }
        out["shape"] = arr;
    }

    // TEST(S2Loop, FindValidationError)
    {
        struct Case {
            string name;
            vector<S2Point> vertices;
            bool valid;
        };
        vector<Case> cases;
        cases.push_back({"empty_loop", {S2Point(0, 0, 1)}, true});
        cases.push_back({"full_loop", {S2Point(0, 0, -1)}, true});
        cases.push_back(
            {"valid_triangle", ParseLatLngs("0:0, 0:10, 10:0"), true});
        cases.push_back({"too_few_vertices", ParseLatLngs("0:0, 0:10"), false});
        cases.push_back(
            {"duplicate_vertices", ParseLatLngs("0:0, 0:10, 0:0"), false});
        // Antipodal vertices (vertex(i) == -vertex(i+1)).
        cases.push_back(
            {"antipodal_vertices",
             {S2Point(1, 0, 0), S2Point(-1, 0, 0), S2Point(0, 1, 0)},
             false});
        // Non-unit-length vertex.
        cases.push_back({"non_unit_length",
                         {S2Point(1, 0, 0), S2Point(2, 0, 0).Normalize() * 1.5,
                          S2Point(0, 1, 0)},
                         false});
        json arr = json::array();
        for (const auto &c : cases) {
            auto loop = MakeLoop(c.vertices);
            S2Error err;
            bool found = loop->FindValidationError(&err);
            arr.push_back({{"name", c.name},
                           {"vertices", points_json(c.vertices)},
                           {"is_valid", !found},
                           {"expected_valid", c.valid}});
        }
        out["validation"] = arr;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
