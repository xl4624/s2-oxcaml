// Golden data generator for S2Polygon.

#include "s2/s2polygon.h"

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <utility>
#include <vector>

#include "s2/s1angle.h"
#include "s2/s2cap.h"
#include "s2/s2cell.h"
#include "s2/s2cell_id.h"
#include "s2/s2debug.h"
#include "s2/s2latlng.h"
#include "s2/s2latlng_rect.h"
#include "s2/s2loop.h"
#include "s2/s2point.h"
#include "s2/s2shape.h"

using json = nlohmann::json;
using std::string;
using std::unique_ptr;
using std::vector;

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
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

// Parse comma-separated "lat:lng" pairs in degrees. Copied from s2loop.cc so
// both generators stay self-contained.
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

// A polygon defined as a set of named loops. Each loop is described by its
// vertex array; the OCaml side rebuilds the identical loop values.
struct NamedPolygon {
    string name;
    vector<vector<S2Point>> loop_vertices;
};

static unique_ptr<S2Polygon> MakePolygon(const NamedPolygon &np) {
    vector<unique_ptr<S2Loop>> loops;
    for (const auto &v : np.loop_vertices)
        loops.push_back(MakeLoop(v));
    return std::make_unique<S2Polygon>(std::move(loops));
}

static vector<NamedPolygon> Catalog() {
    return {
        {"empty", {}},
        {"full", {{S2Point(0, 0, -1)}}},
        {"single_shell_near", {ParseLatLngs("35:20, 45:20, 40:25")}},
        {"square", {ParseLatLngs("-10:-10, -10:10, 10:10, 10:-10")}},
        {"big_square", {ParseLatLngs("-30:-30, -30:30, 30:30, 30:-30")}},
        {"north_hemi", {ParseLatLngs("0:-180, 0:-90, 0:0, 0:90")}},
        {"arctic_80", {ParseLatLngs("80:-150, 80:-30, 80:90")}},
        {"antarctic_80", {ParseLatLngs("-80:120, -80:0, -80:-120")}},
        // Disjoint multi-shell polygon: two non-nested shells with a
        // longitude gap. Valid input for S2BooleanOperation-based
        // contains / intersects - exercises the multi-loop path now
        // that S2_polygon routes through S2BooleanOperation.
        {"two_shells",
         {ParseLatLngs("5:-20, 5:-10, 15:-10, 15:-20"),
          ParseLatLngs("-15:10, -15:20, -5:20, -5:10")}},
        // Polygon with a hole. Property tests only - the CW outer-shell
        // vertex order used here makes the polygon "invalid" per S2's
        // nesting validation, and C++ S2BooleanOperation has special
        // undefined-on-invalid-input behavior that this port does not
        // mirror.
        {"square_with_hole",
         {ParseLatLngs("-20:-20, -20:20, 20:20, 20:-20"),
          ParseLatLngs("-5:-5, 5:-5, 5:5, -5:5")}},
    };
}

static vector<NamedPolygon> RelationsCatalog() {
    // Exercise multi-loop polygons (two_shells) while keeping out
    // "square_with_hole", whose CW shell orientation makes the polygon
    // formally invalid. The simplified predicate implementation assumes
    // valid inputs and does not mirror C++ undefined-on-invalid
    // behaviour.
    vector<NamedPolygon> all = Catalog();
    vector<NamedPolygon> out;
    for (const auto &np : all) {
        if (np.name != "square_with_hole")
            out.push_back(np);
    }
    return out;
}

static json polygon_properties(const NamedPolygon &np) {
    auto p = MakePolygon(np);
    json loops = json::array();
    bool has_holes = false;
    for (int i = 0; i < p->num_loops(); ++i) {
        const S2Loop *l = p->loop(i);
        if (l->is_hole())
            has_holes = true;
        vector<S2Point> verts;
        for (int j = 0; j < l->num_vertices(); ++j)
            verts.push_back(l->vertex(j));
        json entry;
        entry["vertices"] = points_json(verts);
        entry["depth"] = l->depth();
        entry["is_hole"] = l->is_hole();
        entry["contains_origin"] = l->Contains(S2::Origin());
        loops.push_back(entry);
    }
    json out;
    out["name"] = np.name;
    out["loops"] = loops;
    out["num_loops"] = p->num_loops();
    out["num_vertices"] = p->num_vertices();
    out["is_empty"] = p->is_empty();
    out["is_full"] = p->is_full();
    out["has_holes"] = has_holes;
    out["rect_bound"] = rect_json(p->GetRectBound());
    out["area"] = p->GetArea();
    out["centroid"] = point_json(p->GetCentroid());
    S2Polygon::Shape shape(p.get());
    out["num_edges"] = shape.num_edges();
    out["num_chains"] = shape.num_chains();
    out["dimension"] = shape.dimension();
    return out;
}

int main() {
    json out;
    auto catalog = Catalog();

    // Dump polygon identities so the OCaml side can rebuild the same loops
    // (including any reordering performed by S2Polygon during construction).
    {
        json arr = json::array();
        for (const auto &np : catalog) {
            json entry;
            entry["name"] = np.name;
            json loops = json::array();
            for (const auto &v : np.loop_vertices)
                loops.push_back(points_json(v));
            entry["loops"] = loops;
            arr.push_back(entry);
        }
        out["catalog"] = arr;
    }

    // TEST(S2Polygon, ...) - basic per-polygon properties.
    {
        json arr = json::array();
        for (const auto &np : catalog)
            arr.push_back(polygon_properties(np));
        out["polygons"] = arr;
    }

    // Point containment: a short battery of (polygon, point, expected).
    {
        const vector<S2Point> probes = {
            S2LatLng::FromDegrees(0, 0).ToPoint(),
            S2LatLng::FromDegrees(40, 22).ToPoint(),
            S2LatLng::FromDegrees(0, 0).ToPoint(),
            S2LatLng::FromDegrees(85, 0).ToPoint(),
            S2LatLng::FromDegrees(-85, 0).ToPoint(),
            S2LatLng::FromDegrees(5, 5).ToPoint(),
            S2LatLng::FromDegrees(10, 10).ToPoint(),
            S2LatLng::FromDegrees(50, 50).ToPoint(),
        };
        json arr = json::array();
        for (const auto &np : catalog) {
            auto p = MakePolygon(np);
            json cases = json::array();
            for (const auto &pt : probes) {
                cases.push_back({
                    {"point", point_json(pt)},
                    {"contains", p->Contains(pt)},
                });
            }
            arr.push_back({{"name", np.name}, {"cases", cases}});
        }
        out["point_containment"] = arr;
    }

    // Cell containment / intersection.
    {
        const vector<string> cell_debugs = {"1/03", "2/12", "4/210",
                                            "5/00112233", "1/0"};
        json arr = json::array();
        for (const auto &np : catalog) {
            auto p = MakePolygon(np);
            json cases = json::array();
            for (const auto &s : cell_debugs) {
                S2CellId id = S2CellId::FromDebugString(s);
                S2Cell cell(id);
                cases.push_back({
                    {"cell_id_token", id.ToToken()},
                    {"contains_cell", p->Contains(cell)},
                    {"may_intersect", p->MayIntersect(cell)},
                });
            }
            arr.push_back({{"name", np.name}, {"cases", cases}});
        }
        out["cell_containment"] = arr;
    }

    // Polygon-polygon Contains / Intersects over single-loop polygons only
    // (see RelationsCatalog for why).
    {
        auto relations = RelationsCatalog();
        json arr = json::array();
        for (const auto &a : relations) {
            auto pa = MakePolygon(a);
            for (const auto &b : relations) {
                auto pb = MakePolygon(b);
                json entry;
                entry["a"] = a.name;
                entry["b"] = b.name;
                entry["contains"] = pa->Contains(*pb);
                entry["intersects"] = pa->Intersects(*pb);
                arr.push_back(entry);
            }
        }
        out["polygon_relations"] = arr;
    }

    // Invert.
    {
        json arr = json::array();
        for (const auto &np : catalog) {
            auto p = MakePolygon(np);
            S2Polygon inv;
            inv.Copy(*p);
            inv.Invert();
            arr.push_back({
                {"name", np.name},
                {"num_loops_before", p->num_loops()},
                {"num_loops_after", inv.num_loops()},
                {"area_before", p->GetArea()},
                {"area_after", inv.GetArea()},
            });
        }
        out["invert"] = arr;
    }

    // Shape interface - exercise edges / chains for each polygon.
    {
        json arr = json::array();
        for (const auto &np : catalog) {
            auto p = MakePolygon(np);
            S2Polygon::Shape shape(p.get());
            json edges = json::array();
            for (int i = 0; i < shape.num_edges(); ++i) {
                auto e = shape.edge(i);
                edges.push_back(
                    {{"v0", point_json(e.v0)}, {"v1", point_json(e.v1)}});
            }
            json chains = json::array();
            for (int i = 0; i < shape.num_chains(); ++i) {
                auto c = shape.chain(i);
                chains.push_back({{"start", c.start}, {"length", c.length}});
            }
            auto rp = shape.GetReferencePoint();
            arr.push_back({
                {"name", np.name},
                {"edges", edges},
                {"chains", chains},
                {"dimension", shape.dimension()},
                {"reference_point",
                 {{"point", point_json(rp.point)},
                  {"contained", rp.contained}}},
            });
        }
        out["shape"] = arr;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
