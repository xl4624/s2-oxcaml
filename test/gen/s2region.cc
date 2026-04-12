// Golden data generator for S2_region.
//
// Covers the Region interface methods that are added in this port:
//   - S2Cap::Contains(Cell), MayIntersect(Cell), GetCellUnionBound,
//   GetRectBound
//   - S2LatLngRect::Contains(Cell), MayIntersect(Cell), GetCellUnionBound,
//     GetCapBound (already exists in OCaml; verified for parity)
//   - S2Cell::GetRectBound, GetCellUnionBound (Contains/Intersects/CapBound
//     already exist in OCaml)
//   - S2CellUnion::GetCapBound, GetRectBound, Contains(Cell),
//   MayIntersect(Cell),
//     GetCellUnionBound

#include <iostream>
#include <nlohmann/json.hpp>
#include <vector>

#include "s2/s1angle.h"
#include "s2/s1chord_angle.h"
#include "s2/s2cap.h"
#include "s2/s2cell.h"
#include "s2/s2cell_id.h"
#include "s2/s2cell_union.h"
#include "s2/s2latlng.h"
#include "s2/s2latlng_rect.h"
#include "s2/s2point.h"

using json = nlohmann::json;

static S2Point LatLngPoint(double lat_deg, double lng_deg) {
    return S2LatLng::FromDegrees(lat_deg, lng_deg).ToPoint();
}

static json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static json rect_json(const S2LatLngRect &r) {
    return {{"lat", {r.lat().lo(), r.lat().hi()}},
            {"lng", {r.lng().lo(), r.lng().hi()}}};
}

static json cap_json(const S2Cap &c) {
    return {{"center", point_json(c.center())},
            {"length2", c.radius().length2()}};
}

static json cell_ids_json(const std::vector<S2CellId> &ids) {
    json arr = json::array();
    for (const auto &id : ids)
        arr.push_back(id.ToToken());
    return arr;
}

static json cell_id_json(const S2CellId &id) {
    return id.ToToken();
}

// Build a representative set of cells (varying face/level) used as probes for
// contains_cell/intersects_cell tests across all region kinds.
static std::vector<S2CellId> ProbeCells() {
    std::vector<S2CellId> out;
    for (int face = 0; face < 6; ++face)
        out.push_back(S2CellId::FromFace(face));
    out.push_back(S2CellId::FromFace(0).child(1));
    out.push_back(S2CellId::FromFace(2).child(0).child(2));
    out.push_back(S2CellId::FromFace(3).child(0).child(0).child(0));
    out.push_back(S2CellId(LatLngPoint(40, -74)).parent(8));
    out.push_back(S2CellId(LatLngPoint(-30, 150)).parent(12));
    out.push_back(S2CellId(LatLngPoint(0, 0)).parent(5));
    out.push_back(S2CellId(LatLngPoint(85, 10)).parent(6));
    return out;
}

// Build a representative set of points used as probes for contains_point.
static std::vector<S2Point> ProbePoints() {
    std::vector<S2Point> out;
    out.push_back(LatLngPoint(0, 0));
    out.push_back(LatLngPoint(40, -74));
    out.push_back(LatLngPoint(-30, 150));
    out.push_back(LatLngPoint(85, 10));
    out.push_back(LatLngPoint(-89.9, 0));
    out.push_back(LatLngPoint(45, 90));
    out.push_back(S2Point(1, 0, 0));
    out.push_back(S2Point(0, 1, 0).Normalize());
    out.push_back(S2Point(1, 1, 1).Normalize());
    return out;
}

static json probe_points_json() {
    json arr = json::array();
    for (const auto &p : ProbePoints())
        arr.push_back(point_json(p));
    return arr;
}

static json probe_cells_json() {
    json arr = json::array();
    for (const auto &id : ProbeCells())
        arr.push_back(id.ToToken());
    return arr;
}

// Apply standard region API and emit results for each probe.
template <typename Region>
static json region_results_json(const Region &r) {
    json out;
    out["cap_bound"] = cap_json(r.GetCapBound());
    out["rect_bound"] = rect_json(r.GetRectBound());
    std::vector<S2CellId> ids;
    r.GetCellUnionBound(&ids);
    out["cell_union_bound"] = cell_ids_json(ids);

    json contains_cell = json::array();
    json intersects_cell = json::array();
    for (const auto &id : ProbeCells()) {
        S2Cell cell(id);
        contains_cell.push_back(r.Contains(cell));
        intersects_cell.push_back(r.MayIntersect(cell));
    }
    out["contains_cell"] = contains_cell;
    out["intersects_cell"] = intersects_cell;

    json contains_point = json::array();
    for (const auto &p : ProbePoints()) {
        contains_point.push_back(r.Contains(p));
    }
    out["contains_point"] = contains_point;
    return out;
}

int main() {
    json root;
    root["probe_cells"] = probe_cells_json();
    root["probe_points"] = probe_points_json();

    // ---- S2Cap ----
    {
        json arr = json::array();

        auto add = [&](const std::string &name, const S2Cap &c) {
            json e;
            e["name"] = name;
            e["cap"] = cap_json(c);
            e["region"] = region_results_json(c);
            arr.push_back(e);
        };

        add("empty", S2Cap::Empty());
        add("full", S2Cap::Full());
        add("tiny_xaxis", S2Cap(S2Point(1, 0, 0), S1Angle::Degrees(0.001)));
        add("small_nyc", S2Cap(LatLngPoint(40, -74), S1Angle::Degrees(0.5)));
        add("medium_equator", S2Cap(LatLngPoint(0, 0), S1Angle::Degrees(20)));
        add("large_north", S2Cap(LatLngPoint(60, 30), S1Angle::Degrees(60)));
        add("hemisphere_z", S2Cap::FromCenterHeight(S2Point(0, 0, 1), 1.0));
        add("near_full", S2Cap(S2Point(1, 0, 0), S1Angle::Degrees(179.5)));

        root["cap"] = arr;
    }

    // ---- S2LatLngRect ----
    {
        json arr = json::array();

        auto add = [&](const std::string &name, const S2LatLngRect &r) {
            json e;
            e["name"] = name;
            e["rect"] = rect_json(r);
            e["region"] = region_results_json(r);
            arr.push_back(e);
        };

        add("empty", S2LatLngRect::Empty());
        add("full", S2LatLngRect::Full());
        add("nyc", S2LatLngRect(R1Interval(S1Angle::Degrees(40.5).radians(),
                                           S1Angle::Degrees(41).radians()),
                                S1Interval(S1Angle::Degrees(-74.5).radians(),
                                           S1Angle::Degrees(-73.5).radians())));
        add("equator_band",
            S2LatLngRect(R1Interval(S1Angle::Degrees(-5).radians(),
                                    S1Angle::Degrees(5).radians()),
                         S1Interval(S1Angle::Degrees(-30).radians(),
                                    S1Angle::Degrees(30).radians())));
        add("antimeridian",
            S2LatLngRect(R1Interval(S1Angle::Degrees(-10).radians(),
                                    S1Angle::Degrees(10).radians()),
                         S1Interval(S1Angle::Degrees(170).radians(),
                                    S1Angle::Degrees(-170).radians())));
        add("polar_north",
            S2LatLngRect(R1Interval(S1Angle::Degrees(80).radians(),
                                    S1Angle::Degrees(90).radians()),
                         S1Interval::Full()));
        add("single_point",
            S2LatLngRect::FromPoint(S2LatLng::FromDegrees(0, 0)));

        root["rect"] = arr;
    }

    // ---- S2Cell ----
    {
        json arr = json::array();

        auto add = [&](const std::string &name, const S2CellId &id) {
            json e;
            e["name"] = name;
            e["cell_id"] = id.ToToken();
            S2Cell cell(id);
            e["region"] = region_results_json(cell);
            arr.push_back(e);
        };

        for (int face = 0; face < 6; ++face) {
            add("face_" + std::to_string(face), S2CellId::FromFace(face));
        }
        add("face0_child0", S2CellId::FromFace(0).child(0));
        add("face2_child1_child2", S2CellId::FromFace(2).child(1).child(2));
        add("nyc_level10", S2CellId(LatLngPoint(40, -74)).parent(10));
        add("antarctic_level5", S2CellId(LatLngPoint(-85, 0)).parent(5));
        add("leaf_origin", S2CellId(LatLngPoint(0, 0)));

        root["cell"] = arr;
    }

    // ---- S2CellUnion ----
    {
        json arr = json::array();

        auto add = [&](const std::string &name,
                       const std::vector<S2CellId> &ids) {
            json e;
            e["name"] = name;
            json id_arr = json::array();
            for (const auto &id : ids)
                id_arr.push_back(id.ToToken());
            e["cell_ids"] = id_arr;
            S2CellUnion union_(ids);
            e["region"] = region_results_json(union_);
            arr.push_back(e);
        };

        add("empty", {});
        add("single_face0", {S2CellId::FromFace(0)});
        add("two_faces", {S2CellId::FromFace(0), S2CellId::FromFace(3)});
        add("nyc_neighborhood", {S2CellId(LatLngPoint(40.5, -74.5)).parent(8),
                                 S2CellId(LatLngPoint(40.5, -73.5)).parent(8),
                                 S2CellId(LatLngPoint(41, -74)).parent(8)});
        add("scattered", {S2CellId(LatLngPoint(40, -74)).parent(10),
                          S2CellId(LatLngPoint(-30, 150)).parent(10),
                          S2CellId(LatLngPoint(85, 10)).parent(8)});
        add("whole_sphere", {S2CellId::FromFace(0), S2CellId::FromFace(1),
                             S2CellId::FromFace(2), S2CellId::FromFace(3),
                             S2CellId::FromFace(4), S2CellId::FromFace(5)});

        root["cell_union"] = arr;
    }

    std::cout << root.dump(2) << std::endl;
    return 0;
}
