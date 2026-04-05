// Golden data generator for S2Cell.

#include "s2/s2cell.h"

#include <iostream>
#include <nlohmann/json.hpp>
#include <vector>

#include "s2/s2cell_id.h"
#include "s2/s2coords.h"
#include "s2/s2latlng.h"
#include "s2/s2point.h"

using json = nlohmann::json;

json point_to_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

json rect_to_json(const R2Rect &r) {
    return {{"x", {r.x().lo(), r.x().hi()}}, {"y", {r.y().lo(), r.y().hi()}}};
}

int main() {
    json root;

    // TEST(S2Cell, TestFaces) - face constructors, accessors, bounds,
    // center, vertices, edges
    {
        json faces = json::array();
        for (int face = 0; face < 6; ++face) {
            S2CellId id = S2CellId::FromFace(face);
            S2Cell cell(id);
            json c;
            c["id"] = id.ToToken();
            c["face"] = cell.face();
            c["level"] = cell.level();
            c["orientation"] = cell.orientation();
            c["uv"] = rect_to_json(cell.GetBoundUV());
            c["center"] = point_to_json(cell.GetCenter());
            c["center_raw"] = point_to_json(cell.GetCenterRaw());
            json vertices = json::array();
            json vertices_raw = json::array();
            json edges = json::array();
            json edges_raw = json::array();
            for (int k = 0; k < 4; ++k) {
                vertices.push_back(point_to_json(cell.GetVertex(k)));
                vertices_raw.push_back(point_to_json(cell.GetVertexRaw(k)));
                edges.push_back(point_to_json(cell.GetEdge(k)));
                edges_raw.push_back(point_to_json(cell.GetEdgeRaw(k)));
            }
            c["vertices"] = vertices;
            c["vertices_raw"] = vertices_raw;
            c["edges"] = edges;
            c["edges_raw"] = edges_raw;
            faces.push_back(c);
        }
        root["faces"] = faces;
    }

    // TEST(S2Cell, TestSubdivide) - child ids, face/level/orientation, bound_uv
    {
        json subdivide = json::array();
        S2CellId id = S2CellId::FromFace(0).child(0).child(3);
        S2Cell cell(id);
        S2Cell children[4];
        cell.Subdivide(children);
        for (int i = 0; i < 4; ++i) {
            json c;
            c["id"] = children[i].id().ToToken();
            c["face"] = children[i].face();
            c["level"] = children[i].level();
            c["orientation"] = children[i].orientation();
            c["uv"] = rect_to_json(children[i].GetBoundUV());
            subdivide.push_back(c);
        }
        root["subdivide"] = subdivide;
    }

    // Extra coverage: areas
    {
        json areas = json::array();
        S2CellId ids[] = {S2CellId::FromFace(0), S2CellId::FromFace(1).child(1),
                          S2CellId::FromFace(2).child(0).child(2)};
        for (auto id : ids) {
            S2Cell cell(id);
            json a;
            a["id"] = id.ToToken();
            a["exact_area"] = cell.ExactArea();
            a["approx_area"] = cell.ApproxArea();
            a["avg_area"] = cell.AverageArea(cell.level());
            areas.push_back(a);
        }
        root["areas"] = areas;
    }

    // TEST(S2Cell, GetDistanceToPoint) - point distances and contains_point
    {
        json dist_point = json::array();
        S2Cell cell(S2CellId::FromFace(0).child(1).child(2));
        S2Point points[] = {cell.GetCenter(), cell.GetVertex(0),
                            S2Point(1, 1, 1).Normalize(),
                            S2Point(-1, -1, -1).Normalize()};
        for (auto p : points) {
            json d;
            d["cell_id"] = cell.id().ToToken();
            d["target"] = point_to_json(p);
            d["distance"] = cell.GetDistance(p).length2();
            d["boundary_distance"] = cell.GetBoundaryDistance(p).length2();
            d["max_distance"] = cell.GetMaxDistance(p).length2();
            d["contains"] = cell.Contains(p);
            dist_point.push_back(d);
        }
        root["distance_point"] = dist_point;
    }

    // TEST(S2Cell, GetUVCoordOfEdge) / TEST(S2Cell, GetSizeIJAgreesWithCellId)
    // / TEST(S2Cell, GetIJCoordOfEdge) - edge coordinate accessors
    {
        json edge_coords = json::array();
        std::vector<S2CellId> ids = {S2CellId::FromToken("0f"),
                                     S2CellId::FromToken("05"),
                                     S2CellId::FromToken("1b"),
                                     S2CellId::FromToken("11"),
                                     S2CellId::FromToken("8f"),
                                     S2CellId::FromToken("85"),
                                     S2CellId::FromToken("9b"),
                                     S2CellId::FromToken("91"),
                                     S2CellId::FromFace(2).child(0).child(2),
                                     S2CellId::FromToken("123456789")};
        for (const auto &id : ids) {
            S2Cell cell(id);
            json c;
            c["id"] = id.ToToken();
            c["size_ij"] = cell.GetSizeIJ();
            json uv_coords = json::array();
            json ij_coords = json::array();
            for (int k = 0; k < 4; ++k) {
                uv_coords.push_back(cell.GetUVCoordOfEdge(k));
                ij_coords.push_back(cell.GetIJCoordOfEdge(k));
            }
            c["uv_coords"] = uv_coords;
            c["ij_coords"] = ij_coords;
            edge_coords.push_back(c);
        }
        root["edge_coords"] = edge_coords;
    }

    // TEST(S2Cell, ConsistentWithS2CellIdFromPointExample1) /
    // TEST(S2CellId, AmbiguousContainsPoint) - known near-boundary examples
    {
        json contains_examples = json::array();

        {
            S2Point p(0.38203141040035632, 0.030196609707941954,
                      0.9236558700239289);
            S2Cell cell{S2CellId(p)};
            json c;
            c["cell_id"] = cell.id().ToToken();
            c["target"] = point_to_json(p);
            c["contains"] = cell.Contains(p);
            contains_examples.push_back(c);
        }

        {
            S2Point p = S2LatLng::FromDegrees(-2, 90).ToPoint();
            S2CellId cell_id = S2CellId(p).parent(1);
            S2Cell cell(cell_id);
            json c;
            c["cell_id"] = cell.id().ToToken();
            c["target"] = point_to_json(p);
            c["contains"] = cell.Contains(p);
            contains_examples.push_back(c);
        }

        root["contains_examples"] = contains_examples;
    }

    std::cout << root.dump(2) << std::endl;
    return 0;
}
