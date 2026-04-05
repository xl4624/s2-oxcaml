// Golden data generator for S2_coords.

#include "s2/s2coords.h"

#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/s2coords_internal.h"
#include "s2/s2point.h"

using json = nlohmann::json;

json point_json(const S2Point &p) {
    return json::array({p.x(), p.y(), p.z()});
}

int main() {
    json out;

    // TEST(S2, ST_UV_Conversions) - st_uv_conversions
    {
        json cases = json::array();
        // Boundary conditions: STtoUV at 0, 0.5, 1
        for (double s : {0.0, 0.5, 1.0}) {
            double u = S2::STtoUV(s);
            cases.push_back({
                {"op", "st_to_uv"},
                {"input", s},
                {"result", u},
                {"expected_linear", 2 * s - 1},
            });
        }
        // Boundary conditions: UVtoST at -1, 0, 1
        for (double u : {-1.0, 0.0, 1.0}) {
            double s = S2::UVtoST(u);
            cases.push_back({
                {"op", "uv_to_st"},
                {"input", u},
                {"result", s},
                {"expected_linear", 0.5 * (u + 1)},
            });
        }
        // Round-trip: UVtoST(STtoUV(x)) ≈ x
        for (double x = 0; x <= 1; x += 0.0001) {
            double rt = S2::UVtoST(S2::STtoUV(x));
            cases.push_back({
                {"op", "st_roundtrip"},
                {"input", x},
                {"result", rt},
            });
        }
        // Round-trip: STtoUV(UVtoST(u)) ≈ u
        for (double x = 0; x <= 1; x += 0.0001) {
            double u = 2 * x - 1;
            double rt = S2::STtoUV(S2::UVtoST(u));
            cases.push_back({
                {"op", "uv_roundtrip"},
                {"input", u},
                {"result", rt},
            });
        }
        out["st_uv_conversions"] = cases;
    }

    // TEST(S2, STtoIJBoundaries) and TEST(S2, STtoIJHalfway) - st_to_ij
    {
        json cases = json::array();
        auto add = [&](double s, int expected) {
            cases.push_back({
                {"input", s},
                {"expected", expected},
            });
        };

        // TEST(S2, STtoIJBoundaries)
        add(0.0, S2::STtoIJ(0.0));
        add(1.0, S2::STtoIJ(1.0));

        // TEST(S2, STtoIJHalfway)
        constexpr double kRecipLimitIJ = 1.0 / S2::kLimitIJ;
        add(0.5 * kRecipLimitIJ, S2::STtoIJ(0.5 * kRecipLimitIJ));
        add(1.0 * kRecipLimitIJ, S2::STtoIJ(1.0 * kRecipLimitIJ));
        add(1.5 * kRecipLimitIJ, S2::STtoIJ(1.5 * kRecipLimitIJ));
        add(2.0 * kRecipLimitIJ, S2::STtoIJ(2.0 * kRecipLimitIJ));
        add(2.5 * kRecipLimitIJ, S2::STtoIJ(2.5 * kRecipLimitIJ));
        add(3.0 * kRecipLimitIJ, S2::STtoIJ(3.0 * kRecipLimitIJ));
        add((S2::kLimitIJ - 2.0) * kRecipLimitIJ,
            S2::STtoIJ((S2::kLimitIJ - 2.0) * kRecipLimitIJ));
        add((S2::kLimitIJ - 1.0) * kRecipLimitIJ,
            S2::STtoIJ((S2::kLimitIJ - 1.0) * kRecipLimitIJ));
        add((S2::kLimitIJ - 0.5) * kRecipLimitIJ,
            S2::STtoIJ((S2::kLimitIJ - 0.5) * kRecipLimitIJ));

        // Negative input
        add(-0.1, S2::STtoIJ(-0.1));

        out["st_to_ij"] = cases;
    }

    // TEST(S2, FaceUVtoXYZ) - face_uv_to_xyz
    {
        json cases = json::array();

        // Face centers (u=0, v=0)
        json sum = json::array({0.0, 0.0, 0.0});
        for (int face = 0; face < 6; ++face) {
            S2Point center = S2::FaceUVtoXYZ(face, 0, 0);
            S2Point norm = S2::GetNorm(face);
            cases.push_back({
                {"face", face},
                {"u", 0.0},
                {"v", 0.0},
                {"result", point_json(center)},
                {"norm", point_json(norm)},
                {"largest_component", center.LargestAbsComponent()},
                {"largest_abs", fabs(center[center.LargestAbsComponent()])},
            });
        }

        // Right-handed coordinate system check
        for (int face = 0; face < 6; ++face) {
            S2Point u_axis = S2::GetUAxis(face);
            S2Point v_axis = S2::GetVAxis(face);
            S2Point center = S2::FaceUVtoXYZ(face, 0, 0);
            double cross_dot = u_axis.CrossProd(v_axis).DotProd(center);
            cases.push_back({
                {"face", face},
                {"u", -99.0},
                {"v", -99.0},
                {"result", point_json(center)},
                {"cross_dot", cross_dot},
            });
        }

        // Hilbert curve continuity
        for (int face = 0; face < 6; ++face) {
            int sign = (face & S2::internal::kSwapMask) ? -1 : 1;
            S2Point end_pt = S2::FaceUVtoXYZ(face, sign, -sign);
            S2Point next_start = S2::FaceUVtoXYZ((face + 1) % 6, -1, -1);
            cases.push_back({
                {"face", face},
                {"u", (double)sign},
                {"v", (double)(-sign)},
                {"result", point_json(end_pt)},
                {"next_start", point_json(next_start)},
            });
        }

        out["face_uv_to_xyz"] = cases;
    }

    // TEST(S2, FaceXYZtoUVW) - face_xyz_to_uvw
    {
        json cases = json::array();
        for (int face = 0; face < 6; ++face) {
            auto test = [&](const S2Point &input, const S2Point &expected) {
                S2Point result = S2::FaceXYZtoUVW(face, input);
                cases.push_back({
                    {"face", face},
                    {"input", point_json(input)},
                    {"expected", point_json(expected)},
                    {"result", point_json(result)},
                });
            };
            test(S2Point(0, 0, 0), S2Point(0, 0, 0));
            test(S2::GetUAxis(face), S2Point(1, 0, 0));
            test(-S2::GetUAxis(face), S2Point(-1, 0, 0));
            test(S2::GetVAxis(face), S2Point(0, 1, 0));
            test(-S2::GetVAxis(face), S2Point(0, -1, 0));
            test(S2::GetNorm(face), S2Point(0, 0, 1));
            test(-S2::GetNorm(face), S2Point(0, 0, -1));
        }
        out["face_xyz_to_uvw"] = cases;
    }

    // TEST(S2, UVNorms) - uv_norms
    {
        json cases = json::array();
        for (int face = 0; face < 6; ++face) {
            for (double x = -1; x <= 1; x += 1.0 / 8) {
                S2Point p1 = S2::FaceUVtoXYZ(face, x, -1);
                S2Point p2 = S2::FaceUVtoXYZ(face, x, 1);
                S2Point u_norm = S2::GetUNorm(face, x);
                double u_angle = p1.CrossProd(p2).Angle(u_norm);

                S2Point q1 = S2::FaceUVtoXYZ(face, -1, x);
                S2Point q2 = S2::FaceUVtoXYZ(face, 1, x);
                S2Point v_norm = S2::GetVNorm(face, x);
                double v_angle = q1.CrossProd(q2).Angle(v_norm);

                cases.push_back({
                    {"face", face},
                    {"x", x},
                    {"u_norm", point_json(u_norm)},
                    {"v_norm", point_json(v_norm)},
                    {"u_angle", u_angle},
                    {"v_angle", v_angle},
                });
            }
        }
        out["uv_norms"] = cases;
    }

    // TEST(S2, UVWAxis) - uvw_axis
    {
        json cases = json::array();
        for (int face = 0; face < 6; ++face) {
            S2Point u_axis = S2::GetUAxis(face);
            S2Point v_axis = S2::GetVAxis(face);
            S2Point norm = S2::GetNorm(face);
            S2Point center = S2::FaceUVtoXYZ(face, 0, 0);
            S2Point u_from_face =
                S2::FaceUVtoXYZ(face, 1, 0) - S2::FaceUVtoXYZ(face, 0, 0);
            S2Point v_from_face =
                S2::FaceUVtoXYZ(face, 0, 1) - S2::FaceUVtoXYZ(face, 0, 0);
            double cross_dot = u_axis.CrossProd(v_axis).DotProd(norm);

            cases.push_back({
                {"face", face},
                {"u_axis", point_json(u_axis)},
                {"v_axis", point_json(v_axis)},
                {"norm", point_json(norm)},
                {"center", point_json(center)},
                {"u_from_face", point_json(u_from_face)},
                {"v_from_face", point_json(v_from_face)},
                {"cross_dot", cross_dot},
                {"uvw_axis_0", point_json(S2::GetUVWAxis(face, 0))},
                {"uvw_axis_1", point_json(S2::GetUVWAxis(face, 1))},
                {"uvw_axis_2", point_json(S2::GetUVWAxis(face, 2))},
            });
        }
        out["uvw_axis"] = cases;
    }

    // TEST(S2, UVWFace) - uvw_face
    {
        json cases = json::array();
        for (int face = 0; face < 6; ++face) {
            for (int axis = 0; axis < 3; ++axis) {
                S2Point axis_vec = S2::GetUVWAxis(face, axis);
                int neg_face = S2::GetFace(-axis_vec);
                int pos_face = S2::GetFace(axis_vec);
                cases.push_back({
                    {"face", face},
                    {"axis", axis},
                    {"neg_face", neg_face},
                    {"pos_face", pos_face},
                    {"uvw_face_neg", S2::GetUVWFace(face, axis, 0)},
                    {"uvw_face_pos", S2::GetUVWFace(face, axis, 1)},
                });
            }
        }
        out["uvw_face"] = cases;
    }

    // SiTi/ST conversions - si_ti_conversions
    {
        json cases = json::array();
        auto add = [&](unsigned int si) {
            double st = S2::SiTitoST(si);
            cases.push_back({
                {"si", si},
                {"st", st},
            });
        };
        add(0);
        add(1);
        add(S2::kMaxSiTi / 2);
        add(S2::kMaxSiTi);
        add(S2::kMaxSiTi - 1);

        // STtoSiTi round-trips
        json rt_cases = json::array();
        for (double s : {0.0, 0.25, 0.5, 0.75, 1.0}) {
            unsigned int si = S2::STtoSiTi(s);
            double rt = S2::SiTitoST(si);
            rt_cases.push_back({
                {"input_st", s},
                {"si_ti", si},
                {"roundtrip_st", rt},
            });
        }

        out["si_ti_to_st"] = cases;
        out["st_to_si_ti"] = rt_cases;
    }

    // IJtoSTMin - ij_to_st_min
    {
        json cases = json::array();
        for (int i : {0, 1, S2::kLimitIJ / 2, S2::kLimitIJ - 1, S2::kLimitIJ}) {
            double st_min = S2::IJtoSTMin(i);
            cases.push_back({
                {"i", i},
                {"st_min", st_min},
            });
        }
        out["ij_to_st_min"] = cases;
    }

    // FaceXYZtoUV (with validity check) - face_xyz_to_uv
    {
        json cases = json::array();
        for (int face = 0; face < 6; ++face) {
            // Point on the positive side of the face
            S2Point p = S2::FaceUVtoXYZ(face, 0.5, 0.5);
            double u, v;
            bool ok = S2::FaceXYZtoUV(face, p, &u, &v);
            cases.push_back({
                {"face", face},
                {"point", point_json(p)},
                {"ok", ok},
                {"u", u},
                {"v", v},
            });

            // Point on the wrong face
            S2Point neg = -S2::GetNorm(face);
            double u2, v2;
            bool ok2 = S2::FaceXYZtoUV(face, neg, &u2, &v2);
            cases.push_back({
                {"face", face},
                {"point", point_json(neg)},
                {"ok", ok2},
                {"u", 0.0},
                {"v", 0.0},
            });
        }
        out["face_xyz_to_uv"] = cases;
    }

    // GetFace - get_face
    {
        json cases = json::array();
        S2Point test_points[] = {
            S2Point(1, 0, 0),       S2Point(0, 1, 0),
            S2Point(0, 0, 1),       S2Point(-1, 0, 0),
            S2Point(0, -1, 0),      S2Point(0, 0, -1),
            S2Point(1, 1, 1),       S2Point(-1, -1, -1),
            S2Point(0.5, 0.3, 0.1), S2Point(-0.7, 0.2, -0.8),
        };
        for (const auto &p : test_points) {
            cases.push_back({
                {"point", point_json(p)},
                {"face", S2::GetFace(p)},
            });
        }
        out["get_face"] = cases;
    }

    // XYZtoFaceUV - xyz_to_face_uv
    {
        json cases = json::array();
        S2Point test_points[] = {
            S2Point(1, 0.5, 0.3),   S2Point(-0.5, 1, 0.2),
            S2Point(-0.3, -0.2, 1), S2Point(-1, -0.4, -0.6),
            S2Point(0.7, -1, -0.3), S2Point(0.1, 0.2, -1),
        };
        for (const auto &p : test_points) {
            double u, v;
            int face = S2::XYZtoFaceUV(p, &u, &v);
            cases.push_back({
                {"point", point_json(p)},
                {"face", face},
                {"u", u},
                {"v", v},
            });
        }
        out["xyz_to_face_uv"] = cases;
    }

    // Constants
    {
        json constants;
        constants["max_cell_level"] = S2::kMaxCellLevel;
        constants["limit_ij"] = S2::kLimitIJ;
        constants["max_si_ti"] = S2::kMaxSiTi;
        constants["max_xyz_to_uv_error"] = S2::kMaxXYZtoUVError;
        out["constants"] = constants;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
