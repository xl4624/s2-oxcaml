// Golden data generator for S2EdgeClipping.
// Mirrors s2geometry/src/s2/s2edge_clipping_test.cc.

#include "s2/s2edge_clipping.h"

#include <cfloat>
#include <cmath>
#include <iostream>
#include <nlohmann/json.hpp>
#include <vector>

#include "s2/r1interval.h"
#include "s2/r2.h"
#include "s2/r2rect.h"
#include "s2/s2coords.h"
#include "s2/s2edge_crossings.h"
#include "s2/s2point.h"

using json = nlohmann::json;

static json point3_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static json point2_json(const R2Point &p) {
    return {p.x(), p.y()};
}

static json rect_json(const R2Rect &r) {
    return {{"x", {r.x().lo(), r.x().hi()}}, {"y", {r.y().lo(), r.y().hi()}}};
}

// Emit one FaceSegments test case: raw input points, normalized points,
// and the resulting FaceSegmentVector computed by C++.
static json face_segments_case(const char *name, S2Point a_raw, S2Point b_raw) {
    S2Point a = a_raw.Normalize();
    S2Point b = b_raw.Normalize();
    S2::FaceSegmentVector segments;
    S2::GetFaceSegments(a, b, &segments);
    json segs = json::array();
    for (const auto &s : segments) {
        segs.push_back({{"face", s.face},
                        {"a", point2_json(s.a)},
                        {"b", point2_json(s.b)}});
    }
    return {{"name", name},
            {"a", point3_json(a)},
            {"b", point3_json(b)},
            {"segments", segs}};
}

// Emit one ClipToPaddedFace case per face, using a fixed padding.
static json clip_to_padded_face_case(const char *name, S2Point a_raw,
                                     S2Point b_raw, double padding) {
    S2Point a = a_raw.Normalize();
    S2Point b = b_raw.Normalize();
    json faces = json::array();
    for (int face = 0; face < 6; ++face) {
        R2Point a_uv, b_uv;
        bool ok = S2::ClipToPaddedFace(a, b, face, padding, &a_uv, &b_uv);
        json entry = {{"face", face}, {"intersects", ok}};
        if (ok) {
            entry["a_uv"] = point2_json(a_uv);
            entry["b_uv"] = point2_json(b_uv);
        }
        faces.push_back(entry);
    }
    return {{"name", name},
            {"a", point3_json(a)},
            {"b", point3_json(b)},
            {"padding", padding},
            {"faces", faces}};
}

// Emit one ClipEdge case for a 2D edge clipped against a rectangle.
static json clip_edge_case(const char *name, R2Point a, R2Point b,
                           R2Rect clip) {
    R2Point a_clip, b_clip;
    bool ok = S2::ClipEdge(a, b, clip, &a_clip, &b_clip);
    json entry = {{"name", name},
                  {"a", point2_json(a)},
                  {"b", point2_json(b)},
                  {"clip", rect_json(clip)},
                  {"intersects", ok}};
    if (ok) {
        entry["a_clipped"] = point2_json(a_clip);
        entry["b_clipped"] = point2_json(b_clip);
    }
    // Also emit the R2Rect GetClippedEdgeBound result (may be empty).
    R2Rect bound = S2::GetClippedEdgeBound(a, b, clip);
    entry["bound"] = rect_json(bound);
    entry["bound_is_empty"] = bound.is_empty();
    // And IntersectsRect for the same inputs.
    entry["intersects_rect"] = S2::IntersectsRect(a, b, clip);
    return entry;
}

int main() {
    json out;

    // TEST(S2, FaceClipping) - simple hand-picked cases from the upstream
    // test suite.  The randomized portion of the C++ test is omitted because
    // the randomized inputs depend on abseil's BitGen seeded from the
    // environment.
    {
        json cases = json::array();

        // An edge entirely contained within one cube face.
        cases.push_back(face_segments_case(
            "within_one_face", S2Point(1, -0.5, -0.5), S2Point(1, 0.5, 0.5)));
        // An edge that crosses one cube edge.
        cases.push_back(face_segments_case("cross_one_edge", S2Point(1, 0, 0),
                                           S2Point(0, 1, 0)));
        // An edge that crosses two opposite edges of face 0.
        cases.push_back(face_segments_case(
            "opposite_edges", S2Point(0.75, 0, -1), S2Point(0.75, 0, 1)));
        // An edge that crosses two adjacent edges of face 2.
        cases.push_back(face_segments_case(
            "adjacent_edges", S2Point(1, 0, 0.75), S2Point(0, 1, 0.75)));
        // An edge that crosses three cube edges (four faces).
        cases.push_back(face_segments_case("four_faces", S2Point(1, 0.9, 0.95),
                                           S2Point(-1, 0.95, 0.9)));
        // An edge from cube vertex to cube vertex (along a face diagonal).
        cases.push_back(face_segments_case("face_diagonal", S2Point(1, 1, 1),
                                           S2Point(1, -1, -1)));
        // An edge from one corner to the opposite corner through the origin.
        cases.push_back(face_segments_case("body_diagonal", S2Point(1, 1, 1),
                                           S2Point(-1, -1, -1)));
        // A zero-length edge (degenerate).
        cases.push_back(face_segments_case("zero_length", S2Point(1, 0.2, 0.3),
                                           S2Point(1, 0.2, 0.3)));

        out["face_segments"] = cases;
    }

    // TEST(S2, FaceClipping) - clip_to_padded_face cases.
    {
        json cases = json::array();
        cases.push_back(clip_to_padded_face_case("within_one_face",
                                                 S2Point(1, -0.5, -0.5),
                                                 S2Point(1, 0.5, 0.5), 0.0));
        cases.push_back(clip_to_padded_face_case(
            "cross_one_edge", S2Point(1, 0, 0), S2Point(0, 1, 0), 0.0));
        cases.push_back(clip_to_padded_face_case(
            "opposite_edges", S2Point(0.75, 0, -1), S2Point(0.75, 0, 1), 0.0));
        cases.push_back(clip_to_padded_face_case(
            "adjacent_edges", S2Point(1, 0, 0.75), S2Point(0, 1, 0.75), 0.0));
        cases.push_back(clip_to_padded_face_case(
            "four_faces", S2Point(1, 0.9, 0.95), S2Point(-1, 0.95, 0.9), 0.0));
        cases.push_back(clip_to_padded_face_case("padded_within",
                                                 S2Point(1, -0.5, -0.5),
                                                 S2Point(1, 0.5, 0.5), 1e-10));
        cases.push_back(clip_to_padded_face_case(
            "padded_cross_edge", S2Point(1, 0, 0), S2Point(0, 1, 0), 1e-12));
        cases.push_back(
            clip_to_padded_face_case("padded_four_faces", S2Point(1, 0.9, 0.95),
                                     S2Point(-1, 0.95, 0.9), 1e-10));

        out["clip_to_padded_face"] = cases;
    }

    // TEST(S2, EdgeClipping) - representative deterministic cases for
    // ClipEdge, GetClippedEdgeBound, IntersectsRect.  These exercise the
    // axis-aligned clip path including degenerate rectangles.
    {
        json cases = json::array();
        R2Rect biunit(R1Interval(-1, 1), R1Interval(-1, 1));

        // Edge fully inside the rectangle.
        cases.push_back(clip_edge_case("interior_diag", R2Point(-0.5, -0.5),
                                       R2Point(0.5, 0.5), biunit));
        // Edge fully outside the rectangle.
        cases.push_back(
            clip_edge_case("exterior", R2Point(2, 2), R2Point(3, 3), biunit));
        // Edge that crosses the lower-left corner.
        cases.push_back(clip_edge_case("crosses_lower_left", R2Point(-2, -2),
                                       R2Point(0.5, 0.5), biunit));
        // Edge that clips against only one axis.
        cases.push_back(clip_edge_case("one_axis_clip", R2Point(-2, 0.25),
                                       R2Point(2, 0.75), biunit));
        // Positive slope edge.
        cases.push_back(clip_edge_case("positive_slope", R2Point(-2, -1),
                                       R2Point(3, 2), biunit));
        // Negative slope edge.
        cases.push_back(clip_edge_case("negative_slope", R2Point(-2, 2),
                                       R2Point(3, -3), biunit));
        // Horizontal edge crossing entire rectangle.
        cases.push_back(clip_edge_case("horizontal", R2Point(-2, 0.3),
                                       R2Point(2, 0.3), biunit));
        // Vertical edge crossing entire rectangle.
        cases.push_back(clip_edge_case("vertical", R2Point(0.3, -2),
                                       R2Point(0.3, 2), biunit));
        // Degenerate edge (A == B) inside the rectangle.
        cases.push_back(clip_edge_case("point_inside", R2Point(0.3, 0.4),
                                       R2Point(0.3, 0.4), biunit));
        // Degenerate edge outside the rectangle.
        cases.push_back(clip_edge_case("point_outside", R2Point(1.5, 1.5),
                                       R2Point(1.5, 1.5), biunit));
        // Clip against a narrow rectangle.
        R2Rect narrow(R1Interval(-0.7, -0.7), R1Interval(0.3, 0.35));
        cases.push_back(clip_edge_case("narrow_clip_crossing",
                                       R2Point(-1.0, 0.2), R2Point(-0.5, 0.4),
                                       narrow));
        cases.push_back(clip_edge_case("narrow_clip_miss", R2Point(0.2, 0.3),
                                       R2Point(0.8, 0.33), narrow));
        // Clip against a horizontal strip.
        R2Rect hstrip(R1Interval(0.2, 0.5), R1Interval(0.3, 0.3));
        cases.push_back(clip_edge_case("hstrip_crossing", R2Point(0.1, 0.1),
                                       R2Point(0.6, 0.5), hstrip));
        // Clip against a singleton rectangle.
        R2Rect single = R2Rect::FromPoint(R2Point(0.3, 0.8));
        cases.push_back(clip_edge_case("singleton_hit", R2Point(-1, 0),
                                       R2Point(1, 1.6), single));
        cases.push_back(clip_edge_case("singleton_miss", R2Point(-1, 0),
                                       R2Point(1, 1), single));
        // Empty rectangle.
        cases.push_back(clip_edge_case("empty_rect", R2Point(-0.5, -0.5),
                                       R2Point(0.5, 0.5), R2Rect::Empty()));

        out["clip_edge"] = cases;
    }

    // TEST(S2, EdgeClipping) - a separate group of IntersectsRect cases
    // exercising endpoints on the extended diagonals and exactly on a vertex.
    {
        json cases = json::array();
        R2Rect clip(R1Interval(0.2, 0.6), R1Interval(0.1, 0.4));
        auto add = [&](const char *name, R2Point a, R2Point b) {
            cases.push_back(
                {{"name", name},
                 {"a", point2_json(a)},
                 {"b", point2_json(b)},
                 {"clip", rect_json(clip)},
                 {"intersects_rect", S2::IntersectsRect(a, b, clip)}});
        };
        // Entirely inside the rectangle.
        add("inside", R2Point(0.3, 0.2), R2Point(0.5, 0.35));
        // Edge crossing the rectangle.
        add("crossing", R2Point(0.0, 0.25), R2Point(1.0, 0.25));
        // Edge touching a vertex.
        add("touch_vertex", R2Point(0.0, 0.0), R2Point(0.2, 0.1));
        // Edge strictly above the rectangle.
        add("above", R2Point(0.0, 0.5), R2Point(1.0, 0.5));
        // Edge strictly to the left.
        add("left", R2Point(0.0, 0.0), R2Point(0.0, 0.5));
        // Degenerate edge inside.
        add("point_inside", R2Point(0.4, 0.25), R2Point(0.4, 0.25));
        // Degenerate edge outside.
        add("point_outside", R2Point(0.9, 0.9), R2Point(0.9, 0.9));
        // Edge that crosses one extended diagonal.
        add("diag_cross", R2Point(-0.2, -0.1), R2Point(0.8, 0.5));

        out["intersects_rect"] = cases;
    }

    // Interpolate double hand-picked cases, exercising the guarantees in
    // the header comment (exact endpoints, interpolation in both directions,
    // and equal-endpoint fallback).
    {
        json cases = json::array();
        auto add = [&](const char *name, double x, double a, double b,
                       double a1, double b1) {
            double y = S2::InterpolateDouble(x, a, b, a1, b1);
            cases.push_back({{"name", name},
                             {"x", x},
                             {"a", a},
                             {"b", b},
                             {"a1", a1},
                             {"b1", b1},
                             {"y", y}});
        };
        add("x_eq_a", 0.2, 0.2, 0.8, -1.0, 1.0);
        add("x_eq_b", 0.8, 0.2, 0.8, -1.0, 1.0);
        add("midpoint", 0.5, 0.0, 1.0, 10.0, 20.0);
        add("closer_to_a", 0.1, 0.0, 1.0, 10.0, 20.0);
        add("closer_to_b", 0.9, 0.0, 1.0, 10.0, 20.0);
        add("equal_ab_fallback", 0.5, 0.5, 0.5, 3.0, 3.0);
        add("reversed_ab", 0.5, 1.0, 0.0, 10.0, 20.0);
        add("equal_a1b1", 0.3, 0.0, 1.0, 7.0, 7.0);
        out["interpolate_double"] = cases;
    }

    // Error constants (emitted so the OCaml test can verify parity).
    {
        out["constants"] = {
            {"face_clip_error_radians", S2::kFaceClipErrorRadians},
            {"face_clip_error_uv_dist", S2::kFaceClipErrorUVDist},
            {"face_clip_error_uv_coord", S2::kFaceClipErrorUVCoord},
            {"intersects_rect_error_uv_dist", S2::kIntersectsRectErrorUVDist},
            {"edge_clip_error_uv_coord", S2::kEdgeClipErrorUVCoord},
            {"edge_clip_error_uv_dist", S2::kEdgeClipErrorUVDist}};
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
