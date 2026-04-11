// Golden data generator for S2PaddedCell.

#include "s2/s2padded_cell.h"

#include <algorithm>
#include <cstdint>
#include <iostream>
#include <nlohmann/json.hpp>
#include <vector>

#include "s2/r1interval.h"
#include "s2/r2rect.h"
#include "s2/s2cell.h"
#include "s2/s2cell_id.h"
#include "s2/s2point.h"

using json = nlohmann::json;

namespace {

    json point_to_json(const S2Point &p) {
        return {p.x(), p.y(), p.z()};
    }

    json rect_to_json(const R2Rect &r) {
        return {{"x", {r.x().lo(), r.x().hi()}},
                {"y", {r.y().lo(), r.y().hi()}}};
    }

    json padded_cell_to_json(const S2PaddedCell &pc) {
        json j;
        j["id"] = pc.id().ToToken();
        j["level"] = pc.level();
        j["padding"] = pc.padding();
        j["bound"] = rect_to_json(pc.bound());
        j["middle"] = rect_to_json(pc.middle());
        j["center"] = point_to_json(pc.GetCenter());
        j["entry_vertex"] = point_to_json(pc.GetEntryVertex());
        j["exit_vertex"] = point_to_json(pc.GetExitVertex());
        json ij = json::array();
        for (int pos = 0; pos < 4; ++pos) {
            int i, j2;
            pc.GetChildIJ(pos, &i, &j2);
            ij.push_back({i, j2});
        }
        j["child_ij"] = ij;
        return j;
    }

    // Stable sample of S2CellIds across levels and faces, including a leaf.
    std::vector<S2CellId> sample_ids() {
        std::vector<S2CellId> ids;
        // All six face cells.
        for (int f = 0; f < 6; ++f)
            ids.push_back(S2CellId::FromFace(f));
        // A few deeper cells.
        ids.push_back(S2CellId::FromFace(0).child(0));
        ids.push_back(S2CellId::FromFace(0).child(3).child(1));
        ids.push_back(S2CellId::FromFace(2).child(1).child(2).child(0));
        ids.push_back(S2CellId::FromToken("1b"));
        ids.push_back(S2CellId::FromToken("8f"));
        ids.push_back(S2CellId::FromToken("123456789"));
        // Leaf cell (level 30).
        ids.push_back(S2CellId::FromToken("1000000000000001"));
        return ids;
    }

}  // namespace

int main() {
    json root;

    // TEST(S2PaddedCell, S2CellMethods)
    // For a selection of cells and paddings, compare the PaddedCell accessors
    // (id, level, padding, bound, middle, GetCenter) against golden data.
    // Unlike the upstream randomized test, we use a fixed sample so the
    // OCaml side has stable expected values.
    {
        json cases = json::array();
        std::vector<double> paddings = {0.0,  1e-15, 1e-10, 1e-5,
                                        0.01, 0.5,   1.0};
        for (const auto &id : sample_ids()) {
            for (double padding : paddings) {
                S2PaddedCell pc(id, padding);
                cases.push_back(padded_cell_to_json(pc));
            }
        }
        root["s2cell_methods"] = cases;
    }

    // TEST(S2PaddedCell, S2CellMethods) - child construction via
    // S2PaddedCell(parent, i, j). For each non-leaf sample, record the child
    // padded cells.
    {
        json cases = json::array();
        std::vector<double> paddings = {0.0, 0.01, 0.5};
        for (const auto &id : sample_ids()) {
            if (id.is_leaf())
                continue;
            for (double padding : paddings) {
                S2PaddedCell parent(id, padding);
                json entry;
                entry["parent_id"] = id.ToToken();
                entry["padding"] = padding;
                json children = json::array();
                for (int i = 0; i < 2; ++i) {
                    for (int j = 0; j < 2; ++j) {
                        S2PaddedCell child(parent, i, j);
                        json c = padded_cell_to_json(child);
                        c["i"] = i;
                        c["j"] = j;
                        children.push_back(c);
                    }
                }
                entry["children"] = children;
                cases.push_back(entry);
            }
        }
        root["child_construction"] = cases;
    }

    // TEST(S2PaddedCell, GetEntryExitVertices)
    // Check that entry/exit vertices do not depend on padding (golden
    // recording of both padding=0 and padding=0.5), and that the exit vertex
    // of one cell equals the entry vertex of the next cell (next_wrap).
    {
        json cases = json::array();
        for (const auto &id : sample_ids()) {
            S2PaddedCell pc0(id, 0.0);
            S2PaddedCell pc1(id, 0.5);
            S2PaddedCell pc_next(id.next_wrap(), 0.0);
            json c;
            c["id"] = id.ToToken();
            c["next_id"] = id.next_wrap().ToToken();
            c["entry_pad0"] = point_to_json(pc0.GetEntryVertex());
            c["entry_pad05"] = point_to_json(pc1.GetEntryVertex());
            c["exit_pad0"] = point_to_json(pc0.GetExitVertex());
            c["exit_pad05"] = point_to_json(pc1.GetExitVertex());
            c["next_entry"] = point_to_json(pc_next.GetEntryVertex());
            if (!id.is_leaf()) {
                S2PaddedCell first(id.child(0), 0.0);
                S2PaddedCell last(id.child(3), 0.0);
                c["child0_entry"] = point_to_json(first.GetEntryVertex());
                c["child3_exit"] = point_to_json(last.GetExitVertex());
            }
            cases.push_back(c);
        }
        root["entry_exit_vertices"] = cases;
    }

    // TEST(S2PaddedCell, ShrinkToFit)
    // Hand-picked deterministic cases rather than reproducing the randomized
    // upstream test - we pick a target cell, build a padding and a rectangle
    // that forces ShrinkToFit to return that cell, and then record the
    // (initial cell, padding, rect, expected result).
    {
        json cases = json::array();

        auto make_case = [](S2CellId result, double padding, const R2Rect &rect,
                            S2CellId initial) {
            S2PaddedCell pc(initial, padding);
            S2CellId got = pc.ShrinkToFit(rect);
            json c;
            c["initial_id"] = initial.ToToken();
            c["padding"] = padding;
            c["rect"] = rect_to_json(rect);
            c["expected"] = got.ToToken();
            return c;
        };

        // Case A: rect is the face bound itself - ShrinkToFit returns the face.
        {
            S2CellId face = S2CellId::FromFace(0);
            R2Rect bound = face.GetBoundUV();
            cases.push_back(make_case(face, 0.0, bound, face));
        }

        // Case B: rect shrunk to a single interior point of a deep cell -
        // ShrinkToFit should return that deep cell (or a descendant).
        {
            S2CellId deep = S2CellId::FromFace(2).child(1).child(2).child(0);
            R2Point center = deep.GetCenterUV();
            // Tiny rect around the center.
            R2Rect rect(R1Interval(center.x() - 1e-12, center.x() + 1e-12),
                        R1Interval(center.y() - 1e-12, center.y() + 1e-12));
            cases.push_back(make_case(deep, 0.0, rect, S2CellId::FromFace(2)));
        }

        // Case C: rect larger than max_rect for that cell (includes center
        // across both axes), so ShrinkToFit stops at the initial cell.
        {
            S2CellId face = S2CellId::FromFace(0);
            R2Rect bound = face.GetBoundUV();
            cases.push_back(make_case(face, 0.1, bound, face));
        }

        // Case D: shrink a face cell to a (small) rect inside one child.
        {
            S2CellId child = S2CellId::FromFace(0).child(0);
            R2Point center = child.GetCenterUV();
            R2Rect rect(R1Interval(center.x() - 1e-9, center.x() + 1e-9),
                        R1Interval(center.y() - 1e-9, center.y() + 1e-9));
            S2PaddedCell pc(S2CellId::FromFace(0), 0.0);
            S2CellId got = pc.ShrinkToFit(rect);
            json c;
            c["initial_id"] = S2CellId::FromFace(0).ToToken();
            c["padding"] = 0.0;
            c["rect"] = rect_to_json(rect);
            c["expected"] = got.ToToken();
            cases.push_back(c);
        }

        // Case E: mirror the upstream randomized test for a few fixed seeds.
        // Start with a target cell, choose padding < half the min UV size,
        // and pick a rect that intersects at most one child of the target.
        std::vector<S2CellId> targets = {
            S2CellId::FromFace(0).child(1).child(2),
            S2CellId::FromFace(3).child(0).child(1).child(2),
            S2CellId::FromFace(5).child(3).child(3).child(0).child(2),
        };
        for (const auto &target : targets) {
            R2Rect bound = target.GetBoundUV();
            R2Point size = bound.GetSize();
            double padding = 0.25 * std::min(size.x(), size.y());
            R2Rect max_rect = bound.Expanded(-padding);
            // Use a rect strictly inside max_rect that straddles the center
            // along both axes so that no child is a strict ShrinkToFit result.
            R2Point c = target.GetCenterUV();
            R2Rect rect(R1Interval(max_rect.x().lo() * 0.5 + c.x() * 0.5,
                                   max_rect.x().hi() * 0.5 + c.x() * 0.5),
                        R1Interval(max_rect.y().lo() * 0.5 + c.y() * 0.5,
                                   max_rect.y().hi() * 0.5 + c.y() * 0.5));
            S2CellId initial = target.parent(0);
            S2PaddedCell pc(initial, padding);
            S2CellId got = pc.ShrinkToFit(rect);
            json jc;
            jc["initial_id"] = initial.ToToken();
            jc["padding"] = padding;
            jc["rect"] = rect_to_json(rect);
            jc["expected"] = got.ToToken();
            cases.push_back(jc);
        }

        root["shrink_to_fit"] = cases;
    }

    std::cout << root.dump(2) << std::endl;
    return 0;
}
