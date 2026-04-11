// Golden data generator for S2WedgeRelations.

#include "s2/s2wedge_relations.h"

#include <iostream>
#include <nlohmann/json.hpp>

#include "s2/s2point.h"

using json = nlohmann::json;

json point_json(const S2Point &p) {
    return {p.x(), p.y(), p.z()};
}

static int wedge_relation_to_int(S2::WedgeRelation r) {
    switch (r) {
        case S2::WEDGE_EQUALS: return 0;
        case S2::WEDGE_PROPERLY_CONTAINS: return 1;
        case S2::WEDGE_IS_PROPERLY_CONTAINED: return 2;
        case S2::WEDGE_PROPERLY_OVERLAPS: return 3;
        case S2::WEDGE_IS_DISJOINT: return 4;
    }
    return -1;
}

static json wedge_case(const char *label, const S2Point &a0, const S2Point &ab1,
                       const S2Point &a2, const S2Point &b0,
                       const S2Point &b2) {
    // Match the upstream TestWedge helper: normalize every point before the
    // canonical calls, then emit the normalized points so OCaml consumes the
    // same bits.
    S2Point na0 = a0.Normalize();
    S2Point nab1 = ab1.Normalize();
    S2Point na2 = a2.Normalize();
    S2Point nb0 = b0.Normalize();
    S2Point nb2 = b2.Normalize();

    json c = json::object();
    c["label"] = label;
    c["a0"] = point_json(na0);
    c["ab1"] = point_json(nab1);
    c["a2"] = point_json(na2);
    c["b0"] = point_json(nb0);
    c["b2"] = point_json(nb2);
    c["contains"] = S2::WedgeContains(na0, nab1, na2, nb0, nb2);
    c["intersects"] = S2::WedgeIntersects(na0, nab1, na2, nb0, nb2);
    c["wedge_relation"] =
        wedge_relation_to_int(S2::GetWedgeRelation(na0, nab1, na2, nb0, nb2));
    return c;
}

int main() {
    json out;

    // Document the integer encoding used for the WedgeRelation enum so the
    // OCaml test can mirror it without magic numbers.
    {
        json e = json::object();
        e["equals"] = wedge_relation_to_int(S2::WEDGE_EQUALS);
        e["properly_contains"] =
            wedge_relation_to_int(S2::WEDGE_PROPERLY_CONTAINS);
        e["is_properly_contained"] =
            wedge_relation_to_int(S2::WEDGE_IS_PROPERLY_CONTAINED);
        e["properly_overlaps"] =
            wedge_relation_to_int(S2::WEDGE_PROPERLY_OVERLAPS);
        e["is_disjoint"] = wedge_relation_to_int(S2::WEDGE_IS_DISJOINT);
        out["enum"] = e;
    }

    // TEST(S2WedgeRelations, Wedges)
    // Mirrors every sub-case in the single upstream test, with the same shared
    // origin vertex (0, 0, 1). Each case carries the expected results of
    // WedgeContains, WedgeIntersects, and GetWedgeRelation as returned by
    // canonical S2.
    {
        json cases = json::array();
        const S2Point ab1(0, 0, 1);

        // Intersection in one wedge.
        cases.push_back(wedge_case("overlap_one_wedge", S2Point(-1, 0, 10), ab1,
                                   S2Point(1, 2, 10), S2Point(0, 1, 10),
                                   S2Point(1, -2, 10)));

        // Intersection in two wedges.
        cases.push_back(wedge_case("overlap_two_wedges", S2Point(-1, -1, 10),
                                   ab1, S2Point(1, -1, 10), S2Point(1, 0, 10),
                                   S2Point(-1, 1, 10)));

        // Normal containment.
        cases.push_back(wedge_case("normal_containment", S2Point(-1, -1, 10),
                                   ab1, S2Point(1, -1, 10), S2Point(-1, 0, 10),
                                   S2Point(1, 0, 10)));

        // Containment with equality on one side.
        cases.push_back(wedge_case("contains_equal_one_side", S2Point(2, 1, 10),
                                   ab1, S2Point(-1, -1, 10), S2Point(2, 1, 10),
                                   S2Point(1, -5, 10)));

        // Containment with equality on the other side.
        cases.push_back(wedge_case("contains_equal_other_side",
                                   S2Point(2, 1, 10), ab1, S2Point(-1, -1, 10),
                                   S2Point(1, -2, 10), S2Point(-1, -1, 10)));

        // Containment with equality on both sides.
        cases.push_back(wedge_case("equals", S2Point(-2, 3, 10), ab1,
                                   S2Point(4, -5, 10), S2Point(-2, 3, 10),
                                   S2Point(4, -5, 10)));

        // Disjoint with equality on one side.
        cases.push_back(wedge_case("disjoint_equal_one_side",
                                   S2Point(-2, 3, 10), ab1, S2Point(4, -5, 10),
                                   S2Point(4, -5, 10), S2Point(-2, -3, 10)));

        // Disjoint with equality on the other side.
        cases.push_back(wedge_case("disjoint_equal_other_side",
                                   S2Point(-2, 3, 10), ab1, S2Point(0, 5, 10),
                                   S2Point(4, -5, 10), S2Point(-2, 3, 10)));

        // Disjoint with equality on both sides.
        cases.push_back(wedge_case("disjoint_equal_both_sides",
                                   S2Point(-2, 3, 10), ab1, S2Point(4, -5, 10),
                                   S2Point(4, -5, 10), S2Point(-2, 3, 10)));

        // B contains A with equality on one side.
        cases.push_back(wedge_case("is_contained_equal_one_side",
                                   S2Point(2, 1, 10), ab1, S2Point(1, -5, 10),
                                   S2Point(2, 1, 10), S2Point(-1, -1, 10)));

        // B contains A with equality on the other side.
        cases.push_back(wedge_case("is_contained_equal_other_side",
                                   S2Point(2, 1, 10), ab1, S2Point(1, -5, 10),
                                   S2Point(-2, 1, 10), S2Point(1, -5, 10)));

        out["wedges"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
