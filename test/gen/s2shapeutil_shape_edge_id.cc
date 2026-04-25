// Golden data generator for s2shapeutil::ShapeEdgeId.

#include "s2/s2shapeutil_shape_edge_id.h"

#include <iostream>
#include <nlohmann/json.hpp>

using json = nlohmann::json;
using s2shapeutil::ShapeEdgeId;

static json id_json(const ShapeEdgeId &id) {
    return json::array({id.shape_id, id.edge_id});
}

static json relation_case(const ShapeEdgeId &lhs, const ShapeEdgeId &rhs) {
    return json{
        {"lhs", id_json(lhs)}, {"rhs", id_json(rhs)}, {"eq", lhs == rhs},
        {"ne", lhs != rhs},    {"lt", lhs < rhs},     {"le", lhs <= rhs},
        {"gt", lhs > rhs},     {"ge", lhs >= rhs},
    };
}

int main() {
    json out;

    // Default constructor returns the (-1, -1) sentinel.
    {
        ShapeEdgeId def;
        out["default"] = id_json(def);
    }

    // TEST(ShapeEdgeIdTest, BothFieldsEqualIsEqual)
    // TEST(ShapeEdgeIdTest, BothShapeIdUnequalIsUnequal)
    // TEST(ShapeEdgeIdTest, BothEdgeIdUnequalIsUnequal)
    // TEST(ShapeEdgeIdTest, LessThanIsLexicographicShapeIdFirst)
    // TEST(ShapeEdgeIdTest, LessEqIsLexicographicShapeIdFirst)
    // TEST(ShapeEdgeIdTest, GreaterThanIsLexicographicShapeIdFirst)
    // TEST(ShapeEdgeIdTest, GreaterEqIsLexicographicShapeIdFirst)
    {
        json cases = json::array();
        // Equal pairs.
        cases.push_back(
            relation_case(ShapeEdgeId(10, 20), ShapeEdgeId(10, 20)));
        cases.push_back(relation_case(ShapeEdgeId(0, 0), ShapeEdgeId(0, 0)));
        // Different shape_id.
        cases.push_back(
            relation_case(ShapeEdgeId(11, 20), ShapeEdgeId(10, 20)));
        cases.push_back(relation_case(ShapeEdgeId(9, 20), ShapeEdgeId(10, 20)));
        // Different edge_id, same shape_id.
        cases.push_back(
            relation_case(ShapeEdgeId(10, 21), ShapeEdgeId(10, 20)));
        cases.push_back(
            relation_case(ShapeEdgeId(10, 19), ShapeEdgeId(10, 20)));
        // Lexicographic: shape_id wins over edge_id.
        cases.push_back(relation_case(ShapeEdgeId(10, 99), ShapeEdgeId(11, 0)));
        cases.push_back(relation_case(ShapeEdgeId(11, 0), ShapeEdgeId(10, 99)));
        // Sentinel against valid id.
        cases.push_back(relation_case(ShapeEdgeId(), ShapeEdgeId(0, 0)));
        cases.push_back(relation_case(ShapeEdgeId(), ShapeEdgeId()));
        out["relations"] = cases;
    }

    std::cout << out.dump(2) << std::endl;
    return 0;
}
