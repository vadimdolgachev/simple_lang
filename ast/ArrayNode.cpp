//
// Created by vadim on 14.05.2025.
//

#include "ArrayNode.h"

ArrayNode::ArrayNode(std::vector<ExprNodePtr> elements):
    elements(std::move(elements)) {}

void ArrayNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

std::string ArrayNode::toString() const {
    return "array";
}

TypePtr ArrayNode::getType() const {
    return arrayType;
}

void ArrayNode::setType(TypePtr type) {
    arrayType = std::move(type);
}
