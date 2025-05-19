//
// Created by vadim on 18.05.2025.
//

#include "IndexAccessNode.h"

IndexAccessNode::IndexAccessNode(ExprNodePtr object, ExprNodePtr index):
    object(std::move(object)),
    index(std::move(index)) {}

void IndexAccessNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

std::string IndexAccessNode::toString() const {
    return std::format("{}[{}]", object->toString(), index->toString());
}

TypePtr IndexAccessNode::getType() const {
    return resultType;
}

void IndexAccessNode::setType(TypePtr type) {
    resultType = std::move(type);
}
