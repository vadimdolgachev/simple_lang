//
// Created by vadim on 16.02.25.
//

#include "BooleanNode.h"

#include "type/TypeFactory.h"

BooleanNode::BooleanNode(const bool v):
    value(v) {}

std::string BooleanNode::toString() const {
    return value ? "true" : "false";
}

void BooleanNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

TypePtr BooleanNode::getType() const {
    return TypeFactory::makePrimitiveType(TypeKind::Boolean);
}

void BooleanNode::setType(TypePtr /*type*/) {
    throw std::logic_error("Unsupported operation");
}
