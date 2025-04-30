//
// Created by vadim on 20.04.25.
//

#include "TypeCastNode.h"

TypeCastNode::TypeCastNode(ExprNodePtr expr, TypePtr targetType):
    expr(std::move(expr)),
    targetType(std::move(targetType)) {}

void TypeCastNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

std::string TypeCastNode::toString() const {
    return "TypeCastNode";
}

TypePtr TypeCastNode::getType() const {
    return targetType;
}

void TypeCastNode::setType(TypePtr /*type*/) {
    throw std::logic_error("Not allow");
}
