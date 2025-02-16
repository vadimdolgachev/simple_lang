//
// Created by vadim on 16.02.25.
//

#include "BooleanNode.h"

BooleanNode::BooleanNode(const bool v):
    value(v) {}

std::string BooleanNode::toString() const {
    return value ? "true" : "false";
}

void BooleanNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
