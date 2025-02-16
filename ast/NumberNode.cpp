#include "NumberNode.h"

NumberNode::NumberNode(const double v) :
    value(v) {}

std::string NumberNode::toString() const {
    return "number=" + std::to_string(value);
}

void NumberNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
