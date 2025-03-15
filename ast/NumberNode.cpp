#include "NumberNode.h"

NumberNode::NumberNode(const double value, const bool isFloat) :
    value(value),
    isFloat(isFloat) {}

std::string NumberNode::toString() const {
    return "number=" + std::to_string(value);
}

void NumberNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
