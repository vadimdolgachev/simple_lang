#include "StringNode.h"

#include <format>

StringNode::StringNode(std::string v):
    str(std::move(v)) {}

std::string StringNode::toString() const {
    return std::format("StringNode({})", str);
}

void StringNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
