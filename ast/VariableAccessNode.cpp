#include "VariableAccessNode.h"

VariableAccessNode::VariableAccessNode(std::string name) : name(std::move(name)) {
}

std::string VariableAccessNode::toString() const { return "var=" + name; }

void VariableAccessNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
