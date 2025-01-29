#include "AssignmentNode.h"

AssignmentNode::AssignmentNode(
    std::string name, std::unique_ptr<ExpressionNode> rvalue)
    : name(std::move(name)),
      rvalue(std::move(rvalue)) {
}

std::string AssignmentNode::toString() const {
    return "var definition name=" + name + ", rvalue=" + rvalue->toString();
}

void AssignmentNode::visit(NodeVisitor *const visitor) const {
    visitor->visit(this);
}
