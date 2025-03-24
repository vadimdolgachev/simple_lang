#include "AssignmentNode.h"

#include "IdentNode.h"

AssignmentNode::AssignmentNode(std::unique_ptr<IdentNode> lvalue,
                               std::unique_ptr<ExpressionNode> rvalue) :
    lvalue(std::move(lvalue)),
    rvalue(std::move(rvalue)) {}

std::string AssignmentNode::toString() const {
    return lvalue->name + "=" + rvalue->toString();
}

void AssignmentNode::visit(NodeVisitor *const visitor) const {
    visitor->visit(this);
}
