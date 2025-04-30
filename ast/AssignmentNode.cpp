#include "AssignmentNode.h"

#include "IdentNode.h"

AssignmentNode::AssignmentNode(std::unique_ptr<IdentNode> lvalue,
                               ExprNodePtr rvalue) :
    lvalue(std::move(lvalue)),
    rvalue(std::move(rvalue)) {}

std::string AssignmentNode::toString() const {
    return lvalue->name + "=" + rvalue->toString();
}

void AssignmentNode::visit(NodeVisitor *const visitor) {
    visitor->visit(this);
}

TypePtr AssignmentNode::getType() const {
    return rvalue->getType();
}

void AssignmentNode::setType(TypePtr type) {
    this->type = std::move(type);
}
