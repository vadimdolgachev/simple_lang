#include "AssignmentNode.h"

#include "IdentNode.h"

AssignmentNode::AssignmentNode(ExprNodePtr lvalue,
                               ExprNodePtr rvalue) :
    lvalue(std::move(lvalue)),
    rvalue(std::move(rvalue)) {}

std::string AssignmentNode::toString() const {
    if (const auto ident = asNode<IdentNode>(lvalue.get())) {
        return ident.value()->name + "=" + rvalue->toString();
    }
    throw std::runtime_error("Unknown lvalue type");
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
