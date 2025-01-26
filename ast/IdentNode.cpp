#include "IdentNode.h"

IdentNode::IdentNode(std::string name) :
    name(std::move(name)) {}

std::string IdentNode::toString() const {
    return "ident=" + name;
}

void IdentNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
