#include "IdentNode.h"

IdentNode::IdentNode(std::string name) :
    name(std::move(name)) {}

std::string IdentNode::toString() const {
    return "ident=" + name;
}

void IdentNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}

std::unique_ptr<BaseNode> IdentNode::clone() const {
    return std::make_unique<IdentNode>(*this);
}
