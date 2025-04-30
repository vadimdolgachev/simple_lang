#include "IdentNode.h"

IdentNode::IdentNode(std::string name) :
    name(std::move(name)) {}

std::string IdentNode::toString() const {
    return "ident=" + name;
}

void IdentNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

BaseNodePtr IdentNode::clone() const {
    return std::make_unique<IdentNode>(*this);
}

TypePtr IdentNode::getType() const {
    return type;
}

void IdentNode::setType(TypePtr type) {
    this->type = std::move(type);
}
