//
// Created by vadim on 06.04.25.
//

#include "FieldAccessNode.h"

#include "IdentNode.h"

FieldAccessNode::FieldAccessNode(ExprNodePtr object, std::unique_ptr<IdentNode> field):
    MemberAccessNode(std::move(object)),
    field(std::move(field)) {}

void FieldAccessNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

std::string FieldAccessNode::toString() const {
    return "field: " + field->name;
}

TypePtr FieldAccessNode::getType() const {
    throw std::runtime_error("Not implemented");
}

void FieldAccessNode::setType(TypePtr type) {
    this->type = std::move(type);
}
