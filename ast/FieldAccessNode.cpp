//
// Created by vadim on 06.04.25.
//

#include "FieldAccessNode.h"

#include "IdentNode.h"

FieldAccessNode::FieldAccessNode(std::unique_ptr<ExpressionNode> object, std::unique_ptr<IdentNode> field):
    MemberAccessNode(std::move(object)),
    field(std::move(field)) {}

void FieldAccessNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}

std::string FieldAccessNode::toString() const {
    return "field: " + field->name;
}
