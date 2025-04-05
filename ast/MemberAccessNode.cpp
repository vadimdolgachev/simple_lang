//
// Created by vadim on 27.03.25.
//

#include "MemberAccessNode.h"

MemberAccessNode::MemberAccessNode(std::unique_ptr<ExpressionNode> object,
                                   std::unique_ptr<ExpressionNode> member):
    object(std::move(object)),
    member(std::move(member)) {}

void MemberAccessNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}

std::string MemberAccessNode::toString() const {
    return "member name: " + member->toString();
}
