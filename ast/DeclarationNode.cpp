//
// Created by vadim on 04.03.25.
//

#include "DeclarationNode.h"

#include "TypeNode.h"

DeclarationNode::DeclarationNode(std::unique_ptr<IdentNode> ident,
                                 std::unique_ptr<PrimitiveType> type,
                                 std::optional<std::unique_ptr<ExpressionNode>> init):
    ident(std::move(ident)),
    type(std::move(type)),
    init(std::move(init)) {}

std::string DeclarationNode::toString() const {
    return "DeclarationNode";
}

void DeclarationNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
