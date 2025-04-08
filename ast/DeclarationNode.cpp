//
// Created by vadim on 04.03.25.
//

#include "DeclarationNode.h"

#include <utility>

#include "TypeNode.h"
#include "Util.h"

DeclarationNode::DeclarationNode(std::unique_ptr<IdentNode> ident,
                                 TypeNode type,
                                 std::optional<std::unique_ptr<ExpressionNode>> init,
                                 const bool isGlobal):
    ident(std::move(ident)),
    type(std::move(type)),
    init(std::move(init)),
    isGlobal(isGlobal) {}

DeclarationNode::DeclarationNode(const DeclarationNode &other):
    ident(dynCast<IdentNode>(other.ident->clone())),
    type(other.type),
    init(other.init
             ? std::make_optional(dynCast<ExpressionNode>(other.init.value()->clone()))
             : std::nullopt),
    isGlobal(other.ident) {}

std::string DeclarationNode::toString() const {
    return "DeclarationNode";
}

void DeclarationNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}

std::unique_ptr<BaseNode> DeclarationNode::clone() const {
    return std::make_unique<DeclarationNode>(*this);
}
