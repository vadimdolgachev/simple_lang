//
// Created by vadim on 04.03.25.
//

#include "DeclarationNode.h"

#include <utility>

#include "../type/Type.h"
#include "Util.h"

DeclarationNode::DeclarationNode(std::unique_ptr<IdentNode> ident,
                                 TypePtr type,
                                 std::optional<ExprNodePtr> init,
                                 const bool isConst,
                                 const bool isGlobal) :
    ident(std::move(ident)),
    type(std::move(type)),
    init(std::move(init)),
    isConst(isConst),
    isGlobal(isGlobal) {
}

DeclarationNode::DeclarationNode(const DeclarationNode &other):
    ident(dynCast<IdentNode>(other.ident->clone())),
    type(other.type),
    init(other.init
             ? std::make_optional(dynCast<ExpressionNode>(other.init.value()->clone()))
             : std::nullopt),
    isConst(other.isConst),
    isGlobal(other.ident) {}

std::string DeclarationNode::toString() const {
    return "DeclarationNode";
}

void DeclarationNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

BaseNodePtr DeclarationNode::clone() const {
    return std::make_unique<DeclarationNode>(*this);
}
