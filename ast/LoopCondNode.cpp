//
// Created by vadim on 16.02.25.
//

#include "LoopCondNode.h"

LoopCondNode::LoopCondNode(const Type loopType,
                           CondBranch condBranch,
                           std::optional<BaseNodePtr> init,
                           std::optional<ExprNodePtr> increment):
    loopType(loopType),
    condBranch(std::move(condBranch)),
    init(std::move(init)),
    increment(std::move(increment)) {}

std::string LoopCondNode::toString() const {
    std::string typeStr;
    switch (loopType) {
        case Type::For:
            typeStr = "For";
            break;
        case Type::While:
            typeStr = "While";
            break;
        case Type::DoWhile:
            typeStr = "DoWhile";
            break;
    }
    return "LoopCondNode (" + typeStr + ")";
}

void LoopCondNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}
