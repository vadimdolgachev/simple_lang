//
// Created by vadim on 16.02.25.
//

#include "LoopCondNode.h"

LoopCondNode::LoopCondNode(const Type loopType,
                           CondBranch condBranch,
                           std::optional<std::unique_ptr<BaseNode>> init,
                           std::optional<std::unique_ptr<ExpressionNode>> increment):
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

void LoopCondNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
