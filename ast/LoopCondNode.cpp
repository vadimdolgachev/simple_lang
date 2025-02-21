//
// Created by vadim on 16.02.25.
//

#include "LoopCondNode.h"

LoopCondNode::LoopCondNode(std::unique_ptr<ExpressionNode> conditional,
                           std::vector<std::unique_ptr<BaseNode>> body,
                           const LoopType loopType):
    conditional(std::move(conditional)),
    body(std::move(body)),
    loopType(loopType) {}

std::string LoopCondNode::toString() const {
    return "WhileLoopNode";
}

void LoopCondNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
