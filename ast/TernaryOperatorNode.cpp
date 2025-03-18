//
// Created by vadim on 18.03.25.
//

#include "TernaryOperatorNode.h"

TernaryOperatorNode::TernaryOperatorNode(std::unique_ptr<ExpressionNode> cond,
                                         std::unique_ptr<ExpressionNode> trueExpr,
                                         std::unique_ptr<ExpressionNode> falseExpr):
    cond(std::move(cond)),
    trueExpr(std::move(trueExpr)),
    falseExpr(std::move(falseExpr)) {}

void TernaryOperatorNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}

std::string TernaryOperatorNode::toString() const {
    return "ternary operator";
}
