//
// Created by vadim on 18.03.25.
//

#include "TernaryOperatorNode.h"

TernaryOperatorNode::TernaryOperatorNode(std::shared_ptr<ExpressionNode> cond,
                                         std::shared_ptr<ExpressionNode> trueExpr,
                                         std::shared_ptr<ExpressionNode> falseExpr):
    cond(std::move(cond)),
    trueExpr(std::move(trueExpr)),
    falseExpr(std::move(falseExpr)) {}

void TernaryOperatorNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

std::string TernaryOperatorNode::toString() const {
    return "ternary operator";
}

TypePtr TernaryOperatorNode::getType() const {
    throw std::runtime_error("not implemented");
}

void TernaryOperatorNode::setType(TypePtr type) {

}
