//
// Created by vadim on 18.03.25.
//

#include "TernaryOperatorNode.h"

TernaryOperatorNode::TernaryOperatorNode(ExprNodePtr cond,
                                         ExprNodePtr trueExpr,
                                         ExprNodePtr falseExpr):
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
