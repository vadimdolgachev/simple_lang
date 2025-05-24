#include "BinOpNode.h"

BinOpNode::BinOpNode(const TokenType binOp, ExprNodePtr lhs,
                     ExprNodePtr rhs) :
    binOp(binOp), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

std::string BinOpNode::toString() const {
    const bool isLhsBinOp = asNode<BinOpNode>(lhs.get()) != nullptr;
    const bool isRhsBinOp = asNode<BinOpNode>(rhs.get()) != nullptr;
    return std::string("op=")
           .append(std::to_string(static_cast<int>(binOp)))
           .append(", lhs=")
           .append(isLhsBinOp ? "(" : "")
           .append(lhs->toString())
           .append(isLhsBinOp ? ")" : "")
           .append(", rhs=")
           .append(isRhsBinOp ? "(" : "")
           .append(rhs->toString())
           .append(isRhsBinOp ? ")" : "");
}

void BinOpNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

TypePtr BinOpNode::getType() const {
    return exprType;
}

void BinOpNode::setType(TypePtr type) {
    exprType = std::move(type);
}
