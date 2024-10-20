#include "BinOpNode.h"

BinOpNode::BinOpNode(const TokenType binOp, std::unique_ptr<ExpressionNode> lhs,
                     std::unique_ptr<ExpressionNode> rhs)
    : binOp(binOp), lhs(std::move(lhs)), rhs(std::move(rhs)) {
}

std::string BinOpNode::toString() const {
    const bool isLhsBinOp = dynamic_cast<BinOpNode *>(lhs.get()) != nullptr;
    const bool isRhsBinOp = dynamic_cast<BinOpNode *>(rhs.get()) != nullptr;
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

void BinOpNode::visit(NodeVisitor *visitor) const { visitor->visit(this); }
