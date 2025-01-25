#include "UnaryOpNode.h"

UnaryOpNode::UnaryOpNode(const TokenType operatorType,
                         const UnaryOpType unaryPosType,
                         std::unique_ptr<ExpressionNode> expr) :
    operatorType(operatorType),
    unaryPosType(unaryPosType),
    expr(std::move(expr)) {}

std::string UnaryOpNode::toString() const {
    return "unary operator";
}

void UnaryOpNode::visit(NodeVisitor *const visitor) const {
    visitor->visit(this);
}
