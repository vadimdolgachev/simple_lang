#include "UnaryOpNode.h"

UnaryOpNode::UnaryOpNode(const TokenType operatorType,
                         const UnaryOpType unaryPosType,
                         ExprNodePtr expr) :
    operatorType(operatorType),
    unaryPosType(unaryPosType),
    expr(std::move(expr)) {}

std::string UnaryOpNode::toString() const {
    return "unary operator";
}

void UnaryOpNode::visit(NodeVisitor *const visitor) {
    visitor->visit(this);
}

TypePtr UnaryOpNode::getType() const {
    return expr->getType();
}

void UnaryOpNode::setType(TypePtr type) {
    throw std::runtime_error("Unsupported operation");
}
