#include "UnaryOpNode.h"

UnaryOpNode::UnaryOpNode(const OperatorType operatorType,
                         std::unique_ptr<ExpressionNode> expr)
    : expr(std::move(expr)),
      operatorType(operatorType) {
}

std::string UnaryOpNode::toString() const {
    return "unary operator";
}

void UnaryOpNode::visit(NodeVisitor *const visitor) const {
    visitor->visit(this);
}
