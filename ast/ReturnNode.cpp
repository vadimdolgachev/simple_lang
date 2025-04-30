//
// Created by vadim on 16.03.25.
//

#include "ReturnNode.h"

ReturnNode::ReturnNode(std::unique_ptr<ExpressionNode> expr):
    expr(std::move(expr)) {}

void ReturnNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

std::string ReturnNode::toString() const {
    return "return node";
}
