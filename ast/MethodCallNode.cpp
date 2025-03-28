//
// Created by vadim on 27.03.25.
//

#include "MethodCallNode.h"

#include "FunctionCallNode.h"

MethodCallNode::MethodCallNode(std::unique_ptr<ExpressionNode> object,
                               std::unique_ptr<FunctionCallNode> functionCall):
    object(std::move(object)),
    method(std::move(functionCall)) {}

void MethodCallNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}

std::string MethodCallNode::toString() const {
    return "method name: " + method->ident->name;
}
