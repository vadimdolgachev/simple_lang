//
// Created by vadim on 06.04.25.
//

#include "MethodCallNode.h"

#include "type/FunctionType.h"

void MethodCallNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

std::string MethodCallNode::toString() const {
    return "method call";
}

TypePtr MethodCallNode::getType() const {
    if (const auto fType = method->getType()->asFunction()) {
        return fType.value()->returnType();
    }
    throw std::runtime_error("Unknown type for method call");
}

void MethodCallNode::setType(const TypePtr type) {
    if (const auto fType = type->asFunction()) {
        method->setType(fType.value());
    } else {
        throw std::runtime_error("Unknown type in method call");
    }
}
