//
// Created by vadim on 06.04.25.
//

#include "MethodCallNode.h"

void MethodCallNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

std::string MethodCallNode::toString() const {
    return "method call";
}

TypePtr MethodCallNode::getType() const {
    throw std::runtime_error("not implemented");
}

void MethodCallNode::setType(TypePtr type) {

}
