//
// Created by vadim on 06.04.25.
//

#include "MethodCallNode.h"

void MethodCallNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}

std::string MethodCallNode::toString() const {
    return "method call";
}
