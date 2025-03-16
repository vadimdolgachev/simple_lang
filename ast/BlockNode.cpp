//
// Created by vadim on 22.02.2025.
//

#include "BlockNode.h"

BlockNode::BlockNode(Statements statements) :
    statements(std::move(statements)) {}

void BlockNode::visit(NodeVisitor *visitor) const {}

std::string BlockNode::toString() const {
    return "BlockNode";
}
