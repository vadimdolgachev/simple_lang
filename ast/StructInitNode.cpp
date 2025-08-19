//
// Created by vadim on 03.08.2025.
//

#include "StructInitNode.h"

StructInitNode::StructInitNode(std::string ident, Designator designator) :
    ident(std::move(ident)),
    designator(std::move(designator)) {}

void StructInitNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

std::string StructInitNode::toString() const {
    return std::format("LiteralStructNode {}", ident);
}

TypePtr StructInitNode::getType() const {
    return type;
}

void StructInitNode::setType(const TypePtr type) {
    if (auto casted = std::dynamic_pointer_cast<const StructType>(type)) {
        this->type = std::move(casted);
    }
}
