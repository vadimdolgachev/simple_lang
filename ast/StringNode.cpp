#include "StringNode.h"

#include <format>

#include "type/TypeFactory.h"

StringNode::StringNode(std::string text):
    text(std::move(text)) {}

std::string StringNode::toString() const {
    return std::format("StringNode({})", text);
}

void StringNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

TypePtr StringNode::getType() const {
    return TypeFactory::makePrimitiveType(TypeKind::Str);
}

void StringNode::setType(TypePtr /*type*/) {
    throw std::runtime_error("Unsupported operation");
}
