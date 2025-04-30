#include "NumberNode.h"

#include "type/TypeFactory.h"

NumberNode::NumberNode(const double value, const bool isFloat) :
    value(value),
    isFloat(isFloat),
    type(TypeFactory::makePrimitiveType(isFloat ? TypeKind::Double : TypeKind::Integer)) {}

std::string NumberNode::toString() const {
    return "number=" + std::to_string(value);
}

void NumberNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

TypePtr NumberNode::getType() const {
    return type;
}

void NumberNode::setType(TypePtr /*type*/) {
    throw std::logic_error("Unsupported operation");
}
