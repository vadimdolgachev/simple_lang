//
// Created by vadim on 27.04.25.
//

#include "NumericType.h"

#include "TypeFactory.h"

NumericType::NumericType(const TypeKind kind) :
    PrimitiveType(kind) {}

BoolResult NumericType::canCastTo(const TypePtr &target, const CastMode mode) const {
    if (*this == *target) {
        return true;
    }
    if (mode == CastMode::Implicit && target->getKind() == TypeKind::Double) {
        return true;
    }
    return std::unexpected(std::format("Cannot cast {} to {}", getName(), target->getName()));
}

ResultType NumericType::getResultTypeUnary(const TokenType op) {
    switch (op) {
        case TokenType::Plus:
        case TokenType::Minus:
        case TokenType::Increment:
        case TokenType::Decrement:
            return shared_from_this();
        default:
            break;
    }
    return std::unexpected("Unsupported unary operator for numeric type");
}

ResultType NumericType::getCommonType(const TypePtr &other) {
    if (other->isNumeric()) {
        if (getKind() == TypeKind::Double || other->getKind() == TypeKind::Double) {
            return TypeFactory::makePrimitiveType(TypeKind::Double);
        }
        return shared_from_this();
    }
    return std::unexpected(std::format("Cannot cast numeric type to {}", other->getName()));
}

ResultType NumericType::getComparableType(const TypePtr &type) {
    if (type->isNumeric()) {
        return getCommonType(type);
    }
    return std::unexpected("Cannot compare numeric with non-numeric type");
}
