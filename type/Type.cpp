//
// Created by vadim on 04.03.25.
//

#include "Type.h"

#include "ArrayType.h"
#include "FunctionType.h"

BoolResult Type::canCastTo(const TypePtr &target, CastMode /*mode*/) const {
    if (*this == *target) {
        return true;
    }
    return std::unexpected("Cannot cast");
}

ResultType Type::getResultTypeUnary(TokenType op) const {
    return std::unexpected("Cannot cast");
}

ResultType Type::getCommonType(const TypePtr &other) const {
    if (*this == *other) {
        return shared_from_this();
    }
    return std::unexpected("Type mismatch");
}

std::vector<TypePtr> Type::getFieldTypes() const {
    throw std::logic_error("Not a composite type");
}

MethodInfoOpt Type::findMethod(const std::string &name, const std::optional<std::vector<TypePtr>> &signature) const {
    if (const auto it = std::ranges::find_if(getMethods(), [&name, &signature](const auto &method) {
        return method->name == name && method->type->parametersType() == signature;
    }); it != getMethods().end()) {
        return *it;
    }
    return std::nullopt;
}

const std::vector<MethodInfoPtr> &Type::getMethods() const {
    throw std::logic_error("Type does not contain methods");
}

TypePtr Type::getElementType() const {
    throw std::logic_error("Not a container type");
}

ResultType Type::getComparableType(const TypePtr &type) const {
    return std::unexpected("Types are not comparable");
}

PrimitiveType::PrimitiveType(const TypeKind kind, const bool isConst):
    kind(kind),
    isConst(isConst) {}

TypeKind PrimitiveType::getKind() const noexcept {
    return kind;
}

bool PrimitiveType::operator==(const Type &other) const {
    return getKind() == other.getKind();
}

std::string PrimitiveType::getName() const {
    switch (kind) {
        case TypeKind::Boolean:
            return "bool";
        case TypeKind::Byte:
            return "byte";
        case TypeKind::Char:
            return "char";
        case TypeKind::Double:
            return "double";
        case TypeKind::Integer:
            return "int";
        case TypeKind::Void:
            return "void";
        case TypeKind::Str:
            return "str";
        case TypeKind::Custom:
            return "custom";
        default:
            break;
    }
    return "unknown";
}

bool Type::isBoolean() const noexcept {
    return getKind() == TypeKind::Boolean;
}

bool Type::isNumeric() const noexcept {
    return isInteger() || isDouble();
}

bool Type::isVoid() const noexcept {
    return getKind() == TypeKind::Void;
}

bool Type::isDouble() const noexcept {
    return getKind() == TypeKind::Double;
}

bool Type::isInteger() const noexcept {
    return getKind() == TypeKind::Integer;
}

bool Type::isStr() const noexcept {
    return getKind() == TypeKind::Str;
}

bool Type::isArray() const noexcept {
    return getKind() == TypeKind::Array;
}

std::optional<FunctionTypePtr> Type::asFunction() const {
    return std::dynamic_pointer_cast<const FunctionType>(shared_from_this());
}

std::optional<ArrayTypePtr> Type::asArray() const {
    return std::dynamic_pointer_cast<const ArrayType>(shared_from_this());
}

OperationCategory getOperationCategory(const TokenType op) {
    switch (op) {
        case TokenType::Plus:
        case TokenType::Minus:
        case TokenType::Multiply:
        case TokenType::Divide:
            return OperationCategory::Arithmetic;
        case TokenType::Less:
        case TokenType::LessEqual:
        case TokenType::Greater:
        case TokenType::GreaterEqual:
        case TokenType::Equal:
        case TokenType::NotEqual:
            return OperationCategory::Comparison;
        case TokenType::LogicalAnd:
        case TokenType::LogicalOr:
            return OperationCategory::Logical;
        default:
            return OperationCategory::Other;
    }
}
