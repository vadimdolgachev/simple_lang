//
// Created by vadim on 27.04.25.
//

#include "StrType.h"

StrType::StrType():
    PrimitiveType(TypeKind::Str) {}

ResultType StrType::getResultTypeUnary(TokenType op) const {
    return std::unexpected("Unsupported operator for string type");
}

bool StrType::operator==(const Type &other) const {
    return dynamic_cast<const StrType *>(&other) != nullptr;
}

std::string StrType::getName() const {
    return "str";
}