//
// Created by vadim on 27.04.25.
//

#include "PointerType.h"

PointerType::PointerType(TypePtr baseType):
    PrimitiveType(TypeKind::Pointer),
    baseType(std::move(baseType)) {}

bool PointerType::operator==(const Type &other) const {
    if (const auto *pointerType = dynamic_cast<const PointerType *>(&other)) {
        return *baseType == *pointerType->baseType;
    }
    return false;
}

std::string PointerType::getName() const {
    return std::format("{} *", baseType->getName());
}
