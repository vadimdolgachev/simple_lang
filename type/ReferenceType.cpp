//
// Created by vadim on 20.08.2025.
//

#include "ReferenceType.h"

bool ReferenceType::operator==(const Type &other) const {
    return getKind() == other.getKind() && getName() == other.getName();
}

std::string ReferenceType::getName() const {
    return name;
}

TypeKind ReferenceType::getKind() const noexcept {
    return TypeKind::Reference;
}