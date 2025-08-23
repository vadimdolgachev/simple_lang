//
// Created by vadim on 20.08.2025.
//

#include "UnresolvedType.h"

bool UnresolvedType::operator==(const Type &other) const {
    return getKind() == other.getKind() && getName() == other.getName();
}

std::string UnresolvedType::getName() const {
    return name;
}

TypeKind UnresolvedType::getKind() const noexcept {
    return TypeKind::Unresolved;
}
