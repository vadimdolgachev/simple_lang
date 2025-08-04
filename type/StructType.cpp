//
// Created by vadim on 26.06.2025.
//

#include "StructType.h"


StructType::StructType(std::string name,
                       const StructKind kind,
                       std::vector<Field> fields) :
    name(std::move(name)),
    structKind(kind),
    fields(std::move(fields)) {}

bool StructType::operator==(const Type &other) const {
    return other.getKind() == TypeKind::Struct && name == other.getName();
}

std::string StructType::getName() const {
    return name;
}

TypeKind StructType::getKind() const noexcept {
    return TypeKind::Struct;
}

StructKind StructType::getStructKind() const noexcept {
    return structKind;
}

const std::vector<Field> &StructType::getFields() const noexcept {
    return fields;
}
