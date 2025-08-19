//
// Created by vadim on 26.06.2025.
//

#include "StructType.h"


StructType::StructType(std::string name,
                       const StructKind kind,
                       std::vector<StructField> fields) :
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

const std::vector<StructField> &StructType::getFields() const noexcept {
    return fields;
}

std::optional<TypePtr> StructType::findFieldType(const std::string &fieldName) const {
    if (const auto index = findFieldIndex(fieldName)) {
        return fields[index.value()].type;
    }
    return std::nullopt;
}

std::optional<std::size_t> StructType::findFieldIndex(const std::string &fieldName) const {
    const auto it = std::ranges::find_if(fields, [fieldName](const auto &structField) {
        const auto &structFieldName = structField.name;
        return structFieldName && structFieldName == fieldName;
    });

    if (it != std::end(fields)) {
        return std::distance(fields.begin(), it);
    }
    return std::nullopt;
}
