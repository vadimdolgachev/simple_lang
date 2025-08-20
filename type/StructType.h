//
// Created by vadim on 26.06.2025.
//

#ifndef STRUCTTYPE_H
#define STRUCTTYPE_H

#include "Type.h"

enum class StructKind: std::uint8_t {
    Named,
    Tuple
};

struct StructField final {
    std::optional<std::string> name;
    TypePtr type;
};

class StructType final : public Type {
public:
    StructType(std::string name,
              StructKind kind,
              std::vector<StructField> fields);

    bool operator==(const Type &other) const override;
    [[nodiscard]] std::string getName() const override;
    [[nodiscard]] TypeKind getKind() const noexcept override;
    [[nodiscard]] StructKind getStructKind() const noexcept;
    [[nodiscard]] size_t getFieldSize() const noexcept;
    [[nodiscard]] StructField &getFieldByIndex(size_t index) noexcept;
    [[nodiscard]] const std::vector<StructField> &getFields() const noexcept;
    [[nodiscard]] std::optional<TypePtr> findFieldType(const std::string &fieldName) const;
    [[nodiscard]] std::optional<std::size_t> findFieldIndex(const std::string &fieldName) const;

private:
    const std::string name;
    StructKind structKind;
    std::vector<StructField> fields;
};

#endif //STRUCTTYPE_H
