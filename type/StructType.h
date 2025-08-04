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

struct Field final {
    std::optional<std::string> name;
    TypePtr type;
};

class StructType final : public Type {
public:
    explicit StructType(std::string name,
                        StructKind kind,
                        std::vector<Field> fields);

    bool operator==(const Type &other) const override;
    [[nodiscard]] std::string getName() const override;
    [[nodiscard]] TypeKind getKind() const noexcept override;
    [[nodiscard]] StructKind getStructKind() const noexcept;
    [[nodiscard]] const std::vector<Field> &getFields() const noexcept;

private:
    const std::string name;
    StructKind structKind;
    std::vector<Field> fields;
};

using StructTypePtr = std::shared_ptr<const StructType>;

#endif //STRUCTTYPE_H
