//
// Created by vadim on 16.05.25.
//

#ifndef ARRAYTYPE_H
#define ARRAYTYPE_H

#include "Type.h"

class ArrayType final : public Type {
public:
    ArrayType(TypePtr elementType, size_t size);

    bool operator==(const Type &other) const override;

    [[nodiscard]] std::string getName() const override;

    [[nodiscard]] TypeKind getKind() const noexcept override;

    [[nodiscard]] TypePtr getElementType() const override;

    size_t size() const noexcept;

private:
    TypePtr elementType;
    const size_t arraySize;
};

using ArrayTypePtr = std::shared_ptr<const ArrayType>;

#endif //ARRAYTYPE_H
