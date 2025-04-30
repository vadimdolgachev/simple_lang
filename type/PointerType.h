//
// Created by vadim on 27.04.25.
//

#ifndef POINTERTYPE_H
#define POINTERTYPE_H

#include "Type.h"

class PointerType final : public PrimitiveType {
public:
    explicit PointerType(TypePtr baseType);

    bool operator==(const Type &other) const override;

    [[nodiscard]] std::string getName() const override;

private:
    const TypePtr baseType;
};

#endif //POINTERTYPE_H
