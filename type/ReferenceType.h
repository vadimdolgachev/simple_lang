//
// Created by vadim on 20.08.2025.
//

#ifndef REFERENCETYPE_H
#define REFERENCETYPE_H

#include "Type.h"

class ReferenceType final : public Type {
public:
    explicit ReferenceType(std::string name) :
        name(std::move(name)) {}

    bool operator==(const Type &other) const override;

    [[nodiscard]] std::string getName() const override;

    [[nodiscard]] TypeKind getKind() const noexcept override;

private:
    const std::string name;
};

#endif //REFERENCETYPE_H