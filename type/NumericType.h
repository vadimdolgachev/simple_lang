//
// Created by vadim on 27.04.25.
//

#ifndef NUMERICTYPE_H
#define NUMERICTYPE_H

#include "Type.h"

class NumericType final : public PrimitiveType {
public:
    explicit NumericType(TypeKind kind);

    BoolResult canCastTo(const TypePtr &target, CastMode mode) const override;

    ResultType getCommonType(const TypePtr &other) const override;

    ResultType getResultTypeUnary(TokenType op) const override;

    ResultType getComparableType(const TypePtr &type) const override;
};

#endif //NUMERICTYPE_H
