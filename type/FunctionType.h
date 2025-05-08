//
// Created by vadim on 27.04.25.
//

#ifndef FUNCTIONTYPE_H
#define FUNCTIONTYPE_H

#include "Type.h"

class FunctionType final : public PrimitiveType {
public:
    FunctionType(TypePtr returnType,
                 std::vector<TypePtr> paramsType,
                 bool isVarArg);

    bool operator==(const Type &other) const override;

    [[nodiscard]] TypePtr returnType() const;

    [[nodiscard]] std::vector<TypePtr> parametersType() const;

    [[nodiscard]] std::string getName() const override;

    bool isVariadic() const;

private:
    TypePtr retType;
    std::vector<TypePtr> paramsType;
    bool isVarArgs;
};

using FunctionPtr = std::shared_ptr<const FunctionType>;

#endif //FUNCTIONTYPE_H
