//
// Created by vadim on 27.04.25.
//

#include "FunctionType.h"

FunctionType::FunctionType(TypePtr returnType,
                           std::vector<TypePtr> paramsType,
                           const bool isVarArg):
    PrimitiveType(TypeKind::Function),
    retType(std::move(returnType)),
    paramsType(std::move(paramsType)),
    isVarArgs(isVarArg) {}

bool FunctionType::operator==(const Type &other) const {
    if (const auto *fType = dynamic_cast<const FunctionType *>(&other)) {
        return *fType->retType == *retType &&
               std::equal(paramsType.begin(), paramsType.end(),
                          fType->paramsType.begin(),
                          [](const auto &a, const auto &b) {
                              return *a == *b;
                          });
    }
    return false;
}

TypePtr FunctionType::returnType() const {
    return retType;
}

std::vector<TypePtr> FunctionType::parametersType() const {
    return paramsType;
}

std::string FunctionType::getName() const {
    std::string params;
    for (const auto &param: paramsType) {
        params += param->getName() + ", ";
    }
    if (!paramsType.empty()) {
        params = params.substr(0, params.size() - 2);
    }
    return std::format("function({}) -> {}", params, retType->getName());
}

bool FunctionType::isVariadic() const {
    return isVarArgs;
}
