//
// Created by vadim on 27.04.25.
//

#ifndef TYPEFACTORY_H
#define TYPEFACTORY_H

#include <unordered_map>

#include "FunctionType.h"
#include "Type.h"

class TypeFactory {
public:
    TypeFactory() = delete;
    TypeFactory(const TypeFactory &) = delete;
    TypeFactory &operator=(const TypeFactory &) = delete;

    static TypePtr makePrimitiveType(TypeKind kind);
    static TypePtr makePointer(const TypePtr &base);
    static FunctionPtr makeFunction(const TypePtr &returnType,
                                    const std::vector<TypePtr> &paramTypes,
                                    bool isVarArg = false);

private:
    static std::unordered_map<TypeKind, TypePtr> typeCache;
    static std::unordered_map<TypePtr, TypePtr> pointerCache;
};

#endif //TYPEFACTORY_H
