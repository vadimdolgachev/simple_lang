//
// Created by vadim on 27.04.25.
//

#include <unordered_map>

#include "TypeFactory.h"
#include "BoolType.h"
#include "ByteType.h"
#include "CharType.h"
#include "FunctionType.h"
#include "NumericType.h"
#include "PointerType.h"
#include "StrType.h"
#include "VoidType.h"

std::unordered_map<TypeKind, TypePtr> TypeFactory::typeCache;
std::unordered_map<TypePtr, TypePtr> TypeFactory::pointerCache;

TypePtr TypeFactory::makeReference(const std::string &value) {
    return std::make_shared<ReferenceType>(value);
}

TypePtr TypeFactory::makePrimitiveType(TypeKind kind) {
    if (!typeCache.contains(kind)) {
        switch (kind) {
            case TypeKind::Integer:
            case TypeKind::Double:
                typeCache[kind] = std::make_shared<NumericType>(kind);
                break;
            case TypeKind::Void:
                typeCache[kind] = std::make_shared<VoidType>();
                break;
            case TypeKind::Str:
                typeCache[kind] = std::make_shared<StrType>();
                break;
            case TypeKind::Boolean:
                typeCache[kind] = std::make_shared<BoolType>();
                break;
            case TypeKind::Char:
                typeCache[kind] = std::make_shared<CharType>();
                break;
            case TypeKind::Byte:
                typeCache[kind] = std::make_shared<ByteType>();
                break;
            default:
                throw std::logic_error("Invalid primitive type");
        }
    }

    return typeCache[kind];
}

TypePtr TypeFactory::makePointer(const TypePtr &base) {
    if (!pointerCache.contains(base)) {
        pointerCache[base] = std::make_shared<PointerType>(base);
    }
    return pointerCache[base];
}

FunctionTypePtr TypeFactory::makeFunction(const TypePtr &returnType,
                                          const std::vector<TypePtr> &paramTypes,
                                          bool isVarArg) {
    return std::make_shared<FunctionType>(returnType, paramTypes, isVarArg);
}

ArrayTypePtr TypeFactory::makeArrayType(const TypePtr &elementType, size_t size) {
    return std::make_shared<ArrayType>(elementType, size);
}
