//
// Created by vadim on 04.03.25.
//

#include "TypeNode.h"

TypeNode::TypeNode(const TypeKind type,
                   const bool isPointer,
                   std::optional<std::string> typeName):
    kind(type),
    isPointer(isPointer),
    typeName(std::move(typeName)) {}

bool TypeNode::operator==(const TypeNode &other) const {
    return kind == other.kind && isPointer == other.isPointer;
}

TypeNode TypeNode::makePrimitive(const TypeKind kind, const bool isPtr) {
    return {kind, isPtr, std::nullopt};
}

TypeNode TypeNode::makeCustom(const std::string &name, const bool isPtr) {
    return {TypeKind::Custom, isPtr, name};
}
