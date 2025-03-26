//
// Created by vadim on 19.03.25.
//

#ifndef TYPEMANAGER_H
#define TYPEMANAGER_H

#include "IRType.h"

#include "ast/TypeNode.h"

class TypeManager final {
public:
    void registerType(TypeKind typeKind, std::unique_ptr<IRType> type);
    IRType *getType(const std::unique_ptr<TypeNode> &typeNode) const;

    static TypeManager &getInstance();

private:
    std::unordered_map<TypeKind, std::unique_ptr<IRType>> types;
};

#endif // TYPEMANAGER_H
