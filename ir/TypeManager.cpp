//
// Created by vadim on 19.03.25.
//

#include "TypeManager.h"

void TypeManager::registerType(const TypeKind typeKind, std::unique_ptr<IRType> type) {
    types[typeKind] = std::move(type);
}

IRType *TypeManager::getType(const std::unique_ptr<TypeNode> &typeNode) const {
    if (const auto it = types.find(typeNode->kind); it != types.end()) {
        return it->second.get();
    }
    return nullptr;
}

TypeManager &TypeManager::getInstance() {
    static TypeManager typeManager;
    return typeManager;
}
