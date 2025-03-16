//
// Created by vadim on 13.03.25.
//

#include "SymbolTable.h"

void SymbolTable::enterScope() {
    scopes.emplace_back();
}

void SymbolTable::exitScope() {
    scopes.pop_back();
}

void SymbolTable::insert(const std::string &name, llvm::AllocaInst *const alloca) {
    scopes.back()[name] = alloca;
}

llvm::AllocaInst *SymbolTable::lookup(const std::string &name) const {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        if (auto found = it->find(name); found != it->end()) {
            return found->second;
        }
    }
    return nullptr;
}
