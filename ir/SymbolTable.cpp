//
// Created by vadim on 13.03.25.
//

#include "SymbolTable.h"

#include <ranges>

void SymbolTable::enterScope() {
    scopes.emplace_back();
}

void SymbolTable::exitScope() {
    scopes.pop_back();
}

void SymbolTable::insert(const std::string &name,
                         TypeNode type,
                         llvm::AllocaInst *const alloca) {
    scopes.back().emplace(name, SymbolInfo{std::move(type), false, alloca});
}

void SymbolTable::insert(const std::string &name, TypeNode type, llvm::GlobalVariable *value) {
    globalValues.emplace(name, SymbolInfo{std::move(type), false, value});
}

void SymbolTable::insert(std::unique_ptr<ProtoFunctionStatement> proto) {
    functions.insert(std::move(proto));
}

ProtoFunctionStatement *SymbolTable::lookupFunction(const std::string &name) const {
    if (const auto it = std::ranges::find_if(functions, [&name](const auto &f) {
        return f->name == name;
    }); it != functions.end()) {
        return it->get();
    }
    return {};
}

std::optional<SymbolInfo<llvm::AllocaInst>> SymbolTable::lookup(const std::string &name) const {
    for (const auto &scope: std::ranges::reverse_view(scopes)) {
        if (auto found = scope.find(name); found != scope.end()) {
            return found->second;
        }
    }
    return {};
}

std::optional<SymbolInfo<llvm::GlobalVariable>> SymbolTable::lookupGlobal(const std::string &name) const {
    if (const auto it = globalValues.find(name); it != globalValues.end()) {
        return it->second;
    }
    return {};
}
