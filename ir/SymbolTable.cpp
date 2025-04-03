//
// Created by vadim on 13.03.25.
//

#include "SymbolTable.h"

#include <ranges>
#include <utility>

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
    //TODO: mangle name
    auto name = proto->name;
    functions.emplace(std::move(name), std::move(proto));
}

std::optional<FuncSymbol> SymbolTable::lookupFunction(const std::string &name) const {
    if (const auto function = functions.find(name); function != functions.end()) {
        return function->second;
    }
    return {};
}

std::optional<SymbolInfo<TypeNode, llvm::AllocaInst *>> SymbolTable::lookup(const std::string &name) const {
    for (const auto &scope: std::ranges::reverse_view(scopes)) {
        if (auto found = scope.find(name); found != scope.end()) {
            return found->second;
        }
    }
    return {};
}

std::optional<SymbolInfo<TypeNode, llvm::GlobalVariable *>> SymbolTable::lookupGlobal(const std::string &name) const {
    if (const auto it = globalValues.find(name); it != globalValues.end()) {
        return it->second;
    }
    return {};
}
