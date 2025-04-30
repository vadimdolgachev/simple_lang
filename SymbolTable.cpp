//
// Created by vadim on 13.03.25.
//

#include "SymbolTable.h"

#include <ranges>
#include <sstream>
#include <utility>

void SymbolTable::enterScope() {
    scopes.emplace_back();
}

void SymbolTable::exitScope() {
    scopes.pop_back();
}

bool SymbolTable::isDeclaredInCurrentScope(const std::string &name) const {
    return !scopes.empty() && scopes.back().contains(name);
}

void SymbolTable::insert(const std::string &name, std::shared_ptr<SymbolInfo> symbolInfo) {
    if (isDeclaredInCurrentScope(name)) {
        throw std::logic_error("Symbol " + name + " already declared");
    }
    scopes.back().emplace(name, symbolInfo);
}

std::optional<std::shared_ptr<SymbolInfo>> SymbolTable::lookup(const std::string &name) const {
    for (const auto &scope: std::ranges::reverse_view(scopes)) {
        if (auto found = scope.find(name); found != scope.end()) {
            return found->second;
        }
    }
    return {};
}

void SymbolTable::insertGlobal(const std::string &name, std::shared_ptr<SymbolInfo> type) {
    globalValues.emplace(name, type);
}

std::optional<std::shared_ptr<SymbolInfo>> SymbolTable::lookupGlobal(const std::string &name) const {
    const auto type = globalValues.find(name);
    return type != globalValues.end() ? std::make_optional(type->second) : std::nullopt;
}

std::string SymbolTable::mangleFunction(const std::string &name, const std::vector<TypePtr> &params) {
    std::ostringstream oss;
    oss << name;
    for (const auto &param: params) {
        oss << "_" << param->getName();
    }
    return oss.str();
}
