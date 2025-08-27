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

void SymbolTable::insert(const std::string &name, const std::shared_ptr<const SymbolInfo> &symbolInfo) {
    if (isDeclaredInCurrentScope(name)) {
        throw std::logic_error(std::format("'{}' is already declared in current scope", name));
    }
    scopes.back().emplace(name, symbolInfo);
}

std::optional<std::shared_ptr<const SymbolInfo>> SymbolTable::lookup(const std::string &name) const {
    for (const auto &scope: std::ranges::reverse_view(scopes)) {
        if (auto found = scope.find(name); found != scope.end()) {
            return found->second;
        }
    }
    return {};
}

void SymbolTable::insertFunction(const std::string &name, SymbolInfoPtr type) {
    functions[name].push_back(std::move(type));
}

std::vector<SymbolInfoPtr> SymbolTable::lookupFunction(const std::string &name) const {
    if (const auto signatures = functions.find(name); signatures != functions.end()) {
        return signatures->second;
    }
    return {};
}

std::string SymbolTable::mangleFunction(const std::string &name, const std::vector<TypePtr> &params) {
    std::ostringstream oss;
    oss << name;
    for (const auto &param: params) {
        oss << "_" << param->getName();
    }
    return oss.str();
}
