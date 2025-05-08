//
// Created by vadim on 04.05.25.
//

#ifndef BUILTINSYMBOLS_H
#define BUILTINSYMBOLS_H

#include <string>
#include <unordered_map>
#include <vector>

#include "SymbolTable.h"

class BuiltinSymbols final {
public:
    const std::unordered_map<std::string, std::vector<SymbolInfoPtr>> &getFunctions() const;

    static const BuiltinSymbols &getInstance();

    BuiltinSymbols(const BuiltinSymbols &) = delete;
    BuiltinSymbols &operator=(const BuiltinSymbols &) = delete;

private:
    BuiltinSymbols();

    std::unordered_map<std::string, std::vector<SymbolInfoPtr>> builtinFunctions;
};

#endif //BUILTINSYMBOLS_H
