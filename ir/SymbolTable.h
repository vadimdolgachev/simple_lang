//
// Created by vadim on 13.03.25.
//

#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include <deque>
#include <unordered_map>
#include <string>
#include <unordered_set>

#include <llvm/IR/Instructions.h>

#include "ast/ProtoFunctionStatement.h"
#include "ast/TypeNode.h"

template<typename T>
struct SymbolInfo final {
    TypeNode type;
    bool isConst = false;
    T *value = nullptr;
};

class SymbolTable final {
public:
    void enterScope();

    void exitScope();

    void insert(const std::string &name, TypeNode type, llvm::AllocaInst *alloca);

    [[nodiscard]] std::optional<SymbolInfo<llvm::AllocaInst>> lookup(const std::string &name) const;

    void insert(const std::string &name, TypeNode type, llvm::GlobalVariable *value);

    [[nodiscard]] std::optional<SymbolInfo<llvm::GlobalVariable>> lookupGlobal(
            const std::string &name) const;

    void insert(std::unique_ptr<ProtoFunctionStatement> proto);

    [[nodiscard]] ProtoFunctionStatement *lookupFunction(const std::string &name) const;

private:
    std::deque<std::unordered_map<std::string, SymbolInfo<llvm::AllocaInst>>> scopes;
    std::unordered_map<std::string, SymbolInfo<llvm::GlobalVariable>> globalValues;
    std::unordered_set<std::unique_ptr<ProtoFunctionStatement>> functions;
};

#endif //SYMBOLTABLE_H
