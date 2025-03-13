//
// Created by vadim on 13.03.25.
//

#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include <deque>
#include <unordered_map>
#include <string>

#include <llvm/IR/Instructions.h>

class SymbolTable final {
public:
    void enterScope();

    void exitScope();

    void insert(const std::string &name, llvm::AllocaInst *alloca);

    [[nodiscard]] llvm::AllocaInst *lookup(const std::string &name) const;

private:
    std::deque<std::unordered_map<std::string, llvm::AllocaInst *>> scopes;
};

#endif //SYMBOLTABLE_H
