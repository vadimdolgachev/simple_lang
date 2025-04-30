//
// Created by vadim on 13.03.25.
//

#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include <deque>
#include <unordered_map>
#include <string>

#include <llvm/IR/Instructions.h>

#include "type/Type.h"

struct SymbolInfo {
    virtual ~SymbolInfo() = default;

    explicit SymbolInfo(const TypePtr &type, const bool isConst = false) :
        type(type), isConst(isConst) {}

    TypePtr type;
    bool isConst = false;
};

struct AllocaInstSymbol final : SymbolInfo {
    AllocaInstSymbol(const TypePtr &type,
                     llvm::AllocaInst *const inst) :
        SymbolInfo(type),
        inst(inst) {}

    llvm::AllocaInst *const inst = nullptr;
};

struct GlobalSymbol final : SymbolInfo {
    GlobalSymbol(const TypePtr &type,
                 llvm::GlobalVariable *const var) :
        SymbolInfo(type),
        var(var) {}

    llvm::GlobalVariable *const var = nullptr;
};

class SymbolTable final {
public:
    void enterScope();

    void exitScope();

    bool isDeclaredInCurrentScope(const std::string &name) const;

    void insert(const std::string &name, std::shared_ptr<SymbolInfo> symbolInfo);

    [[nodiscard]] std::optional<std::shared_ptr<SymbolInfo>> lookup(const std::string &name) const;

    void insertGlobal(const std::string &name, std::shared_ptr<SymbolInfo> type);

    [[nodiscard]] std::optional<std::shared_ptr<SymbolInfo>> lookupGlobal(const std::string &name) const;

    static std::string mangleFunction(const std::string &name, const std::vector<TypePtr> &params);

private:
    std::deque<std::unordered_map<std::string, std::shared_ptr<SymbolInfo>>> scopes;
    std::unordered_map<std::string, std::shared_ptr<SymbolInfo>> globalValues;
};

#endif //SYMBOLTABLE_H
