//
// Created by vadim on 13.03.25.
//

#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include <deque>
#include <unordered_map>
#include <string>
#include <utility>

#include <llvm/IR/Instructions.h>

#include "type/Type.h"

struct SymbolInfo {
    virtual ~SymbolInfo() = default;

    explicit SymbolInfo(TypePtr type, const bool isConst = false) :
        type(std::move(type)), isConst(isConst) {}

    TypePtr type;
    bool isConst = false;
};

using SymbolInfoPtr = std::shared_ptr<const SymbolInfo>;

struct AllocaInstSymbolInfo final : SymbolInfo {
    AllocaInstSymbolInfo(const TypePtr &type,
                         llvm::AllocaInst *const inst) :
        SymbolInfo(type),
        inst(inst) {}

    llvm::AllocaInst *const inst = nullptr;
};

struct GlobalSymbolInfo final : SymbolInfo {
    GlobalSymbolInfo(const TypePtr &type,
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

    void insert(const std::string &name, const SymbolInfoPtr &symbolInfo);

    [[nodiscard]] std::optional<SymbolInfoPtr> lookup(const std::string &name) const;

    void insertGlobal(const std::string &name, SymbolInfoPtr type);

    void insertFunction(const std::string &name, SymbolInfoPtr type);

    [[nodiscard]] std::optional<SymbolInfoPtr> lookupGlobal(const std::string &name) const;

    [[nodiscard]] std::vector<SymbolInfoPtr> lookupFunction(const std::string &name) const;

    static std::string mangleFunction(const std::string &name, const std::vector<TypePtr> &params);

private:
    std::deque<std::unordered_map<std::string, SymbolInfoPtr>> scopes;
    std::unordered_map<std::string, SymbolInfoPtr> globals;
    std::unordered_map<std::string, std::vector<SymbolInfoPtr>> functions;
};

#endif //SYMBOLTABLE_H
