//
// Created by vadim on 23.03.25.
//

#ifndef MODULECONTEXT_H
#define MODULECONTEXT_H

#include "../SymbolTable.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

struct ModuleContext final {
    explicit ModuleContext(const std::string_view moduleID) :
        context(std::make_unique<llvm::LLVMContext>()),
        module(std::make_unique<llvm::Module>(moduleID, *context)),
        builder(std::make_unique<llvm::IRBuilder<>>(*context)) {
    }

    ModuleContext(ModuleContext &&other) noexcept :
        symTable(std::move(other.symTable)),
        context(std::move(other.context)),
        module(std::move(other.module)) {
    }

    ModuleContext &operator=(ModuleContext &&other) noexcept {
        symTable = std::move(other.symTable);
        context = std::move(other.context);
        module = std::move(other.module);
        return *this;
    }

    SymbolTable symTable;
    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
    const std::unique_ptr<llvm::IRBuilder<>> builder;
};

#endif //MODULECONTEXT_H
