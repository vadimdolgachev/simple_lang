//
// Created by vadim on 23.03.25.
//

#ifndef MODULECONTEXT_H
#define MODULECONTEXT_H

#include "../SymbolTable.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

struct ModuleContext final {
    ModuleContext(const std::unique_ptr<llvm::Module> &llvmContext,
                  const std::unique_ptr<llvm::IRBuilder<>> &irBuilder):
        module(llvmContext),
        builder(irBuilder) {}

    ModuleContext(const ModuleContext &) = delete;
    ModuleContext &operator=(ModuleContext &) = delete;

    SymbolTable symTable;
    const std::unique_ptr<llvm::Module> &module;
    const std::unique_ptr<llvm::IRBuilder<>> &builder;
};

#endif //MODULECONTEXT_H
