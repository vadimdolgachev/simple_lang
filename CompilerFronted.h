//
// Created by vadim on 15.04.25.
//

#ifndef COMPILERFRONTED_H
#define COMPILERFRONTED_H

#include <iosfwd>
#include <memory>

#include <llvm/IR/IRBuilder.h>
#include <llvm/Passes/OptimizationLevel.h>

#include "ir/ModuleContext.h"

class ModuleNode;

class CompilerFronted final {
public:
    CompilerFronted(std::unique_ptr<std::istream> stream,
                    std::unordered_map<std::string, std::vector<SymbolInfoPtr>> builtinSymbols);

    void generateIR(ModuleContext &moduleContext);
    void optimizeModule(llvm::Module &module, llvm::OptimizationLevel OL = llvm::OptimizationLevel::O2);
    void verifyModule(const llvm::Module &module);

private:
    std::unique_ptr<ModuleNode> compile();
    static std::unique_ptr<ModuleNode> semanticAnalysis(std::unique_ptr<ModuleNode>,
                                                        const std::unordered_map<
                                                            std::string, std::vector<SymbolInfoPtr>> &
                                                        builtinSymbols);
    std::unique_ptr<ModuleNode> parse();

    std::unique_ptr<std::istream> stream;
    const std::unordered_map<std::string, std::vector<SymbolInfoPtr>> builtinSymbols;
};

#endif //COMPILERFRONTED_H
