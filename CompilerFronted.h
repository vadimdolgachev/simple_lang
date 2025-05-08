//
// Created by vadim on 15.04.25.
//

#ifndef COMPILERFRONTED_H
#define COMPILERFRONTED_H

#include <iosfwd>
#include <memory>

#include <llvm/IR/IRBuilder.h>

#include "ir/ModuleContext.h"

class Parser;
class Lexer;
class ModuleNode;

class CompilerFronted {
public:
    explicit CompilerFronted(std::unique_ptr<std::istream> stream,
                             std::unordered_map<std::string, std::vector<SymbolInfoPtr>> builtinSymbols);

    void generateIR(ModuleContext &moduleContext);

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
