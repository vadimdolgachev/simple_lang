//
// Created by vadim on 15.04.25.
//

#include "CompilerFronted.h"
#include "Parser.h"
#include "Lexer.h"
#include "NodePrinter.h"
#include "SemanticAnalyzer.h"

#include "ast/ModuleNode.h"
#include "ir/LLVMCodegen.h"

CompilerFronted::CompilerFronted(std::unique_ptr<std::istream> stream,
                                 std::unordered_map<std::string, std::vector<SymbolInfoPtr>> builtinSymbols):
    stream(std::move(stream)),
    builtinSymbols(std::move(builtinSymbols)) {}

void CompilerFronted::generateIR(ModuleContext &moduleContext) {
    const auto module = compile();

    NodePrinter printer;
    module->visit(&printer);

    LLVMCodegen::generate(module.get(), moduleContext);
}

std::unique_ptr<ModuleNode> CompilerFronted::compile() {
    auto module = parse();
    module = semanticAnalysis(std::move(module), builtinSymbols);
    return module;
}

std::unique_ptr<ModuleNode> CompilerFronted::semanticAnalysis(std::unique_ptr<ModuleNode> module,
                                                              const std::unordered_map<
                                                                  std::string, std::vector<SymbolInfoPtr>> &
                                                              builtinSymbols) {
    SymbolTable symbolTable;
    for (const auto &[name, signatures]: builtinSymbols) {
        for (const auto &signature: signatures) {
            symbolTable.insertFunction(name, signature);
        }
    }
    SemanticAnalyzer analyzer(symbolTable);
    module->visit(&analyzer);
    return module;
}

std::unique_ptr<ModuleNode> CompilerFronted::parse() {
    const auto parser = std::make_unique<Parser>(
            std::make_unique<Lexer>(std::move(stream)));
    auto module = std::make_unique<ModuleNode>();
    while (parser->hasNextNode()) {
        module->statements.push_back(parser->nextNode());
    }
    return module;
}
