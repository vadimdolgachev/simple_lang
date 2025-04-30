//
// Created by vadim on 15.04.25.
//

#include "CompilerFronted.h"
#include "Parser.h"
#include "Lexer.h"
#include "SemanticAnalyzer.h"

#include "ast/ModuleNode.h"

CompilerFronted::CompilerFronted(std::unique_ptr<std::istream> stream):
    stream(std::move(stream)) {}

void CompilerFronted::generateIR(const std::unique_ptr<llvm::IRBuilder<>> &llvmIRBuilder,
                                 const std::unique_ptr<llvm::Module> &llvmModule) {
    auto module = compile();
}

std::unique_ptr<ModuleNode> CompilerFronted::compile() {
    auto module = parse();
    module = semanticAnalysis(std::move(module));
    return module;
}

std::unique_ptr<ModuleNode> CompilerFronted::semanticAnalysis(std::unique_ptr<ModuleNode> module) {
    SemanticAnalyzer analyzer;
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
