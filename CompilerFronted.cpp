//
// Created by vadim on 15.04.25.
//

#include "CompilerFronted.h"

#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>

#include "DeclarationCollector.h"
#include "Parser.h"
#include "Lexer.h"
#include "SemanticAnalyzer.h"
#include "ast/ModuleNode.h"
#include "ir/LLVMCodegen.h"

CompilerFronted::CompilerFronted(std::unique_ptr<std::istream> stream,
                                 std::unordered_map<std::string, std::vector<SymbolInfoPtr>> builtinSymbols) :
    stream(std::move(stream)),
    builtinSymbols(std::move(builtinSymbols)) {
}

void CompilerFronted::generateIR(ModuleContext &moduleContext) {
    const auto module = compile();
    LLVMCodegen::generate(module.get(), moduleContext);
}

void CompilerFronted::optimizeModule(llvm::Module &module, const llvm::OptimizationLevel OL) {
    llvm::LLVMContext &context = module.getContext();

    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;

    llvm::PassInstrumentationCallbacks PIC;
    llvm::StandardInstrumentations SI(context, true);
    SI.registerCallbacks(PIC, &MAM);

    llvm::PassBuilder passBuilder(nullptr, llvm::PipelineTuningOptions(), std::nullopt, &PIC);

    passBuilder.registerModuleAnalyses(MAM);
    passBuilder.registerCGSCCAnalyses(CGAM);
    passBuilder.registerFunctionAnalyses(FAM);
    passBuilder.registerLoopAnalyses(LAM);
    passBuilder.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    passBuilder.buildPerModuleDefaultPipeline(OL).run(module, MAM);
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
    DeclarationCollector declCollector;
    module->visit(&declCollector);

    SymbolTable symbolTable;
    for (const auto &[name, signatures]: builtinSymbols) {
        for (const auto &signature: signatures) {
            symbolTable.insertFunction(name, signature);
        }
    }

    SemanticAnalyzer analyzer(symbolTable, declCollector.getTypeDeclarations());
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
