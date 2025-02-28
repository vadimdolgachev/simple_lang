#include <iostream>
#include <memory>
#include <sstream>
#include <utility>
#include <vector>
#include <unordered_map>
#include <cstdarg>

#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <llvm/IR/Verifier.h>

#include "KaleidoscopeJIT.h"
#include "Lexer.h"
#include "ast/ProtoFunctionStatement.h"
#include "ir/LLVMCodegen.h"

#include "Parser.h"

namespace {
    std::unique_ptr<llvm::LLVMContext> llvmContext;
    std::unique_ptr<llvm::Module> llvmModule;
    std::unique_ptr<llvm::IRBuilder<>> llvmIRBuilder;
    std::unique_ptr<llvm::orc::KaleidoscopeJIT> llvmJit;
    std::unordered_map<std::string, llvm::Value *> namedValues;
    std::unique_ptr<llvm::FunctionPassManager> functionPassManager;
    std::unique_ptr<llvm::LoopAnalysisManager> loopAnalysisManager;
    std::unique_ptr<llvm::FunctionAnalysisManager> functionAnalysisManager;
    std::unique_ptr<llvm::CGSCCAnalysisManager> cGSCCAnalysisManager;
    std::unique_ptr<llvm::ModuleAnalysisManager> moduleAnalysisManager;
    std::unique_ptr<llvm::PassInstrumentationCallbacks> passInstsCallbacks;
    std::unique_ptr<llvm::StandardInstrumentations> standardInsts;
    const llvm::ExitOnError ExitOnError;

    void initLlvmModules() {
        llvmContext = std::make_unique<llvm::LLVMContext>();
        llvmModule = std::make_unique<llvm::Module>("my cool jit", *llvmContext);
        llvmModule->setDataLayout(llvmJit->getDataLayout().getStringRepresentation());

        llvmIRBuilder = std::make_unique<llvm::IRBuilder<>>(*llvmContext);

        functionPassManager = std::make_unique<llvm::FunctionPassManager>();
        loopAnalysisManager = std::make_unique<llvm::LoopAnalysisManager>();
        functionAnalysisManager = std::make_unique<llvm::FunctionAnalysisManager>();
        cGSCCAnalysisManager = std::make_unique<llvm::CGSCCAnalysisManager>();
        moduleAnalysisManager = std::make_unique<llvm::ModuleAnalysisManager>();
        passInstsCallbacks = std::make_unique<llvm::PassInstrumentationCallbacks>();
        standardInsts = std::make_unique<llvm::StandardInstrumentations>(*llvmContext, /*DebugLogging*/ true);
        standardInsts->registerCallbacks(*passInstsCallbacks, moduleAnalysisManager.get());

        // Add transform passes.
        functionPassManager->addPass(llvm::VerifierPass());
        // Do simple "peephole" optimizations and bit-twiddling optzns.
        functionPassManager->addPass(llvm::InstCombinePass());
        // Reassociate expressions.
        functionPassManager->addPass(llvm::ReassociatePass());
        // Eliminate Common SubExpressions.
        functionPassManager->addPass(llvm::GVNPass());
        // Simplify the control flow graph (deleting unreachable blocks, etc).
        functionPassManager->addPass(llvm::SimplifyCFGPass());

        // Register analysis passes used in these transform passes.
        llvm::PassBuilder passBuilder;
        passBuilder.registerModuleAnalyses(*moduleAnalysisManager);
        passBuilder.registerFunctionAnalyses(*functionAnalysisManager);
        passBuilder.crossRegisterProxies(*loopAnalysisManager,
                                         *functionAnalysisManager,
                                         *cGSCCAnalysisManager,
                                         *moduleAnalysisManager);
    }

    std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement>> functionProtos;
    std::unordered_map<std::string, llvm::GlobalVariable *> globalValues;

    void print(const llvm::Value *const llvmIR) {
        llvm::outs() << "IR: ";
        llvmIR->print(llvm::outs(), true);
        llvm::outs() << '\n';
    }

    void print(const llvm::Module *const module) {
        llvm::outs() << "Module IR: ";
        module->print(llvm::outs(), nullptr);
        llvm::outs() << '\n';
    }

    extern "C" void libPrint(const char *fmt, ...) {
        va_list args;
        va_start(args, fmt);
        vprintf(fmt, args);
        va_end(args);
    }

    extern "C" void libPrintln(const char *fmt, ...) {
        va_list args;
        va_start(args, fmt);
        vprintf(fmt, args);
        va_end(args);
        putchar('\n');
    }

    void executeMain(const std::unique_ptr<Parser> &parser) {
        while (parser->hasNextNode()) {
            auto node = parser->nextNode();
            [[maybe_unused]] const auto *const llvmIR = LLVMCodegen::generate(node.get(),
                                                                              llvmContext,
                                                                              llvmIRBuilder,
                                                                              llvmModule,
                                                                              globalValues,
                                                                              functionProtos,
                                                                              namedValues);
        }

        const auto resourceTracker = llvmJit->getMainJITDylib().createResourceTracker();
        print(llvmModule.get());
        ExitOnError(llvmJit->addModule(
                llvm::orc::ThreadSafeModule(std::move(llvmModule), std::move(llvmContext)),
                resourceTracker));
        const auto mainSymbolDef = ExitOnError(llvmJit->lookup("main"));
        auto *const main = mainSymbolDef.getAddress().toPtr<void (*)()>();
        main();
        ExitOnError(resourceTracker->remove());
    }

    void defineEmbeddedFunctions() {
        llvm::orc::MangleAndInterner mangle(llvmJit->getMainJITDylib().getExecutionSession(),
                                            llvmJit->getDataLayout());
        llvm::orc::SymbolMap symbols;

        constexpr auto printlnName = "println";
        functionProtos[printlnName] = std::make_unique<ProtoFunctionStatement>(printlnName,
                                                                               std::vector<std::string>{"fmt"},
                                                                               true);
        symbols[mangle(printlnName)] = {
                llvm::orc::ExecutorAddr::fromPtr<decltype(libPrintln)>(&libPrintln),
                llvm::JITSymbolFlags(llvm::JITSymbolFlags::Callable | llvm::JITSymbolFlags::Exported)
        };

        constexpr auto printName = "print";
        functionProtos[printName] = std::make_unique<ProtoFunctionStatement>(printName,
                                                                             std::vector<std::string>{"fmt"},
                                                                             true);
        symbols[mangle(printName)] = {
                llvm::orc::ExecutorAddr::fromPtr<decltype(libPrint)>(&libPrint),
                llvm::JITSymbolFlags(llvm::JITSymbolFlags::Callable | llvm::JITSymbolFlags::Exported)
        };

        ExitOnError(llvmJit->getMainJITDylib().define(absoluteSymbols(std::move(symbols))));
    }
} // namespace

int main() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    llvmJit = ExitOnError(llvm::orc::KaleidoscopeJIT::Create());
    initLlvmModules();
    defineEmbeddedFunctions();

    const auto parser = std::make_unique<Parser>(std::make_unique<Lexer>(std::make_unique<std::istringstream>(R"(
        fn main() {
            message = "Hello, World%s";
            println(message, "!");
        }
    )")));
    auto stream = std::make_unique<std::istringstream>();
    stream->basic_ios::rdbuf(std::cin.rdbuf());
    const auto lexer = std::make_unique<Lexer>(std::move(stream));

    executeMain(parser);
    return 0;
}
