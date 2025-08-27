#include <memory>
#include <sstream>
#include <utility>
#include <cstdarg>

#include <llvm/Analysis/MemorySSA.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils/Mem2Reg.h>

#include "BuiltinSymbols.h"
#include "CompilerFronted.h"
#include "KaleidoscopeJIT.h"

namespace {
    std::unique_ptr<llvm::LLVMContext> llvmContext;
    std::unique_ptr<llvm::Module> llvmModule;
    std::unique_ptr<llvm::IRBuilder<>> llvmIRBuilder;
    std::unique_ptr<llvm::orc::KaleidoscopeJIT> llvmJit;
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

        llvmModule->setDataLayout(llvmJit->getDataLayout());

        llvmIRBuilder = std::make_unique<llvm::IRBuilder<>>(*llvmContext);

        functionPassManager = std::make_unique<llvm::FunctionPassManager>();
        loopAnalysisManager = std::make_unique<llvm::LoopAnalysisManager>();
        functionAnalysisManager = std::make_unique<llvm::FunctionAnalysisManager>();
        cGSCCAnalysisManager = std::make_unique<llvm::CGSCCAnalysisManager>();
        moduleAnalysisManager = std::make_unique<llvm::ModuleAnalysisManager>();
        passInstsCallbacks = std::make_unique<llvm::PassInstrumentationCallbacks>();
        standardInsts = std::make_unique<llvm::StandardInstrumentations>(
                *llvmContext, /*DebugLogging*/ true);
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
        functionPassManager->addPass(llvm::PromotePass());
        functionPassManager->addPass(llvm::InstCombinePass());
        functionPassManager->addPass(llvm::ReassociatePass());

        // Register analysis passes used in these transform passes.
        llvm::PassBuilder passBuilder;
        passBuilder.registerModuleAnalyses(*moduleAnalysisManager);
        passBuilder.registerFunctionAnalyses(*functionAnalysisManager);
        passBuilder.crossRegisterProxies(*loopAnalysisManager,
                                         *functionAnalysisManager,
                                         *cGSCCAnalysisManager,
                                         *moduleAnalysisManager);
    }

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

    void executeMain() {
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

    void defineEmbeddedFunctions(ModuleContext &cm) {
        llvm::orc::MangleAndInterner mangle(llvmJit->getMainJITDylib().getExecutionSession(),
                                            llvmJit->getDataLayout());
        llvm::orc::SymbolMap symbols;

        for (const auto &[name, signatures]: BuiltinSymbols::getInstance().getFunctions()) {
            cm.symTable.insertFunction(name, signatures[0]);
            const auto printFun = name == "println" ? &libPrintln : &libPrint;
            symbols[mangle(name)] = {
                    llvm::orc::ExecutorAddr::fromPtr<std::remove_reference_t<decltype(*printFun)>>(printFun),
                    llvm::JITSymbolFlags(
                            llvm::JITSymbolFlags::Callable | llvm::JITSymbolFlags::Exported)
            };
        }

        ExitOnError(llvmJit->getMainJITDylib().define(absoluteSymbols(std::move(symbols))));
    }
} // namespace

int main() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    llvmJit = ExitOnError(llvm::orc::KaleidoscopeJIT::Create());
    initLlvmModules();
    ModuleContext moduleContext(llvmModule, llvmIRBuilder);
    defineEmbeddedFunctions(moduleContext);

    CompilerFronted compiler(std::make_unique<std::istringstream>(
    R"(
        globalInt: int = 1;
        globalText: str = "Hello";
        fn getStr(): str {
            return globalText;
        }
        globalText2: str = globalText;
        globalText3: str = globalText2;

        fn main() {
            greeting: Greeting = Greeting {hello: "Hello"};
            println("%s", greeting.hello);
            text: str = getStr();
            println("%s", text);
            println("%s", getStr());
        }

        struct Greeting {
            hello: str;
        }
    )"), BuiltinSymbols::getInstance().getFunctions());

    compiler.generateIR(moduleContext);
    executeMain();
    return 0;
}
