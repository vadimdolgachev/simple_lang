//
// Created by vadim on 28.08.2025.
//

#include <cstdarg>

#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>

#include "Interpreter.h"

const llvm::ExitOnError ExitOnError;

Interpreter::Interpreter(PrintfFunctionType printfCallback) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    llvmJit = ExitOnError(llvm::orc::KaleidoscopeJIT::Create());
    llvm::orc::MangleAndInterner mangle(llvmJit->getMainJITDylib().getExecutionSession(), llvmJit->getDataLayout());
    if (printfCallback != nullptr) {
        llvm::orc::SymbolMap symbols = {
                {mangle("printf"),
                 {llvm::orc::ExecutorAddr::fromPtr<std::remove_reference_t<PrintfFunctionType>>(printfCallback),
                  llvm::JITSymbolFlags(llvm::JITSymbolFlags::Callable | llvm::JITSymbolFlags::Exported)}}};
        ExitOnError(llvmJit->getMainJITDylib().define(absoluteSymbols(std::move(symbols))));
    }
}

void Interpreter::execute(ModuleContext context, const std::string_view launchFunctionName) {
    context.module->setDataLayout(llvmJit->getDataLayout());
    const auto resourceTracker = llvmJit->getMainJITDylib().createResourceTracker();
    ExitOnError(llvmJit->addModule(llvm::orc::ThreadSafeModule(std::move(context.module), std::move(context.context)),
                                   resourceTracker));
    const auto launchSymbolDef = ExitOnError(llvmJit->lookup(launchFunctionName));
    auto *const launchFunctionPtr = launchSymbolDef.getAddress().toPtr<void (*)()>();
    launchFunctionPtr();
    ExitOnError(resourceTracker->remove());
}
