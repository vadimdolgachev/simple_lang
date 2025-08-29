//
// Created by vadim on 28.08.2025.
//

#ifndef INTERPRETER_H
#define INTERPRETER_H

#include "KaleidoscopeJIT.h"
#include "ir/ModuleContext.h"

using PrintfFunctionType = void (const char *fmt, ...);

class Interpreter final {
public:
    explicit Interpreter(PrintfFunctionType printfCallback = nullptr);
    Interpreter(const Interpreter &) = delete;
    Interpreter &operator=(const Interpreter &) = delete;

    void execute(ModuleContext context, std::string_view launchFunctionName);

private:
    std::unique_ptr<llvm::orc::KaleidoscopeJIT> llvmJit;
};

#endif //INTERPRETER_H
