//
// Created by vadim on 14.12.24.
//

#include <memory>
#include <sstream>
#include <cstdarg>

#include "BuiltinSymbols.h"
#include "CompilerFronted.h"
#include "Interpreter.h"

namespace {
    std::string programLog;
} // namespace

extern "C" void irTestPrint(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    char *buf = nullptr;
    vasprintf(&buf, fmt, args);
    std::unique_ptr<char, void (*)(char *)> ptr(buf, [](char *const p) {
        free(p);
    });
    va_end(args);
    printf("%s", buf);
    programLog.append(buf);
}

void testGlobalStrVar() {
    ModuleContext moduleContext("my cool jit");
    for (const auto &[name, signatures]: BuiltinSymbols::getInstance().getFunctions()) {
        moduleContext.symTable.insertFunction(name, signatures[0]);
    }
    CompilerFronted compiler(std::make_unique<std::istringstream>(R"(
            text: str = "Hello World!";
            fn main() {
                printf("%s", text);
            }
        )"), BuiltinSymbols::getInstance().getFunctions());
    compiler.generateIR(moduleContext);
    moduleContext.module->print(llvm::errs(), nullptr);
    compiler.optimizeModule(*moduleContext.module, llvm::OptimizationLevel::O0);
    Interpreter interpreter(irTestPrint);
    interpreter.execute(std::move(moduleContext), "main");
    assert(programLog == "Hello World!");
    programLog.clear();
}

int main() {
    testGlobalStrVar();
    return 0;
}
