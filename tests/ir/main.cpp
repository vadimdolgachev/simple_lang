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
    programLog.append(buf);
}

namespace {
    void execProgram(const std::string &text) {
        ModuleContext moduleContext("my cool jit");
        for (const auto &[name, signatures]: BuiltinSymbols::getInstance().getFunctions()) {
            moduleContext.symTable.insertFunction(name, signatures[0]);
        }
        CompilerFronted compiler(std::make_unique<std::istringstream>(text),
                                 BuiltinSymbols::getInstance().getFunctions());
        compiler.generateIR(moduleContext);
        moduleContext.module->print(llvm::errs(), nullptr);
        compiler.optimizeModule(*moduleContext.module, llvm::OptimizationLevel::O0);
        Interpreter interpreter(irTestPrint);
        programLog.clear();
        interpreter.execute(std::move(moduleContext), "main");
    }

    void testGlobalStrVar() {
        execProgram(R"(
            text: str = "Hello World!";
            fn main() {
                printf("%s\n", text);
                text = "New string";
                printf("%s\n", text);
            }
        )");
        assert(programLog == "Hello World!\nNew string\n");
    }

    void testGlobalIntVar() {
        execProgram(R"(
            i: int = 1;
            fn main() {
                printf("%d\n", i);
                i = 2;
                printf("%d\n", i);
            }
        )");
        assert(programLog == "1\n2\n");
    }

    void testArrayIndexRead() {
        execProgram(R"(
            a: [int; 1] = [1];
            fn main() {
                printf("a=%d\n", a[0]);
            }
        )");
        assert(programLog == "a=1\n");
    }

    void testArrayIndexWrite() {
        execProgram(R"(
            a: [int; 2] = [1, 2];
            fn main() {
                a[0] = 3;
                a[1] = 4;
                printf("a=[%d, %d]\n", a[0], a[1]);
            }
        )");
        assert(programLog == "a=[3, 4]\n");
    }
} // namespace

int main() {
    testGlobalStrVar();
    testGlobalIntVar();
    testArrayIndexRead();
    testArrayIndexWrite();
    return 0;
}
