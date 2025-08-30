#include <memory>
#include <utility>
#include <fstream>
#include <iostream>

#include <llvm/IR/Module.h>

#include "BuiltinSymbols.h"
#include "CompilerFronted.h"
#include "Interpreter.h"

namespace {
    struct ProgramArgs final {
        bool execute;
        std::string inputFile;
    };

    void printHelp(const char *progName) {
        std::cout << "Usage: " << progName << " [options] [file]\n\n"
                << "Options:\n"
                << "  -e, --exec       Execute the program after compilation\n"
                << "  -h, --help       Show this help message\n\n"
                << "Arguments:\n"
                << "  file             Input source file (required)\n";
    }

    ProgramArgs parseArgs(const int argc, char **argv) {
        ProgramArgs programArgs = {.execute = false, .inputFile = ""};

        std::string fileArg;

        for (int i = 1; i < argc; i++) {
            if (const std::string_view arg{argv[i]}; arg == "--exec" || arg == "-e") {
                programArgs.execute = true;
            } else if (arg == "--help" || arg == "-h") {
                printHelp(argv[0]);
                std::exit(0);
            } else if (arg.starts_with('-')) {
                std::cerr << "Unknown option: " << arg << "\n";
                printHelp(argv[0]);
                std::exit(1);
            } else {
                if (!fileArg.empty()) {
                    std::cerr << "Error: multiple input files specified: " << fileArg << " and " << arg << "\n";
                    printHelp(argv[0]);
                    std::exit(1);
                }
                fileArg = arg;
            }
        }

        if (fileArg.empty()) {
            std::cerr << "Error: input file is required\n";
            printHelp(argv[0]);
            std::exit(1);
        }

        programArgs.inputFile = fileArg;
        return programArgs;
    }
} // namespace

int main(int argc, char **argv) {
    const auto [execute, inputFile] = parseArgs(argc, argv);
    ModuleContext moduleContext("my cool jit");
    for (const auto &[name, signatures]: BuiltinSymbols::getInstance().getFunctions()) {
        moduleContext.symTable.insertFunction(name, signatures[0]);
    }

    CompilerFronted compiler(std::make_unique<std::ifstream>(inputFile),
                             BuiltinSymbols::getInstance().getFunctions());
    compiler.generateIR(moduleContext);
    compiler.optimizeModule(*moduleContext.module);
    compiler.verifyModule(*moduleContext.module);
    moduleContext.module->print(llvm::outs(), nullptr);

    if (execute) {
        Interpreter interpreter;
        interpreter.execute(std::move(moduleContext), "main");
    }
    return 0;
}
