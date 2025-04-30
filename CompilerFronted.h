//
// Created by vadim on 15.04.25.
//

#ifndef COMPILERFRONTED_H
#define COMPILERFRONTED_H

#include <iosfwd>
#include <memory>

#include <llvm/IR/IRBuilder.h>

class Parser;
class Lexer;
class ModuleNode;

class CompilerFronted {
public:
    explicit CompilerFronted(std::unique_ptr<std::istream> stream);

    void generateIR(const std::unique_ptr<llvm::IRBuilder<>> &llvmIRBuilder,
                    const std::unique_ptr<llvm::Module> &llvmModule);

private:
    std::unique_ptr<ModuleNode> compile();
    static std::unique_ptr<ModuleNode> semanticAnalysis(std::unique_ptr<ModuleNode>);
    std::unique_ptr<ModuleNode> parse();

    std::unique_ptr<std::istream> stream;
};

#endif //COMPILERFRONTED_H
